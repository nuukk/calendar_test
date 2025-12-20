(() => {
  const cfg = window.__SB_CFG__ || {};

  // -------------------------------------------------
  // 1) Shiny가 준비되기 전 setInputValue 호출을 버퍼링
  // -------------------------------------------------
  const pending = [];
  const canSend = () =>
    window.Shiny && typeof window.Shiny.setInputValue === "function";

  const flushPending = () => {
    if (!canSend()) return false;
    while (pending.length > 0) {
      const [name, value] = pending.shift();
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
    return true;
  };

  const sendToShiny = (name, value) => {
    if (canSend()) {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    } else {
      pending.push([name, value]);
    }
  };

  document.addEventListener("shiny:connected", () => {
    flushPending();
    if (window.__sb_last_session) {
      const s = window.__sb_last_session;
      sendToShiny("sb_user_email", s?.user?.email ?? null);
      sendToShiny("sb_access_token", s?.access_token ?? null);
    }
  });

  // 연결 이벤트 누락 대비: 처음 몇 초는 폴링으로도 플러시
  let pollN = 0;
  const poll = setInterval(() => {
    pollN += 1;
    const ok = flushPending();
    if ((ok && pending.length === 0) || pollN > 40) clearInterval(poll); // 약 10초
  }, 250);

  // -------------------------------------------------
  // 2) Supabase client 준비
  // -------------------------------------------------
  if (!cfg.url || !cfg.anonKey) {
    console.error("[sb] missing cfg.url/cfg.anonKey");
    sendToShiny("sb_oauth_error", { message: "SUPABASE_URL/ANON_KEY 미설정" });
    return;
  }
  if (!window.supabase || typeof window.supabase.createClient !== "function") {
    console.error("[sb] supabase-js not loaded");
    sendToShiny("sb_oauth_error", { message: "supabase-js CDN 로딩 실패" });
    return;
  }

  const sb = window.supabase.createClient(cfg.url, cfg.anonKey, {
    auth: {
      persistSession: true,
      autoRefreshToken: true,
      detectSessionInUrl: true,
    },
  });

  window.__sb__ = sb;

  const cleanOauthParams = () => {
    try {
      const u = new URL(window.location.href);
      const keys = ["code", "state", "error", "error_description"];
      let changed = false;
      keys.forEach((k) => {
        if (u.searchParams.has(k)) {
          u.searchParams.delete(k);
          changed = true;
        }
      });
      if (changed) window.history.replaceState({}, document.title, u.toString());
    } catch (e) {}
  };

  const pushSessionToShiny = (session) => {
    window.__sb_last_session = session || null;
    sendToShiny("sb_user_email", session?.user?.email ?? null);
    sendToShiny("sb_access_token", session?.access_token ?? null);
    if (session) cleanOauthParams();
  };

  // -------------------------------------------------
  // 3) ✅ Realtime(Postgres Changes) 구독 (핵심)
  //    - events  -> sb_rt_events
  //    - major   -> sb_rt_major
  //    - delivable -> sb_rt_delivable
  // -------------------------------------------------
  const SCHEMA = cfg.schema && String(cfg.schema) ? String(cfg.schema) : "public";
  const tables = cfg.tables && typeof cfg.tables === "object" ? cfg.tables : {};

  // 채널 상태 + 재시도(중복 구독 방지/재구독)
  const rtState = {
    channels: {}, // { events: ch, major: ch, delivable: ch }
    retry: {}, // { events: n, major: n, delivable: n }
  };

  const rtStatus = (obj) => {
    sendToShiny("sb_rt_status", { ...obj, ts: Date.now() });
  };

  const setRealtimeAuth = (session) => {
    const token = session?.access_token || cfg.anonKey || "";
    try {
      if (sb.realtime && typeof sb.realtime.setAuth === "function") {
        sb.realtime.setAuth(token);
      }
    } catch (e) {
      console.warn("[sb] realtime.setAuth failed/unsupported:", e);
    }
  };

  const RETRY_BASE_MS = 800;
  const RETRY_MAX_MS = 30000;
  const backoffMs = (n) =>
    Math.min(RETRY_MAX_MS, RETRY_BASE_MS * Math.pow(2, Math.max(0, n - 1)));

  const removeCh = (ch) => {
    if (!ch) return;
    try {
      if (typeof sb.removeChannel === "function") sb.removeChannel(ch);
    } catch (e) {}
    try {
      if (typeof ch.unsubscribe === "function") ch.unsubscribe();
    } catch (e) {}
  };

  const stopRealtime = () => {
    const chans = rtState.channels || {};
    Object.keys(chans).forEach((k) => removeCh(chans[k]));
    rtState.channels = {};
    rtState.retry = {};
    rtStatus({ status: "STOPPED" });
  };

  const subscribeTable = (kind, tableName, shinyInputName) => {
    if (!tableName || !shinyInputName) return;

    // 이미 살아있는 채널이 있으면 재생성하지 않음
    if (rtState.channels[kind]) return;

    const channelName = `rt:${SCHEMA}:${tableName}:${kind}`;
    const ch = sb.channel(channelName);

    ch.on(
      "postgres_changes",
      { event: "*", schema: SCHEMA, table: tableName },
      (payload) => {
        sendToShiny(shinyInputName, { payload, ts: Date.now() });
      }
    );

    ch.subscribe((status) => {
      rtStatus({ kind, table: tableName, status });

      if (status === "SUBSCRIBED") {
        rtState.retry[kind] = 0;
        return;
      }

      if (status === "TIMED_OUT" || status === "CHANNEL_ERROR" || status === "CLOSED") {
        // 실패/종료 → 해당 채널 제거 후 백오프로 재시도
        removeCh(ch);
        if (rtState.channels[kind] === ch) delete rtState.channels[kind];

        const n = (rtState.retry[kind] || 0) + 1;
        rtState.retry[kind] = n;
        const wait = backoffMs(n);

        rtStatus({ kind, table: tableName, status: "RETRYING", retry: n, wait });

        setTimeout(() => {
          const sess = window.__sb_last_session || null;
          if (sess) ensureRealtime(sess);
        }, wait);
      }
    });

    rtState.channels[kind] = ch;
  };

  const ensureRealtime = (session) => {
    if (!session) {
      stopRealtime();
      return;
    }

    setRealtimeAuth(session);

    // ✅ 중요: "채널이 하나라도 있으면 return" 하지 말고,
    //         누락된 채널이 있으면 다시 붙인다.
    rtStatus({ status: "ENSURE", schema: SCHEMA });

    subscribeTable("events", tables.events, "sb_rt_events");
    subscribeTable("major", tables.major, "sb_rt_major");
    subscribeTable("delivable", tables.delivable, "sb_rt_delivable");
  };

  // -------------------------------------------------
  // 4) 로그인 버튼
  // -------------------------------------------------
  document.addEventListener(
    "click",
    async (e) => {
      const btn = e.target.closest("#sb_login_google");
      if (!btn) return;

      e.preventDefault();

      const redirectTo =
        cfg.appBaseUrl && String(cfg.appBaseUrl).length > 0
          ? String(cfg.appBaseUrl)
          : window.location.origin + window.location.pathname;

      const { error } = await sb.auth.signInWithOAuth({
        provider: "google",
        options: { redirectTo },
      });

      if (error) {
        console.error("[sb] OAuth error:", error);
        sendToShiny("sb_oauth_error", { message: error.message, ts: Date.now() });
        alert("Google OAuth 시작 실패: " + (error.message || "unknown"));
      }
    },
    true
  );

  // -------------------------------------------------
  // 5) Auth 상태 변화: email/JWT 전달 + ✅ Realtime 연결/갱신
  // -------------------------------------------------
  sb.auth.onAuthStateChange((event, session) => {
    pushSessionToShiny(session);

    if (session) {
      ensureRealtime(session);
    } else {
      stopRealtime();
    }

    rtStatus({ auth_event: event, authed: !!session });
  });

  // -------------------------------------------------
  // 6) 초기 세션 복구: 자동 로그인 + ✅ Realtime 시작
  // -------------------------------------------------
  sb.auth.getSession().then(({ data }) => {
    const session = data?.session ?? null;
    pushSessionToShiny(session);
    ensureRealtime(session);
  });

  // -------------------------------------------------
  // 7) 서버에서 강제 로그아웃
  // -------------------------------------------------
  if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
    window.Shiny.addCustomMessageHandler("sb_signOut", async () => {
      try {
        await sb.auth.signOut();
      } finally {
        pushSessionToShiny(null);
        stopRealtime();
      }
    });
  }
})();