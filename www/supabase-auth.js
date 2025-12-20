// www/supabase-auth.js
(() => {
  const cfg = window.__SB_CFG__ || {};

  // --- 1) Shiny가 준비되기 전 setInputValue 호출을 버퍼링 ---
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

  // 연결 이벤트가 누락되는 환경 대비: 처음 몇 초는 폴링으로도 플러시
  let pollN = 0;
  const poll = setInterval(() => {
    pollN += 1;
    const ok = flushPending();
    if ((ok && pending.length === 0) || pollN > 40) clearInterval(poll); // 약 10초
  }, 250);

  // --- 2) Supabase client 준비 ---
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

  // --- 3) 로그인 버튼 ---
  document.addEventListener(
    "click",
    async (e) => {
      const btn = e.target.closest("#sb_login_google");
      if (!btn) return;

      e.preventDefault();

      const redirectTo =
        (cfg.appBaseUrl && cfg.appBaseUrl.length > 0)
          ? cfg.appBaseUrl
          : (window.location.origin + window.location.pathname);

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

  // --- 4) Auth 상태 변화: 여기가 “로그인 성공 → Shiny로 email/JWT 전달” 핵심 ---
  sb.auth.onAuthStateChange((_event, session) => {
    pushSessionToShiny(session);
  });

  // --- 5) 초기 세션 복구: 두 번째 접속부터 자동 로그인 되게 하는 핵심 ---
  sb.auth.getSession().then(({ data }) => {
    pushSessionToShiny(data?.session ?? null);
  });

  // 서버에서 강제 로그아웃을 시키고 싶을 때 사용
  if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
    window.Shiny.addCustomMessageHandler("sb_signOut", async () => {
      await sb.auth.signOut();
      pushSessionToShiny(null);
    });
  }
})();