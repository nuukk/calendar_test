# 목차
- [설정](#설정)
  - [Supabase](#supabase)
    - [테이블 생성 및 RLS 설정](#테이블-생성-및-rls-설정)
  - [Authentication](#authentication)
- [Google Cloud](#google-cloud)

## 설정

### Supbase

#### 테이블 생성 및 RLS 설정
CREATE TABLE IF NOT EXISTS events (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  is_allday BOOLEAN DEFAULT TRUE,
  start_time TEXT,
  end_time TEXT,
  category TEXT,
  location TEXT,
  participants TEXT,
  creator_email TEXT NOT NULL,
  creator_name TEXT,
  google_calendar_id TEXT,
  google_event_id TEXT,
  google_synced BOOLEAN DEFAULT FALSE,
  memo_history JSONB DEFAULT '[]'::jsonb,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS major_schedules (
  id SERIAL PRIMARY KEY,
  isoweek INTEGER,
  date DATE NOT NULL,
  release_schedule TEXT,
  customer_work TEXT,
  note TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

create table public.delivable_items (
  id        bigint generated always as identity primary key,
  isoweek   integer not null,
  deadline  date not null,
  assignees text default ''::text,
  content   text not null,
  status    text not null check (status in ('DONE','Holding')),
  note      text default ''::text,
  inserted_at timestamptz default timezone('utc'::text, now()),
  updated_at  timestamptz default timezone('utc'::text, now())
);

ALTER TABLE events ENABLE ROW LEVEL SECURITY;
ALTER TABLE major_schedules ENABLE ROW LEVEL SECURITY;
ALTER TABLE delivable_items ENABLE ROW LEVEL SECURITY;

alter publication supabase_realtime add table public.events;
alter publication supabase_realtime add table public.major_schedules;
alter publication supabase_realtime add table public.delivable_items;

-- 권한 --
-- (C) authenticated: SELECT 전체 허용 (Realtime payload를 위해 핵심)
drop policy if exists authenticated_select_all_events on public.events;
create policy authenticated_select_all_events
on public.events
for select
to authenticated
using (true);

drop policy if exists authenticated_select_all_major on public.major_schedules;
create policy authenticated_select_all_major
on public.major_schedules
for select
to authenticated
using (true);

drop policy if exists authenticated_select_all_delivable on public.delivable_items;
create policy authenticated_select_all_delivable
on public.delivable_items
for select
to authenticated
using (true);

-- (D) authenticated: UPDATE/DELETE 전체 허용
drop policy if exists authenticated_update_all_events on public.events;
create policy authenticated_update_all_events
on public.events
for update
to authenticated
using (true)
with check (true);

drop policy if exists authenticated_delete_all_events on public.events;
create policy authenticated_delete_all_events
on public.events
for delete
to authenticated
using (true);

drop policy if exists authenticated_update_all_major on public.major_schedules;
create policy authenticated_update_all_major
on public.major_schedules
for update
to authenticated
using (true)
with check (true);

drop policy if exists authenticated_delete_all_major on public.major_schedules;
create policy authenticated_delete_all_major
on public.major_schedules
for delete
to authenticated
using (true);

drop policy if exists authenticated_update_all_delivable on public.delivable_items;
create policy authenticated_update_all_delivable
on public.delivable_items
for update
to authenticated
using (true)
with check (true);

drop policy if exists authenticated_delete_all_delivable on public.delivable_items;
create policy authenticated_delete_all_delivable
on public.delivable_items
for delete
to authenticated
using (true);

-- (E) (권장) 앱에서 추가(INSERT)도 하는 경우 같이 허용
drop policy if exists authenticated_insert_all_events on public.events;
create policy authenticated_insert_all_events
on public.events
for insert
to authenticated
with check (true);

drop policy if exists authenticated_insert_all_major on public.major_schedules;
create policy authenticated_insert_all_major
on public.major_schedules
for insert
to authenticated
with check (true);

drop policy if exists authenticated_insert_all_delivable on public.delivable_items;
create policy authenticated_insert_all_delivable
on public.delivable_items
for insert
to authenticated
with check (true);

-- (F) GRANT도 필요할 수 있음(프로젝트 기본값에 따라 다름)
grant usage on schema public to authenticated;
grant select, insert, update, delete on table public.events, public.major_schedules, public.delivable_items to authenticated;
grant usage, select on all sequences in schema public to authenticated;


### Authentication
* Authentication > URL Configuration > Redirect URLs에 다음을 입력한다.
    * https://nuukk-calendar-test.share.connect.posit.cloud/**
    * https://nuukk-calendar-test.share.connect.posit.cloud

* Authentication > Sign In / Providers > Auth Providers에서 Google을 Enable을 한다.
    * Client IDs와 Client Secret (for OAuth)에 Google Cloud Oauth 설정에서 얻은 값을 입력한다.


## Google Cloud
* Ouath 동의 화면 > 클라이언트 > 클라이언트 만들기
    * 웹 애플리케이션으로 설정한다
    * 승인된 JavaScript 원본에 https://nuukk-calendar-test.share.connect.posit.cloud를 입력한다.
    * 승인된 리디렉션 URI에 https://dlaactzmafpmrkvqtqlp.supabase.co/auth/v1/callback를 입력한다.


