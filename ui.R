# ui.R
APP_BASE_URL <- Sys.getenv("APP_BASE_URL", unset = "")  # ✅ Connect Cloud의 고정 URL(권장)

ui <- tagList(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/@supabase/supabase-js@2"),
    
    tags$script(HTML(sprintf(
      "window.__SB_CFG__ = %s;",
      jsonlite::toJSON(
        list(
          url = SUPABASE_URL,
          anonKey = SUPABASE_ANON_KEY,
          appBaseUrl = APP_BASE_URL,   # ✅ redirectTo를 고정하기 위한 값
          schema = "public",
          tables = list(
            events = SUPABASE_EVENTS_TABLE,
            major = SUPABASE_MAJOR_TABLE,
            deliverable = SUPABASE_DELIVERABLE_TABLE
          )
        ),
        auto_unbox = TRUE
      )
    ))),
    
    tags$script(src = "supabase-auth.js")
  ),
  uiOutput("app_ui")
)