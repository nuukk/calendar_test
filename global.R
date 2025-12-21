# global.R
library(shiny)
library(bslib)
library(bsicons)
library(toastui)
library(dplyr)
library(shinyjs)
library(htmlwidgets)
library(DT)
library(lubridate)
library(httr2)
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# -------------------------------------------------
# 1) 사용자
# -------------------------------------------------
parse_env_json <- function(x) {
  x <- x %||% ""
  if (!nzchar(x)) return(list())
  tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) list())
}

normalize_user_map <- function(raw, value_field = NULL) {
  if (is.null(raw) || length(raw) == 0) return(character(0))
  
  out <- NULL
  if (is.list(raw)) {
    if (!is.null(names(raw)) && all(nzchar(names(raw)))) {
      out <- unlist(raw, use.names = TRUE)
    } else {
      entries <- lapply(raw, function(item) {
        if (!is.list(item)) return(NULL)
        email <- item$email %||% item$Email %||% item$user %||% item$user_email
        if (!nzchar(email %||% "")) return(NULL)
        
        val <- NULL
        if (!is.null(value_field)) val <- item[[value_field]]
        if (is.null(val)) val <- item$value %||% item$name %||% item$color
        if (is.null(val) || !nzchar(as.character(val))) return(NULL)
        
        stats::setNames(as.character(val), email)
      })
      
      out <- unlist(Filter(Negate(is.null), entries), use.names = TRUE)
    }
  } else {
    out <- unlist(raw, use.names = TRUE)
  }
  
  if (is.null(out) || length(out) == 0) return(character(0))
  
  nms <- names(out) %||% character(0)
  if (length(nms) == 0) return(character(0))
  
  keep <- !is.na(nms) & nzchar(nms) & !is.na(out) & nzchar(out)
  out <- out[keep]
  
  if (length(out) == 0) character(0) else out
}

USER_COLORS <- normalize_user_map(parse_env_json(Sys.getenv("USER_COLORS")), value_field = "color")
USER_NAMES  <- normalize_user_map(parse_env_json(Sys.getenv("USER_NAMES")), value_field = "name")

if (is.null(USER_COLORS)) USER_COLORS <- character(0)
if (is.null(USER_NAMES))  USER_NAMES  <- character(0)

# 기존 코드는 USER_COLORS names만 허용 이메일로 썼는데,
# 색/이름 둘 중 하나만 설정한 경우도 고려해서 union 처리
ALLOWED_EMAILS <- unique(c(names(USER_COLORS), names(USER_NAMES)))

user_name <- function(email) {
  nm <- USER_NAMES[email]
  if (is.null(nm) || is.na(nm) || !nzchar(nm)) email else unname(nm)
}

user_color <- function(email) {
  col <- USER_COLORS[email]
  if (is.null(col) || is.na(col) || !nzchar(col)) "#7f7f7f" else unname(col)
}

build_calendar_props <- function() {
  if (length(USER_COLORS) == 0) {
    data.frame(
      id = "default",
      name = "기본",
      color = "#FFFFFF",
      backgroundColor = "#1f77b4",
      borderColor = "#1f77b4",
      stringsAsFactors = FALSE
    )
  } else {
    ids <- names(USER_COLORS)
    data.frame(
      id = ids,
      name = vapply(ids, user_name, character(1)),
      color = "#FFFFFF",
      backgroundColor = unname(USER_COLORS),
      borderColor = unname(USER_COLORS),
      stringsAsFactors = FALSE
    )
  }
}

# -------------------------------------------------
# 2) 상수
# -------------------------------------------------
MEETING_LOCATIONS <- c("The 3rd 메인홀","The 3rd 대회의실","3층 클레이튼 대회의실",
                       "3층 마크뉴먼","3층 바이런샤프","4층 빌 그로스")
TIME_OPTIONS <- sprintf("%02d:%02d", rep(0:23, each = 2), rep(c(0, 30), 24))
CATEGORY_OPTIONS <- c("업무","회의","휴가")

HOLIDAYS <- as.Date(c(
  "2025-12-25",
  "2026-01-01"
))

# -------------------------------------------------
# 3) Supabase 저장소
# -------------------------------------------------
empty_events <- function() {
  df <- data.frame(
    id = integer(0),
    title = character(0),
    start_date = as.Date(character(0)),
    end_date = as.Date(character(0)),
    is_allday = logical(0),
    start_time = character(0),
    end_time = character(0),
    category = character(0),
    location = character(0),
    participants = character(0),
    creator_email = character(0),
    creator_name = character(0),
    google_calendar_id = character(0),
    google_event_id = character(0),
    google_synced = logical(0),
    stringsAsFactors = FALSE
  )
  df$memo_history <- vector("list", nrow(df))
  df
}

empty_major <- function() {
  data.frame(
    id = integer(0),
    isoweek = integer(0),
    date = as.Date(character(0)),
    release_schedule = character(0),
    customer_work = character(0),
    note = character(0),
    stringsAsFactors = FALSE
  )
}

empty_deliverable <- function() {
  data.frame(
    id = integer(0),
    isoweek = integer(0),
    deadline = as.Date(character(0)),
    assignees = character(0),
    content = character(0),
    status = character(0),
    note = character(0),
    stringsAsFactors = FALSE
  )
}

# ---- Supabase ENV (중요: service_role 키 사용 X) ----
SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
SUPABASE_ANON_KEY <- Sys.getenv("SUPABASE_ANON_KEY")

SUPABASE_EVENTS_TABLE <- Sys.getenv("SUPABASE_EVENTS_TABLE", unset = "events")
SUPABASE_MAJOR_TABLE <- Sys.getenv("SUPABASE_MAJOR_TABLE", unset = "major_schedules")
SUPABASE_DELIVERABLE_TABLE <- Sys.getenv("SUPABASE_DELIVERABLE_TABLE", unset = "deliverable_items")

validate_supabase_config <- function() {
  if (!nzchar(SUPABASE_URL) || !nzchar(SUPABASE_ANON_KEY)) {
    stop("SUPABASE_URL 또는 SUPABASE_ANON_KEY가 설정되지 않았습니다.")
  }
}

# jwt가 있으면 Bearer jwt, 없으면 Bearer anon_key
supabase_request <- function(path, jwt = NULL) {
  validate_supabase_config()
  
  token <- if (!is.null(jwt) && nzchar(jwt)) jwt else SUPABASE_ANON_KEY
  
  request(paste0(SUPABASE_URL, "/rest/v1/", path)) %>%
    req_headers(
      apikey = SUPABASE_ANON_KEY,
      Authorization = paste("Bearer", token)
    )
}

# -------------------------------------------------
# 3-1) normalize helpers
# -------------------------------------------------
normalize_memo_history <- function(memo_col, n) {
  res <- vector("list", n)
  if (is.null(memo_col)) return(res)
  
  wrap_single_memo <- function(item) {
    if (is.null(item)) return(list())
    
    if (is.character(item) && length(item) == 1) {
      item <- tryCatch(
        jsonlite::fromJSON(item, simplifyVector = FALSE),
        error = function(...) list()
      )
    }
    
    if (is.data.frame(item)) item <- as.list(item)
    
    if (!is.list(item) || length(item) == 0) return(list())
    
    is_list_entry <- vapply(item, is.list, logical(1))
    if (!any(is_list_entry)) return(list(item))
    
    item
  }
  
  for (i in seq_len(n)) {
    res[[i]] <- wrap_single_memo(memo_col[[i]])
  }
  
  res
}

normalize_events_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(empty_events())
  if (!"memo_history" %in% names(df)) df$memo_history <- vector("list", nrow(df))
  df$memo_history <- normalize_memo_history(df$memo_history, nrow(df))
  if ("start_date" %in% names(df)) df$start_date <- as.Date(df$start_date)
  if ("end_date" %in% names(df)) df$end_date <- as.Date(df$end_date)
  if ("is_allday" %in% names(df)) df$is_allday <- as.logical(df$is_allday)
  if ("google_synced" %in% names(df)) df$google_synced <- as.logical(df$google_synced)
  df
}

normalize_major_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(empty_major())
  if ("date" %in% names(df)) df$date <- as.Date(df$date)
  df
}

normalize_deliverable_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(empty_deliverable())
  if ("deadline" %in% names(df)) df$deadline <- as.Date(df$deadline)
  df
}

# -------------------------------------------------
# 3-2) PostgREST CRUD (jwt 인자 추가)
# -------------------------------------------------
fetch_events <- function(jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_EVENTS_TABLE, jwt) %>%
      req_url_query(select = "*", order = "id.asc") %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(data) == 0) return(empty_events())
    df <- normalize_events_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df
  }, error = function(e) {
    message("이벤트 데이터를 불러오는 중 오류 발생: ", e$message)
    empty_events()
  })
}

fetch_major <- function(jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_MAJOR_TABLE, jwt) %>%
      req_url_query(select = "*", order = "id.asc") %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(data) == 0) return(empty_major())
    df <- normalize_major_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df$isoweek <- suppressWarnings(as.integer(df$isoweek))
    df
  }, error = function(e) {
    message("주요 일정 데이터를 불러오는 중 오류 발생: ", e$message)
    empty_major()
  })
}

fetch_deliverable <- function(jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_DELIVERABLE_TABLE, jwt) %>%
      req_url_query(select = "*", order = "id.asc") %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(data) == 0) return(empty_deliverable())
    df <- normalize_deliverable_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df$isoweek <- suppressWarnings(as.integer(df$isoweek))
    df
  }, error = function(e) {
    message("deliverable 데이터를 불러오는 중 오류 발생: ", e$message)
    empty_deliverable()
  })
}

insert_event_supabase <- function(row, jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_EVENTS_TABLE, jwt) %>%
      req_method("POST") %>%
      req_headers(Prefer = "return=representation") %>%
      req_body_json(list(row), auto_unbox = TRUE) %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    df <- normalize_events_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df
  }, error = function(e) {
    message("이벤트 저장 중 오류 발생: ", e$message)
    empty_events()
  })
}

update_event_supabase <- function(id, updates, jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_EVENTS_TABLE, jwt) %>%
      req_method("PATCH") %>%
      req_url_query(id = paste0("eq.", id)) %>%
      req_headers(Prefer = "return=representation") %>%
      req_body_json(updates, auto_unbox = TRUE) %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    df <- normalize_events_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df
  }, error = function(e) {
    message("이벤트 수정 중 오류 발생: ", e$message)
    empty_events()
  })
}

delete_event_supabase <- function(id, jwt = NULL) {
  tryCatch({
    supabase_request(SUPABASE_EVENTS_TABLE, jwt) %>%
      req_method("DELETE") %>%
      req_url_query(id = paste0("eq.", id)) %>%
      req_perform()
    TRUE
  }, error = function(e) {
    message("이벤트 삭제 중 오류 발생: ", e$message)
    FALSE
  })
}

insert_major_supabase <- function(row, jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_MAJOR_TABLE, jwt) %>%
      req_method("POST") %>%
      req_headers(Prefer = "return=representation") %>%
      req_body_json(list(row), auto_unbox = TRUE) %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    df <- normalize_major_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df$isoweek <- suppressWarnings(as.integer(df$isoweek))
    df
  }, error = function(e) {
    message("주요 일정 저장 중 오류 발생: ", e$message)
    empty_major()
  })
}

update_major_supabase <- function(id, updates, jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_MAJOR_TABLE, jwt) %>%
      req_method("PATCH") %>%
      req_url_query(id = paste0("eq.", id)) %>%
      req_headers(Prefer = "return=representation") %>%
      req_body_json(updates, auto_unbox = TRUE) %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    df <- normalize_major_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df$isoweek <- suppressWarnings(as.integer(df$isoweek))
    df
  }, error = function(e) {
    message("주요 일정 수정 중 오류 발생: ", e$message)
    empty_major()
  })
}

delete_major_supabase <- function(id, jwt = NULL) {
  tryCatch({
    supabase_request(SUPABASE_MAJOR_TABLE, jwt) %>%
      req_method("DELETE") %>%
      req_url_query(id = paste0("eq.", id)) %>%
      req_perform()
    TRUE
  }, error = function(e) {
    message("주요 일정 삭제 중 오류 발생: ", e$message)
    FALSE
  })
}

insert_deliverable_supabase <- function(row, jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_DELIVERABLE_TABLE, jwt) %>%
      req_method("POST") %>%
      req_headers(Prefer = "return=representation") %>%
      req_body_json(list(row), auto_unbox = TRUE) %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    df <- normalize_deliverable_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df$isoweek <- suppressWarnings(as.integer(df$isoweek))
    df
  }, error = function(e) {
    message("deliverable 저장 중 오류 발생: ", e$message)
    empty_deliverable()
  })
}

update_deliverable_supabase <- function(id, updates, jwt = NULL) {
  tryCatch({
    resp <- supabase_request(SUPABASE_DELIVERABLE_TABLE, jwt) %>%
      req_method("PATCH") %>%
      req_url_query(id = paste0("eq.", id)) %>%
      req_headers(Prefer = "return=representation") %>%
      req_body_json(updates, auto_unbox = TRUE) %>%
      req_perform()
    data <- resp_body_json(resp, simplifyVector = TRUE)
    df <- normalize_deliverable_df(as.data.frame(data, stringsAsFactors = FALSE))
    df$id <- suppressWarnings(as.integer(df$id))
    df$isoweek <- suppressWarnings(as.integer(df$isoweek))
    df
  }, error = function(e) {
    message("deliverable 수정 중 오류 발생: ", e$message)
    empty_deliverable()
  })
}

delete_deliverable_supabase <- function(id, jwt = NULL) {
  tryCatch({
    supabase_request(SUPABASE_DELIVERABLE_TABLE, jwt) %>%
      req_method("DELETE") %>%
      req_url_query(id = paste0("eq.", id)) %>%
      req_perform()
    TRUE
  }, error = function(e) {
    message("deliverable 삭제 중 오류 발생: ", e$message)
    FALSE
  })
}

# -------------------------------------------------
# 3-3) ✅ Realtime payload 기반 "증분 업데이트" helpers
# -------------------------------------------------
clean_realtime_payload_list <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) return(x)
  
  lapply(x, function(val) {
    # 1. NULL이면 NA로 변환 (as.data.frame 에러 방지)
    if (is.null(val)) return(NA)
    
    # 2. 리스트(예: memo_history)인 경우, I(list(val))로 감싸서
    #    as.data.frame 변환 시 행이 늘어나거나 에러가 나는 것을 방지
    if (is.list(val)) return(I(list(val)))
    
    return(val)
  })
}

na_like <- function(x0, n) {
  if (inherits(x0, "Date")) return(rep(as.Date(NA), n))
  if (is.integer(x0)) return(rep(NA_integer_, n))
  if (is.character(x0)) return(rep(NA_character_, n))
  if (is.logical(x0)) return(rep(NA, n))
  rep(NA, n)
}

events_coerce_schema <- function(df) {
  tmpl <- empty_events()
  if (is.null(df) || nrow(df) == 0) return(tmpl)
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n <- nrow(df)
  
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) {
      if (nm == "memo_history") df[[nm]] <- vector("list", n)
      else df[[nm]] <- na_like(tmpl[[nm]], n)
    }
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  
  # memo_history 보정
  if (!is.list(df$memo_history)) df$memo_history <- as.list(df$memo_history)
  if (length(df$memo_history) != n) df$memo_history <- rep_len(df$memo_history, n)
  
  df <- normalize_events_df(df)
  df$id <- suppressWarnings(as.integer(df$id))
  df
}

events_upsert_row <- function(df, row_df) {
  df <- events_coerce_schema(df)
  row_df <- events_coerce_schema(row_df)
  if (nrow(row_df) == 0) return(df)
  
  id <- row_df$id[1]
  if (is.na(id)) return(df)
  
  idx <- match(id, df$id)
  if (is.na(idx)) {
    out <- dplyr::bind_rows(df, row_df)
  } else {
    out <- df
    for (nm in names(out)) {
      out[[nm]][idx] <- row_df[[nm]][1]
    }
  }
  
  out <- events_coerce_schema(out)
  out <- out[order(out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

events_delete_id <- function(df, id) {
  df <- events_coerce_schema(df)
  id <- suppressWarnings(as.integer(id))
  if (is.na(id) || nrow(df) == 0) return(df)
  out <- df[df$id != id, , drop = FALSE]
  rownames(out) <- NULL
  out
}

normalize_single_event_row <- function(x) {
  if (is.null(x)) return(NULL)
  
  # [수정됨] NULL 처리 적용
  x_clean <- clean_realtime_payload_list(x)
  
  df <- tryCatch(
    as.data.frame(x_clean, stringsAsFactors = FALSE),
    error = function(e) {
      # 디버깅용 로그 (필요시 주석 해제)
      # message("[Events Realtime Error] ", e$message)
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  events_coerce_schema(df)
}

events_apply_realtime_payload <- function(current_df, input_value) {
  payload <- input_value$payload %||% input_value
  if (is.null(payload)) return(current_df)
  
  ev_type <- payload$eventType %||% payload$type %||% payload$event_type %||% NA_character_
  ev_type <- toupper(as.character(ev_type %||% ""))
  
  if (ev_type %in% c("INSERT", "UPDATE")) {
    row_df <- normalize_single_event_row(payload$new)
    if (is.null(row_df)) return(current_df)
    return(events_upsert_row(current_df, row_df))
  }
  
  if (ev_type == "DELETE") {
    old <- payload$old
    id <- old$id %||% old[["id"]]
    return(events_delete_id(current_df, id))
  }
  
  current_df
}

# --- major/deliverable도 동일 패턴 적용 ---

major_coerce_schema <- function(df) {
  tmpl <- empty_major()
  if (is.null(df) || nrow(df) == 0) return(tmpl)
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n <- nrow(df)
  
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) df[[nm]] <- na_like(tmpl[[nm]], n)
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  df <- normalize_major_df(df)
  df$id <- suppressWarnings(as.integer(df$id))
  df$isoweek <- suppressWarnings(as.integer(df$isoweek))
  df
}

major_upsert_row <- function(df, row_df) {
  df <- major_coerce_schema(df)
  row_df <- major_coerce_schema(row_df)
  if (nrow(row_df) == 0) return(df)
  
  id <- row_df$id[1]
  if (is.na(id)) return(df)
  
  idx <- match(id, df$id)
  if (is.na(idx)) {
    out <- dplyr::bind_rows(df, row_df)
  } else {
    out <- df
    for (nm in names(out)) out[[nm]][idx] <- row_df[[nm]][1]
  }
  
  out <- major_coerce_schema(out)
  out <- out[order(out$date, out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

major_delete_id <- function(df, id) {
  df <- major_coerce_schema(df)
  id <- suppressWarnings(as.integer(id))
  if (is.na(id) || nrow(df) == 0) return(df)
  out <- df[df$id != id, , drop = FALSE]
  rownames(out) <- NULL
  out
}

major_apply_realtime_payload <- function(current_df, input_value) {
  payload <- input_value$payload %||% input_value
  if (is.null(payload)) return(current_df)
  
  ev_type <- payload$eventType %||% payload$type %||% payload$event_type %||% NA_character_
  ev_type <- toupper(as.character(ev_type %||% ""))
  
  if (ev_type %in% c("INSERT", "UPDATE")) {
    # [수정됨] NULL 처리 적용
    x_clean <- clean_realtime_payload_list(payload$new)
    
    row_df <- tryCatch(
      as.data.frame(x_clean, stringsAsFactors = FALSE), 
      error = function(e) NULL
    )
    if (is.null(row_df) || nrow(row_df) == 0) return(current_df)
    row_df <- major_coerce_schema(row_df)
    return(major_upsert_row(current_df, row_df))
  }
  
  if (ev_type == "DELETE") {
    old <- payload$old
    id <- old$id %||% old[["id"]]
    return(major_delete_id(current_df, id))
  }
  
  current_df
}

deliverable_coerce_schema <- function(df) {
  tmpl <- empty_deliverable()
  if (is.null(df) || nrow(df) == 0) return(tmpl)
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n <- nrow(df)
  
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) df[[nm]] <- na_like(tmpl[[nm]], n)
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  df <- normalize_deliverable_df(df)
  df$id <- suppressWarnings(as.integer(df$id))
  df$isoweek <- suppressWarnings(as.integer(df$isoweek))
  df
}

deliverable_upsert_row <- function(df, row_df) {
  df <- deliverable_coerce_schema(df)
  row_df <- deliverable_coerce_schema(row_df)
  if (nrow(row_df) == 0) return(df)
  
  id <- row_df$id[1]
  if (is.na(id)) return(df)
  
  idx <- match(id, df$id)
  if (is.na(idx)) {
    out <- dplyr::bind_rows(df, row_df)
  } else {
    out <- df
    for (nm in names(out)) out[[nm]][idx] <- row_df[[nm]][1]
  }
  
  out <- deliverable_coerce_schema(out)
  out <- out[order(out$deadline, out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

deliverable_delete_id <- function(df, id) {
  df <- deliverable_coerce_schema(df)
  id <- suppressWarnings(as.integer(id))
  if (is.na(id) || nrow(df) == 0) return(df)
  out <- df[df$id != id, , drop = FALSE]
  rownames(out) <- NULL
  out
}

deliverable_apply_realtime_payload <- function(current_df, input_value) {
  payload <- input_value$payload %||% input_value
  if (is.null(payload)) return(current_df)
  
  ev_type <- payload$eventType %||% payload$type %||% payload$event_type %||% NA_character_
  ev_type <- toupper(as.character(ev_type %||% ""))
  
  if (ev_type %in% c("INSERT", "UPDATE")) {
    # [수정됨] NULL 처리 적용
    x_clean <- clean_realtime_payload_list(payload$new)
    
    row_df <- tryCatch(
      as.data.frame(x_clean, stringsAsFactors = FALSE), 
      error = function(e) NULL
    )
    if (is.null(row_df) || nrow(row_df) == 0) return(current_df)
    row_df <- deliverable_coerce_schema(row_df)
    return(deliverable_upsert_row(current_df, row_df))
  }
  
  if (ev_type == "DELETE") {
    old <- payload$old
    id <- old$id %||% old[["id"]]
    return(deliverable_delete_id(current_df, id))
  }
  
  current_df
}

# -------------------------------------------------
# 4) toastui 스케줄 변환 (캘린더 일정)
# -------------------------------------------------
events_to_schedules <- function(events_df) {
  if (is.null(events_df) || nrow(events_df) == 0) return(NULL)
  
  fmt_date <- function(d) format(as.Date(d), "%Y-%m-%d")
  
  events_df <- as.data.frame(events_df, stringsAsFactors = FALSE)
  if (!"category" %in% names(events_df)) events_df$category <- NA_character_
  
  is_vac <- !is.na(events_df$category) & events_df$category == "휴가"
  
  normal_df <- events_df[!is_vac, , drop = FALSE]
  normal_sched <- NULL
  if (nrow(normal_df) > 0) {
    normal_sched <- normal_df %>%
      mutate(
        id = as.character(id),
        calendarId = creator_email,
        start = ifelse(
          is_allday,
          paste0(fmt_date(start_date), "T00:00:00"),
          paste0(fmt_date(start_date), "T", start_time, ":00")
        ),
        end = ifelse(
          is_allday,
          paste0(fmt_date(end_date), "T23:59:59"),
          paste0(fmt_date(end_date), "T", end_time, ":00")
        ),
        category_type = ifelse(is_allday, "allday", "time"),
        isAllday = is_allday
      ) %>%
      select(id, calendarId, title, start, end, category_type, isAllday) %>%
      rename(category = category_type) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  
  vac_df <- events_df[is_vac, , drop = FALSE]
  vac_sched <- NULL
  if (nrow(vac_df) > 0) {
    vac_rows <- lapply(seq_len(nrow(vac_df)), function(i) {
      ev <- vac_df[i, , drop = FALSE]
      s <- as.Date(ev$start_date)
      e <- as.Date(ev$end_date)
      if (is.na(s) || is.na(e)) return(NULL)
      
      days <- seq(s, e, by = "day")
      title_vac <- paste0("⛱ (", ev$creator_name, " 휴가)")
      
      if (isTRUE(ev$is_allday)) {
        data.frame(
          id = paste0("vac-", ev$id, "-", format(days, "%Y%m%d")),
          calendarId = ev$creator_email,
          title = title_vac,
          start = paste0(format(days, "%Y-%m-%d"), "T00:00:00"),
          end = paste0(format(days, "%Y-%m-%d"), "T23:59:59"),
          category = "allday",
          isAllday = TRUE,
          color = "#6c757d",
          backgroundColor = "transparent",
          borderColor = "transparent",
          dragBackgroundColor = "transparent",
          bgColor = "transparent",
          dragBgColor = "transparent",
          stringsAsFactors = FALSE
        )
      } else {
        st <- ev$start_time
        et <- ev$end_time
        if (is.na(st) || !nzchar(st)) st <- "09:00"
        if (is.na(et) || !nzchar(et)) et <- "18:00"
        
        data.frame(
          id = paste0("vac-", ev$id, "-", format(days, "%Y%m%d")),
          calendarId = ev$creator_email,
          title = title_vac,
          start = paste0(format(days, "%Y-%m-%d"), "T", st, ":00"),
          end = paste0(format(days, "%Y-%m-%d"), "T", et, ":00"),
          category = "time",
          isAllday = FALSE,
          color = "#6c757d",
          backgroundColor = "rgba(108,117,125,0.12)",
          borderColor = "rgba(108,117,125,0.35)",
          dragBackgroundColor = "rgba(108,117,125,0.12)",
          bgColor = "rgba(108,117,125,0.12)",
          dragBgColor = "rgba(108,117,125,0.12)",
          stringsAsFactors = FALSE
        )
      }
    })
    
    vac_rows <- Filter(Negate(is.null), vac_rows)
    if (length(vac_rows) > 0) vac_sched <- bind_rows(vac_rows) %>% as.data.frame(stringsAsFactors = FALSE)
  }
  
  sched <- bind_rows(normal_sched, vac_sched)
  if (is.null(sched) || nrow(sched) == 0) return(NULL)
  as.data.frame(sched, stringsAsFactors = FALSE)
}

# -------------------------------------------------
# 5) Theme
# -------------------------------------------------
THEME <- bs_theme(
  version = 5,
  preset = "shiny",
  primary = "#0d6efd",
  success = "#198754",
  info = "#5dcaf0",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = font_face(
    family = "Noto Sans KR",
    src = "data/fonts/NotoSansKR-VariableFont_wght.woff2"
  ),
  heading_font = font_face(
    family = "Noto Sans KR",
    src = "data/fonts/NotoSansKR-VariableFont_wght.woff2",
    weight = 600
  ),
  font_scale = 0.95
)

# -------------------------------------------------
# 6) UI 정의 (server.R에서 참조하기 위해 global에 정의)
# -------------------------------------------------
login_ui <- page_fluid(
  title = "주간 회의 캘린더",
  theme = THEME,
  tags$head(tags$style(HTML("
    .login-wrap {max-width: 520px; margin: 0 auto;}
    .login-card {margin-top: 10vh;}
  "))),
  div(
    class = "login-wrap",
    card(
      class = "login-card",
      card_header(class = "bg-primary text-white", div(bs_icon("google"), " 로그인")),
      card_body(
        tags$p(class = "text-muted", "구글 계정으로 로그인합니다."),
        tags$button(
          id = "sb_login_google",
          type = "button",
          class = "btn btn-primary w-100",
          bs_icon("google"),
          span(class = "ms-2", "Google로 로그인")
        ),
        uiOutput("login_msg")
      )
    )
  )
)

main_ui <- page_navbar(
  title = div(bs_icon("calendar3"), "3사업부 주간 회의 캘린더"),
  theme = THEME,
  
  header = tagList(
    tags$head(
      tags$style(HTML("
        .navbar { border-bottom: 1px solid rgba(0,0,0,.175); }
        .navbar .navbar-nav { gap: 6px; align-items: flex-end; }
        .navbar .navbar-nav .nav-link{
          border: 1px solid rgba(0,0,0,.175);
          border-radius: 10px 10px 0 0;
          background: #f8f9fa;
          padding: 10px 14px;
          margin: 0;
        }
        .navbar .navbar-nav .nav-link:hover{ background: #eef1f4; }
        .navbar .navbar-nav .nav-link.active,
        .navbar .navbar-nav .show > .nav-link{
          background: #ffffff;
          font-weight: 700;
          border-bottom-color: #ffffff;
        }

        .bslib-sidebar-layout > .collapse-toggle,
        .bslib-sidebar-layout .collapse-toggle,
        .collapse-toggle,
        .bslib-sidebar-layout > .bslib-sidebar-toggle,
        .bslib-sidebar-layout .bslib-sidebar-toggle,
        .bslib-sidebar-toggle,
        button.bslib-sidebar-toggle,
        a.bslib-sidebar-toggle,
        .navbar-toggler,
        button.navbar-toggler {
          display: none !important;
        }

        .modal-body {overflow: visible !important;}
        .modal-content {overflow: visible !important;}
        .card-body {overflow: visible !important;}
        .selectize-dropdown {z-index: 10000 !important;}

        .timeline-bar {display:flex; flex-direction:column; gap:4px; height:100%;}
        .timeline-title {display:flex; align-items:baseline; gap:6px; min-width:0;}
        .timeline-title-text {
          flex:1; min-width:0;
          font-weight:600; font-size:0.85rem; line-height:1.15;
          overflow:hidden; text-overflow:ellipsis; white-space:nowrap;
        }
        .timeline-date {flex:none; font-size:0.75rem; color:#6c757d; white-space:nowrap;}
        .timeline-people {display:flex; align-items:center; gap:4px; min-width:0; overflow:hidden; white-space:nowrap;}
        .timeline-chip{
          display:inline-flex; align-items:center;
          height:18px; padding:0 6px;
          border-radius:9999px;
          font-size:0.72rem; line-height:1;
          color:#fff; flex:none;
        }

        .timeline-vac-wrap {display:flex; flex-direction:column; gap:2px; margin-top:2px;}
        .timeline-vac{
          color:#6c757d !important;
          font-size:0.72rem;
          line-height:1.1;
          white-space:nowrap;
          overflow:hidden;
          text-overflow:ellipsis;
          cursor:pointer;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('clearCalendarSelection', function(message) {
          var selections = document.querySelectorAll('.toastui-calendar-grid-selection');
          selections.forEach(function(el) { el.remove(); });
          var guides = document.querySelectorAll('.toastui-calendar-grid-selection-guide');
          guides.forEach(function(el) { el.remove(); });
        });
      "))
    ),
    useShinyjs()
  ),
  
  sidebar = sidebar(
    width = 320,
    open = "always",
    collapsible = FALSE,
    
    card(
      card_header(class = "bg-primary text-white", div(bs_icon("person-circle", size = "1.2em"), " 사용자 정보")),
      card_body(
        uiOutput("current_user_info"),
        actionButton(
          "logout",
          label = div(bs_icon("box-arrow-right"), "로그아웃"),
          class = "btn-outline-secondary w-100 mt-3"
        )
      )
    ),
    card(
      card_header(class = "bg-info text-white", div(bs_icon("eye"), " 뷰 설정")),
      card_body(
        radioButtons(
          "view_mode",
          label = div(bs_icon("calendar-range"), "캘린더 보기"),
          choices = c("1주" = "week", "월간" = "month"),
          selected = "month"
        )
      )
    ),
    card(
      card_header(class = "bg-success text-white", div(bs_icon("funnel"), " 필터")),
      card_body(
        selectInput(
          "category_filter",
          label = div(bs_icon("tags"), "업무 구분"),
          choices = c("전체", CATEGORY_OPTIONS),
          selected = "전체"
        ),
        checkboxInput("show_only_mine", label = div(bs_icon("person-check"), "내 일정만 보기"), value = FALSE)
      )
    ),
    div(
      class = "d-grid gap-2",
      actionButton(
        "new_event",
        label = div(bs_icon("plus-circle"), "새 일정 추가"),
        class = "btn-primary btn-lg",
        width = "100%"
      )
    )
  ),
  
  nav_panel(
    title = div(bs_icon("calendar3"), " 캘린더"),
    value = "calendar_tab",
    card(full_screen = TRUE, card_body(uiOutput("calendar_ui")))
  ),
  
  nav_panel(
    title = div(bs_icon("clock-history"), " 연속 보기"),
    value = "timeline_tab",
    card(
      full_screen = TRUE,
      card_header(
        class = "bg-light",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(
            bs_icon("calendar-week"),
            span(class = "ms-2 fw-bold", "통합 타임라인"),
            span(class = "ms-2 text-muted small", "(좌우 스크롤)")
          ),
          div(class = "d-flex gap-2 align-items-center", uiOutput("timeline_range_label"))
        )
      ),
      card_body(
        class = "p-0",
        div(style = "overflow-x:auto; overflow-y:auto; max-height:800px;", uiOutput("unified_timeline"))
      )
    )
  ),
  
  nav_panel(
    title = div(bs_icon("list-check"), " 주요 일정"),
    value = "major_tab",
    card(
      full_screen = TRUE,
      card_header(
        class = "bg-light",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(
            bs_icon("list-check"),
            span(class = "ms-2 fw-bold", "주요 일정"),
            tags$small(class = "ms-2 text-muted", "행 선택 후 수정/삭제")
          ),
          div(
            class = "d-flex gap-2",
            actionButton("major_add", div(bs_icon("plus-circle"), "추가"), class = "btn-primary"),
            actionButton("major_edit", div(bs_icon("pencil"), "수정"), class = "btn-outline-primary"),
            actionButton("major_delete", div(bs_icon("trash"), "삭제"), class = "btn-outline-danger")
          )
        )
      ),
      card_body(DT::DTOutput("major_table"))
    )
  ),
  
  nav_panel(
    title = div(bs_icon("check2-circle"), " Deliverable"),
    value = "deliverable_tab",
    card(
      full_screen = TRUE,
      card_header(
        class = "bg-light",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(
            bs_icon("check2-circle"),
            span(class = "ms-2 fw-bold", "Deliverable"),
            tags$small(class = "ms-2 text-muted", "행 선택 후 수정/삭제")
          ),
          div(
            class = "d-flex gap-2",
            actionButton("deliverable_add", div(bs_icon("plus-circle"), "추가"), class = "btn-primary"),
            actionButton("deliverable_edit", div(bs_icon("pencil"), "수정"), class = "btn-outline-primary"),
            actionButton("deliverable_delete", div(bs_icon("trash"), "삭제"), class = "btn-outline-danger")
          )
        )
      ),
      card_body(DT::DTOutput("deliverable_table"))
    )
  )
)