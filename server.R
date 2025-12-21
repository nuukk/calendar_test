# server.R
server <- function(input, output, session) {
  
  # -------------------------------------------------
  # 로그인 (Supabase Auth session -> Shiny input)
  # -------------------------------------------------
  login_msg <- reactiveVal(NULL)
  user_email_rv <- reactiveVal(NULL)
  access_token_rv <- reactiveVal(NULL)
  
  # ✅ 권장: USER_COLORS/USER_NAMES 중 하나에라도 있으면 허용
  ALLOWED_EMAILS_SAFE <- unique(c(names(USER_COLORS), names(USER_NAMES)))
  
  is_allowed_email <- function(em) {
    if (length(ALLOWED_EMAILS_SAFE) == 0) TRUE else em %in% ALLOWED_EMAILS_SAFE
  }
  
  authed <- reactive({
    em <- user_email_rv()
    tok <- access_token_rv()
    nzchar(em %||% "") && nzchar(tok %||% "")
  })
  
  observeEvent(list(input$sb_user_email, input$sb_access_token), {
    em <- trimws(tolower(input$sb_user_email %||% ""))
    tok <- input$sb_access_token %||% ""
    
    if (!nzchar(em) || !nzchar(tok)) {
      user_email_rv(NULL)
      access_token_rv(NULL)
      return()
    }
    
    if (!is_allowed_email(em)) {
      login_msg("권한이 없습니다.")
      user_email_rv(NULL)
      access_token_rv(NULL)
      session$sendCustomMessage("sb_signOut", list())
      return()
    }
    
    login_msg(NULL)
    user_email_rv(em)
    access_token_rv(tok)
  }, ignoreInit = FALSE)
  
  observeEvent(input$logout, {
    session$sendCustomMessage("sb_signOut", list())
    user_email_rv(NULL)
    access_token_rv(NULL)
    login_msg(NULL)
  }, ignoreInit = TRUE)
  
  output$app_ui <- renderUI({
    if (isTRUE(authed())) main_ui else login_ui
  })
  
  output$login_msg <- renderUI({
    msg <- login_msg()
    if (is.null(msg)) return(NULL)
    div(class = "alert alert-danger mt-3", bs_icon("exclamation-triangle"), paste0(" ", msg))
  })
  
  output$app_ui <- renderUI({
    if (isTRUE(authed())) main_ui else login_ui
  })
  
  current_user_email <- reactive({
    req(authed())
    user_email_rv()
  })
  
  current_user_name <- reactive({
    user_name(current_user_email())
  })
  
  current_jwt <- reactive({
    req(authed())
    access_token_rv()
  })
  
  output$current_user_info <- renderUI({
    req(authed())
    em <- current_user_email()
    nm <- current_user_name()
    col <- user_color(em)
    
    div(
      class = "mt-1",
      div(
        class = "d-flex align-items-center mb-2",
        div(style = paste0("width:12px; height:12px; border-radius:50%; background-color:", col, "; margin-right:8px;")),
        tags$strong(nm)
      ),
      div(class = "text-muted small", bs_icon("envelope-fill"), span(class = "ms-1", em))
    )
  })
  
  # -------------------------------------------------
  # Supabase 연동 (JWT 기반 + Realtime 증분 업데이트)
  # -------------------------------------------------
  events_rv <- reactiveVal(empty_events())
  major_rv <- reactiveVal(empty_major())
  deliverable_rv <- reactiveVal(empty_deliverable())
  
  
  # -------------------------------------------------
  # ✅ Realtime 상태 기반 Polling 제어
  #   - events/major/deliverable 채널이 모두 SUBSCRIBED면 polling OFF
  #   - 하나라도 SUBSCRIBED가 아니면(끊김/오류/타임아웃 등) 5초 polling ON
  # -------------------------------------------------
  rt_channel_status <- reactiveValues(
    events = NA_character_,
    major = NA_character_,
    deliverable = NA_character_
  )
  
  poll_interval_ms <- reactiveVal(0L)  # 0이면 polling OFF
  poll_busy <- reactiveVal(FALSE)      # 폴링 겹침 방지
  
  # 로그인/로그아웃 시 초기 로드/초기화 + ✅ polling 초기 상태 세팅
  observeEvent(authed(), {
    if (isTRUE(authed())) {
      jwt <- current_jwt()
      events_rv(fetch_events(jwt))
      major_rv(fetch_major(jwt))
      deliverable_rv(fetch_deliverable(jwt))
      
      # 로그인 직후에는 realtime이 SUBSCRIBED인지 확정되기 전이므로
      # 끊김으로 간주하고 5초 polling을 켠 상태로 시작
      poll_interval_ms(5000L)
      
      # 채널 상태 리셋
      rt_channel_status$events <- NA_character_
      rt_channel_status$major <- NA_character_
      rt_channel_status$deliverable <- NA_character_
    } else {
      events_rv(empty_events())
      major_rv(empty_major())
      deliverable_rv(empty_deliverable())
      
      # 로그아웃이면 polling OFF
      poll_interval_ms(0L)
      
      # 채널 상태 리셋
      rt_channel_status$events <- NA_character_
      rt_channel_status$major <- NA_character_
      rt_channel_status$deliverable <- NA_character_
    }
  }, ignoreInit = FALSE)
  
  
  # ✅ Realtime payload로 "증분 업데이트"
  observeEvent(input$sb_rt_events, {
    req(authed())
    events_rv(events_apply_realtime_payload(events_rv(), input$sb_rt_events))
  }, ignoreInit = TRUE)
  
  # (선택) major/deliverable도 동일하게 증분 업데이트
  observeEvent(input$sb_rt_major, {
    req(authed())
    major_rv(major_apply_realtime_payload(major_rv(), input$sb_rt_major))
  }, ignoreInit = TRUE)
  
  observeEvent(input$sb_rt_deliverable, {
    req(authed())
    deliverable_rv(deliverable_apply_realtime_payload(deliverable_rv(), input$sb_rt_deliverable))
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------
  # ✅ Realtime 채널 상태 수신 -> polling ON/OFF 전환
  #   supabase-auth.js가 sb_rt_status로 kind/status를 보내줌
  # -------------------------------------------------
  observeEvent(input$sb_rt_status, {
    if (!isTRUE(authed())) return()
    
    x <- input$sb_rt_status
    if (is.null(x) || !is.list(x)) return()
    
    kind <- tolower(as.character(x$kind %||% ""))
    status <- toupper(as.character(x$status %||% ""))
    
    # 과거 오타(delivable) 대응 (혹시 남아있으면 deliverable로 매핑)
    if (identical(kind, "delivable")) kind <- "deliverable"
    
    if (!nzchar(kind) || !nzchar(status)) return()
    if (!kind %in% c("events", "major", "deliverable")) return()
    
    # 채널별 최신 상태 저장
    rt_channel_status[[kind]] <- status
    
    # 3개 채널이 모두 SUBSCRIBED면 realtime 정상으로 보고 polling OFF
    st <- c(rt_channel_status$events, rt_channel_status$major, rt_channel_status$deliverable)
    all_subscribed <- length(st) == 3 &&
      all(!is.na(st)) &&
      all(st == "SUBSCRIBED")
    
    if (isTRUE(all_subscribed)) {
      poll_interval_ms(0L)
    } else {
      poll_interval_ms(5000L)
    }
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------
  # ✅ Polling fallback (Realtime이 끊긴 경우에만 5초 full sync)
  # -------------------------------------------------
  observe({
    req(authed())
    
    interval <- poll_interval_ms()
    if (is.null(interval) || !is.numeric(interval) || interval <= 0) return()
    
    invalidateLater(as.integer(interval), session)
    
    # (옵션) 이전 폴링이 아직 진행 중이면 중복 실행 방지
    if (isTRUE(isolate(poll_busy()))) return()
    poll_busy(TRUE)
    on.exit(poll_busy(FALSE), add = TRUE)
    
    jwt <- current_jwt()
    events_rv(fetch_events(jwt))
    major_rv(fetch_major(jwt))
    deliverable_rv(fetch_deliverable(jwt))
  })
  
  # -------------------------------------------------
  # 상태
  # -------------------------------------------------
  editing_event_id <- reactiveVal(NULL)
  viewing_event_id <- reactiveVal(NULL)
  
  major_editing_id <- reactiveVal(NULL)
  deliverable_editing_id <- reactiveVal(NULL)
  
  clear_calendar_selection <- function() {
    session$sendCustomMessage("clearCalendarSelection", list())
  }
  
  get_participant_choices_new <- function() {
    me <- current_user_email()
    all_users <- names(USER_NAMES) %||% character(0)
    others <- setdiff(all_users, me)
    labels <- USER_NAMES[others]
    stats::setNames(others, labels)
  }
  
  get_participant_choices_edit <- function(creator_email) {
    all_users <- names(USER_NAMES) %||% character(0)
    others <- setdiff(all_users, creator_email)
    labels <- USER_NAMES[others]
    stats::setNames(others, labels)
  }
  
  # ✅ 버그 수정(others 미정의)
  get_all_participant_choices <- function() {
    all_users <- names(USER_NAMES) %||% character(0)
    labels <- USER_NAMES[all_users]
    stats::setNames(all_users, labels)
  }
  
  # -------------------------------------------------
  # 캘린더 필터
  # -------------------------------------------------
  events_filtered <- reactive({
    req(authed())
    df <- events_rv()
    if (is.null(df) || nrow(df) == 0) return(df)
    
    if (isTRUE(input$show_only_mine)) {
      em <- current_user_email()
      my_created <- df$creator_email == em
      my_participated <- vapply(seq_len(nrow(df)), function(i) {
        part <- df$participants[i]
        if (is.na(part) || !nzchar(part)) return(FALSE)
        em %in% trimws(strsplit(part, ",")[[1]])
      }, logical(1))
      df <- df[my_created | my_participated, , drop = FALSE]
    }
    
    if (!is.null(input$category_filter) && input$category_filter != "전체") {
      df <- df[df$category == input$category_filter, , drop = FALSE]
    }
    
    df
  })
  
  # -------------------------------------------------
  # 월간뷰 높이 자동( n more 방지용 )
  # -------------------------------------------------
  month_max_events <- reactive({
    req(authed())
    df <- events_filtered()
    if (is.null(df) || nrow(df) == 0) return(1L)
    
    s <- as.Date(df$start_date)
    e <- as.Date(df$end_date)
    
    if (all(is.na(s)) || all(is.na(e))) return(1L)
    
    min_d <- suppressWarnings(min(s, na.rm = TRUE))
    max_d <- suppressWarnings(max(e, na.rm = TRUE))
    if (!is.finite(min_d) || !is.finite(max_d)) return(1L)
    
    rng <- seq(min_d, max_d, by = "day")
    if (length(rng) == 0) return(1L)
    
    cnt <- vapply(rng, function(d) sum(s <= d & e >= d, na.rm = TRUE), integer(1))
    k <- suppressWarnings(max(cnt, na.rm = TRUE))
    if (!is.finite(k) || is.na(k) || k < 1L) 1L else as.integer(k)
  })
  
  output$calendar_ui <- renderUI({
    req(authed())
    view_mode <- input$view_mode %||% "month"
    
    if (view_mode == "month") {
      k_max <- month_max_events()
      
      weeks <- 6L
      line_px <- 26L
      header_px <- 46L
      pad_px <- 20L
      extra_px <- 160L
      
      h <- max(900L, weeks * (header_px + k_max * line_px + pad_px) + extra_px)
      calendarOutput("calendar", height = paste0(h, "px"))
    } else {
      calendarOutput("calendar", height = "800px")
    }
  })
  
  # -------------------------------------------------
  # ✅ 주요 일정 (DT + CRUD)
  # -------------------------------------------------
  major_table_data <- reactive({
    df <- major_rv()
    if (is.null(df) || nrow(df) == 0) return(empty_major())
    df <- df %>% mutate(date = as.Date(date)) %>% arrange(date, id)
    df
  })
  
  output$major_table <- DT::renderDT({
    req(authed())
    df <- major_table_data()
    if (is.null(df) || nrow(df) == 0) df <- empty_major()
    
    df_disp <- df %>% select(id, isoweek, date, release_schedule, customer_work, note)
    colnames(df_disp) <- c("id", "주차(isoweek)", "날짜", "출시 일정", "고객사 주요 업무", "비고")
    
    DT::datatable(
      df_disp,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(2, "asc")),
        columnDefs = list(
          list(targets = 0, visible = FALSE)
        )
      )
    )
  }, server = FALSE)
  
  observe({
    req(authed())
    sel <- input$major_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      shinyjs::disable("major_edit")
      shinyjs::disable("major_delete")
    } else {
      shinyjs::enable("major_edit")
      shinyjs::enable("major_delete")
    }
  })
  
  major_selected_id <- reactive({
    req(authed())
    sel <- input$major_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    
    df <- major_table_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (sel > nrow(df)) return(NULL)
    
    df$id[sel]
  })
  
  show_major_modal <- function(row = NULL) {
    req(authed())
    
    if (is.null(row)) {
      major_editing_id(NULL)
      d0 <- Sys.Date()
      release0 <- ""
      customer0 <- ""
      note0 <- ""
      modal_title <- "주요 일정 추가"
    } else {
      major_editing_id(row$id[1])
      d0 <- as.Date(row$date[1])
      release0 <- row$release_schedule[1] %||% ""
      customer0 <- row$customer_work[1] %||% ""
      note0 <- row$note[1] %||% ""
      modal_title <- "주요 일정 수정"
    }
    
    showModal(
      modalDialog(
        title = div(bs_icon("list-check"), paste0(" ", modal_title)),
        size = "l",
        dateInput("major_date", div(bs_icon("calendar-event"), "날짜"), value = d0),
        textInput("major_release", div(bs_icon("box-seam"), "출시 일정"), value = release0),
        textAreaInput("major_customer", div(bs_icon("building"), "고객사 주요 업무"), value = customer0, rows = 3),
        textAreaInput("major_note", div(bs_icon("chat-left-text"), "비고"), value = note0, rows = 3),
        footer = tagList(
          modalButton("취소"),
          actionButton("major_save", div(bs_icon("check-circle"), "저장"), class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  }
  
  observeEvent(input$major_add, {
    req(authed())
    show_major_modal(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$major_edit, {
    req(authed())
    id <- major_selected_id()
    req(id)
    
    df <- major_table_data()
    row <- df[df$id == id, , drop = FALSE]
    if (nrow(row) == 0) return()
    show_major_modal(row[1, ])
  }, ignoreInit = TRUE)
  
  observeEvent(input$major_delete, {
    req(authed())
    id <- major_selected_id()
    req(id)
    
    showModal(
      modalDialog(
        title = div(bs_icon("trash"), " 주요 일정 삭제"),
        div(class = "alert alert-danger", "선택한 주요 일정을 삭제하시겠습니까?"),
        footer = tagList(
          modalButton("아니오"),
          actionButton("major_confirm_delete", "예", class = "btn-danger")
        ),
        easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$major_confirm_delete, {
    req(authed())
    removeModal()
    
    id <- major_selected_id()
    req(id)
    
    deleted <- delete_major_supabase(id, jwt = current_jwt())
    if (isTRUE(deleted)) {
      major_rv(major_delete_id(major_rv(), id))
    }
    
    showNotification(div(bs_icon("trash"), "주요 일정이 삭제되었습니다."), type = "message")
  }, ignoreInit = TRUE)
  
  observeEvent(input$major_save, {
    req(authed())
    
    d <- as.Date(input$major_date)
    if (is.na(d)) {
      showNotification("날짜를 입력하세요.", type = "error")
      return()
    }
    
    iso <- as.integer(lubridate::isoweek(d))
    release <- trimws(input$major_release %||% "")
    customer <- trimws(input$major_customer %||% "")
    note <- trimws(input$major_note %||% "")
    
    id <- isolate(major_editing_id())
    
    if (is.null(id)) {
      inserted <- insert_major_supabase(list(
        isoweek = iso,
        date = format(d, "%Y-%m-%d"),
        release_schedule = release,
        customer_work = customer,
        note = note
      ), jwt = current_jwt())
      
      if (!is.null(inserted) && nrow(inserted) > 0) {
        major_rv(major_upsert_row(major_rv(), inserted[1, , drop = FALSE]))
      } else {
        major_rv(fetch_major(current_jwt()))
      }
      
      removeModal()
      showNotification(div(bs_icon("check-circle"), "주요 일정이 추가되었습니다."), type = "message")
      return()
    }
    
    updated <- update_major_supabase(id, list(
      isoweek = iso,
      date = format(d, "%Y-%m-%d"),
      release_schedule = release,
      customer_work = customer,
      note = note
    ), jwt = current_jwt())
    
    if (!is.null(updated) && nrow(updated) > 0) {
      major_rv(major_upsert_row(major_rv(), updated[1, , drop = FALSE]))
    } else {
      major_rv(fetch_major(current_jwt()))
    }
    
    removeModal()
    showNotification(div(bs_icon("check-circle"), "주요 일정이 수정되었습니다."), type = "message")
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------
  # ✅ Delivable (DT + CRUD)
  # -------------------------------------------------
  deliverable_table_data <- reactive({
    df <- deliverable_rv()
    if (is.null(df) || nrow(df) == 0) return(empty_deliverable())
    df %>% mutate(deadline = as.Date(deadline)) %>% arrange(deadline, id)
  })
  
  output$deliverable_table <- DT::renderDT({
    req(authed())
    df <- deliverable_table_data()
    if (is.null(df) || nrow(df) == 0) df <- empty_deliverable()
    
    assignee_display <- vapply(df$assignees, function(x) {
      if (is.na(x) || !nzchar(x)) return("")
      ems <- trimws(strsplit(x, ",")[[1]])
      nm <- vapply(ems, function(e) user_name(e), character(1))
      paste(nm, collapse = ", ")
    }, character(1))
    
    df_disp <- df %>% mutate(assignees_label = assignee_display) %>%
      select(id, isoweek, deadline, assignees_label, content, status, note)
    colnames(df_disp) <- c("id", "주차(isoweek)", "마감일", "담당", "내용", "완료", "비고")
    
    DT::datatable(
      df_disp,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(2, "asc")),
        columnDefs = list(
          list(targets = 0, visible = FALSE)
        )
      )
    )
  }, server = FALSE)
  
  observe({
    req(authed())
    sel <- input$deliverable_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      shinyjs::disable("deliverable_edit")
      shinyjs::disable("deliverable_delete")
    } else {
      shinyjs::enable("deliverable_edit")
      shinyjs::enable("deliverable_delete")
    }
  })
  
  deliverable_selected_id <- reactive({
    req(authed())
    sel <- input$deliverable_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    
    df <- deliverable_table_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (sel > nrow(df)) return(NULL)
    
    df$id[sel]
  })
  
  show_deliverable_modal <- function(row = NULL) {
    req(authed())
    
    choices <- get_participant_choices_new()
    
    if (is.null(row)) {
      deliverable_editing_id(NULL)
      d0 <- Sys.Date()
      assignees0 <- character(0)
      content0 <- ""
      status0 <- "Holding"
      note0 <- ""
      modal_title <- "Delivable 추가"
    } else {
      deliverable_editing_id(row$id[1])
      d0 <- as.Date(row$deadline[1])
      raw_assignees <- row$assignees[1]
      if (is.na(raw_assignees)) raw_assignees <- ""
      assignees0 <- trimws(strsplit(raw_assignees %||% "", ",")[[1]])
      if (length(assignees0) == 1 && assignees0 == "") assignees0 <- character(0)
      content0 <- row$content[1] %||% ""
      status0 <- row$status[1] %||% "Holding"
      note0 <- row$note[1] %||% ""
      modal_title <- "Delivable 수정"
    }
    
    showModal(
      modalDialog(
        title = div(bs_icon("check2-circle"), paste0(" ", modal_title)),
        size = "l",
        dateInput("deliverable_deadline", div(bs_icon("calendar-event"), "마감일"), value = d0),
        checkboxGroupInput("deliverable_assignees", div(bs_icon("people"), "담당"), choices = choices, selected = assignees0, inline = FALSE),
        textAreaInput("deliverable_content", div(bs_icon("pencil"), "내용"), value = content0, rows = 3),
        selectInput("deliverable_status", div(bs_icon("flag"), "완료"), choices = c("DONE", "Holding"), selected = status0),
        textAreaInput("deliverable_note", div(bs_icon("chat-left-text"), "비고"), value = note0, rows = 3),
        footer = tagList(
          modalButton("취소"),
          actionButton("deliverable_save", div(bs_icon("check-circle"), "저장"), class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  }
  
  observeEvent(input$deliverable_add, {
    req(authed())
    show_deliverable_modal(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$deliverable_edit, {
    req(authed())
    id <- deliverable_selected_id()
    req(id)
    
    df <- deliverable_table_data()
    row <- df[df$id == id, , drop = FALSE]
    if (nrow(row) == 0) return()
    show_deliverable_modal(row[1, ])
  }, ignoreInit = TRUE)
  
  observeEvent(input$deliverable_delete, {
    req(authed())
    id <- deliverable_selected_id()
    req(id)
    
    showModal(
      modalDialog(
        title = div(bs_icon("trash"), " Delivable 삭제"),
        div(class = "alert alert-danger", "선택한 Delivable 항목을 삭제하시겠습니까?"),
        footer = tagList(
          modalButton("아니오"),
          actionButton("deliverable_confirm_delete", "예", class = "btn-danger")
        ),
        easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$deliverable_confirm_delete, {
    req(authed())
    removeModal()
    
    id <- deliverable_selected_id()
    req(id)
    
    deleted <- delete_deliverable_supabase(id, jwt = current_jwt())
    if (isTRUE(deleted)) {
      deliverable_rv(deliverable_delete_id(deliverable_rv(), id))
    }
    
    showNotification(div(bs_icon("trash"), "Delivable 항목이 삭제되었습니다."), type = "message")
  }, ignoreInit = TRUE)
  
  observeEvent(input$deliverable_save, {
    req(authed())
    
    d <- as.Date(input$deliverable_deadline)
    if (is.na(d)) {
      showNotification("마감일을 입력하세요.", type = "error")
      return()
    }
    
    iso <- as.integer(lubridate::isoweek(d))
    
    assignees <- input$deliverable_assignees
    if (is.null(assignees)) assignees <- character(0)
    assignees <- paste(assignees, collapse = ",")
    
    content <- trimws(input$deliverable_content %||% "")
    status <- input$deliverable_status %||% "Holding"
    note <- trimws(input$deliverable_note %||% "")
    
    id <- isolate(deliverable_editing_id())
    
    if (is.null(id)) {
      inserted <- insert_deliverable_supabase(list(
        isoweek = iso,
        deadline = format(d, "%Y-%m-%d"),
        assignees = assignees,
        content = content,
        status = status,
        note = note
      ), jwt = current_jwt())
      
      if (!is.null(inserted) && nrow(inserted) > 0) {
        deliverable_rv(deliverable_upsert_row(deliverable_rv(), inserted[1, , drop = FALSE]))
      } else {
        deliverable_rv(fetch_deliverable(current_jwt()))
      }
      
      removeModal()
      showNotification(div(bs_icon("check-circle"), "Delivable이 추가되었습니다."), type = "message")
      return()
    }
    
    updated <- update_deliverable_supabase(id, list(
      isoweek = iso,
      deadline = format(d, "%Y-%m-%d"),
      assignees = assignees,
      content = content,
      status = status,
      note = note
    ), jwt = current_jwt())
    
    if (!is.null(updated) && nrow(updated) > 0) {
      deliverable_rv(deliverable_upsert_row(deliverable_rv(), updated[1, , drop = FALSE]))
    } else {
      deliverable_rv(fetch_deliverable(current_jwt()))
    }
    
    removeModal()
    showNotification(div(bs_icon("check-circle"), "Delivable이 수정되었습니다."), type = "message")
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------
  # ✅ 캘린더 일정 모달 (신규/상세/수정)
  # -------------------------------------------------
  show_new_event_modal <- function(start_date = Sys.Date(), end_date = start_date) {
    req(authed())
    editing_event_id(NULL)
    clear_calendar_selection()
    
    participant_choices <- get_participant_choices_new()
    
    showModal(
      modalDialog(
        title = div(bs_icon("calendar-plus"), " 새 일정 추가"),
        size = "l",
        textInput("event_title", div(bs_icon("pencil"), "제목"), value = ""),
        layout_columns(
          col_widths = c(6, 6),
          dateInput("event_start_date", div(bs_icon("calendar-event"), "시작일"), value = start_date),
          dateInput("event_end_date", div(bs_icon("calendar-check"), "종료일"), value = end_date)
        ),
        selectInput("event_category", div(bs_icon("tag"), "업무 구분"),
                    choices = CATEGORY_OPTIONS, selected = "업무"),
        conditionalPanel(
          condition = "input.event_category == '회의'",
          selectInput("event_location", div(bs_icon("geo-alt"), "장소"),
                      choices = MEETING_LOCATIONS, selected = MEETING_LOCATIONS[1])
        ),
        card(
          class = "mb-3",
          card_header(class = "bg-light", div(bs_icon("clock"), "시간 설정")),
          card_body(
            radioButtons(
              "event_time_type",
              NULL,
              choices = c("종일" = "allday", "시간 지정" = "time"),
              selected = "allday",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.event_time_type == 'time'",
              layout_columns(
                col_widths = c(6, 6),
                selectInput("event_start_time", "시작 시간", choices = TIME_OPTIONS, selected = "09:00"),
                selectInput("event_end_time", "종료 시간", choices = TIME_OPTIONS, selected = "18:00")
              )
            )
          )
        ),
        checkboxGroupInput("event_participants", div(bs_icon("people"), "함께하는 사람"),
                           choices = participant_choices),
        textAreaInput("event_memo", div(bs_icon("chat-left-text"), "메모"),
                      rows = 3, placeholder = "메모를 입력하세요", value = ""),
        checkboxInput("event_sync_google", div(bs_icon("google"), "구글 캘린더 연동 (예정)"), value = FALSE),
        footer = tagList(
          modalButton("취소"),
          actionButton("save_event", div(bs_icon("check-circle"), "저장"), class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  }
  
  show_view_event_modal <- function(ev) {
    req(authed())
    clear_calendar_selection()
    
    selected_participants <- character(0)
    if (!is.na(ev$participants) && nzchar(ev$participants)) {
      selected_participants <- trimws(strsplit(ev$participants, ",")[[1]])
    }
    
    time_display <- if (isTRUE(ev$is_allday)) "종일" else paste0(ev$start_time, " - ", ev$end_time)
    
    memo_history <- ev$memo_history[[1]]
    has_memos <- !is.null(memo_history) && length(memo_history) > 0
    
    all_participant_choices <- get_all_participant_choices()
    
    display_title <- if (!is.na(ev$category) && ev$category == "휴가") {
      paste0("⛱ (", ev$creator_name, " 휴가)")
    } else {
      ev$title
    }
    
    showModal(
      modalDialog(
        title = div(bs_icon("calendar-event"), " 일정 상세"),
        size = "l",
        div(class = "mb-3", tags$h5(bs_icon("pencil"), " 제목"), tags$p(class = "ps-3", display_title)),
        div(
          class = "mb-3",
          tags$h5(bs_icon("calendar-range"), " 일정"),
          tags$p(class = "ps-3", paste(format(ev$start_date, "%Y-%m-%d"), "~", format(ev$end_date, "%Y-%m-%d")))
        ),
        div(class = "mb-3", tags$h5(bs_icon("clock"), " 시간"), tags$p(class = "ps-3", time_display)),
        div(class = "mb-3", tags$h5(bs_icon("tag"), " 업무 구분"), tags$p(class = "ps-3", ev$category)),
        if (!is.na(ev$location) && nzchar(ev$location)) {
          div(class = "mb-3", tags$h5(bs_icon("geo-alt"), " 장소"), tags$p(class = "ps-3", ev$location))
        },
        div(
          class = "mb-3",
          tags$h5(bs_icon("people"), " 참여자"),
          div(
            class = "ps-3",
            div(
              class = "border rounded p-2 bg-light",
              checkboxGroupInput(
                "view_participants",
                NULL,
                choices = all_participant_choices,
                selected = c(ev$creator_email, selected_participants)
              ),
              tags$script(HTML("setTimeout(function() { $('#view_participants input').prop('disabled', true); }, 50);"))
            ),
            tags$small(class = "text-muted", bs_icon("info-circle"), sprintf(" 작성자: %s", ev$creator_name))
          )
        ),
        if (has_memos) {
          div(
            class = "mb-3",
            tags$h5(bs_icon("chat-left-text"), " 메모 이력"),
            div(
              style = "max-height:300px; overflow-y:auto;",
              lapply(rev(memo_history), function(memo_item) {
                card(
                  class = "mb-2",
                  card_body(
                    class = "py-2",
                    tags$p(class = "mb-2", memo_item$content),
                    tags$small(
                      class = "text-muted",
                      bs_icon("person"),
                      sprintf(" %s | ", memo_item$author),
                      bs_icon("clock"),
                      sprintf(" %s", memo_item$time)
                    )
                  )
                )
              })
            )
          )
        },
        footer = tagList(
          if (ev$creator_email == current_user_email()) {
            actionButton("edit_from_view", div(bs_icon("pencil"), "수정"), class = "btn-primary")
          } else {
            actionButton("request_edit", div(bs_icon("pencil"), "수정 요청"), class = "btn-warning")
          },
          modalButton("닫기")
        ),
        easyClose = TRUE
      )
    )
  }
  
  show_edit_event_modal <- function(ev) {
    req(authed())
    clear_calendar_selection()
    
    is_synced <- isTRUE(ev$google_synced)
    participant_choices <- get_participant_choices_edit(ev$creator_email)
    
    selected_part <- character(0)
    if (!is.na(ev$participants) && nzchar(ev$participants)) {
      selected_part <- trimws(strsplit(ev$participants, ",")[[1]])
    }
    selected_part <- intersect(selected_part, unname(participant_choices))
    
    time_type <- if (isTRUE(ev$is_allday)) "allday" else "time"
    start_time_val <- if (!is.na(ev$start_time) && nzchar(ev$start_time)) ev$start_time else "09:00"
    end_time_val <- if (!is.na(ev$end_time) && nzchar(ev$end_time)) ev$end_time else "18:00"
    
    memo_history <- ev$memo_history[[1]]
    has_memos <- !is.null(memo_history) && length(memo_history) > 0
    
    showModal(
      modalDialog(
        title = div(bs_icon("pencil-square"), " 일정 수정"),
        size = "l",
        div(
          class = "alert alert-secondary mb-3",
          bs_icon("person-fill"),
          sprintf(" 작성자: %s (%s)", ev$creator_name, ev$creator_email)
        ),
        textInput("event_title", div(bs_icon("pencil"), "제목"), value = ev$title),
        layout_columns(
          col_widths = c(6, 6),
          dateInput("event_start_date", div(bs_icon("calendar-event"), "시작일"), value = as.Date(ev$start_date)),
          dateInput("event_end_date", div(bs_icon("calendar-check"), "종료일"), value = as.Date(ev$end_date))
        ),
        selectInput("event_category", div(bs_icon("tag"), "업무 구분"), choices = CATEGORY_OPTIONS, selected = ev$category),
        conditionalPanel(
          condition = "input.event_category == '회의'",
          selectInput(
            "event_location",
            div(bs_icon("geo-alt"), "장소"),
            choices = MEETING_LOCATIONS,
            selected = if (!is.na(ev$location) && nzchar(ev$location)) ev$location else MEETING_LOCATIONS[1]
          )
        ),
        card(
          class = "mb-3",
          card_header(class = "bg-light", div(bs_icon("clock"), "시간 설정")),
          card_body(
            radioButtons(
              "event_time_type",
              NULL,
              choices = c("종일" = "allday", "시간 지정" = "time"),
              selected = time_type,
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.event_time_type == 'time'",
              layout_columns(
                col_widths = c(6, 6),
                selectInput("event_start_time", "시작 시간", choices = TIME_OPTIONS, selected = start_time_val),
                selectInput("event_end_time", "종료 시간", choices = TIME_OPTIONS, selected = end_time_val)
              )
            )
          )
        ),
        checkboxGroupInput(
          "event_participants",
          div(bs_icon("people"), "함께하는 사람 (작성자 제외)"),
          choices = participant_choices,
          selected = selected_part
        ),
        if (has_memos) {
          div(
            class = "mb-3",
            tags$label(class = "form-label", bs_icon("clock-history"), " 기존 메모 이력"),
            div(
              class = "border rounded p-2",
              style = "max-height:200px; overflow-y:auto; background-color:#f8f9fa;",
              lapply(rev(memo_history), function(memo_item) {
                div(
                  class = "mb-2 pb-2 border-bottom",
                  tags$p(class = "mb-1 small", memo_item$content),
                  tags$small(
                    class = "text-muted",
                    bs_icon("person"),
                    sprintf(" %s | ", memo_item$author),
                    bs_icon("clock"),
                    sprintf(" %s", memo_item$time)
                  )
                )
              })
            )
          )
        },
        textAreaInput("event_memo", div(bs_icon("chat-left-text"), "새 메모 추가"),
                      rows = 3, placeholder = "새 메모(선택)", value = ""),
        if (is_synced) {
          div(class = "alert alert-info", bs_icon("info-circle"), " 이 일정은 구글 캘린더 연동(더미) 상태입니다.")
        } else {
          checkboxInput("event_sync_google", div(bs_icon("google"), "구글 캘린더 연동 (더미)"), value = FALSE)
        },
        footer = tagList(
          actionButton("delete_event", div(bs_icon("trash"), "삭제"),
                       class = "btn-danger", style = "float:left;"),
          modalButton("취소"),
          actionButton("save_event", div(bs_icon("check-circle"), "저장"), class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  }
  
  # -------------------------------------------------
  # ✅ 캘린더 렌더링
  # -------------------------------------------------
  output$calendar <- renderCalendar({
    req(authed())
    df <- events_filtered()
    
    view_mode <- input$view_mode %||% "month"
    view <- if (view_mode == "week") "week" else "month"
    
    hol <- HOLIDAYS
    hol <- hol[!is.na(hol)]
    hol_ymd <- format(hol, "%Y-%m-%d")
    hol_md  <- format(hol, "%m/%d")
    
    hol_js <- if (length(hol_ymd) == 0) "[]" else paste0("[", paste(sprintf("'%s'", hol_ymd), collapse = ","), "]")
    hol_md_js <- if (length(hol_md) == 0) "[]" else paste0("[", paste(sprintf("'%s'", hol_md), collapse = ","), "]")
    
    monthGridHeader_js <- JS(sprintf(
      "function(dayModel) {
         var holidays = %s;
         var dateStr = (dayModel && dayModel.date) ? dayModel.date : '';
         if (!dateStr) return '';

         var dt = new Date(dateStr + 'T00:00:00');
         var day = parseInt(dateStr.split('-')[2], 10);

         var isWeekend = (dt.getDay() === 0 || dt.getDay() === 6);
         var isHoliday = isWeekend || (holidays.indexOf(dateStr) >= 0);

         var classNames = ['tui-full-calendar-weekday-grid-date'];
         if (dayModel && dayModel.isToday) classNames.push('tui-full-calendar-weekday-grid-date-decorator');

         var style = isHoliday ? ' style=\"color:#dc3545 !important;font-weight:700;\"' : '';
         return '<span class=\"' + classNames.join(' ') + '\"' + style + '>' + day + '</span>';
       }",
      hol_js
    ))
    
    monthDayname_js <- JS(
      "function(model) {
         var label = '';
         if (model && (model.label || model.dayName)) label = (model.label || model.dayName);
         else label = model;

         label = (label || '').toString();
         var u = label.toUpperCase();
         var isSat = (u === 'SAT' || label === '토');
         var isSun = (u === 'SUN' || label === '일');

         var style = (isSat || isSun) ? ' style=\"color:#dc3545 !important;font-weight:700;\"' : '';
         return '<span' + style + '>' + label + '</span>';
       }"
    )
    
    weekDayname_js <- JS(sprintf(
      "function(model) {
         var holYMD = %s;
         var holMD  = %s;

         var dateTxt = (model && model.date) ? model.date : '';
         var dayName = (model && model.dayName) ? model.dayName : '';

         var u = (dayName || '').toString().toUpperCase();
         var isSat = (u === 'SAT' || dayName === '토');
         var isSun = (u === 'SUN' || dayName === '일');

         var isHoliday = false;
         if (dateTxt && dateTxt.length >= 10) {
           isHoliday = (holYMD.indexOf(dateTxt.substring(0, 10)) >= 0);
         } else if (dateTxt) {
           isHoliday = (holMD.indexOf(dateTxt) >= 0);
         }

         var red = (isSat || isSun || isHoliday);
         var style = red ? ' style=\"color:#dc3545 !important;font-weight:700;\"' : '';

         return '<span class=\"tui-full-calendar-dayname-date\"' + style + '>' + dateTxt + '</span>&nbsp;&nbsp;' +
                '<span class=\"tui-full-calendar-dayname-name\"' + style + '>' + dayName + '</span>';
       }",
      hol_js, hol_md_js
    ))
    
    cal_obj <- calendar(
      view = view,
      defaultDate = Sys.Date(),
      useDetailPopup = FALSE,
      useCreationPopup = FALSE,
      isReadOnly = FALSE,
      navigation = TRUE,
      gridSelection = TRUE,
      navOpts = navigation_options(today_label = "오늘", prev_label = "<", next_label = ">")
    ) %>%
      cal_props(build_calendar_props()) %>%
      cal_theme(
        common.holiday.color = "#dc3545",
        common.saturday.color = "#dc3545"
      ) %>%
      cal_template(
        monthGridHeader = monthGridHeader_js,
        monthDayname = monthDayname_js,
        weekDayname = weekDayname_js,
        monthDayName = monthDayname_js,
        weekDayName = weekDayname_js
      )
    
    sched <- events_to_schedules(df)
    if (!is.null(sched) && nrow(sched) > 0) cal_obj <- cal_obj %>% cal_schedules(sched)
    
    if (view == "month") {
      cal_obj <- cal_obj %>% cal_month_options(
        startDayOfWeek = 1,
        isAlways6Week = FALSE,
        visibleEventCount = max(999L, month_max_events())
      )
    } else {
      cal_obj <- cal_obj %>% cal_week_options(
        startDayOfWeek = 1,
        workweek = FALSE,
        eventView = c("allday", "time"),
        hourStart = 0,
        hourEnd = 24
      )
    }
    
    cal_obj %>%
      cal_events(
        clickSchedule = JS(
          "function(event) {",
          "  var s = event.schedule || event.event || event;",
          "  if (!s || !s.id) return;",
          "  var id = s.id;",
          "  if (typeof id === 'string') {",
          "    var m = id.match(/^vac-(\\d+)-/);",
          "    if (m && m[1]) id = m[1];",
          "  }",
          "  Shiny.setInputValue('calendar_click_id', id, {priority: 'event'});",
          "}"
        ),
        selectDateTime = JS(
          "function(event) {",
          "  var start = event.start;",
          "  if (start && typeof start.toDate === 'function') start = start.toDate();",
          "  var yyyy = start.getFullYear();",
          "  var mm   = ('0' + (start.getMonth() + 1)).slice(-2);",
          "  var dd   = ('0' + start.getDate()).slice(-2);",
          "  var dateStr = yyyy + '-' + mm + '-' + dd;",
          "  Shiny.setInputValue('calendar_new_date', dateStr, {priority: 'event'});",
          "}"
        )
      )
  })
  
  outputOptions(output, "calendar", suspendWhenHidden = FALSE)
  
  # -------------------------------------------------
  # 타임라인 범위 라벨 + unified timeline
  # (이하 로직은 원본과 동일)
  # -------------------------------------------------
  output$timeline_range_label <- renderUI({
    req(authed())
    df <- events_filtered()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    all_dates <- c(as.Date(df$start_date), as.Date(df$end_date))
    min_date <- min(all_dates, na.rm = TRUE)
    max_date <- max(all_dates, na.rm = TRUE)
    
    span(class = "text-muted small",
         sprintf("%s ~ %s", format(min_date, "%Y-%m-%d"), format(max_date, "%Y-%m-%d")))
  })
  
  output$unified_timeline <- renderUI({
    req(authed())
    df <- events_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(
        div(
          class = "text-center text-muted p-5",
          bs_icon("calendar-x", size = "3em"),
          tags$p(class = "mt-3", "표시할 일정이 없습니다.")
        )
      )
    }
    
    all_dates <- c(as.Date(df$start_date), as.Date(df$end_date))
    min_date <- min(all_dates, na.rm = TRUE)
    max_date <- max(all_dates, na.rm = TRUE)
    
    days <- seq(min_date, max_date, by = "day")
    n_days <- length(days)
    if (n_days <= 0) return(NULL)
    
    hol <- HOLIDAYS
    hol <- hol[!is.na(hol)]
    is_holiday <- days %in% hol
    
    day_u <- as.integer(format(days, "%u"))
    day_name <- c("월","화","수","목","금","토","일")[day_u]
    is_weekend <- day_u >= 6
    day_num <- format(days, "%m/%d")
    is_month_start <- c(TRUE, format(days[-1], "%d") == "01")
    month_label <- ifelse(is_month_start, format(days, "%Y년 %m월"), "")
    
    date_to_idx <- function(d) as.integer(as.Date(d) - min_date) + 1L
    
    vac_df <- df[!is.na(df$category) & df$category == "휴가", , drop = FALSE]
    norm_df <- df[is.na(df$category) | df$category != "휴가", , drop = FALSE]
    
    evs <- norm_df %>%
      mutate(
        start_idx = date_to_idx(start_date),
        end_idx = date_to_idx(end_date),
        duration = end_idx - start_idx + 1
      ) %>%
      arrange(start_idx, end_idx, id)
    
    lane_end <- integer(0)
    lane <- integer(nrow(evs))
    for (i in seq_len(nrow(evs))) {
      s <- evs$start_idx[i]
      e <- evs$end_idx[i]
      k <- which(lane_end < s)[1]
      if (is.na(k)) {
        lane_end <- c(lane_end, e)
        lane[i] <- length(lane_end)
      } else {
        lane_end[k] <- e
        lane[i] <- k
      }
    }
    evs$lane <- lane
    n_lanes <- max(evs$lane, 1L)
    
    col_width <- 120
    grid_cols <- paste(rep(paste0(col_width, "px"), n_days), collapse = " ")
    
    rgba <- function(hex, alpha = 0.18) {
      x <- gsub("^#", "", hex)
      if (nchar(x) == 3) x <- paste0(rep(strsplit(x, "")[[1]], each = 2), collapse = "")
      r <- strtoi(substr(x, 1, 2), base = 16L)
      g <- strtoi(substr(x, 3, 4), base = 16L)
      b <- strtoi(substr(x, 5, 6), base = 16L)
      sprintf("rgba(%d,%d,%d,%.3f)", r, g, b, alpha)
    }
    
    make_chip <- function(label, color_hex) {
      span(class = "timeline-chip", style = paste0("background-color:", color_hex, ";"), label)
    }
    
    header <- div(
      style = paste0(
        "display:grid; grid-template-columns:", grid_cols, "; ",
        "position: sticky; top: 0; z-index: 20; background: white;"
      ),
      lapply(seq_len(n_days), function(i) {
        special <- is_weekend[i] || is_holiday[i]
        
        vac_on_day <- NULL
        if (nrow(vac_df) > 0) {
          vac_on_day <- vac_df[
            as.Date(vac_df$start_date) <= days[i] & as.Date(vac_df$end_date) >= days[i],
            ,
            drop = FALSE
          ]
        }
        
        vac_nodes <- NULL
        if (!is.null(vac_on_day) && nrow(vac_on_day) > 0) {
          max_show <- 2L
          show_n <- min(nrow(vac_on_day), max_show)
          vac_nodes <- c(
            lapply(seq_len(show_n), function(j) {
              evv <- vac_on_day[j, , drop = FALSE]
              div(
                class = "timeline-vac",
                title = "클릭하여 휴가 일정 보기",
                onclick = paste0(
                  "Shiny.setInputValue('calendar_click_id', '",
                  evv$id[1],
                  "', {priority: 'event'})"
                ),
                paste0("⛱ (", evv$creator_name[1], " 휴가)")
              )
            }),
            if (nrow(vac_on_day) > max_show) {
              list(
                div(
                  class = "timeline-vac",
                  style = "cursor:default;",
                  paste0("+", nrow(vac_on_day) - max_show)
                )
              )
            } else {
              NULL
            }
          )
        }
        
        div(
          class = paste0("text-center py-2 border-end border-bottom ", if (is_weekend[i]) "bg-light" else "bg-white"),
          div(class = paste("small", if (special) "text-danger" else "text-muted"), day_name[i]),
          div(class = if (special) "fw-bold text-danger" else "fw-bold", day_num[i]),
          if (nzchar(month_label[i])) div(class = "small fw-bold text-primary", month_label[i]),
          if (!is.null(vac_nodes)) div(class = "timeline-vac-wrap", tagList(vac_nodes))
        )
      })
    )
    
    body <- div(
      style = paste0(
        "display:grid; grid-template-columns:", grid_cols, "; ",
        "grid-auto-rows: 62px; position: relative;"
      ),
      
      lapply(seq_len(n_days), function(i) {
        bg <- if (is_holiday[i]) "#fff5f5" else if (is_weekend[i]) "#f8f9fa" else "transparent"
        div(
          style = paste0(
            "grid-column:", i, "; grid-row: 1 / ", (n_lanes + 1), "; ",
            "background:", bg, "; ",
            "border-right: 1px solid #dee2e6; pointer-events:none;"
          )
        )
      }),
      
      lapply(seq_len(nrow(evs)), function(i) {
        ev <- evs[i, ]
        
        creator_email <- ev$creator_email[1]
        creator_name <- ev$creator_name[1]
        title <- ev$title[1]
        
        start_d <- as.Date(ev$start_date[1])
        end_d <- as.Date(ev$end_date[1])
        
        fmt_bar <- function(d) format(as.Date(d), "%m/%d")
        date_label_bar <- if (start_d == end_d) fmt_bar(start_d) else paste0(fmt_bar(start_d), "~", fmt_bar(end_d))
        date_label_long <- if (start_d == end_d) {
          format(start_d, "%Y-%m-%d")
        } else {
          paste0(format(start_d, "%Y-%m-%d"), " ~ ", format(end_d, "%Y-%m-%d"))
        }
        
        time_label <- if (isTRUE(ev$is_allday[1])) {
          "종일"
        } else {
          st <- ev$start_time[1]
          et <- ev$end_time[1]
          if (!is.na(st) && !is.na(et) && nzchar(st) && nzchar(et)) paste0(st, " ~ ", et) else ""
        }
        
        participant_emails <- character(0)
        part_raw <- ev$participants[1]
        if (!is.na(part_raw) && nzchar(part_raw)) {
          participant_emails <- trimws(strsplit(part_raw, ",")[[1]])
          participant_emails <- participant_emails[nzchar(participant_emails)]
          participant_emails <- setdiff(unique(participant_emails), creator_email)
        }
        
        participant_names <- if (length(participant_emails) > 0) {
          vapply(participant_emails, user_name, character(1))
        } else {
          character(0)
        }
        
        people_emails_for_bg <- c(creator_email, participant_emails)
        people_colors_for_bg <- unique(vapply(people_emails_for_bg, user_color, character(1)))
        if (length(people_colors_for_bg) == 0) people_colors_for_bg <- "#7f7f7f"
        
        bg_css <- if (length(people_colors_for_bg) >= 2) {
          paste0(
            "background: linear-gradient(90deg, ",
            paste(vapply(people_colors_for_bg, function(h) rgba(h, 0.18), character(1)), collapse = ", "),
            ");"
          )
        } else {
          paste0("background-color: ", rgba(people_colors_for_bg[1], 0.18), ";")
        }
        
        max_part_show <- 3L
        part_show <- head(participant_emails, max_part_show)
        part_left <- length(participant_emails) - length(part_show)
        
        chips <- list(make_chip(paste0("✍ ", creator_name), user_color(creator_email)))
        if (length(part_show) > 0) chips <- c(chips, lapply(part_show, function(em) make_chip(user_name(em), user_color(em))))
        if (part_left > 0) chips <- c(chips, list(make_chip(paste0("+", part_left), "#6c757d")))
        
        people_full_label <- if (length(participant_names) > 0) {
          paste0("작성: ", creator_name, " | 참여: ", paste(participant_names, collapse = ", "))
        } else {
          paste0("작성: ", creator_name)
        }
        
        loc <- ev$location[1]
        title_attr <- paste0(
          title, "\n",
          "날짜: ", date_label_long,
          if (nzchar(time_label)) paste0("\n시간: ", time_label) else "",
          if (!is.na(loc) && nzchar(loc)) paste0("\n장소: ", loc) else "",
          "\n", people_full_label
        )
        
        div(
          title = title_attr,
          onclick = paste0("Shiny.setInputValue('calendar_click_id', '", ev$id[1], "', {priority: 'event'})"),
          style = paste0(
            "grid-column:", ev$start_idx[1], " / ", (ev$end_idx[1] + 1), "; ",
            "grid-row:", ev$lane[1], "; ",
            "margin: 6px; padding: 6px 8px; ",
            "border-left: 4px solid ", user_color(creator_email), "; ",
            bg_css,
            "border-radius: 10px; cursor: pointer; ",
            "overflow: hidden; z-index: 5;"
          ),
          div(
            class = "timeline-bar",
            div(
              class = "timeline-title",
              span(class = "timeline-title-text", title),
              span(class = "timeline-date", paste0("· ", date_label_bar))
            ),
            div(class = "timeline-people", tagList(chips))
          )
        )
      })
    )
    
    div(style = "padding: 12px 20px;", header, body)
  })
  
  outputOptions(output, "unified_timeline", suspendWhenHidden = FALSE)
  
  # -------------------------------------------------
  # ✅ 캘린더 일정 인터랙션
  # -------------------------------------------------
  observeEvent(input$new_event, {
    req(authed())
    show_new_event_modal(Sys.Date(), Sys.Date())
  }, ignoreInit = TRUE)
  
  observeEvent(input$calendar_new_date, {
    req(authed())
    req(input$calendar_new_date)
    date <- as.Date(input$calendar_new_date)
    show_new_event_modal(date, date)
  }, ignoreInit = TRUE)
  
  observeEvent(input$calendar_click_id, {
    req(authed())
    req(input$calendar_click_id)
    
    event_id <- suppressWarnings(as.integer(input$calendar_click_id))
    if (is.na(event_id)) return()
    
    df <- events_rv()
    ev <- df[df$id == event_id, , drop = FALSE]
    if (nrow(ev) == 0) {
      showNotification("일정을 찾을 수 없습니다.", type = "error")
      return()
    }
    
    ev <- ev[1, ]
    viewing_event_id(ev$id)
    show_view_event_modal(ev)
  }, ignoreInit = TRUE)
  
  observeEvent(input$edit_from_view, {
    req(authed())
    id <- viewing_event_id()
    req(id)
    
    df <- events_rv()
    ev <- df[df$id == id, , drop = FALSE]
    if (nrow(ev) == 0) return()
    
    editing_event_id(id)
    removeModal()
    show_edit_event_modal(ev[1, ])
  }, ignoreInit = TRUE)
  
  observeEvent(input$request_edit, {
    req(authed())
    id <- viewing_event_id()
    req(id)
    
    df <- events_rv()
    ev <- df[df$id == id, , drop = FALSE]
    if (nrow(ev) == 0) return()
    
    creator_label <- user_name(ev$creator_email[1])
    removeModal()
    
    showModal(
      modalDialog(
        title = div(bs_icon("exclamation-triangle"), "다른 사용자의 일정입니다."),
        div(class = "alert alert-warning",
            paste0(creator_label, "님이 작성한 일정입니다. 정말 수정하시겠습니까?")),
        footer = tagList(
          modalButton("아니오"),
          actionButton("confirm_edit_other", "예", class = "btn-danger")
        ),
        easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_edit_other, {
    req(authed())
    removeModal()
    id <- viewing_event_id()
    req(id)
    
    df <- events_rv()
    ev <- df[df$id == id, , drop = FALSE]
    if (nrow(ev) == 0) {
      showNotification("일정을 찾을 수 없습니다.", type = "error")
      return()
    }
    
    editing_event_id(id)
    show_edit_event_modal(ev[1, ])
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------
  # ✅ 저장: INSERT/UPDATE 후 events_rv를 "증분 업데이트" (full fetch X)
  # -------------------------------------------------
  observeEvent(input$save_event, {
    req(authed())
    
    title <- input$event_title
    start_date <- input$event_start_date
    end_date <- input$event_end_date
    memo <- input$event_memo
    cat <- input$event_category
    time_type <- input$event_time_type
    
    if (!is.null(cat) && cat == "휴가") {
      if (is.null(title) || !nzchar(trimws(title))) title <- "휴가"
    }
    
    if (is.null(title) || !nzchar(trimws(title))) {
      showNotification("제목을 입력하세요.", type = "error")
      return()
    }
    if (is.null(start_date) || is.null(end_date)) {
      showNotification("시작일과 종료일을 입력하세요.", type = "error")
      return()
    }
    
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    if (end_date < start_date) {
      showNotification("종료일은 시작일보다 빠를 수 없습니다.", type = "error")
      return()
    }
    
    is_allday <- is.null(time_type) || (time_type == "allday")
    
    if (is_allday) {
      start_time <- NA_character_
      end_time <- NA_character_
    } else {
      start_time <- input$event_start_time
      end_time <- input$event_end_time
      
      if (is.null(start_time) || is.null(end_time)) {
        showNotification("시작 시간과 종료 시간을 선택하세요.", type = "error")
        return()
      }
      
      st <- as.POSIXct(paste(start_date, start_time))
      et <- as.POSIXct(paste(start_date, end_time))
      if (!is.na(st) && !is.na(et) && et <= st) {
        showNotification("종료 시간은 시작 시간보다 늦어야 합니다.", type = "error")
        return()
      }
    }
    
    if (!is.null(cat) && cat == "회의") {
      loc <- input$event_location
      if (is.null(loc) || !nzchar(loc)) {
        showNotification("회의 일정은 장소를 선택해야 합니다.", type = "error")
        return()
      }
    } else {
      loc <- NA_character_
    }
    
    id <- isolate(editing_event_id())
    jwt <- current_jwt()
    
    # 신규
    if (is.null(id)) {
      creator <- current_user_email()
      
      checkbox_part <- input$event_participants
      if (is.null(checkbox_part)) checkbox_part <- character(0)
      participants_vec <- setdiff(checkbox_part, creator)
      participants_str <- if (length(participants_vec) > 0) paste(participants_vec, collapse = ",") else ""
      
      google_synced <- isTRUE(input$event_sync_google)
      
      memo_history_list <- list()
      if (!is.null(memo) && nzchar(trimws(memo))) {
        memo_history_list <- list(list(
          content = memo,
          author = current_user_name(),
          time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ))
      }
      
      inserted_df <- insert_event_supabase(list(
        title = title,
        start_date = format(start_date, "%Y-%m-%d"),
        end_date = format(end_date, "%Y-%m-%d"),
        is_allday = is_allday,
        start_time = start_time,
        end_time = end_time,
        category = cat,
        location = loc,
        participants = participants_str,
        creator_email = creator,
        creator_name = current_user_name(),
        google_calendar_id = if (google_synced) "local_dummy_calendar" else NA_character_,
        google_event_id = if (google_synced) "pending" else NA_character_,
        google_synced = google_synced,
        memo_history = memo_history_list
      ), jwt = jwt)
      
      if (!is.null(inserted_df) && nrow(inserted_df) > 0) {
        df0 <- events_rv()
        for (k in seq_len(nrow(inserted_df))) {
          df0 <- events_upsert_row(df0, inserted_df[k, , drop = FALSE])
        }
        events_rv(df0)
      } else {
        # fallback
        events_rv(fetch_events(jwt))
      }
      
      removeModal()
      showNotification(div(bs_icon("check-circle"), "일정이 추가되었습니다."), type = "message")
      return()
    }
    
    # 수정
    ev_df <- events_rv()
    ev0 <- ev_df[ev_df$id == id, , drop = FALSE]
    if (nrow(ev0) == 0) {
      showNotification("일정을 찾을 수 없습니다.", type = "error")
      return()
    }
    
    checkbox_part <- input$event_participants
    if (is.null(checkbox_part)) checkbox_part <- character(0)
    participants_str <- if (length(checkbox_part) > 0) paste(checkbox_part, collapse = ",") else ""
    
    is_synced_existing <- isTRUE(ev0$google_synced[1])
    sync_to_google <- if (is_synced_existing) TRUE else isTRUE(input$event_sync_google)
    
    existing_memo_history <- ev0$memo_history[[1]]
    if (is.null(existing_memo_history)) existing_memo_history <- list()
    
    if (!is.null(memo) && nzchar(trimws(memo))) {
      new_memo_item <- list(
        content = memo,
        author = current_user_name(),
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      existing_memo_history <- c(existing_memo_history, list(new_memo_item))
    }
    
    updated_df <- update_event_supabase(id, list(
      title = title,
      start_date = format(start_date, "%Y-%m-%d"),
      end_date = format(end_date, "%Y-%m-%d"),
      is_allday = is_allday,
      start_time = start_time,
      end_time = end_time,
      category = cat,
      location = loc,
      participants = participants_str,
      google_synced = sync_to_google,
      google_calendar_id = if (sync_to_google) "local_dummy_calendar" else NA_character_,
      google_event_id = if (sync_to_google) paste0("updated-", id) else NA_character_,
      memo_history = existing_memo_history
    ), jwt = jwt)
    
    if (!is.null(updated_df) && nrow(updated_df) > 0) {
      df0 <- events_rv()
      for (k in seq_len(nrow(updated_df))) {
        df0 <- events_upsert_row(df0, updated_df[k, , drop = FALSE])
      }
      events_rv(df0)
    } else {
      events_rv(fetch_events(jwt))
    }
    
    removeModal()
    showNotification(div(bs_icon("check-circle"), "일정이 수정되었습니다."), type = "message")
  }, ignoreInit = TRUE)
  
  observeEvent(input$delete_event, {
    req(authed())
    id <- isolate(editing_event_id())
    req(id)
    
    showModal(
      modalDialog(
        title = div(bs_icon("trash"), " 일정 삭제"),
        div(class = "alert alert-danger", "정말 삭제하시겠습니까?"),
        footer = tagList(
          modalButton("아니오"),
          actionButton("confirm_delete", "예", class = "btn-danger")
        ),
        easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_delete, {
    req(authed())
    removeModal()
    id <- isolate(editing_event_id())
    req(id)
    
    deleted <- delete_event_supabase(id, jwt = current_jwt())
    if (isTRUE(deleted)) {
      events_rv(events_delete_id(events_rv(), id))
    }
    
    editing_event_id(NULL)
    showNotification(div(bs_icon("trash"), "일정이 삭제되었습니다."), type = "message")
  }, ignoreInit = TRUE)
}
