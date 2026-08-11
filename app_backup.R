# ============================================
# تطبيق Dashboard المشتريات - النسخة المتكاملة (v3)
# ============================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(shinyWidgets)
library(readxl)
library(openxlsx)
library(janitor)
library(glue)
library(gridExtra)
library(grid)

# ============================================
# ملفات التخزين الدائم (تُنشأ تلقائياً في مجلد data/)
# ============================================

if (!dir.exists("data")) dir.create("data")

USERS_FILE    <- "data/users.rds"
REQUESTS_FILE <- "data/pending_requests.rds"
DATASET_FILE  <- "data/dataset_current.rds"

# --- المستخدمون الافتراضيون (تُستخدم فقط أول مرة عند عدم وجود users.rds) ---
default_users <- data.frame(
  username  = c("admin", "user1", "user2"),
  password  = c("admin123", "user123", "user456"),
  full_name = c("مدير النظام", "أحمد محمد (Viewer)", "سارة علي (Tester)"),
  role      = c("admin", "viewer", "tester"),
  stringsAsFactors = FALSE
)

if (file.exists(USERS_FILE)) {
  valid_users_init <- readRDS(USERS_FILE)
} else {
  valid_users_init <- default_users
  saveRDS(valid_users_init, USERS_FILE)
}

if (file.exists(REQUESTS_FILE)) {
  pending_requests_init <- readRDS(REQUESTS_FILE)
} else {
  pending_requests_init <- data.frame(
    id = character(0), username = character(0), password = character(0),
    full_name = character(0), requested_role = character(0),
    request_date = character(0), stringsAsFactors = FALSE
  )
  saveRDS(pending_requests_init, REQUESTS_FILE)
}

check_login <- function(username, password, users_df) {
  username <- trimws(username)
  password <- trimws(password)
  
  user <- users_df[tolower(users_df$username) == tolower(username), ]
  
  if (nrow(user) == 0) {
    return(list(success = FALSE, message = "❌ اسم المستخدم غير موجود", type = "error"))
  }
  if (user$password[1] != password) {
    return(list(success = FALSE, message = "❌ كلمة المرور غير صحيحة", type = "error"))
  }
  return(list(
    success = TRUE,
    message = paste0("✅ مرحباً ", user$full_name[1]),
    type = "default",
    full_name = user$full_name[1],
    role = user$role[1]
  ))
}

# ============================================
# إعدادات التطبيق والبيانات
# ============================================

APP_TITLE <- "📊 Dashboard des Achats v2.2"

fichiers_excel <- list.files("data/", pattern = "\\.xlsx$", full.names = TRUE)

# ============================================================
# DATE NORMALIZATION
# Handles Excel serial dates, Date/POSIXct values and character
# dates without accidentally treating Unix seconds as Excel days.
# ============================================================
normalize_date <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "Date")) {
    n <- as.numeric(x)
    out <- rep(as.Date(NA), length(n))
    
    # Normal Date values are stored as days since 1970.
    normal_idx <- is.finite(n) & n >= -20000 & n <= 100000
    out[normal_idx] <- as.Date(n[normal_idx], origin = "1970-01-01")
    
    # If a Date column was accidentally created from Unix seconds,
    # values are around 1.7 billion. Convert them as seconds.
    unix_idx <- is.finite(n) & n > 1000000 & n < 100000000000
    if (any(unix_idx)) {
      out[unix_idx] <- as.Date(as.POSIXct(n[unix_idx], origin = "1970-01-01", tz = "UTC"))
    }
    
    return(out)
  }
  
  if (is.numeric(x)) {
    n <- as.numeric(x)
    out <- rep(as.Date(NA), length(n))
    
    # Excel serial date (e.g. 45000 = 2023-03-15).
    excel_idx <- is.finite(n) & n >= 1 & n <= 100000
    out[excel_idx] <- as.Date(n[excel_idx], origin = "1899-12-30")
    
    # Unix timestamp in seconds (e.g. 1754...).
    unix_idx <- is.finite(n) & n > 1000000 & n < 100000000000
    if (any(unix_idx)) {
      out[unix_idx] <- as.Date(as.POSIXct(n[unix_idx], origin = "1970-01-01", tz = "UTC"))
    }
    
    return(out)
  }
  
  # Character dates: try the most common formats used in the files.
  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "N/A", "NULL", "-")] <- NA_character_
  
  out <- suppressWarnings(lubridate::parse_date_time(
    s,
    orders = c(
      "Y-m-d", "d/m/Y", "d-m-Y", "Y/m/d",
      "d.m.Y", "Y.m.d", "m/d/Y", "m-d-Y",
      "Y-m-d H:M:S", "d/m/Y H:M:S", "d-m-Y H:M:S"
    ),
    quiet = TRUE
  ))
  
  as.Date(out)
}

# Recalculate all date-derived columns after normalizing the date.
repair_dataset_dates <- function(d) {
  if (!"date" %in% names(d)) return(d)
  
  d$date <- normalize_date(d$date)
  
  d$annee <- lubridate::year(d$date)
  d$mois <- as.character(lubridate::month(d$date, label = TRUE, abbr = FALSE))
  d$mois_num <- lubridate::month(d$date)
  d$trimestre <- lubridate::quarter(d$date)
  
  d
}

charger_donnees <- function(path) {
  d <- read_excel(path, sheet = "GLOBAL") %>%
    janitor::clean_names()
  
  d <- d %>%
    mutate(
      date              = normalize_date(date),
      montant_total_ht  = as.numeric(montant_total_ht),
      tva_19_percent    = as.numeric(tva_19_percent),
      total_ttc         = as.numeric(total_ttc),
      quantite          = as.numeric(quantite),
      prix_unitaire     = as.numeric(prix_unitaire)
    ) %>%
    repair_dataset_dates() %>%
    filter(!is.na(date) & !is.na(montant_total_ht) & !is.na(total_ttc) & !is.na(quantite))
  
  d
}

if (file.exists(DATASET_FILE)) {
  dataset <- readRDS(DATASET_FILE)
  dataset <- repair_dataset_dates(dataset)
  saveRDS(dataset, DATASET_FILE)
  message("📂 Chargé depuis dataset_current.rds et dates normalisées")
} else if (length(fichiers_excel) > 0) {
  file_path <- fichiers_excel[1]
  message(paste("📂 Fichier trouvé:", file_path))
  dataset <- charger_donnees(file_path)
  saveRDS(dataset, DATASET_FILE)
} else {
  dataset <- data.frame(
    date = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")[1:100],
    article = sample(c("BETON", "ACIER", "CIMENT"), 100, replace = TRUE),
    quantite = sample(1:50, 100, replace = TRUE),
    unite = "U",
    prix_unitaire = runif(100, 100, 5000),
    montant_total_ht = runif(100, 1000, 50000),
    tva_19_percent = runif(100, 190, 9500),
    total_ttc = runif(100, 1200, 60000),
    fournisseur = sample(c("Fournisseur A", "Fournisseur B", "Fournisseur C"), 100, replace = TRUE),
    projet = sample(c("Projet Alpha", "Projet Beta", "Projet Gamma"), 100, replace = TRUE),
    n_facture = paste0("FACT-", 1001:1100),
    num_cheque = paste0("CH-", 2001:2100)
  ) %>%
    mutate(
      date = normalize_date(date),
      annee = year(date),
      mois = as.character(month(date, label = TRUE, abbr = FALSE)),
      mois_num = month(date),
      trimestre = quarter(date)
    )
  saveRDS(dataset, DATASET_FILE)
}

message(paste("✅", nrow(dataset), "lignes chargées"))

# ============================================
# تنسيقات الرسوم البيانية والتصميم
# ============================================

PALETTE <- c("#22d3ee", "#f59e0b", "#a78bfa", "#34d399", "#f472b6",
             "#60a5fa", "#fb923c", "#4ade80", "#f87171", "#c084fc")

theme_set(
  theme_minimal() +
    theme(
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "#8b9db8"),
      plot.title = element_text(color = "#22d3ee", face = "bold"),
      panel.background = element_rect(fill = "#1a2332", color = NA),
      plot.background = element_rect(fill = "#1a2332", color = NA),
      panel.grid.major = element_line(color = "#333333"),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "#1a2332", color = NA),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "#8b9db8")
    )
)

# ============================================
# شاشة تسجيل الدخول (UI)
# ============================================

login_ui <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        .login-box {
          max-width: 420px;
          margin: 100px auto;
          padding: 40px;
          background: linear-gradient(145deg, #1a2332, #2d3e5a);
          border-radius: 16px;
          box-shadow: 0 20px 60px rgba(0,0,0,0.5);
        }
        .login-box h2 { color: #22d3ee; text-align: center; margin-bottom: 30px; }
        .login-box .form-control {
          background: rgba(255,255,255,0.08);
          border: 1px solid rgba(255,255,255,0.15);
          color: white; padding: 12px 16px; border-radius: 10px;
        }
        .login-box .form-control:focus {
          border-color: #22d3ee; box-shadow: 0 0 0 3px rgba(34,211,238,0.2);
        }
        .login-box .btn {
          width: 100%; padding: 12px; font-size: 16px; font-weight: 600;
          border-radius: 10px; background: linear-gradient(135deg, #0891b2, #22d3ee);
          border: none; color: white;
        }
        .login-box .btn:hover { transform: translateY(-2px); box-shadow: 0 8px 25px rgba(34,211,238,0.3); }
        .login-box label { color: #94a3b8; }
        .login-box .text-muted { color: #64748b; text-align: center; margin-top: 20px; }
        .login-box .request-link { display:block; text-align:center; margin-top: 15px; color:#22d3ee; cursor:pointer; }
      "))
    ),
    div(
      class = "login-box",
      h2("🔐 Dashboard des Achats"),
      textInput("login_user", "👤 اسم المستخدم", placeholder = "أدخل اسم المستخدم"),
      passwordInput("login_pass", "🔑 كلمة المرور", placeholder = "أدخل كلمة المرور"),
      br(),
      actionButton("login_btn", "🚀 دخول", class = "btn-primary", width = "100%"),
      actionLink("goto_request_link", "🆕 ليس لديك حساب؟ اطلب صلاحية دخول", class = "request-link"),
      div(class = "text-muted", p("👑 admin / admin123"))
    )
  )
}

# ============================================
# شاشة طلب صلاحية الدخول (UI)
# ============================================

request_ui <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        .login-box {
          max-width: 460px; margin: 70px auto; padding: 40px;
          background: linear-gradient(145deg, #1a2332, #2d3e5a);
          border-radius: 16px; box-shadow: 0 20px 60px rgba(0,0,0,0.5);
        }
        .login-box h2 { color: #22d3ee; text-align: center; margin-bottom: 20px; }
        .login-box .form-control {
          background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15);
          color: white; padding: 12px 16px; border-radius: 10px;
        }
        .login-box .btn {
          width: 100%; padding: 12px; font-size: 16px; font-weight: 600;
          border-radius: 10px; background: linear-gradient(135deg, #0891b2, #22d3ee);
          border: none; color: white;
        }
        .login-box label { color: #94a3b8; }
        .login-box .request-link { display:block; text-align:center; margin-top: 15px; color:#22d3ee; cursor:pointer; }
      "))
    ),
    div(
      class = "login-box",
      h2("🆕 طلب صلاحية دخول"),
      textInput("req_fullname", "👤 الاسم الكامل"),
      textInput("req_username", "🆔 اسم المستخدم المطلوب"),
      passwordInput("req_password", "🔑 كلمة المرور"),
      passwordInput("req_password2", "🔑 تأكيد كلمة المرور"),
      selectInput("req_role", "🎭 نوع الصلاحية المطلوبة",
                  choices = c("Viewer (مشاهدة فقط)" = "viewer",
                              "Tester (يمكنه إضافة مشتريات)" = "tester")),
      actionButton("submit_request", "📨 إرسال الطلب", class = "btn-primary"),
      actionLink("goto_login_link", "⬅ العودة لتسجيل الدخول", class = "request-link")
    )
  )
}

# ============================================
# واجهة المستخدم الرئيسية (UI)
# ============================================

ui <- dashboardPage(
  dashboardHeader(
    title = APP_TITLE,
    tags$li(class = "dropdown", style = "padding-top: 8px; padding-right: 15px;", uiOutput("user_info")),
    tags$li(
      class = "dropdown", style = "padding-top: 8px; padding-right: 15px;",
      awesomeRadio(
        inputId = "theme_toggle", label = NULL,
        choices = c("☀️ Light Mode", "🌙 Dark Mode"),
        selected = "🌙 Dark Mode", inline = TRUE, status = "info"
      )
    ),
    tags$li(class = "dropdown", style = "padding-top: 8px; padding-right: 10px;",
            actionButton("logout_btn", "🚪 خروج", class = "btn-danger btn-sm"))
  ),
  dashboardSidebar(uiOutput("sidebar_panel")),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("
        /* ==========================================================
           GLOBAL THEME / TEXT VISIBILITY FIX
           ========================================================== */
        body,
        body.light-mode,
        body.dark-mode {
          opacity: 1 !important;
        }

        .light-mode .content-wrapper,
        .light-mode .main-sidebar,
        .light-mode .main-header {
          background-color: #f4f6f9 !important;
        }

        .dark-mode .content-wrapper,
        .dark-mode .main-sidebar,
        .dark-mode .main-header {
          background-color: #1a2332 !important;
        }

        /* Boxes must always have an opaque background and readable text. */
        .light-mode .box {
          background-color: #ffffff !important;
          border-color: #d2d6de !important;
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .dark-mode .box {
          background-color: #243447 !important;
          border-color: #2d3e5a !important;
          color: #ffffff !important;
          opacity: 1 !important;
        }

        .light-mode .box-header,
        .light-mode .box-body,
        .light-mode .box-footer {
          background-color: #ffffff !important;
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .dark-mode .box-header,
        .dark-mode .box-body,
        .dark-mode .box-footer {
          background-color: #243447 !important;
          color: #ffffff !important;
          opacity: 1 !important;
        }

        .light-mode .box-title {
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .dark-mode .box-title {
          color: #ffffff !important;
          opacity: 1 !important;
        }

        .light-mode .box-body p,
        .light-mode .box-body label,
        .light-mode .box-body span,
        .light-mode .box-body h1,
        .light-mode .box-body h2,
        .light-mode .box-body h3,
        .light-mode .box-body h4,
        .light-mode .box-body h5,
        .light-mode .box-body h6 {
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .dark-mode .box-body p,
        .dark-mode .box-body label,
        .dark-mode .box-body span,
        .dark-mode .box-body h1,
        .dark-mode .box-body h2,
        .dark-mode .box-body h3,
        .dark-mode .box-body h4,
        .dark-mode .box-body h5,
        .dark-mode .box-body h6 {
          color: #ffffff !important;
          opacity: 1 !important;
        }

        /* ==========================================================
           DATATABLES - FULL OPAQUE TEXT FIX
           ========================================================== */
        .dataTables_wrapper,
        .dataTables_wrapper * {
          opacity: 1 !important;
        }

        .dark-mode .dataTables_wrapper {
          color: #ffffff !important;
          background: #243447 !important;
        }

        .dark-mode table.dataTable {
          color: #ffffff !important;
          background: #243447 !important;
          border-collapse: collapse !important;
        }

        .dark-mode table.dataTable tbody,
        .dark-mode table.dataTable tbody tr,
        .dark-mode table.dataTable tbody td {
          background-color: #243447 !important;
          color: #ffffff !important;
          opacity: 1 !important;
        }

        .dark-mode table.dataTable tbody tr:hover td {
          background-color: #2d3e5a !important;
          color: #ffffff !important;
        }

        .dark-mode table.dataTable thead,
        .dark-mode table.dataTable thead tr,
        .dark-mode table.dataTable thead th {
          background-color: #1a2332 !important;
          color: #ffffff !important;
          opacity: 1 !important;
        }

        .dark-mode .dataTables_wrapper .dataTables_info,
        .dark-mode .dataTables_wrapper .dataTables_paginate,
        .dark-mode .dataTables_wrapper .dataTables_length,
        .dark-mode .dataTables_wrapper .dataTables_filter,
        .dark-mode .dataTables_wrapper label,
        .dark-mode .dataTables_wrapper .paginate_button {
          color: #ffffff !important;
          opacity: 1 !important;
        }

        .dark-mode .dataTables_wrapper .dataTables_filter input,
        .dark-mode .dataTables_wrapper .dataTables_length select,
        .dark-mode .dataTables_wrapper select {
          background-color: #1a2332 !important;
          color: #ffffff !important;
          border: 1px solid #2d3e5a !important;
          opacity: 1 !important;
        }

        .dark-mode .dataTables_wrapper .dataTables_filter input::placeholder {
          color: #aebdca !important;
          opacity: 1 !important;
        }

        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: #22d3ee !important;
          color: #1a2332 !important;
          border: 1px solid #22d3ee !important;
        }

        .light-mode .dataTables_wrapper {
          color: #1a1a1a !important;
          background: #ffffff !important;
        }

        .light-mode table.dataTable {
          color: #1a1a1a !important;
          background: #ffffff !important;
          border-collapse: collapse !important;
        }

        .light-mode table.dataTable tbody,
        .light-mode table.dataTable tbody tr,
        .light-mode table.dataTable tbody td {
          background-color: #ffffff !important;
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .light-mode table.dataTable tbody tr:hover td {
          background-color: #f0f0f0 !important;
          color: #1a1a1a !important;
        }

        .light-mode table.dataTable thead,
        .light-mode table.dataTable thead tr,
        .light-mode table.dataTable thead th {
          background-color: #f4f6f9 !important;
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .light-mode .dataTables_wrapper .dataTables_info,
        .light-mode .dataTables_wrapper .dataTables_paginate,
        .light-mode .dataTables_wrapper .dataTables_length,
        .light-mode .dataTables_wrapper .dataTables_filter,
        .light-mode .dataTables_wrapper label,
        .light-mode .dataTables_wrapper .paginate_button {
          color: #1a1a1a !important;
          opacity: 1 !important;
        }

        .light-mode .dataTables_wrapper .dataTables_filter input,
        .light-mode .dataTables_wrapper .dataTables_length select,
        .light-mode .dataTables_wrapper select {
          background-color: #ffffff !important;
          color: #1a1a1a !important;
          border: 1px solid #d2d6de !important;
          opacity: 1 !important;
        }

        .light-mode .dataTables_wrapper .dataTables_filter input::placeholder {
          color: #777777 !important;
          opacity: 1 !important;
        }

        .light-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        .light-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: #22d3ee !important;
          color: #1a1a1a !important;
          border: 1px solid #22d3ee !important;
        }

        /* Column filter inputs at the top of the Data tab. */
        .dark-mode table.dataTable thead input,
        .dark-mode table.dataTable thead select {
          background: #1a2332 !important;
          color: #ffffff !important;
          border: 1px solid #2d3e5a !important;
          opacity: 1 !important;
        }

        .light-mode table.dataTable thead input,
        .light-mode table.dataTable thead select {
          background: #ffffff !important;
          color: #1a1a1a !important;
          border: 1px solid #d2d6de !important;
          opacity: 1 !important;
        }

        /* ==========================================================
           SIDEBAR FILTERS
           ========================================================== */
        .sidebar-filter-panel {
          padding: 12px 15px 20px 15px;
          border-top: 1px solid rgba(255,255,255,0.15);
          margin-top: 10px;
        }

        .sidebar-filter-panel h5 {
          color: #8b9db8 !important;
          font-size: 13px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: .5px;
          margin-bottom: 12px;
          opacity: 1 !important;
        }

        .sidebar-filter-panel label {
          color: #b8c2d0 !important;
          font-size: 12px;
          opacity: 1 !important;
        }

        .sidebar-filter-panel .form-control,
        .sidebar-filter-panel .selectize-input {
          background: rgba(255,255,255,0.08) !important;
          color: #ffffff !important;
          border: 1px solid rgba(255,255,255,0.15) !important;
          font-size: 12px;
          opacity: 1 !important;
        }

        .sidebar-filter-panel .selectize-dropdown {
          color: #1a1a1a !important;
          background: #ffffff !important;
          opacity: 1 !important;
        }

        .light-mode .sidebar-filter-panel .form-control,
        .light-mode .sidebar-filter-panel .selectize-input {
          background: #ffffff !important;
          color: #1a1a1a !important;
          border-color: #d2d6de !important;
        }

        .light-mode .sidebar-filter-panel label {
          color: #333333 !important;
        }

        /* ==========================================================
           KPI / VALUE BOXES - LIGHT MODE VISIBILITY FIX
           AdminLTE .small-box rules can inherit white/transparent text.
           Force fully opaque, readable text in Light Mode.
           ========================================================== */
        body.light-mode .small-box,
        .light-mode .small-box {
          opacity: 1 !important;
          text-shadow: none !important;
        }

        body.light-mode .small-box .inner,
        .light-mode .small-box .inner {
          color: #111827 !important;
          opacity: 1 !important;
        }

        body.light-mode .small-box h3,
        body.light-mode .small-box h3 *,
        .light-mode .small-box h3,
        .light-mode .small-box h3 * {
          color: #111827 !important;
          opacity: 1 !important;
          visibility: visible !important;
          text-shadow: none !important;
          font-weight: 700 !important;
        }

        body.light-mode .small-box p,
        body.light-mode .small-box p *,
        .light-mode .small-box p,
        .light-mode .small-box p * {
          color: #374151 !important;
          opacity: 1 !important;
          visibility: visible !important;
          text-shadow: none !important;
          font-weight: 600 !important;
        }

        body.light-mode .small-box .icon,
        body.light-mode .small-box .icon *,
        .light-mode .small-box .icon,
        .light-mode .small-box .icon * {
          color: rgba(17, 24, 39, 0.20) !important;
          opacity: 1 !important;
          visibility: visible !important;
        }

        /* KPI cards in the Data tab and Dashboard tab */
        body.light-mode .small-box h3 small,
        .light-mode .small-box h3 small {
          color: #111827 !important;
          opacity: 1 !important;
        }

        /* Dark mode: keep KPI text fully opaque as well. */
        body.dark-mode .small-box .inner,
        .dark-mode .small-box .inner,
        body.dark-mode .small-box h3,
        .dark-mode .small-box h3,
        body.dark-mode .small-box p,
        .dark-mode .small-box p {
          opacity: 1 !important;
          visibility: visible !important;
          text-shadow: none !important;
        }

        /* Prevent Bootstrap/AdminLTE from fading disabled-looking text. */
        .light-mode .content-wrapper,
        .dark-mode .content-wrapper,
        .light-mode .main-sidebar,
        .dark-mode .main-sidebar,
        .light-mode .box,
        .dark-mode .box,
        .light-mode .box-header,
        .dark-mode .box-header,
        .light-mode .box-body,
        .dark-mode .box-body {
          text-shadow: none !important;
        }
            "))
    ),
    uiOutput("main_content")
  )
)

# ============================================
# السيرفر (Server)
# ============================================

server <- function(input, output, session) {
  
  # --- حالة تسجيل الدخول ---
  logged_in       <- reactiveVal(FALSE)
  user_name       <- reactiveVal("")
  user_role       <- reactiveVal("")
  current_username <- reactiveVal("")
  show_request_form <- reactiveVal(FALSE)
  
  # --- المستخدمون وطلبات الدخول (قابلة للتحديث ومحفوظة على القرص) ---
  rv_users    <- reactiveVal(valid_users_init)
  rv_requests <- reactiveVal(pending_requests_init)
  
  observeEvent(input$login_btn, {
    if (input$login_user == "" || input$login_pass == "") {
      showNotification("❌ يرجى إدخال اسم المستخدم وكلمة المرور", type = "error")
      return()
    }
    result <- check_login(input$login_user, input$login_pass, rv_users())
    showNotification(result$message, type = result$type)
    if (result$success) {
      logged_in(TRUE)
      user_name(result$full_name)
      user_role(result$role)
      current_username(tolower(trimws(input$login_user)))
    }
  })
  
  observeEvent(input$logout_btn, {
    logged_in(FALSE); user_name(""); user_role(""); current_username("")
    showNotification("👋 تم تسجيل الخروج", type = "default")
  })
  
  observeEvent(input$goto_request_link, { show_request_form(TRUE) })
  observeEvent(input$goto_login_link,   { show_request_form(FALSE) })
  
  # --- إرسال طلب صلاحية دخول جديد ---
  observeEvent(input$submit_request, {
    fn <- trimws(input$req_fullname); un <- trimws(input$req_username)
    p1 <- input$req_password; p2 <- input$req_password2
    
    if (fn == "" || un == "" || is.null(p1) || p1 == "") {
      showNotification("❌ يرجى تعبئة جميع الحقول", type = "error"); return()
    }
    if (p1 != p2) {
      showNotification("❌ كلمتا المرور غير متطابقتين", type = "error"); return()
    }
    taken <- tolower(un) %in% tolower(rv_users()$username) ||
      tolower(un) %in% tolower(rv_requests()$username)
    if (taken) {
      showNotification("❌ اسم المستخدم هذا مستخدم بالفعل أو قيد المراجعة", type = "error"); return()
    }
    
    new_req <- data.frame(
      id = paste0("req_", as.integer(Sys.time())),
      username = un, password = p1, full_name = fn,
      requested_role = input$req_role,
      request_date = format(Sys.time(), "%Y-%m-%d %H:%M"),
      stringsAsFactors = FALSE
    )
    reqs <- rbind(rv_requests(), new_req)
    rv_requests(reqs); saveRDS(reqs, REQUESTS_FILE)
    
    showNotification("✅ تم إرسال طلبك، بانتظار موافقة المدير", type = "message")
    updateTextInput(session, "req_fullname", value = "")
    updateTextInput(session, "req_username", value = "")
    updateTextInput(session, "req_password", value = "")
    updateTextInput(session, "req_password2", value = "")
    show_request_form(FALSE)
  })
  
  output$user_info <- renderUI({
    if (logged_in()) {
      tags$span(
        style = "color: white; padding-right: 10px;",
        icon("user"), user_name(),
        tags$span(style = "color: #22d3ee; font-size: 12px; margin-left: 5px;",
                  paste0("(", user_role(), ")"))
      )
    }
  })
  
  # --- الشريط الجانبي (يتغيّر حسب الصلاحية) ---
  output$sidebar_panel <- renderUI({
    if (!logged_in()) return(NULL)
    
    items <- list(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Management", tabName = "management", icon = icon("chart-line"))
    )
    
    if (user_role() %in% c("admin", "tester")) {
      items <- append(items, list(menuItem("Ajouter un achat", tabName = "ajouter", icon = icon("plus-circle"))))
    }
    
    items <- append(items, list(
      menuItem("Fournisseurs", tabName = "fournisseurs", icon = icon("truck")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Rapports", tabName = "rapports", icon = icon("file-export"))
    ))
    
    if (user_role() == "admin") {
      n_pending <- nrow(rv_requests())
      label <- if (n_pending > 0) paste0("Utilisateurs (", n_pending, ")") else "Utilisateurs"
      items <- append(items, list(menuItem(label, tabName = "utilisateurs", icon = icon("users-gear"))))
    }
    
    menu <- do.call(sidebarMenu, items)
    
    # Get data for filter choices
    d <- rv_data()
    
    filter_panel <- div(
      class = "sidebar-filter-panel",
      tags$h5(icon("filter"), " Filtres"),
      selectInput("f_fournisseur", "Fournisseur", 
                  choices = c("Tous" = "all", sort(unique(d$fournisseur)))),
      selectInput("f_mois", "Mois", 
                  choices = c("Tous" = "all", d %>% distinct(mois) %>% pull(mois) %>% sort())),
      selectInput("f_projet", "Projet", 
                  choices = c("Tous" = "all", sort(unique(d$projet)))),
      dateRangeInput("f_dates", "Période",
                     start = min(d$date, na.rm = TRUE),
                     end   = max(d$date, na.rm = TRUE),
                     language = "fr")
    )
    
    tagList(menu, filter_panel)
  })
  
  output$main_content <- renderUI({
    if (!logged_in()) {
      return(if (show_request_form()) request_ui() else login_ui())
    }
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("kpi_ht_dash", width = 3),
          valueBoxOutput("kpi_tva_dash", width = 3),
          valueBoxOutput("kpi_ttc_dash", width = 3),
          valueBoxOutput("kpi_four_dash", width = 3)
        ),
        fluidRow(
          valueBoxOutput("total_achats"),
          valueBoxOutput("nb_transactions"),
          valueBoxOutput("nb_fournisseurs")
        ),
        fluidRow(
          box(title = "Évolution des Achats par Mois", status = "primary", solidHeader = TRUE,
              width = 8, plotlyOutput("plot_evolution")),
          box(title = "Répartition par Projet", status = "primary", solidHeader = TRUE,
              width = 4, plotlyOutput("plot_projet_donut"))
        ),
        fluidRow(
          box(title = "Top 10 Fournisseurs", status = "primary", solidHeader = TRUE,
              width = 6, plotlyOutput("plot_fournisseurs"))
        )
      ),
      tabItem(
        tabName = "management",
        
        fluidRow(
          valueBoxOutput("mgmt_kpi_ttc", width = 3),
          valueBoxOutput("mgmt_kpi_ht", width = 3),
          valueBoxOutput("mgmt_kpi_avg", width = 3),
          valueBoxOutput("mgmt_kpi_transactions", width = 3)
        ),
        
        fluidRow(
          valueBoxOutput("mgmt_kpi_suppliers", width = 3),
          valueBoxOutput("mgmt_kpi_projects", width = 3),
          valueBoxOutput("mgmt_kpi_quantity", width = 3),
          valueBoxOutput("mgmt_kpi_growth", width = 3)
        ),
        
        fluidRow(
          box(title = "📅 Comparaison mensuelle par année",
              status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("mgmt_month_year", height = "380px")),
          box(title = "📊 Répartition TTC par projet",
              status = "primary", solidHeader = TRUE, width = 4,
              plotlyOutput("mgmt_project_share", height = "380px"))
        ),
        
        fluidRow(
          box(title = "🏢 Comparaison des projets",
              status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("mgmt_project_compare", height = "400px")),
          box(title = "🏆 Top 10 fournisseurs",
              status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("mgmt_supplier_top", height = "400px"))
        ),
        
        fluidRow(
          box(title = "📈 Évolution trimestrielle",
              status = "success", solidHeader = TRUE, width = 6,
              plotlyOutput("mgmt_quarterly", height = "380px")),
          box(title = "💰 Dépense moyenne par mois",
              status = "warning", solidHeader = TRUE, width = 6,
              plotlyOutput("mgmt_avg_month", height = "380px"))
        ),
        
        fluidRow(
          box(title = "📋 Résumé des projets",
              status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("mgmt_project_table"))
        )
      ),
      
      tabItem(
        tabName = "ajouter",
        fluidRow(
          box(
            title = "Nouvel achat", status = "primary", solidHeader = TRUE, width = 6,
            dateInput("new_date", "Date", value = Sys.Date(), language = "fr"),
            textInput("new_article", "Article", placeholder = "ex. BETON CLASSE C 25/30"),
            fluidRow(
              column(6, numericInput("new_quantite", "Quantité", value = 1, min = 0)),
              column(6, textInput("new_unite", "Unité", value = "U"))
            ),
            numericInput("new_prix_unitaire", "Prix unitaire (HT)", value = 0, min = 0),
            textInput("new_fournisseur", "Fournisseur"),
            selectizeInput("new_projet", "Projet", choices = NULL,
                           options = list(create = TRUE, placeholder = "Choisir ou créer un projet")),
            actionButton("btn_ajouter", "💾 Enregistrer l'achat", class = "btn-info"),
            br(), br(),
            div(
              style = "display: flex; gap: 10px; flex-wrap: wrap;",
              downloadButton("export_ajouter_excel", "📥 Exporter Excel", class = "btn-success"),
              downloadButton("export_ajouter_pdf", "📄 Exporter PDF", class = "btn-warning")
            )
          ),
          box(
            title = "Aperçu du calcul (TVA 19%)", status = "info", solidHeader = TRUE, width = 6,
            htmlOutput("apercu_calcul")
          )
        )
      ),
      tabItem(
        tabName = "fournisseurs",
        fluidRow(
          box(title = "Totaux par fournisseur", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("table_fournisseurs"))
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          valueBoxOutput("kpi_ht_data", width = 3),
          valueBoxOutput("kpi_tva_data", width = 3),
          valueBoxOutput("kpi_ttc_data", width = 3),
          valueBoxOutput("kpi_four_data", width = 3)
        ),
        box(title = "Détail des achats", status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "margin-bottom: 15px;",
              downloadButton("export_data_excel", "📥 Exporter Excel", class = "btn-success")
            ),
            DTOutput("table_data"))
      ),
      tabItem(
        tabName = "rapports",
        fluidRow(
          box(
            title = "Export Excel (.xlsx)", status = "success", solidHeader = TRUE, width = 6,
            p("Contient : résumé des KPI, détail filtré, totaux par projet et par fournisseur."),
            downloadButton("export_excel_2", "📥 Télécharger le fichier Excel", class = "btn-success")
          ),
          box(
            title = "Rapport PDF complet", status = "warning", solidHeader = TRUE, width = 6,
            p("Contient : résumé des KPI, graphiques (évolution, répartition, top fournisseurs) et le détail complet des achats."),
            downloadButton("export_pdf_2", "📄 Télécharger le rapport PDF", class = "btn-warning")
          )
        )
      ),
      tabItem(
        tabName = "utilisateurs",
        fluidRow(
          box(
            title = "➕ Ajouter un utilisateur directement", status = "primary", solidHeader = TRUE, width = 6,
            textInput("admin_new_fullname", "Nom complet"),
            textInput("admin_new_username", "Nom d'utilisateur"),
            passwordInput("admin_new_password", "Mot de passe"),
            selectInput("admin_new_role", "Rôle",
                        choices = c("Viewer" = "viewer", "Tester" = "tester", "Admin" = "admin")),
            actionButton("admin_add_user", "➕ Ajouter", class = "btn-info")
          ),
          box(
            title = "📨 Demandes d'accès en attente", status = "warning", solidHeader = TRUE, width = 6,
            DTOutput("table_requests"),
            br(),
            selectInput("select_request_user", "Sélectionner une demande", choices = character(0)),
            selectInput("approve_role", "Rôle à attribuer",
                        choices = c("Viewer" = "viewer", "Tester" = "tester", "Admin" = "admin")),
            actionButton("approve_request", "✅ Accepter", class = "btn-success"),
            actionButton("reject_request", "❌ Refuser", class = "btn-danger")
          )
        ),
        fluidRow(
          box(
            title = "👥 Utilisateurs actifs", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("table_users"),
            br(),
            selectInput("select_user_remove", "Sélectionner un utilisateur à supprimer", choices = character(0)),
            actionButton("remove_user_btn", "🗑️ Supprimer", class = "btn-danger")
          )
        )
      )
    )
  })
  
  # --- إدارة المستخدمين (Admin فقط، مع تحقق إضافي من الصلاحية) ---
  
  output$table_requests <- renderDT({
    datatable(rv_requests()[, c("full_name", "username", "requested_role", "request_date")],
              rownames = FALSE, options = list(dom = "t", paging = FALSE, scrollX = TRUE))
  })
  
  output$table_users <- renderDT({
    datatable(rv_users()[, c("username", "full_name", "role")],
              rownames = FALSE, options = list(dom = "t", paging = FALSE, scrollX = TRUE))
  })
  
  observe({
    req(logged_in())
    if (user_role() == "admin") {
      updateSelectInput(session, "select_request_user",
                        choices = if (nrow(rv_requests()) > 0) rv_requests()$username else character(0))
      updateSelectInput(session, "select_user_remove", choices = rv_users()$username)
    }
  })
  
  observeEvent(input$admin_add_user, {
    req(user_role() == "admin")
    un <- trimws(input$admin_new_username)
    if (un == "" || is.null(input$admin_new_password) || input$admin_new_password == "") {
      showNotification("❌ يرجى تعبئة اسم المستخدم وكلمة المرور", type = "error"); return()
    }
    users <- rv_users()
    if (tolower(un) %in% tolower(users$username)) {
      showNotification("❌ اسم المستخدم موجود بالفعل", type = "error"); return()
    }
    new_user <- data.frame(username = un, password = input$admin_new_password,
                           full_name = input$admin_new_fullname, role = input$admin_new_role,
                           stringsAsFactors = FALSE)
    users <- rbind(users, new_user)
    rv_users(users); saveRDS(users, USERS_FILE)
    showNotification("✅ تمت إضافة المستخدم بنجاح", type = "message")
    updateTextInput(session, "admin_new_fullname", value = "")
    updateTextInput(session, "admin_new_username", value = "")
    updateTextInput(session, "admin_new_password", value = "")
    updateSelectInput(session, "select_user_remove", choices = rv_users()$username)
  })
  
  observeEvent(input$approve_request, {
    req(user_role() == "admin")
    req(input$select_request_user != "")
    
    reqs <- rv_requests()
    idx <- which(reqs$username == input$select_request_user)
    if (length(idx) == 0) {
      showNotification("❌ الطلب غير موجود", type = "error")
      return()
    }
    new_user <- data.frame(username = reqs$username[idx], password = reqs$password[idx],
                           full_name = reqs$full_name[idx], role = input$approve_role,
                           stringsAsFactors = FALSE)
    users <- rbind(rv_users(), new_user)
    rv_users(users); saveRDS(users, USERS_FILE)
    reqs <- reqs[-idx, , drop = FALSE]
    rv_requests(reqs); saveRDS(reqs, REQUESTS_FILE)
    showNotification(paste0("✅ تم قبول ", new_user$full_name, " كـ ", input$approve_role), type = "message")
    updateSelectInput(session, "select_request_user", choices = if (nrow(reqs) > 0) reqs$username else character(0))
    updateSelectInput(session, "select_user_remove", choices = rv_users()$username)
  })
  
  observeEvent(input$reject_request, {
    req(user_role() == "admin")
    req(input$select_request_user != "")
    
    reqs <- rv_requests()
    idx <- which(reqs$username == input$select_request_user)
    if (length(idx) == 0) {
      showNotification("❌ الطلب غير موجود", type = "error")
      return()
    }
    nm <- reqs$full_name[idx]
    reqs <- reqs[-idx, , drop = FALSE]
    rv_requests(reqs); saveRDS(reqs, REQUESTS_FILE)
    showNotification(paste0("❌ تم رفض طلب ", nm), type = "warning")
    updateSelectInput(session, "select_request_user", choices = if (nrow(reqs) > 0) reqs$username else character(0))
  })
  
  observeEvent(input$remove_user_btn, {
    req(user_role() == "admin")
    req(input$select_user_remove != "")
    
    users <- rv_users()
    target_username <- input$select_user_remove
    target <- users[users$username == target_username, ]
    
    if (nrow(target) == 0) {
      showNotification("❌ المستخدم غير موجود", type = "error")
      return()
    }
    
    # Prevent deleting own account
    if (tolower(target$username[1]) == current_username()) {
      showNotification("❌ لا يمكنك حذف حسابك الحالي", type = "error")
      return()
    }
    
    # Prevent deleting last admin
    if (target$role[1] == "admin" && sum(users$role == "admin") <= 1) {
      showNotification("❌ يجب أن يبقى مدير واحد على الأقل", type = "error")
      return()
    }
    
    # Remove the user
    users <- users[users$username != target_username, ]
    rv_users(users)
    saveRDS(users, USERS_FILE)
    
    # Update select input choices
    updateSelectInput(session, "select_user_remove", choices = users$username)
    
    showNotification(paste0("🗑️ تم حذف المستخدم ", target$full_name[1]), type = "message")
  })
  
  # --- معالجة البيانات ---
  
  rv_data <- reactiveVal(dataset)
  
  # Update filter choices when data changes
  observe({
    d <- rv_data()
    if (nrow(d) > 0) {
      updateSelectInput(session, "f_fournisseur", 
                        choices = c("Tous" = "all", sort(unique(d$fournisseur))))
      updateSelectInput(session, "f_mois", 
                        choices = c("Tous" = "all", d %>% distinct(mois) %>% pull(mois) %>% sort()))
      updateSelectInput(session, "f_projet", 
                        choices = c("Tous" = "all", sort(unique(d$projet))))
      updateDateRangeInput(session, "f_dates",
                           start = min(d$date, na.rm = TRUE), 
                           end = max(d$date, na.rm = TRUE))
    }
  })
  
  observeEvent(logged_in(), {
    req(logged_in())
    d <- rv_data()
    updateSelectizeInput(session, "new_projet", choices = sort(unique(d$projet)), server = TRUE)
  })
  
  filtered_data <- reactive({
    req(logged_in())
    d <- rv_data()
    
    # Apply filters
    if (!is.null(input$f_projet) && input$f_projet != "all") {
      d <- d %>% filter(projet == input$f_projet)
    }
    if (!is.null(input$f_fournisseur) && input$f_fournisseur != "all") {
      d <- d %>% filter(fournisseur == input$f_fournisseur)
    }
    if (!is.null(input$f_mois) && input$f_mois != "all") {
      d <- d %>% filter(mois == input$f_mois)
    }
    if (!is.null(input$f_dates) && length(input$f_dates) == 2 && !any(is.na(input$f_dates))) {
      d <- d %>% filter(date >= input$f_dates[1] & date <= input$f_dates[2])
    }
    
    d
  })
  
  kpis <- reactive({
    d <- filtered_data()
    list(
      ht           = sum(d$montant_total_ht, na.rm = TRUE),
      tva          = sum(d$tva_19_percent, na.rm = TRUE),
      ttc          = sum(d$total_ttc, na.rm = TRUE),
      fournisseurs = length(unique(d$fournisseur))
    )
  })
  
  # --- تبديل الوضع الداكن/الفاتح ---
  observeEvent(input$theme_toggle, {
    if (input$theme_toggle == "🌙 Dark Mode") {
      shinyjs::runjs("document.body.classList.remove('light-mode'); document.body.classList.add('dark-mode');")
    } else {
      shinyjs::runjs("document.body.classList.remove('dark-mode'); document.body.classList.add('light-mode');")
    }
  })
  
  # =========================================================
  # VERSION 2.2 - MANAGEMENT DASHBOARD
  # All metrics follow the active sidebar filters.
  # =========================================================
  
  management_data <- reactive({
    d <- filtered_data()
    if (nrow(d) == 0) return(d)
    
    d %>%
      mutate(
        annee = as.integer(annee),
        mois_num = as.integer(mois_num),
        trimestre = as.integer(trimestre),
        mois_label = as.character(mois)
      )
  })
  
  management_kpis <- reactive({
    d <- management_data()
    
    if (nrow(d) == 0) {
      return(list(
        ttc = 0, ht = 0, avg = 0, transactions = 0,
        suppliers = 0, projects = 0, quantity = 0, growth = NA_real_
      ))
    }
    
    yearly <- d %>%
      group_by(annee) %>%
      summarise(ttc = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee)
    
    growth <- NA_real_
    if (nrow(yearly) >= 2) {
      prev <- yearly$ttc[nrow(yearly) - 1]
      curr <- yearly$ttc[nrow(yearly)]
      if (is.finite(prev) && prev != 0) {
        growth <- (curr - prev) / abs(prev) * 100
      }
    }
    
    list(
      ttc = sum(d$total_ttc, na.rm = TRUE),
      ht = sum(d$montant_total_ht, na.rm = TRUE),
      avg = mean(d$total_ttc, na.rm = TRUE),
      transactions = nrow(d),
      suppliers = n_distinct(d$fournisseur[!is.na(d$fournisseur) & d$fournisseur != ""]),
      projects = n_distinct(d$projet[!is.na(d$projet) & d$projet != ""]),
      quantity = sum(d$quantite, na.rm = TRUE),
      growth = growth
    )
  })
  
  output$mgmt_kpi_ttc <- renderValueBox({
    k <- management_kpis()
    valueBox(scales::comma(k$ttc, accuracy = 0.01), "Total TTC",
             icon = icon("money-bill-wave"), color = "aqua")
  })
  
  output$mgmt_kpi_ht <- renderValueBox({
    k <- management_kpis()
    valueBox(scales::comma(k$ht, accuracy = 0.01), "Total HT",
             icon = icon("file-invoice-dollar"), color = "light-blue")
  })
  
  output$mgmt_kpi_avg <- renderValueBox({
    k <- management_kpis()
    valueBox(scales::comma(k$avg, accuracy = 0.01), "Achat moyen TTC",
             icon = icon("calculator"), color = "purple")
  })
  
  output$mgmt_kpi_transactions <- renderValueBox({
    k <- management_kpis()
    valueBox(scales::comma(k$transactions, accuracy = 1), "Transactions",
             icon = icon("receipt"), color = "orange")
  })
  
  output$mgmt_kpi_suppliers <- renderValueBox({
    k <- management_kpis()
    valueBox(k$suppliers, "Fournisseurs", icon = icon("truck"), color = "green")
  })
  
  output$mgmt_kpi_projects <- renderValueBox({
    k <- management_kpis()
    valueBox(k$projects, "Projets", icon = icon("building"), color = "teal")
  })
  
  output$mgmt_kpi_quantity <- renderValueBox({
    k <- management_kpis()
    valueBox(scales::comma(k$quantity, accuracy = 0.01), "Quantité totale",
             icon = icon("boxes-stacked"), color = "yellow")
  })
  
  output$mgmt_kpi_growth <- renderValueBox({
    k <- management_kpis()
    growth_text <- if (is.na(k$growth)) "N/A" else
      paste0(ifelse(k$growth >= 0, "+", ""),
             scales::number(k$growth, accuracy = 0.1), "%")
    
    valueBox(
      growth_text, "Évolution annuelle TTC",
      icon = icon(ifelse(is.na(k$growth) || k$growth >= 0,
                         "arrow-trend-up", "arrow-trend-down")),
      color = ifelse(is.na(k$growth), "grey",
                     ifelse(k$growth >= 0, "green", "red"))
    )
  })
  
  output$mgmt_month_year <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(annee), !is.na(mois_num)) %>%
      group_by(annee, mois_num) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee, mois_num)
    
    validate(need(nrow(x) > 0, "Aucune donnée mensuelle disponible."))
    
    p <- plot_ly(
      x, x = ~mois_num, y = ~TTC, color = ~factor(annee),
      colors = PALETTE, type = "scatter", mode = "lines+markers",
      hovertemplate = "Année: %{fullData.name}<br>Mois: %{x}<br>TTC: %{y:,.2f} DA<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title = "Mois", tickmode = "array", tickvals = 1:12,
          ticktext = c("Jan","Fév","Mar","Avr","Mai","Juin",
                       "Juil","Août","Sep","Oct","Nov","Déc")
        ),
        yaxis = list(title = "Total TTC (DA)")
      )
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_project_share <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TTC))
    
    validate(need(nrow(x) > 0, "Aucun projet disponible."))
    
    p <- plot_ly(x, labels = ~projet, values = ~TTC, type = "pie",
                 hole = 0.55, marker = list(colors = PALETTE),
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>%{value:,.2f} DA<extra></extra>")
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_project_compare <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(
        HT = sum(montant_total_ht, na.rm = TRUE),
        TVA = sum(tva_19_percent, na.rm = TRUE),
        TTC = sum(total_ttc, na.rm = TRUE),
        Transactions = n(), .groups = "drop"
      ) %>%
      arrange(desc(TTC))
    
    validate(need(nrow(x) > 0, "Aucun projet disponible."))
    
    p <- plot_ly(x, x = ~projet) %>%
      add_bars(y = ~HT, name = "HT") %>%
      add_bars(y = ~TVA, name = "TVA") %>%
      add_bars(y = ~TTC, name = "TTC") %>%
      layout(barmode = "group", xaxis = list(title = ""),
             yaxis = list(title = "Montant (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_supplier_top <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(fournisseur), fournisseur != "") %>%
      group_by(fournisseur) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE),
                Transactions = n(), .groups = "drop") %>%
      arrange(desc(TTC)) %>%
      slice_head(n = 10) %>%
      arrange(TTC)
    
    validate(need(nrow(x) > 0, "Aucun fournisseur disponible."))
    
    p <- plot_ly(
      x, x = ~TTC, y = ~reorder(fournisseur, TTC),
      type = "bar", orientation = "h",
      text = ~paste0(scales::comma(TTC, accuracy = 0.01),
                     " DA<br>", Transactions, " transaction(s)"),
      hoverinfo = "text"
    ) %>%
      layout(xaxis = list(title = "Total TTC (DA)"), yaxis = list(title = ""))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_quarterly <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(annee), !is.na(trimestre)) %>%
      group_by(annee, trimestre) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee, trimestre) %>%
      mutate(Periode = paste0("T", trimestre, " ", annee))
    
    validate(need(nrow(x) > 0, "Aucune donnée trimestrielle disponible."))
    
    p <- plot_ly(
      x, x = ~factor(Periode, levels = Periode), y = ~TTC,
      type = "bar", text = ~scales::comma(TTC, accuracy = 0.01),
      textposition = "auto",
      hovertemplate = "%{x}<br>TTC: %{y:,.2f} DA<extra></extra>"
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Total TTC (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_avg_month <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(annee), !is.na(mois_num)) %>%
      group_by(annee, mois_num) %>%
      summarise(
        TTC_moyen = mean(total_ttc, na.rm = TRUE),
        Transactions = n(), .groups = "drop"
      ) %>%
      arrange(annee, mois_num) %>%
      mutate(Periode = paste0(
        c("Jan","Fév","Mar","Avr","Mai","Juin","Juil","Août",
          "Sep","Oct","Nov","Déc")[mois_num], " ", annee
      ))
    
    validate(need(nrow(x) > 0, "Aucune donnée mensuelle disponible."))
    
    p <- plot_ly(
      x, x = ~factor(Periode, levels = Periode), y = ~TTC_moyen,
      type = "bar",
      text = ~paste0(scales::comma(TTC_moyen, accuracy = 0.01),
                     " DA<br>", Transactions, " transaction(s)"),
      hoverinfo = "text"
    ) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Achat moyen TTC (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_project_table <- renderDT({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés."))
    
    x <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(
        `Total HT` = sum(montant_total_ht, na.rm = TRUE),
        `Total TVA` = sum(tva_19_percent, na.rm = TRUE),
        `Total TTC` = sum(total_ttc, na.rm = TRUE),
        `Transactions` = n(),
        `Fournisseurs` = n_distinct(fournisseur[!is.na(fournisseur) & fournisseur != ""]),
        `Quantité` = sum(quantite, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total TTC`))
    
    datatable(x, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
              class = "stripe hover nowrap") %>%
      formatCurrency(c("Total HT","Total TVA","Total TTC"),
                     currency = "", interval = 3, mark = ",", digits = 2) %>%
      formatRound("Quantité", 2)
  })
  
  # --- أعمدة المؤشرات (KPIs) ---
  make_kpi_ht   <- function() valueBox(scales::comma(kpis()$ht),  "Montant Total HT",  icon = icon("file-invoice-dollar"), color = "light-blue")
  make_kpi_tva  <- function() valueBox(scales::comma(kpis()$tva), "Montant Total TVA", icon = icon("percent"), color = "yellow")
  make_kpi_ttc  <- function() valueBox(scales::comma(kpis()$ttc), "Montant Total TTC", icon = icon("euro-sign"), color = "aqua")
  make_kpi_four <- function() valueBox(kpis()$fournisseurs, "Fournisseurs", icon = icon("truck"), color = "green")
  
  output$kpi_ht_dash   <- renderValueBox({ make_kpi_ht() })
  output$kpi_tva_dash  <- renderValueBox({ make_kpi_tva() })
  output$kpi_ttc_dash  <- renderValueBox({ make_kpi_ttc() })
  output$kpi_four_dash <- renderValueBox({ make_kpi_four() })
  
  output$kpi_ht_data   <- renderValueBox({ make_kpi_ht() })
  output$kpi_tva_data  <- renderValueBox({ make_kpi_tva() })
  output$kpi_ttc_data  <- renderValueBox({ make_kpi_ttc() })
  output$kpi_four_data <- renderValueBox({ make_kpi_four() })
  
  # --- الرسوم البيانية ---
  apply_plotly_theme <- function(p, mode) {
    if (mode == "🌙 Dark Mode") {
      p %>% layout(font = list(color = "white"), paper_bgcolor = "#1a2332", plot_bgcolor = "#1a2332",
                   xaxis = list(gridcolor = "#333333", color = "white"),
                   yaxis = list(gridcolor = "#333333", color = "white"))
    } else {
      p %>% layout(font = list(color = "#333333"), paper_bgcolor = "#ffffff", plot_bgcolor = "#f9fafb",
                   xaxis = list(gridcolor = "#e5e7eb", color = "#333333"),
                   yaxis = list(gridcolor = "#e5e7eb", color = "#333333"))
    }
  }
  
  output$total_achats <- renderValueBox({
    valueBox(scales::comma(sum(filtered_data()$total_ttc, na.rm = TRUE)), "Total Achats (TTC)",
             icon = icon("euro-sign"), color = "aqua")
  })
  output$nb_transactions <- renderValueBox({
    valueBox(nrow(filtered_data()), "Transactions", icon = icon("receipt"), color = "purple")
  })
  output$nb_fournisseurs <- renderValueBox({
    valueBox(length(unique(filtered_data()$fournisseur)), "Fournisseurs", icon = icon("truck"), color = "green")
  })
  
  output$plot_evolution <- renderPlotly({
    d <- filtered_data() %>%
      group_by(annee, mois_num, mois) %>%
      summarise(montant_total_ht = sum(montant_total_ht, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee, mois_num) %>%
      mutate(label = paste(mois, annee))
    
    p <- plot_ly(d, x = ~factor(label, levels = label), y = ~montant_total_ht, type = 'bar',
                 marker = list(color = '#22d3ee')) %>%
      layout(title = "Évolution des Achats par Mois", xaxis = list(title = ""))
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$plot_projet_donut <- renderPlotly({
    d <- filtered_data() %>% group_by(projet) %>% summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop")
    p <- plot_ly(d, labels = ~projet, values = ~total, type = "pie", hole = 0.55,
                 marker = list(colors = PALETTE), textinfo = "label+percent")
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$plot_fournisseurs <- renderPlotly({
    top_fourn <- filtered_data() %>% group_by(fournisseur) %>%
      summarise(total = sum(total_ttc, na.rm = TRUE)) %>% top_n(10, total)
    p <- plot_ly(top_fourn, x = ~total, y = ~reorder(fournisseur, total), type = 'bar', orientation = 'h')
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  # --- الجداول ---
  output$table_data <- renderDT({
    d <- filtered_data()
    
    # Send a plain text date to DataTables so JavaScript cannot reinterpret
    # the R Date as a huge numeric timestamp.
    if ("date" %in% names(d)) {
      d$date <- ifelse(
        is.na(d$date),
        "",
        format(as.Date(d$date), "%d/%m/%Y")
      )
    }
    
    datatable(
      d,
      rownames = FALSE,
      filter = "top",
      extensions = c("Scroller"),
      options = list(
        dom = "lrtip",
        scrollX = TRUE,
        scrollY = 500,
        scrollCollapse = TRUE,
        scroller = TRUE,
        deferRender = TRUE,
        autoWidth = TRUE,
        pageLength = 25
      ),
      class = "stripe hover nowrap",
      escape = TRUE
    )
  })
  
  stats_projets <- reactive({
    rv_data() %>% group_by(projet) %>% summarise(Total_TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_TTC))
  })
  
  stats_fournisseurs <- reactive({
    rv_data() %>% group_by(fournisseur) %>% summarise(Total_TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_TTC))
  })
  
  output$table_fournisseurs <- renderDT({
    datatable(stats_fournisseurs(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatCurrency("Total_TTC", currency = "", interval = 3, mark = ",", digits = 2)
  })
  
  # --- إضافة مشتريات (يتطلب صلاحية admin أو tester) ---
  output$apercu_calcul <- renderUI({
    q  <- ifelse(is.na(input$new_quantite), 0, input$new_quantite)
    pu <- ifelse(is.na(input$new_prix_unitaire), 0, input$new_prix_unitaire)
    ht  <- q * pu
    tva <- ht * 0.19
    ttc <- ht + tva
    tagList(
      p(strong("Montant HT: "), scales::comma(ht)),
      p(strong("TVA (19%): "), scales::comma(tva)),
      p(strong("Montant TTC: "), scales::comma(ttc))
    )
  })
  
  observeEvent(input$btn_ajouter, {
    req(logged_in())
    if (!(user_role() %in% c("admin", "tester"))) {
      showNotification("❌ ليس لديك صلاحية لإضافة مشتريات", type = "error"); return()
    }
    if (is.null(input$new_article) || trimws(input$new_article) == "" ||
        is.null(input$new_fournisseur) || trimws(input$new_fournisseur) == "" ||
        is.null(input$new_projet) || trimws(input$new_projet) == "") {
      showNotification("❌ يرجى تعبئة الحقول المطلوبة (المقال، المورد، المشروع)", type = "error"); return()
    }
    
    q  <- ifelse(is.na(input$new_quantite), 0, input$new_quantite)
    pu <- ifelse(is.na(input$new_prix_unitaire), 0, input$new_prix_unitaire)
    ht  <- q * pu
    tva <- ht * 0.19
    ttc <- ht + tva
    d   <- as.Date(input$new_date)
    
    cur <- rv_data()
    new_row <- data.frame(
      date = d, article = input$new_article, quantite = q, unite = input$new_unite,
      prix_unitaire = pu, montant_total_ht = ht, tva_19_percent = tva, total_ttc = ttc,
      fournisseur = input$new_fournisseur, projet = input$new_projet,
      n_facture = NA_character_, num_cheque = NA_character_,
      annee = year(d), mois = as.character(month(d, label = TRUE, abbr = FALSE)),
      mois_num = month(d), trimestre = quarter(d),
      stringsAsFactors = FALSE
    )
    for (mc in setdiff(names(cur), names(new_row))) new_row[[mc]] <- NA
    new_row <- new_row[, names(cur)]
    
    updated <- bind_rows(cur, new_row)
    rv_data(updated)
    saveRDS(updated, DATASET_FILE)
    
    showNotification("✅ تم إضافة عملية الشراء بنجاح", type = "message")
    updateTextInput(session, "new_article", value = "")
    updateNumericInput(session, "new_quantite", value = 1)
    updateNumericInput(session, "new_prix_unitaire", value = 0)
    updateTextInput(session, "new_fournisseur", value = "")
  })
  
  # ============================================
  # FUNCTION: Export Excel with multiple sheets
  # ============================================
  export_excel_with_sheets <- function(data, filename) {
    wb <- createWorkbook()
    
    # Define column names for display (same as original Excel)
    col_names_display <- c(
      "date" = "Date",
      "article" = "Article",
      "quantite" = "Quantité",
      "unite" = "Unité",
      "prix_unitaire" = "Prix Unitaire",
      "montant_total_ht" = "Montant Total HT",
      "tva_19_percent" = "TVA 19%",
      "total_ttc" = "Total TTC",
      "fournisseur" = "Fournisseur",
      "projet" = "Projet",
      "n_facture" = "N° Facture",
      "num_cheque" = "N° Chèque"
    )
    
    # Select only columns that exist
    existing_cols <- intersect(names(data), names(col_names_display))
    display_cols <- col_names_display[existing_cols]
    
    # Format data for Excel
    format_for_excel <- function(df) {
      df_out <- df[, existing_cols, drop = FALSE]
      # Format date as Date (not numeric)
      if ("date" %in% names(df_out)) {
        df_out$date <- as.Date(df_out$date)
      }
      # Rename columns to French display names
      names(df_out) <- display_cols
      return(df_out)
    }
    
    # 1. GLOBAL sheet - all data
    addWorksheet(wb, "GLOBAL")
    global_data <- format_for_excel(data)
    writeData(wb, "GLOBAL", global_data)
    
    # 2. Sheet for each project
    projects <- unique(data$projet)
    for (proj in projects) {
      # Clean sheet name (Excel max 31 characters)
      sheet_name <- substr(proj, 1, 31)
      # Remove invalid characters for sheet names
      sheet_name <- gsub("[\\[\\]\\*\\?/]", "", sheet_name)
      sheet_name <- gsub(":", "", sheet_name)
      
      # Check if sheet name already exists
      if (sheet_name %in% names(wb)) {
        sheet_name <- paste0(sheet_name, "_", format(Sys.time(), "%H%M%S"))
        sheet_name <- substr(sheet_name, 1, 31)
      }
      
      addWorksheet(wb, sheet_name)
      proj_data <- data %>% filter(projet == proj)
      writeData(wb, sheet_name, format_for_excel(proj_data))
    }
    
    # 3. Summary sheet - KPI
    addWorksheet(wb, "Résumé")
    kpi_summary <- data.frame(
      Indicateur = c("Montant Total HT", "Montant Total TVA (19%)", "Montant Total TTC",
                     "Nombre de Fournisseurs", "Nombre de Transactions", "Nombre de Projets"),
      Valeur = c(
        sum(data$montant_total_ht, na.rm = TRUE),
        sum(data$tva_19_percent, na.rm = TRUE),
        sum(data$total_ttc, na.rm = TRUE),
        length(unique(data$fournisseur)),
        nrow(data),
        length(unique(data$projet))
      )
    )
    writeData(wb, "Résumé", kpi_summary)
    
    # 4. Summary by Fournisseur
    addWorksheet(wb, "Par Fournisseur")
    fourn_summary <- data %>%
      group_by(fournisseur) %>%
      summarise(
        `Total HT` = sum(montant_total_ht, na.rm = TRUE),
        `Total TVA` = sum(tva_19_percent, na.rm = TRUE),
        `Total TTC` = sum(total_ttc, na.rm = TRUE),
        `Nbre Transactions` = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total TTC`))
    writeData(wb, "Par Fournisseur", fourn_summary)
    
    # Save the workbook
    saveWorkbook(wb, filename, overwrite = TRUE)
  }
  
  # --- Export Excel for Data tab ---
  output$export_data_excel <- downloadHandler(
    filename = function() paste0("Export_Data_Achats_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      d <- filtered_data()
      export_excel_with_sheets(d, file)
    }
  )
  
  # --- Export Excel for Ajouter tab ---
  output$export_ajouter_excel <- downloadHandler(
    filename = function() paste0("Export_Ajouter_Achats_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      d <- rv_data()
      export_excel_with_sheets(d, file)
    }
  )
  
  # --- Export PDF for Ajouter tab ---
  output$export_ajouter_pdf <- downloadHandler(
    filename = function() paste0("Export_Ajouter_Achats_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
    content = function(file) {
      d <- rv_data()
      k <- list(
        ht = sum(d$montant_total_ht, na.rm = TRUE),
        tva = sum(d$tva_19_percent, na.rm = TRUE),
        ttc = sum(d$total_ttc, na.rm = TRUE),
        fournisseurs = length(unique(d$fournisseur))
      )
      
      pdf(file, width = 11, height = 8.5)
      
      # --- Page 1: Summary ---
      grid.text("Rapport des Achats - Toutes les données", y = unit(0.92, "npc"),
                gp = gpar(fontsize = 26, fontface = "bold", col = "#0f172a"))
      grid.text(paste("Généré le:", format(Sys.Date(), "%d/%m/%Y")), y = unit(0.87, "npc"),
                gp = gpar(fontsize = 11, col = "#555555"))
      
      summary_txt <- paste(
        paste("Montant Total HT:", scales::comma(k$ht)),
        paste("Montant Total TVA (19%):", scales::comma(k$tva)),
        paste("Montant Total TTC:", scales::comma(k$ttc)),
        paste("Nombre de Fournisseurs:", k$fournisseurs),
        paste("Nombre de Transactions:", nrow(d)),
        sep = "\n\n"
      )
      grid.text(summary_txt, y = unit(0.6, "npc"), gp = gpar(fontsize = 15, col = "#0f172a"), just = "center")
      
      # --- Page 2: Evolution ---
      evo <- d %>%
        group_by(annee, mois_num, mois) %>%
        summarise(montant_total_ht = sum(montant_total_ht, na.rm = TRUE), .groups = "drop") %>%
        arrange(annee, mois_num) %>%
        mutate(label = paste(mois, annee))
      
      if (nrow(evo) > 0) {
        p1 <- ggplot(evo, aes(x = factor(label, levels = label), y = montant_total_ht)) +
          geom_col(fill = "#0891b2") +
          labs(title = "Évolution des Achats par Mois", x = "", y = "Montant HT") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "#0f172a"),
                axis.text.y = element_text(color = "#0f172a"),
                axis.title = element_text(color = "#0f172a"),
                plot.title = element_text(color = "#0f172a", face = "bold"),
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white"))
        print(p1)
      }
      
      # --- Page 3: Distribution by project ---
      proj <- d %>% group_by(projet) %>% summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop")
      if (nrow(proj) > 0) {
        p2 <- ggplot(proj, aes(x = "", y = total, fill = projet)) +
          geom_col(width = 1) + coord_polar("y") +
          labs(title = "Répartition par Projet", fill = "Projet") +
          theme_void() +
          theme(plot.title = element_text(color = "#0f172a", face = "bold", hjust = 0.5))
        print(p2)
      }
      
      # --- Page 4: Top 10 suppliers ---
      topf <- d %>% group_by(fournisseur) %>% summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
        top_n(10, total) %>% arrange(total)
      if (nrow(topf) > 0) {
        p3 <- ggplot(topf, aes(x = reorder(fournisseur, total), y = total)) +
          geom_col(fill = "#22d3ee") + coord_flip() +
          labs(title = "Top 10 Fournisseurs", x = "", y = "Total TTC") +
          theme(axis.text = element_text(color = "#0f172a"),
                axis.title = element_text(color = "#0f172a"),
                plot.title = element_text(color = "#0f172a", face = "bold"),
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white"))
        print(p3)
      }
      
      # --- Page 5+: Detail table ---
      d_table <- d %>% select(date, article, quantite, unite, prix_unitaire,
                              montant_total_ht, tva_19_percent, total_ttc, fournisseur, projet)
      # Format dates in the table
      d_table$date <- format(as.Date(d_table$date), "%Y-%m-%d")
      
      rows_per_page <- 22
      n_pages <- ceiling(nrow(d_table) / rows_per_page)
      
      if (n_pages > 0) {
        for (pg in seq_len(n_pages)) {
          start <- (pg - 1) * rows_per_page + 1
          end   <- min(pg * rows_per_page, nrow(d_table))
          chunk <- d_table[start:end, ]
          
          tt <- ttheme_default(
            base_size = 8,
            core = list(fg_params = list(col = "#0f172a")),
            colhead = list(fg_params = list(col = "white", fontface = "bold"),
                           bg_params = list(fill = "#0891b2"))
          )
          tbl_grob <- tableGrob(chunk, rows = NULL, theme = tt)
          grid.arrange(
            tbl_grob,
            top = grid::textGrob(paste0("Détail des achats (", pg, "/", n_pages, ")"),
                                 gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#0f172a"))
          )
        }
      }
      
      dev.off()
    }
  )
  
  # --- Export Excel (Rapports tab) ---
  output$export_excel_2 <- downloadHandler(
    filename = function() paste0("Export_Achats_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      d <- filtered_data()
      export_excel_with_sheets(d, file)
    }
  )
  
  # --- Export PDF (Rapports tab) ---
  output$export_pdf_2 <- downloadHandler(
    filename = function() paste0("Rapport_Achats_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
    content = function(file) {
      d <- filtered_data()
      k <- kpis()
      
      pdf(file, width = 11, height = 8.5)
      
      # --- صفحة 1: ملخص المؤشرات ---
      grid.text("Rapport des Achats", y = unit(0.92, "npc"),
                gp = gpar(fontsize = 26, fontface = "bold", col = "#0f172a"))
      grid.text(paste("Généré le:", format(Sys.Date(), "%d/%m/%Y")), y = unit(0.87, "npc"),
                gp = gpar(fontsize = 11, col = "#555555"))
      
      summary_txt <- paste(
        paste("Montant Total HT:", scales::comma(k$ht)),
        paste("Montant Total TVA (19%):", scales::comma(k$tva)),
        paste("Montant Total TTC:", scales::comma(k$ttc)),
        paste("Nombre de Fournisseurs:", k$fournisseurs),
        paste("Nombre de Transactions:", nrow(d)),
        sep = "\n\n"
      )
      grid.text(summary_txt, y = unit(0.6, "npc"), gp = gpar(fontsize = 15, col = "#0f172a"), just = "center")
      
      # --- صفحة 2: تطور المشتريات شهرياً ---
      evo <- d %>%
        group_by(annee, mois_num, mois) %>%
        summarise(montant_total_ht = sum(montant_total_ht, na.rm = TRUE), .groups = "drop") %>%
        arrange(annee, mois_num) %>%
        mutate(label = paste(mois, annee))
      
      if (nrow(evo) > 0) {
        p1 <- ggplot(evo, aes(x = factor(label, levels = label), y = montant_total_ht)) +
          geom_col(fill = "#0891b2") +
          labs(title = "Évolution des Achats par Mois", x = "", y = "Montant HT") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "#0f172a"),
                axis.text.y = element_text(color = "#0f172a"),
                axis.title = element_text(color = "#0f172a"),
                plot.title = element_text(color = "#0f172a", face = "bold"),
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white"))
        print(p1)
      }
      
      # --- صفحة 3: التوزيع حسب المشروع ---
      proj <- d %>% group_by(projet) %>% summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop")
      if (nrow(proj) > 0) {
        p2 <- ggplot(proj, aes(x = "", y = total, fill = projet)) +
          geom_col(width = 1) + coord_polar("y") +
          labs(title = "Répartition par Projet", fill = "Projet") +
          theme_void() +
          theme(plot.title = element_text(color = "#0f172a", face = "bold", hjust = 0.5))
        print(p2)
      }
      
      # --- صفحة 4: أفضل 10 موردين ---
      topf <- d %>% group_by(fournisseur) %>% summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
        top_n(10, total) %>% arrange(total)
      if (nrow(topf) > 0) {
        p3 <- ggplot(topf, aes(x = reorder(fournisseur, total), y = total)) +
          geom_col(fill = "#22d3ee") + coord_flip() +
          labs(title = "Top 10 Fournisseurs", x = "", y = "Total TTC") +
          theme(axis.text = element_text(color = "#0f172a"),
                axis.title = element_text(color = "#0f172a"),
                plot.title = element_text(color = "#0f172a", face = "bold"),
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white"))
        print(p3)
      }
      
      # --- صفحات 5+: جدول التفاصيل الكامل (مقسّم على عدة صفحات) ---
      d_table <- d %>% select(date, article, quantite, unite, prix_unitaire,
                              montant_total_ht, tva_19_percent, total_ttc, fournisseur, projet)
      # Format dates in the table
      d_table$date <- format(as.Date(d_table$date), "%Y-%m-%d")
      
      rows_per_page <- 22
      n_pages <- ceiling(nrow(d_table) / rows_per_page)
      
      if (n_pages > 0) {
        for (pg in seq_len(n_pages)) {
          start <- (pg - 1) * rows_per_page + 1
          end   <- min(pg * rows_per_page, nrow(d_table))
          chunk <- d_table[start:end, ]
          
          tt <- ttheme_default(
            base_size = 8,
            core = list(fg_params = list(col = "#0f172a")),
            colhead = list(fg_params = list(col = "white", fontface = "bold"),
                           bg_params = list(fill = "#0891b2"))
          )
          tbl_grob <- tableGrob(chunk, rows = NULL, theme = tt)
          grid.arrange(
            tbl_grob,
            top = grid::textGrob(paste0("Détail des achats (", pg, "/", n_pages, ")"),
                                 gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#0f172a"))
          )
        }
      }
      
      dev.off()
    }
  )
}

# ============================================
# تشغيل التطبيق
# ============================================

shinyApp(ui, server)