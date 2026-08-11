# =============================================================================
# global.R - DEPLOYMENT VERSION (NO SUPABASE) WITH REGISTRATION REQUESTS
# Purchase Management Dashboard
# Version: 3.1.0
# =============================================================================

# =============================================================================
# 1. PACKAGES
# =============================================================================
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "plotly", "DT", "dplyr", "readxl", "janitor", "lubridate",
  "scales", "ggplot2", "tidyr", "glue", "openxlsx",
  "grid", "gridExtra", "digest"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(DT)
library(dplyr)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(ggplot2)
library(tidyr)
library(glue)
library(openxlsx)
library(grid)
library(gridExtra)
library(digest)

# =============================================================================
# 2. APPLICATION SETTINGS
# =============================================================================
APP_TITLE    <- "📊 Dashboard des Achats"
APP_VERSION  <- "v3.1.0"
APP_SUBTITLE <- "Système de gestion des achats avec demandes d'inscription"

DATA_DIR <- "data"

PALETTE <- c(
  "#0891b2", "#0f766e", "#7c3aed", "#ea580c", "#16a34a",
  "#dc2626", "#2563eb", "#ca8a04", "#db2777", "#4f46e5"
)

MOIS_FR <- c(
  "janvier", "fevrier", "mars", "avril", "mai", "juin",
  "juillet", "aout", "septembre", "octobre", "novembre", "decembre"
)

MOIS_FR_SHORT <- c(
  "Jan", "Fev", "Mar", "Avr", "Mai", "Juin",
  "Juil", "Aout", "Sep", "Oct", "Nov", "Dec"
)

# =============================================================================
# 3. FILE PATHS
# =============================================================================
if (!dir.exists(DATA_DIR)) {
  dir.create(DATA_DIR, recursive = TRUE)
}

DATASET_FILE <- file.path(DATA_DIR, "dataset_current.rds")
USERS_FILE <- file.path(DATA_DIR, "users.rds")
AUDIT_FILE <- file.path(DATA_DIR, "audit_log.rds")
PENDING_FILE <- file.path(DATA_DIR, "pending_requests.rds")

find_latest_file <- function() {
  files <- list.files(DATA_DIR, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  files <- files[!grepl("^~\\$", basename(files))]
  if (length(files) == 0) return(NULL)
  info <- file.info(files)
  files[which.max(info$mtime)]
}

# =============================================================================
# 4. USER MANAGEMENT (Local Only)
# =============================================================================
ROLES <- c("admin", "tester", "viewer")

hash_password <- function(pw) {
  digest::digest(pw, algo = "sha256")
}

verify_password <- function(pw, hash) {
  identical(hash_password(pw), hash)
}

load_users <- function() {
  if (file.exists(USERS_FILE)) {
    u <- tryCatch(readRDS(USERS_FILE), error = function(e) NULL)
    if (is.data.frame(u) && all(c("username", "password_hash", "role") %in% names(u))) {
      return(u)
    }
  }
  # Create default users
  u <- data.frame(
    username = c("admin", "tester", "viewer"),
    password_hash = c(
      hash_password("admin123"),
      hash_password("tester123"),
      hash_password("viewer123")
    ),
    role = c("admin", "tester", "viewer"),
    created_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  saveRDS(u, USERS_FILE)
  u
}

save_users <- function(u) {
  tryCatch(saveRDS(u, USERS_FILE), error = function(e) NULL)
}

load_audit <- function() {
  if (file.exists(AUDIT_FILE)) {
    a <- tryCatch(readRDS(AUDIT_FILE), error = function(e) NULL)
    if (is.data.frame(a)) return(a)
  }
  data.frame(
    horodatage = character(0),
    utilisateur = character(0),
    action = character(0),
    details = character(0),
    stringsAsFactors = FALSE
  )
}

log_audit <- function(username, action, details = "") {
  a <- load_audit()
  new_entry <- data.frame(
    horodatage = as.character(Sys.time()),
    utilisateur = username,
    action = action,
    details = details,
    stringsAsFactors = FALSE
  )
  a <- bind_rows(a, new_entry)
  tryCatch(saveRDS(a, AUDIT_FILE), error = function(e) NULL)
  a
}

users_list <- load_users()

# =============================================================================
# 5. PENDING REQUESTS MANAGEMENT
# =============================================================================

load_pending_requests <- function() {
  if (file.exists(PENDING_FILE)) {
    req <- tryCatch(readRDS(PENDING_FILE), error = function(e) NULL)
    if (is.data.frame(req) && nrow(req) > 0) {
      required_cols <- c("id", "username", "password", "role_requested", 
                         "status", "requested_at", "approved_by", "approved_at")
      for (col in required_cols) {
        if (!col %in% names(req)) {
          req[[col]] <- NA_character_
        }
      }
      return(req)
    }
  }
  req <- data.frame(
    id = integer(0),
    username = character(0),
    password = character(0),
    role_requested = character(0),
    status = character(0),
    requested_at = character(0),
    approved_by = character(0),
    approved_at = character(0),
    stringsAsFactors = FALSE
  )
  saveRDS(req, PENDING_FILE)
  req
}

save_pending_requests <- function(req) {
  tryCatch(saveRDS(req, PENDING_FILE), error = function(e) NULL)
}

add_registration_request <- function(username, password, role_requested = "viewer") {
  req <- load_pending_requests()
  users <- load_users()
  
  if (username %in% users$username) {
    return(list(success = FALSE, message = "Ce nom d'utilisateur existe déjà"))
  }
  
  if (username %in% req$username[req$status == "pending"]) {
    return(list(success = FALSE, message = "Vous avez déjà une demande en attente"))
  }
  
  new_id <- if (nrow(req) == 0) 1 else max(req$id) + 1
  
  new_request <- data.frame(
    id = new_id,
    username = username,
    password = password,
    role_requested = role_requested,
    status = "pending",
    requested_at = as.character(Sys.time()),
    approved_by = NA_character_,
    approved_at = NA_character_,
    stringsAsFactors = FALSE
  )
  
  req <- bind_rows(req, new_request)
  save_pending_requests(req)
  
  log_audit("system", "registration_request", paste(username, "-", role_requested))
  
  return(list(success = TRUE, message = "Votre demande a été envoyée à l'administrateur"))
}

approve_request <- function(request_id, admin_username, role = "viewer") {
  req <- load_pending_requests()
  users <- load_users()
  
  idx <- which(req$id == request_id & req$status == "pending")
  
  if (length(idx) == 0) {
    return(list(success = FALSE, message = "Demande non trouvée ou déjà traitée"))
  }
  
  request_data <- req[idx, ]
  
  if (request_data$username %in% users$username) {
    req$status[idx] <- "rejected"
    req$approved_by[idx] <- admin_username
    req$approved_at[idx] <- as.character(Sys.time())
    save_pending_requests(req)
    return(list(success = FALSE, message = "Ce nom d'utilisateur existe déjà"))
  }
  
  new_user <- data.frame(
    username = request_data$username,
    password_hash = hash_password(request_data$password),
    role = role,
    created_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  
  users <- bind_rows(users, new_user)
  save_users(users)
  
  req$status[idx] <- "approved"
  req$approved_by[idx] <- admin_username
  req$approved_at[idx] <- as.character(Sys.time())
  save_pending_requests(req)
  
  log_audit(admin_username, "approve_registration", 
            paste(request_data$username, "-", role))
  
  return(list(success = TRUE, message = paste("Utilisateur", request_data$username, "approuvé avec le rôle", role)))
}

reject_request <- function(request_id, admin_username) {
  req <- load_pending_requests()
  
  idx <- which(req$id == request_id & req$status == "pending")
  
  if (length(idx) == 0) {
    return(list(success = FALSE, message = "Demande non trouvée ou déjà traitée"))
  }
  
  req$status[idx] <- "rejected"
  req$approved_by[idx] <- admin_username
  req$approved_at[idx] <- as.character(Sys.time())
  save_pending_requests(req)
  
  log_audit(admin_username, "reject_registration", req$username[idx])
  
  return(list(success = TRUE, message = "Demande rejetée"))
}

get_pending_count <- function() {
  req <- load_pending_requests()
  if (nrow(req) == 0) return(0)
  sum(req$status == "pending", na.rm = TRUE)
}

get_pending_requests <- function() {
  req <- load_pending_requests()
  req[req$status == "pending", , drop = FALSE]
}

get_request_history <- function() {
  req <- load_pending_requests()
  req[req$status != "pending", , drop = FALSE]
}

# =============================================================================
# 6. DATE CONVERSION
# =============================================================================
convert_excel_date <- function(date_vec) {
  result <- rep(as.Date(NA), length(date_vec))
  
  for (i in seq_along(date_vec)) {
    val <- date_vec[i]
    if (is.null(val) || is.na(val)) next
    char_val <- trimws(as.character(val))
    if (char_val == "") next
    
    numeric_val <- suppressWarnings(as.numeric(char_val))
    if (!is.na(numeric_val) && is.finite(numeric_val)) {
      if (numeric_val >= 1 && numeric_val <= 60000) {
        result[i] <- as.Date(numeric_val, origin = "1899-12-30")
        next
      }
    }
    
    formats <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%m/%d/%Y", "%d.%m.%Y")
    for (fmt in formats) {
      parsed <- tryCatch(as.Date(char_val, format = fmt), error = function(e) as.Date(NA))
      if (!is.na(parsed)) {
        result[i] <- parsed
        break
      }
    }
  }
  return(result)
}

# =============================================================================
# 7. LOAD DATA FROM EXCEL
# =============================================================================
load_excel_data <- function(file_path) {
  if (is.null(file_path) || !file.exists(file_path)) {
    message("File not found: ", file_path)
    return(data.frame())
  }
  
  message("Reading file: ", basename(file_path))
  sheets <- excel_sheets(file_path)
  sheet_to_use <- if ("GLOBAL" %in% sheets) "GLOBAL" else sheets[1]
  
  d <- read_excel(file_path, sheet = sheet_to_use)
  d <- clean_names(d)
  
  expected_columns <- c(
    "date", "article", "quantite", "unite",
    "prix_unitaire", "montant_total_ht", "tva_19_percent", "total_ttc",
    "fournisseur", "projet", "n_facture", "num_cheque"
  )
  
  for (col in expected_columns) {
    if (!col %in% names(d)) {
      if (col == "date") {
        d[[col]] <- as.Date(NA)
      } else if (col %in% c("quantite", "prix_unitaire", "montant_total_ht",
                            "tva_19_percent", "total_ttc")) {
        d[[col]] <- NA_real_
      } else {
        d[[col]] <- NA_character_
      }
    }
  }
  
  if ("date" %in% names(d)) {
    if (!inherits(d$date, "Date")) {
      d$date <- convert_excel_date(d$date)
    }
  }
  
  numeric_columns <- c("quantite", "prix_unitaire", "montant_total_ht",
                       "tva_19_percent", "total_ttc")
  for (col in numeric_columns) {
    if (col %in% names(d)) {
      d[[col]] <- suppressWarnings(
        as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(d[[col]]))))
      )
    }
  }
  
  text_columns <- c("article", "unite", "fournisseur", "projet",
                    "n_facture", "num_cheque")
  for (col in text_columns) {
    if (col %in% names(d)) {
      d[[col]] <- trimws(as.character(d[[col]]))
      d[[col]][d[[col]] == ""] <- NA_character_
    }
  }
  
  d <- d %>%
    mutate(
      annee = ifelse(!is.na(date), year(date), NA_integer_),
      mois_num = ifelse(!is.na(date), month(date), NA_integer_),
      mois = ifelse(!is.na(date), MOIS_FR[month(date)], NA_character_),
      trimestre = ifelse(!is.na(date), quarter(date), NA_integer_),
      date_display = ifelse(!is.na(date), format(date, "%d/%m/%Y"), NA_character_)
    )
  
  d <- d %>%
    filter(
      !(is.na(date) & is.na(article) & is.na(total_ttc) &
          is.na(fournisseur) & is.na(projet))
    )
  
  return(d)
}

# =============================================================================
# 8. LOAD INITIAL DATA
# =============================================================================
latest_file <- find_latest_file()

REQUIRED_COLS <- c(
  "date", "article", "quantite", "unite",
  "prix_unitaire", "montant_total_ht", "tva_19_percent", "total_ttc",
  "fournisseur", "projet", "n_facture", "num_cheque",
  "annee", "mois_num", "mois", "trimestre", "date_display"
)

is_valid_dataset <- function(d) {
  is.data.frame(d) && all(REQUIRED_COLS %in% names(d))
}

load_initial_dataset <- function() {
  rds_exists <- file.exists(DATASET_FILE)
  excel_exists <- !is.null(latest_file)
  d <- NULL
  
  if (rds_exists) {
    d_rds <- tryCatch(readRDS(DATASET_FILE), error = function(e) NULL)
    
    if (is_valid_dataset(d_rds)) {
      if (excel_exists) {
        rds_time <- file.info(DATASET_FILE)$mtime
        excel_time <- file.info(latest_file)$mtime
        if (excel_time > rds_time) {
          d <- NULL
        } else {
          d <- d_rds
        }
      } else {
        d <- d_rds
      }
    } else {
      d <- NULL
    }
  }
  
  if (is.null(d)) {
    if (excel_exists) {
      d <- tryCatch(load_excel_data(latest_file), error = function(e) data.frame())
      if (is_valid_dataset(d)) {
        tryCatch(saveRDS(d, DATASET_FILE), error = function(e) NULL)
      }
    } else {
      d <- data.frame()
    }
  }
  
  if (!is_valid_dataset(d) && !is.data.frame(d)) {
    d <- data.frame()
  }
  
  d
}

dataset <- load_initial_dataset()
if (!is.data.frame(dataset)) dataset <- data.frame()

# =============================================================================
# 9. FILTER LISTS
# =============================================================================
if (nrow(dataset) > 0) {
  fournisseurs_liste <- sort(unique(dataset$fournisseur[!is.na(dataset$fournisseur)]))
  articles_liste <- sort(unique(dataset$article[!is.na(dataset$article)]))
  projets_liste <- sort(unique(dataset$projet[!is.na(dataset$projet)]))
  annees_liste <- sort(unique(dataset$annee[!is.na(dataset$annee)]))
} else {
  fournisseurs_liste <- character(0)
  articles_liste <- character(0)
  projets_liste <- character(0)
  annees_liste <- integer(0)
}

# =============================================================================
# 10. KPI CALCULATION
# =============================================================================
calculer_kpis <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(list(ht = 0, tva = 0, ttc = 0, fournisseurs = 0,
                transactions = 0, quantite = 0, projets = 0))
  }
  list(
    ht = sum(d$montant_total_ht, na.rm = TRUE),
    tva = sum(d$tva_19_percent, na.rm = TRUE),
    ttc = sum(d$total_ttc, na.rm = TRUE),
    fournisseurs = n_distinct(d$fournisseur[!is.na(d$fournisseur) & d$fournisseur != ""]),
    transactions = nrow(d),
    quantite = sum(d$quantite, na.rm = TRUE),
    projets = n_distinct(d$projet[!is.na(d$projet) & d$projet != ""])
  )
}

# =============================================================================
# 11. PLOTLY THEME
# =============================================================================
apply_plotly_theme <- function(p, mode = "Light") {
  if (identical(mode, "Dark")) {
    p %>% layout(
      font = list(color = "white"),
      paper_bgcolor = "#1a2332", plot_bgcolor = "#1a2332",
      xaxis = list(gridcolor = "#333333", color = "white",
                   tickfont = list(color = "white"), titlefont = list(color = "white")),
      yaxis = list(gridcolor = "#333333", color = "white",
                   tickfont = list(color = "white"), titlefont = list(color = "white")),
      legend = list(font = list(color = "white"))
    )
  } else {
    p %>% layout(
      font = list(color = "#333333"),
      paper_bgcolor = "#ffffff", plot_bgcolor = "#f9fafb",
      xaxis = list(gridcolor = "#e5e7eb", color = "#333333",
                   tickfont = list(color = "#333333"), titlefont = list(color = "#333333")),
      yaxis = list(gridcolor = "#e5e7eb", color = "#333333",
                   tickfont = list(color = "#333333"), titlefont = list(color = "#333333")),
      legend = list(font = list(color = "#333333"))
    )
  }
}

# =============================================================================
# 12. EXCEL EXPORT
# =============================================================================
clean_sheet_name <- function(x, used_names = character(0)) {
  x <- as.character(x)
  x <- trimws(x)
  if (x == "" || is.na(x)) x <- "Sheet"
  
  forbidden <- c("[", "]", "*", "?", "/", "\\", ":")
  for (ch in forbidden) {
    x <- gsub(ch, "", x, fixed = TRUE)
  }
  x <- trimws(x)
  if (x == "") x <- "Sheet"
  x <- substr(x, 1, 31)
  
  base_x <- x
  suffix <- 1
  while (x %in% used_names) {
    suffix <- suffix + 1
    tag <- paste0("_", suffix)
    x <- paste0(substr(base_x, 1, 31 - nchar(tag)), tag)
  }
  x
}

export_excel_with_sheets <- function(data, filename) {
  wb <- createWorkbook()
  
  header_style <- createStyle(
    fontName = "Calibri", fontSize = 11, textDecoration = "bold",
    fontColour = "#FFFFFF", fgFill = "#0F766E",
    halign = "center", valign = "center",
    border = "Bottom", borderColour = "#0B4F4A"
  )
  currency_style <- createStyle(numFmt = '#,##0.00 "DA"', halign = "right")
  date_style <- createStyle(numFmt = "dd/mm/yyyy", halign = "center")
  
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    addWorksheet(wb, "GLOBAL")
    writeData(wb, "GLOBAL", data.frame(Message = "Aucune donnee a exporter"),
              startRow = 1, headerStyle = header_style)
    saveWorkbook(wb, filename, overwrite = TRUE)
    return(filename)
  }
  
  used_sheet_names <- character(0)
  
  # GLOBAL SHEET
  addWorksheet(wb, "GLOBAL")
  used_sheet_names <- c(used_sheet_names, "GLOBAL")
  
  display_data <- data %>%
    mutate(
      Date = date_display, Article = article, Quantite = quantite,
      Unite = unite, `Prix Unitaire` = prix_unitaire,
      `Montant HT` = montant_total_ht, TVA = tva_19_percent, TTC = total_ttc,
      Fournisseur = fournisseur, Projet = projet,
      `N Facture` = n_facture, `N Cheque` = num_cheque
    ) %>%
    select(Date, Article, Quantite, Unite, `Prix Unitaire`,
           `Montant HT`, TVA, TTC, Fournisseur, Projet,
           `N Facture`, `N Cheque`)
  
  writeData(wb, "GLOBAL", display_data, startRow = 1, headerStyle = header_style)
  if (nrow(display_data) > 0) {
    addStyle(wb, "GLOBAL", currency_style, rows = 2:(nrow(display_data) + 1),
             cols = c(5, 6, 7, 8), gridExpand = TRUE)
    addStyle(wb, "GLOBAL", date_style, rows = 2:(nrow(display_data) + 1),
             cols = 1, gridExpand = TRUE)
  }
  setColWidths(wb, "GLOBAL", cols = 1:11,
               widths = c(15, 35, 12, 10, 15, 18, 12, 18, 25, 15, 15))
  freezePane(wb, "GLOBAL", firstRow = TRUE)
  addFilter(wb, "GLOBAL", rows = 1, cols = 1:11)
  
  # PROJECT SHEETS
  projects <- sort(unique(data$projet[!is.na(data$projet) & data$projet != ""]))
  
  for (proj in projects) {
    sheet_name <- clean_sheet_name(proj, used_sheet_names)
    used_sheet_names <- c(used_sheet_names, sheet_name)
    addWorksheet(wb, sheet_name)
    
    proj_data <- data %>%
      filter(projet == proj) %>%
      mutate(
        Date = date_display, Article = article, Quantite = quantite,
        Unite = unite, `Prix Unitaire` = prix_unitaire,
        `Montant HT` = montant_total_ht, TVA = tva_19_percent, TTC = total_ttc,
        Fournisseur = fournisseur, `N Facture` = n_facture, `N Cheque` = num_cheque
      ) %>%
      select(Date, Article, Quantite, Unite, `Prix Unitaire`,
             `Montant HT`, TVA, TTC, Fournisseur, `N Facture`, `N Cheque`)
    
    writeData(wb, sheet_name, proj_data, startRow = 1, headerStyle = header_style)
    if (nrow(proj_data) > 0) {
      addStyle(wb, sheet_name, currency_style, rows = 2:(nrow(proj_data) + 1),
               cols = c(5, 6, 7, 8), gridExpand = TRUE)
      addStyle(wb, sheet_name, date_style, rows = 2:(nrow(proj_data) + 1),
               cols = 1, gridExpand = TRUE)
    }
    setColWidths(wb, sheet_name, cols = 1:10,
                 widths = c(15, 35, 12, 10, 15, 18, 12, 18, 25, 15))
    freezePane(wb, sheet_name, firstRow = TRUE)
    addFilter(wb, sheet_name, rows = 1, cols = 1:10)
  }
  
  # RESUME
  addWorksheet(wb, "Resume")
  kpis <- calculer_kpis(data)
  summary_data <- data.frame(
    Indicateur = c("Montant Total HT", "Montant Total TVA (19%)", "Montant Total TTC",
                   "Nombre de Fournisseurs", "Nombre de Transactions",
                   "Nombre de Projets", "Quantite Totale"),
    Valeur = c(kpis$ht, kpis$tva, kpis$ttc, kpis$fournisseurs,
               kpis$transactions, kpis$projets, kpis$quantite)
  )
  summary_data$Valeur <- ifelse(
    grepl("Montant|TVA|TTC", summary_data$Indicateur),
    paste0(format(round(as.numeric(summary_data$Valeur), 2), big.mark = ",", decimal.mark = ","), " DA"),
    format(round(as.numeric(summary_data$Valeur), 0), big.mark = ",", decimal.mark = ",")
  )
  writeData(wb, "Resume", summary_data, startRow = 1, headerStyle = header_style)
  setColWidths(wb, "Resume", cols = 1:2, widths = c(30, 25))
  
  # STATS FOURNISSEURS
  addWorksheet(wb, "Stats Fournisseurs")
  stats_f <- data %>%
    filter(!is.na(fournisseur), fournisseur != "") %>%
    group_by(Fournisseur = fournisseur) %>%
    summarise(
      `Total HT` = sum(montant_total_ht, na.rm = TRUE),
      `Total TVA` = sum(tva_19_percent, na.rm = TRUE),
      `Total TTC` = sum(total_ttc, na.rm = TRUE),
      `Nbre Transactions` = n(),
      Projets = n_distinct(projet[!is.na(projet) & projet != ""]),
      .groups = "drop"
    ) %>%
    arrange(desc(`Total TTC`))
  
  writeData(wb, "Stats Fournisseurs", stats_f, startRow = 1, headerStyle = header_style)
  if (nrow(stats_f) > 0) {
    addStyle(wb, "Stats Fournisseurs", currency_style, rows = 2:(nrow(stats_f) + 1),
             cols = 2:4, gridExpand = TRUE)
  }
  setColWidths(wb, "Stats Fournisseurs", cols = 1:5, widths = c(30, 16, 16, 16, 10))
  freezePane(wb, "Stats Fournisseurs", firstRow = TRUE)
  
  # STATS PROJETS
  addWorksheet(wb, "Stats Projets")
  stats_p <- data %>%
    filter(!is.na(projet), projet != "") %>%
    group_by(Projet = projet) %>%
    summarise(
      `Total HT` = sum(montant_total_ht, na.rm = TRUE),
      `Total TVA` = sum(tva_19_percent, na.rm = TRUE),
      `Total TTC` = sum(total_ttc, na.rm = TRUE),
      `Nbre Transactions` = n(),
      Fournisseurs = n_distinct(fournisseur[!is.na(fournisseur) & fournisseur != ""]),
      Quantite = sum(quantite, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(`Total TTC`))
  
  writeData(wb, "Stats Projets", stats_p, startRow = 1, headerStyle = header_style)
  if (nrow(stats_p) > 0) {
    addStyle(wb, "Stats Projets", currency_style, rows = 2:(nrow(stats_p) + 1),
             cols = 2:4, gridExpand = TRUE)
  }
  setColWidths(wb, "Stats Projets", cols = 1:6, widths = c(25, 16, 16, 16, 10, 14))
  freezePane(wb, "Stats Projets", firstRow = TRUE)
  
  # VALIDATION
  addWorksheet(wb, "Erreurs Validation")
  dup_facture <- data$n_facture[!is.na(data$n_facture) & data$n_facture != ""]
  dup_facture <- dup_facture[duplicated(dup_facture)]
  dup_cheque <- data$num_cheque[!is.na(data$num_cheque) & data$num_cheque != ""]
  dup_cheque <- dup_cheque[duplicated(dup_cheque)]
  
  validation <- data %>%
    mutate(row_id = row_number()) %>%
    filter(
      is.na(date) |
        is.na(fournisseur) | fournisseur == "" |
        is.na(projet) | projet == "" |
        (n_facture %in% dup_facture) |
        (num_cheque %in% dup_cheque)
    ) %>%
    mutate(
      Probleme = case_when(
        is.na(date) ~ "Date manquante",
        is.na(fournisseur) | fournisseur == "" ~ "Fournisseur manquant",
        is.na(projet) | projet == "" ~ "Projet manquant",
        n_facture %in% dup_facture ~ "N Facture en double",
        num_cheque %in% dup_cheque ~ "N Cheque en double",
        TRUE ~ "Autre"
      )
    ) %>%
    select(Ligne = row_id, Date = date_display, Article = article,
           Fournisseur = fournisseur, Projet = projet,
           `N Facture` = n_facture, `N Cheque` = num_cheque, Probleme)
  
  if (nrow(validation) > 0) {
    writeData(wb, "Erreurs Validation", validation, startRow = 1, headerStyle = header_style)
    setColWidths(wb, "Erreurs Validation", cols = 1:8,
                 widths = c(8, 14, 30, 25, 15, 15, 15, 25))
  } else {
    writeData(wb, "Erreurs Validation",
              data.frame(Resultat = "Aucune erreur detectee"),
              startRow = 1, headerStyle = header_style)
  }
  
  saveWorkbook(wb, filename, overwrite = TRUE)
  return(filename)
}

# =============================================================================
# 13. PDF REPORT
# =============================================================================
create_pdf_report <- function(d, file, title = "Rapport des Achats") {
  if (is.null(d) || nrow(d) == 0) {
    pdf(file, width = 11, height = 8.5)
    grid.newpage()
    grid.text("Aucune donnee disponible", y = unit(0.5, "npc"),
              gp = gpar(fontsize = 22, fontface = "bold"))
    dev.off()
    return()
  }
  
  k <- calculer_kpis(d)
  pdf(file, width = 11, height = 8.5)
  
  grid.newpage()
  grid.text(title, y = unit(0.92, "npc"),
            gp = gpar(fontsize = 26, fontface = "bold", col = "#0f172a"))
  
  summary_txt <- paste(
    paste("Montant Total HT :", format(round(k$ht, 2), big.mark = ","), "DA"),
    paste("Montant Total TVA (19%) :", format(round(k$tva, 2), big.mark = ","), "DA"),
    paste("Montant Total TTC :", format(round(k$ttc, 2), big.mark = ","), "DA"),
    paste("Nombre de Fournisseurs :", k$fournisseurs),
    paste("Nombre de Transactions :", k$transactions),
    paste("Nombre de Projets :", k$projets),
    paste("Quantite Totale :", format(round(k$quantite, 0), big.mark = ",")),
    sep = "\n\n"
  )
  grid.text(summary_txt, y = unit(0.55, "npc"),
            gp = gpar(fontsize = 14, col = "#0f172a"), just = "center")
  
  evo <- d %>%
    filter(!is.na(annee), !is.na(mois_num)) %>%
    group_by(annee, mois_num, mois) %>%
    summarise(ht = sum(montant_total_ht, na.rm = TRUE), .groups = "drop") %>%
    arrange(annee, mois_num) %>%
    mutate(label = paste(mois, annee))
  
  if (nrow(evo) > 0) {
    p1 <- ggplot(evo, aes(x = factor(label, levels = label), y = ht)) +
      geom_col(fill = "#0891b2") +
      labs(title = "Evolution des Achats par Mois", x = "", y = "Montant HT") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p1)
  }
  
  proj <- d %>%
    filter(!is.na(projet), projet != "") %>%
    group_by(projet) %>%
    summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop")
  
  if (nrow(proj) > 0) {
    p2 <- ggplot(proj, aes(x = "", y = total, fill = projet)) +
      geom_col(width = 1) +
      coord_polar("y") +
      labs(title = "Repartition par Projet", fill = "Projet") +
      theme_void() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    print(p2)
  }
  
  topf <- d %>%
    filter(!is.na(fournisseur), fournisseur != "") %>%
    group_by(fournisseur) %>%
    summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    slice_head(n = 10) %>%
    arrange(total)
  
  if (nrow(topf) > 0) {
    p3 <- ggplot(topf, aes(x = reorder(fournisseur, total), y = total)) +
      geom_col(fill = "#22d3ee") +
      coord_flip() +
      labs(title = "Top 10 Fournisseurs", x = "", y = "Total TTC") +
      theme_minimal()
    print(p3)
  }
  
  d_table <- d %>%
    select(date_display, article, quantite, unite, prix_unitaire,
           montant_total_ht, tva_19_percent, total_ttc, fournisseur, projet) %>%
    rename(Date = date_display, Article = article, Quantite = quantite,
           Unite = unite, `Prix Unitaire` = prix_unitaire,
           `Montant HT` = montant_total_ht, TVA = tva_19_percent,
           TTC = total_ttc, Fournisseur = fournisseur, Projet = projet)
  
  rows_per_page <- 22
  n_pages <- ceiling(nrow(d_table) / rows_per_page)
  
  if (nrow(d_table) > 0 && n_pages > 0) {
    for (pg in seq_len(n_pages)) {
      start <- (pg - 1) * rows_per_page + 1
      end <- min(pg * rows_per_page, nrow(d_table))
      chunk <- d_table[start:end, , drop = FALSE]
      
      tt <- ttheme_default(
        base_size = 7,
        core = list(fg_params = list(col = "#0f172a")),
        colhead = list(
          fg_params = list(col = "white", fontface = "bold"),
          bg_params = list(fill = "#0891b2")
        )
      )
      
      tbl_grob <- tableGrob(chunk, rows = NULL, theme = tt)
      grid.newpage()
      grid.arrange(tbl_grob,
                   top = grid::textGrob(paste0("Detail des achats (", pg, "/", n_pages, ")"),
                                        gp = grid::gpar(fontsize = 14, fontface = "bold")))
    }
  }
  
  dev.off()
}

# =============================================================================
# 14. STARTUP MESSAGE
# =============================================================================
message("\n", paste(rep("=", 50), collapse = ""))
message(APP_TITLE, " - ", APP_VERSION)
message(paste(rep("=", 50), collapse = ""))
message("Data directory: ", normalizePath(DATA_DIR))
message("Rows in dataset: ", nrow(dataset))
message("Users file: ", USERS_FILE)
message("Pending requests file: ", PENDING_FILE)
message(paste(rep("=", 50), collapse = ""))
message("✅ Application ready to run!\n")