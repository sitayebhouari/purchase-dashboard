# =============================================================================
# server.R - Purchase Management Dashboard
# Version: 3.1.0 - With Registration Requests
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # 1. AUTH STATE
  # ===========================================================================
  auth <- reactiveValues(
    logged_in = FALSE,
    username = NULL,
    role = NULL,
    show_register = FALSE
  )
  
  users_rv <- reactiveVal(load_users())
  audit_rv <- reactiveVal(load_audit())
  
  can_admin <- reactive({ isTRUE(auth$role == "admin") })
  can_edit <- reactive({ isTRUE(auth$role %in% c("admin", "tester")) })
  
  output$login_error <- renderUI(NULL)
  output$register_message <- renderUI(NULL)
  
  # ===========================================================================
  # 2. NAVIGATION BETWEEN LOGIN, REGISTER, MAIN
  # ===========================================================================
  
  observeEvent(input$go_to_register_from_login, {
    shinyjs::hide("login_screen")
    shinyjs::show("register_screen")
  })
  
  observeEvent(input$go_to_register, {
    shinyjs::hide("login_screen")
    shinyjs::show("register_screen")
  })
  
  observeEvent(input$go_to_login_from_register, {
    shinyjs::hide("register_screen")
    shinyjs::show("login_screen")
    output$register_message <- renderUI(NULL)
  })
  
  # ===========================================================================
  # 3. LOGIN
  # ===========================================================================
  
  observeEvent(input$login_btn, {
    u <- trimws(input$login_username %||% "")
    p <- input$login_password %||% ""
    
    users <- users_rv()
    
    if (is.null(users) || nrow(users) == 0) {
      users <- load_users()
      users_rv(users)
    }
    
    match_row <- users[users$username == u, , drop = FALSE]
    
    if (nrow(match_row) == 1 && verify_password(p, match_row$password_hash[1])) {
      auth$logged_in <- TRUE
      auth$username <- u
      auth$role <- match_row$role[1]
      
      shinyjs::hide("login_screen")
      shinyjs::hide("register_screen")
      shinyjs::show("main_app")
      output$login_error <- renderUI(NULL)
      
      log_audit(u, "connexion")
      showNotification(paste0("✅ Bienvenue, ", u, " (", match_row$role[1], ")"), type = "message")
    } else {
      output$login_error <- renderUI(
        tags$div(style = "color:#dc2626; margin: 8px 0;", "❌ Identifiants incorrects")
      )
    }
  })
  
  # ===========================================================================
  # 4. REGISTRATION
  # ===========================================================================
  
  observeEvent(input$btn_register, {
    username <- trimws(input$reg_username %||% "")
    password <- input$reg_password %||% ""
    password_confirm <- input$reg_password_confirm %||% ""
    role <- input$reg_role %||% "viewer"
    
    # Validation
    if (username == "") {
      output$register_message <- renderUI(
        tags$div(style = "color: #dc2626;", "❌ Veuillez saisir un nom d'utilisateur")
      )
      return()
    }
    
    if (nchar(username) < 3) {
      output$register_message <- renderUI(
        tags$div(style = "color: #dc2626;", "❌ Le nom d'utilisateur doit contenir au moins 3 caractères")
      )
      return()
    }
    
    if (password == "") {
      output$register_message <- renderUI(
        tags$div(style = "color: #dc2626;", "❌ Veuillez saisir un mot de passe")
      )
      return()
    }
    
    if (nchar(password) < 4) {
      output$register_message <- renderUI(
        tags$div(style = "color: #dc2626;", "❌ Le mot de passe doit contenir au moins 4 caractères")
      )
      return()
    }
    
    if (password != password_confirm) {
      output$register_message <- renderUI(
        tags$div(style = "color: #dc2626;", "❌ Les mots de passe ne correspondent pas")
      )
      return()
    }
    
    # Submit request
    result <- add_registration_request(username, password, role)
    
    if (result$success) {
      output$register_message <- renderUI(
        tags$div(style = "color: #16a34a;", 
                 "✅ ", result$message, 
                 tags$br(),
                 "📧 Vous serez notifié lorsque votre compte sera approuvé.")
      )
      # Clear form
      updateTextInput(session, "reg_username", value = "")
      updateTextInput(session, "reg_password", value = "")
      updateTextInput(session, "reg_password_confirm", value = "")
    } else {
      output$register_message <- renderUI(
        tags$div(style = "color: #dc2626;", "❌ ", result$message)
      )
    }
  })
  
  # ===========================================================================
  # 5. LOGOUT
  # ===========================================================================
  
  observeEvent(input$logout_btn, {
    if (!is.null(auth$username)) {
      log_audit(auth$username, "deconnexion")
    }
    auth$logged_in <- FALSE
    auth$username <- NULL
    auth$role <- NULL
    shinyjs::show("login_screen")
    shinyjs::hide("register_screen")
    shinyjs::hide("main_app")
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
  })
  
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  output$header_user_info <- renderUI({
    if (!isTRUE(auth$logged_in)) return(NULL)
    pending_count <- get_pending_count()
    pending_badge <- if (pending_count > 0 && can_admin()) {
      tags$span(
        class = "badge",
        style = "background-color: #dc3545; margin-right: 10px;",
        pending_count
      )
    } else {
      NULL
    }
    tags$div(
      style = "padding: 15px; color: white;",
      tags$span(paste0("👤 ", auth$username, " (", auth$role, ")"), style = "margin-right: 15px;"),
      pending_badge,
      actionButton("logout_btn", "🚪 Deconnexion", icon = icon("sign-out-alt"),
                   class = "btn-sm btn-default")
    )
  })
  
  # ===========================================================================
  # 6. DYNAMIC SIDEBAR MENU
  # ===========================================================================
  output$dynamic_menu <- renderMenu({
    if (!isTRUE(auth$logged_in)) return(sidebarMenu())
    
    items <- list(
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("📈 Management", tabName = "management", icon = icon("chart-line")),
      menuItem("📋 Donnees", tabName = "data", icon = icon("table"))
    )
    
    if (can_edit()) {
      items <- c(items, list(menuItem("➕ Ajouter", tabName = "ajouter", icon = icon("plus-circle"))))
    }
    
    items <- c(items, list(menuItem("📄 Rapports", tabName = "reports", icon = icon("file-alt"))))
    
    if (can_admin()) {
      items <- c(items, list(menuItem("👥 Utilisateurs", tabName = "users", icon = icon("users-cog"))))
      
      pending_count <- get_pending_count()
      badge_label <- if (pending_count > 0) {
        tags$span(
          class = "badge",
          style = "background-color: #dc3545; margin-left: 10px;",
          pending_count
        )
      } else {
        NULL
      }
      items <- c(items, list(
        menuItem(
          "📋 Demandes",
          tabName = "requests",
          icon = icon("inbox"),
          badgeLabel = if (pending_count > 0) pending_count else NULL,
          badgeColor = "red"
        )
      ))
    }
    
    do.call(sidebarMenu, c(list(id = "sidebar_menu_dynamic"), items))
  })
  
  observe({
    req(auth$logged_in)
    if (can_edit()) {
      shinyjs::show("sidebar_file")
      shinyjs::show("sidebar_hr_file")
    } else {
      shinyjs::hide("sidebar_file")
      shinyjs::hide("sidebar_hr_file")
    }
  })
  
  observe({
    if (!isTRUE(auth$logged_in)) {
      shinyjs::show("register_link")
    } else {
      shinyjs::hide("register_link")
    }
  })
  
  # ===========================================================================
  # 7. REACTIVE DATA SOURCE
  # ===========================================================================
  
  rv_data <- reactiveVal(dataset)
  current_file <- reactiveVal(if (!is.null(latest_file)) latest_file else NULL)
  
  # ===========================================================================
  # 8. THEME MANAGEMENT
  # ===========================================================================
  observeEvent(input$theme_toggle, {
    if (input$theme_toggle == "Dark") {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  # ===========================================================================
  # 9. UPDATE FILTERS
  # ===========================================================================
  update_filter_choices <- function(d) {
    if (is.null(d) || nrow(d) == 0) return()
    
    projets <- sort(unique(d$projet[!is.na(d$projet) & d$projet != ""]))
    fournisseurs <- sort(unique(d$fournisseur[!is.na(d$fournisseur) & d$fournisseur != ""]))
    annees <- sort(unique(d$annee[!is.na(d$annee)]))
    
    updatePickerInput(session, "f_projet", choices = c("Tous" = "all", projets), selected = input$f_projet)
    updatePickerInput(session, "f_fournisseur", choices = c("Tous" = "all", fournisseurs), selected = input$f_fournisseur)
    updatePickerInput(session, "f_annee", choices = c("Toutes" = "all", annees), selected = input$f_annee)
  }
  
  observe({ update_filter_choices(rv_data()) })
  
  # ===========================================================================
  # 10. CURRENT FILE DISPLAY
  # ===========================================================================
  output$current_file <- renderUI({
    file <- current_file()
    if (is.null(file)) {
      tags$div(style = "color: #dc2626; padding: 5px 0; font-size: 12px;", "❌ Aucun fichier charge")
    } else {
      tags$div(style = "color: #16a34a; padding: 5px 0; font-size: 12px;", 
               tags$b("📁 Fichier: "), basename(file))
    }
  })
  
  # ===========================================================================
  # 11. USE LATEST EXCEL FILE
  # ===========================================================================
  observeEvent(input$use_latest, {
    req(can_edit())
    file <- find_latest_file()
    
    if (is.null(file)) {
      showNotification("❌ Aucun fichier Excel trouve", type = "error")
      return()
    }
    
    d <- tryCatch(load_excel_data(file), error = function(e) {
      showNotification(paste("❌ Erreur:", e$message), type = "error")
      NULL
    })
    
    if (is.null(d) || nrow(d) == 0) {
      showNotification("❌ Fichier vide ou invalide", type = "error")
      return()
    }
    
    rv_data(d)
    current_file(file)
    update_filter_choices(d)
    tryCatch(saveRDS(d, DATASET_FILE), error = function(e) NULL)
    log_audit(auth$username, "import_fichier", basename(file))
    showNotification(paste0("✅ Fichier charge: ", basename(file)), type = "message")
  })
  
  # ===========================================================================
  # 12. BROWSE FILE
  # ===========================================================================
  observeEvent(input$browse_file, {
    req(can_edit())
    file <- tryCatch(file.choose(), error = function(e) NULL)
    if (is.null(file) || !file.exists(file)) return()
    
    d <- tryCatch(load_excel_data(file), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) {
      showNotification("❌ Erreur lors du chargement", type = "error")
      return()
    }
    
    rv_data(d)
    current_file(file)
    update_filter_choices(d)
    tryCatch(saveRDS(d, DATASET_FILE), error = function(e) NULL)
    log_audit(auth$username, "import_fichier", basename(file))
    showNotification(paste0("✅ Fichier charge: ", basename(file)), type = "message")
  })
  
  # ===========================================================================
  # 13. RESET FILTERS
  # ===========================================================================
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "f_projet", selected = "all")
    updatePickerInput(session, "f_fournisseur", selected = "all")
    updatePickerInput(session, "f_annee", selected = "all")
    updatePickerInput(session, "f_mois", selected = "all")
    showNotification("🔄 Filtres reinitialises", type = "message")
  })
  
  # ===========================================================================
  # 14. FILTERED DATA
  # ===========================================================================
  filtered_data <- reactive({
    d <- rv_data()
    if (is.null(d) || nrow(d) == 0) return(data.frame())
    
    if (!is.null(input$f_projet) && input$f_projet != "all") {
      d <- d %>% filter(projet == input$f_projet)
    }
    if (!is.null(input$f_fournisseur) && input$f_fournisseur != "all") {
      d <- d %>% filter(fournisseur == input$f_fournisseur)
    }
    if (!is.null(input$f_annee) && input$f_annee != "all") {
      d <- d %>% filter(annee == as.integer(input$f_annee))
    }
    if (!is.null(input$f_mois) && input$f_mois != "all") {
      d <- d %>% filter(mois == input$f_mois)
    }
    
    d
  })
  
  # ===========================================================================
  # 15. KPIS
  # ===========================================================================
  kpis <- reactive({ calculer_kpis(filtered_data()) })
  
  # ===========================================================================
  # 16. DASHBOARD VALUE BOXES
  # ===========================================================================
  output$kpi_ht_dash <- renderValueBox({
    valueBox(format(round(kpis()$ht, 2), big.mark = ",", decimal.mark = ","), 
             "💰 Total HT",
             icon = icon("file-invoice-dollar"), color = "blue")
  })
  output$kpi_tva_dash <- renderValueBox({
    valueBox(format(round(kpis()$tva, 2), big.mark = ",", decimal.mark = ","), 
             "📊 Total TVA (19%)",
             icon = icon("percent"), color = "yellow")
  })
  output$kpi_ttc_dash <- renderValueBox({
    valueBox(format(round(kpis()$ttc, 2), big.mark = ",", decimal.mark = ","), 
             "💎 Total TTC",
             icon = icon("money-bill-wave"), color = "green")
  })
  output$kpi_four_dash <- renderValueBox({
    valueBox(kpis()$fournisseurs, "🏢 Fournisseurs", 
             icon = icon("truck"), color = "purple")
  })
  output$total_achats <- renderValueBox({
    valueBox(format(round(kpis()$ttc, 2), big.mark = ",", decimal.mark = ","), 
             "🛒 Total Achats (TTC)",
             icon = icon("shopping-cart"), color = "aqua")
  })
  output$nb_transactions <- renderValueBox({
    valueBox(kpis()$transactions, "📋 Transactions", 
             icon = icon("receipt"), color = "orange")
  })
  output$nb_fournisseurs <- renderValueBox({
    valueBox(kpis()$fournisseurs, "🏢 Fournisseurs Actifs", 
             icon = icon("building"), color = "green")
  })
  output$kpi_ht_data <- renderValueBox({
    valueBox(format(round(kpis()$ht, 2), big.mark = ",", decimal.mark = ","), 
             "💰 Total HT",
             icon = icon("file-invoice-dollar"), color = "blue")
  })
  output$kpi_tva_data <- renderValueBox({
    valueBox(format(round(kpis()$tva, 2), big.mark = ",", decimal.mark = ","), 
             "📊 Total TVA (19%)",
             icon = icon("percent"), color = "yellow")
  })
  output$kpi_ttc_data <- renderValueBox({
    valueBox(format(round(kpis()$ttc, 2), big.mark = ",", decimal.mark = ","), 
             "💎 Total TTC",
             icon = icon("money-bill-wave"), color = "green")
  })
  output$kpi_four_data <- renderValueBox({
    valueBox(kpis()$fournisseurs, "🏢 Fournisseurs", 
             icon = icon("truck"), color = "purple")
  })
  
  # ===========================================================================
  # 17. PLOT - EVOLUTION
  # ===========================================================================
  output$plot_evolution <- renderPlotly({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    evo <- d %>%
      filter(!is.na(annee), !is.na(mois_num)) %>%
      group_by(annee, mois_num, mois) %>%
      summarise(total = sum(montant_total_ht, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee, mois_num) %>%
      mutate(label = paste(mois, annee))
    
    p <- plot_ly(
      evo, x = ~factor(label, levels = label), y = ~total, type = "bar",
      marker = list(color = "#0891b2"),
      hovertemplate = "%{x}<br>HT: %{y:,.2f} DA<extra></extra>"
    ) %>% layout(xaxis = list(title = ""), yaxis = list(title = "Montant HT (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  # ===========================================================================
  # 18. PLOT - PROJECT DONUT
  # ===========================================================================
  output$plot_projet_donut <- renderPlotly({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    proj <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop")
    
    validate(need(nrow(proj) > 0, "Aucun projet disponible"))
    
    p <- plot_ly(
      proj, labels = ~projet, values = ~total, type = "pie", hole = 0.55,
      marker = list(colors = PALETTE), textinfo = "label+percent",
      hovertemplate = "%{label}<br>%{value:,.2f} DA<extra></extra>"
    )
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  # ===========================================================================
  # 19. PLOT - TOP SUPPLIERS
  # ===========================================================================
  output$plot_fournisseurs <- renderPlotly({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    topf <- d %>%
      filter(!is.na(fournisseur), fournisseur != "") %>%
      group_by(fournisseur) %>%
      summarise(total = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 10) %>%
      arrange(total)
    
    validate(need(nrow(topf) > 0, "Aucun fournisseur disponible"))
    
    p <- plot_ly(
      topf, x = ~total, y = ~reorder(fournisseur, total), type = "bar", orientation = "h",
      marker = list(color = "#22d3ee"),
      hovertemplate = "%{y}<br>%{x:,.2f} DA<extra></extra>"
    ) %>% layout(xaxis = list(title = "Total TTC (DA)"), yaxis = list(title = ""))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  # ===========================================================================
  # 20. DATA TABLE
  # ===========================================================================
  output$table_data <- renderDT({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    d <- d %>%
      mutate(Date = date_display) %>%
      select(Date, article, quantite, unite, prix_unitaire,
             montant_total_ht, tva_19_percent, total_ttc,
             fournisseur, projet, n_facture, num_cheque)
    
    datatable(
      d, rownames = FALSE, filter = "top", extensions = "Scroller",
      options = list(dom = "lrtip", scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE,
                     scroller = TRUE, deferRender = TRUE, autoWidth = TRUE, pageLength = 25),
      class = "stripe hover nowrap"
    ) %>%
      formatCurrency(c("prix_unitaire", "montant_total_ht", "tva_19_percent", "total_ttc"),
                     currency = "", interval = 3, mark = ",", digits = 2)
  })
  
  # ===========================================================================
  # 21. SUPPLIER STATISTICS
  # ===========================================================================
  output$table_fournisseurs <- renderDT({
    d <- rv_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    stats <- d %>%
      filter(!is.na(fournisseur), fournisseur != "") %>%
      group_by(fournisseur) %>%
      summarise(Total_TTC = sum(total_ttc, na.rm = TRUE), 
                Transactions = n(),
                Projets = n_distinct(projet[!is.na(projet) & projet != ""]), 
                .groups = "drop") %>%
      arrange(desc(Total_TTC))
    
    datatable(stats, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "lrtip")) %>%
      formatCurrency("Total_TTC", currency = "", interval = 3, mark = ",", digits = 2)
  })
  
  # ===========================================================================
  # 22. MANAGEMENT DATA / KPIS / PLOTS
  # ===========================================================================
  management_data <- reactive({
    d <- filtered_data()
    if (nrow(d) == 0) return(d)
    d %>% mutate(annee = as.integer(annee), 
                 mois_num = as.integer(mois_num), 
                 trimestre = as.integer(trimestre))
  })
  
  management_kpis <- reactive({
    d <- management_data()
    if (nrow(d) == 0) {
      return(list(ttc = 0, ht = 0, avg = 0, transactions = 0, suppliers = 0,
                  projects = 0, quantity = 0, growth = NA_real_))
    }
    
    yearly <- d %>% filter(!is.na(annee)) %>% group_by(annee) %>%
      summarise(ttc = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>% arrange(annee)
    
    growth <- NA_real_
    if (nrow(yearly) >= 2) {
      prev <- yearly$ttc[nrow(yearly) - 1]
      curr <- yearly$ttc[nrow(yearly)]
      if (is.finite(prev) && prev != 0) growth <- (curr - prev) / abs(prev) * 100
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
    valueBox(format(round(k$ttc, 2), big.mark = ",", decimal.mark = ","), "💎 Total TTC",
             icon = icon("money-bill-wave"), color = "green")
  })
  output$mgmt_kpi_ht <- renderValueBox({
    k <- management_kpis()
    valueBox(format(round(k$ht, 2), big.mark = ",", decimal.mark = ","), "💰 Total HT",
             icon = icon("file-invoice-dollar"), color = "blue")
  })
  output$mgmt_kpi_avg <- renderValueBox({
    k <- management_kpis()
    valueBox(format(round(k$avg, 2), big.mark = ",", decimal.mark = ","), "📊 Achat Moyen TTC",
             icon = icon("calculator"), color = "purple")
  })
  output$mgmt_kpi_growth <- renderValueBox({
    k <- management_kpis()
    growth_val <- k$growth
    if (is.na(growth_val)) {
      value <- "N/A"; color <- "gray"; icon_used <- icon("minus")
    } else if (growth_val >= 0) {
      value <- paste0("+", round(growth_val, 1), "%"); color <- "green"; icon_used <- icon("arrow-up")
    } else {
      value <- paste0(round(growth_val, 1), "%"); color <- "red"; icon_used <- icon("arrow-down")
    }
    valueBox(value, "📈 Evolution Annuelle TTC", icon = icon_used, color = color)
  })
  output$mgmt_kpi_transactions <- renderValueBox({
    k <- management_kpis()
    valueBox(format(k$transactions, big.mark = ","), "📋 Transactions", 
             icon = icon("receipt"), color = "orange")
  })
  output$mgmt_kpi_suppliers <- renderValueBox({
    k <- management_kpis()
    valueBox(k$suppliers, "🏢 Fournisseurs", icon = icon("truck"), color = "purple")
  })
  output$mgmt_kpi_projects <- renderValueBox({
    k <- management_kpis()
    valueBox(k$projects, "📌 Projets", icon = icon("building"), color = "yellow")
  })
  output$mgmt_kpi_quantity <- renderValueBox({
    k <- management_kpis()
    valueBox(format(round(k$quantity, 2), big.mark = ","), "📦 Quantite Totale", 
             icon = icon("boxes"), color = "aqua")
  })
  
  output$mgmt_month_year <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    month_data <- d %>%
      filter(!is.na(annee), !is.na(mois_num)) %>%
      group_by(annee, mois_num) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee, mois_num)
    
    p <- plot_ly(
      month_data, x = ~mois_num, y = ~TTC, color = ~factor(annee), colors = PALETTE,
      type = "scatter", mode = "lines+markers",
      hovertemplate = "Annee: %{fullData.name}<br>Mois: %{x}<br>TTC: %{y:,.2f} DA<extra></extra>"
    ) %>% layout(xaxis = list(title = "Mois", tickvals = 1:12, ticktext = MOIS_FR_SHORT),
                 yaxis = list(title = "Total TTC (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_project_share <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    proj <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TTC))
    
    p <- plot_ly(
      proj, labels = ~projet, values = ~TTC, type = "pie", hole = 0.55,
      marker = list(colors = PALETTE), textinfo = "label+percent",
      hovertemplate = "%{label}<br>%{value:,.2f} DA<extra></extra>"
    )
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_project_compare <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    compare <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(HT = sum(montant_total_ht, na.rm = TRUE), 
                TVA = sum(tva_19_percent, na.rm = TRUE),
                TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TTC))
    
    p <- plot_ly(compare, x = ~projet) %>%
      add_bars(y = ~HT, name = "HT", marker = list(color = "#0891b2")) %>%
      add_bars(y = ~TVA, name = "TVA", marker = list(color = "#f59e0b")) %>%
      add_bars(y = ~TTC, name = "TTC", marker = list(color = "#10b981")) %>%
      layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Montant (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_quarterly <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    quarterly <- d %>%
      filter(!is.na(annee), !is.na(trimestre)) %>%
      group_by(annee, trimestre) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(annee, trimestre) %>%
      mutate(Periode = paste0("T", trimestre, " ", annee))
    
    p <- plot_ly(
      quarterly, x = ~factor(Periode, levels = Periode), y = ~TTC, type = "bar",
      marker = list(color = "#8b5cf6"),
      hovertemplate = "%{x}<br>TTC: %{y:,.2f} DA<extra></extra>"
    ) %>% layout(xaxis = list(title = ""), yaxis = list(title = "Total TTC (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_supplier_top <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    topf <- d %>%
      filter(!is.na(fournisseur), fournisseur != "") %>%
      group_by(fournisseur) %>%
      summarise(TTC = sum(total_ttc, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TTC)) %>% slice_head(n = 10) %>% arrange(TTC)
    
    p <- plot_ly(
      topf, x = ~TTC, y = ~reorder(fournisseur, TTC), type = "bar", orientation = "h",
      marker = list(color = "#06b6d4"),
      hovertemplate = "%{y}<br>%{x:,.2f} DA<extra></extra>"
    ) %>% layout(xaxis = list(title = "Total TTC (DA)"), yaxis = list(title = ""))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_avg_month <- renderPlotly({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    avg_data <- d %>%
      filter(!is.na(annee), !is.na(mois_num)) %>%
      group_by(annee, mois_num) %>%
      summarise(TTC_moyen = mean(total_ttc, na.rm = TRUE), 
                Transactions = n(), .groups = "drop") %>%
      arrange(annee, mois_num) %>%
      mutate(Periode = paste0(MOIS_FR_SHORT[mois_num], " ", annee))
    
    p <- plot_ly(
      avg_data, x = ~factor(Periode, levels = Periode), y = ~TTC_moyen, type = "bar",
      marker = list(color = "#f472b6"),
      text = ~paste0(format(round(TTC_moyen, 2), big.mark = ","), " DA<br>", Transactions, " transactions"),
      hoverinfo = "text"
    ) %>% layout(xaxis = list(title = ""), yaxis = list(title = "Achat moyen TTC (DA)"))
    
    apply_plotly_theme(p, input$theme_toggle)
  })
  
  output$mgmt_project_table <- renderDT({
    d <- management_data()
    validate(need(nrow(d) > 0, "Aucune donnee disponible"))
    
    summary_tbl <- d %>%
      filter(!is.na(projet), projet != "") %>%
      group_by(projet) %>%
      summarise(
        `Total HT` = sum(montant_total_ht, na.rm = TRUE),
        `Total TVA` = sum(tva_19_percent, na.rm = TRUE),
        `Total TTC` = sum(total_ttc, na.rm = TRUE),
        Transactions = n(),
        Fournisseurs = n_distinct(fournisseur[!is.na(fournisseur) & fournisseur != ""]),
        Quantite = sum(quantite, na.rm = TRUE),
        .groups = "drop"
      ) %>% arrange(desc(`Total TTC`))
    
    datatable(summary_tbl, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
              class = "stripe hover nowrap") %>%
      formatCurrency(c("Total HT", "Total TVA", "Total TTC"), 
                     currency = "", interval = 3, mark = ",", digits = 2) %>%
      formatRound("Quantite", 2)
  })
  
  # ===========================================================================
  # 23. ADD PURCHASE
  # ===========================================================================
  output$apercu_calcul <- renderUI({
    q <- ifelse(is.na(input$new_quantite), 0, input$new_quantite)
    pu <- ifelse(is.na(input$new_prix_unitaire), 0, input$new_prix_unitaire)
    ht <- q * pu
    tva <- ht * 0.19
    ttc <- ht + tva
    
    tagList(
      div(
        style = "padding: 20px; background: #f8f9fa; border-radius: 8px;",
        h4("📋 Details du calcul", style = "margin-top: 0;"),
        p(strong("🔢 Quantite : "), format(q, big.mark = ",")),
        p(strong("💰 Prix Unitaire : "), format(round(pu, 2), big.mark = ",", decimal.mark = ","), " DA"),
        hr(),
        p(strong("💰 Montant HT : "), format(round(ht, 2), big.mark = ",", decimal.mark = ","), " DA",
          style = "font-size: 16px;"),
        p(strong("📊 TVA (19%) : "), format(round(tva, 2), big.mark = ",", decimal.mark = ","), " DA",
          style = "font-size: 16px; color: #f59e0b;"),
        p(strong("💎 Montant TTC : "), format(round(ttc, 2), big.mark = ",", decimal.mark = ","), " DA",
          style = "font-size: 20px; font-weight: bold; color: #10b981;")
      )
    )
  })
  
  observeEvent(input$btn_ajouter, {
    if (!can_edit()) {
      showNotification("❌ Vous n'avez pas la permission d'ajouter des achats", type = "error")
      return()
    }
    
    if (is.null(input$new_article) || trimws(input$new_article) == "") {
      showNotification("❌ Veuillez saisir l'article", type = "error"); return()
    }
    if (is.null(input$new_fournisseur) || trimws(input$new_fournisseur) == "") {
      showNotification("❌ Veuillez saisir le fournisseur", type = "error"); return()
    }
    if (is.null(input$new_projet) || input$new_projet == "") {
      showNotification("❌ Veuillez selectionner le projet", type = "error"); return()
    }
    
    q <- ifelse(is.na(input$new_quantite), 0, input$new_quantite)
    pu <- ifelse(is.na(input$new_prix_unitaire), 0, input$new_prix_unitaire)
    ht <- q * pu
    tva <- ht * 0.19
    ttc <- ht + tva
    d <- as.Date(input$new_date)
    
    new_row <- data.frame(
      date = d,
      article = trimws(as.character(input$new_article)),
      quantite = q,
      unite = trimws(as.character(input$new_unite)),
      prix_unitaire = pu,
      montant_total_ht = ht,
      tva_19_percent = tva,
      total_ttc = ttc,
      fournisseur = trimws(as.character(input$new_fournisseur)),
      projet = trimws(as.character(input$new_projet)),
      n_facture = NA_character_,
      num_cheque = NA_character_,
      annee = year(d),
      mois_num = month(d),
      mois = MOIS_FR[month(d)],
      trimestre = quarter(d),
      date_display = format(d, "%d/%m/%Y"),
      stringsAsFactors = FALSE
    )
    
    cur <- rv_data()
    
    for (col in setdiff(names(cur), names(new_row))) {
      if (is.numeric(cur[[col]])) {
        new_row[[col]] <- NA_real_
      } else {
        new_row[[col]] <- NA_character_
      }
    }
    new_row <- new_row[, names(cur), drop = FALSE]
    
    updated <- bind_rows(cur, new_row)
    rv_data(updated)
    tryCatch(saveRDS(updated, DATASET_FILE), error = function(e) NULL)
    
    log_audit(auth$username, "ajout_achat",
              paste(new_row$article, "-", new_row$fournisseur, "-", new_row$total_ttc, "DA"))
    
    showNotification("✅ Achat ajoute avec succes", type = "success")
    
    updateTextInput(session, "new_article", value = "")
    updateNumericInput(session, "new_quantite", value = 1)
    updateNumericInput(session, "new_prix_unitaire", value = 0)
    updateTextInput(session, "new_unite", value = "U")
    updateTextInput(session, "new_fournisseur", value = "")
  })
  
  # ===========================================================================
  # 24. EXPORTS
  # ===========================================================================
  safe_excel_export <- function(data, file) {
    tryCatch({
      export_excel_with_sheets(data, file)
    }, error = function(e) {
      showNotification(paste("❌ Erreur lors de l'export Excel :", conditionMessage(e)), 
                       type = "error", duration = 10)
      wb <- createWorkbook()
      addWorksheet(wb, "Erreur")
      writeData(wb, "Erreur", data.frame(Erreur = conditionMessage(e)))
      saveWorkbook(wb, file, overwrite = TRUE)
    })
  }
  
  output$export_data_excel <- downloadHandler(
    filename = function() paste0("Export_Data_Achats_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) safe_excel_export(filtered_data(), file)
  )
  
  output$export_data_pdf <- downloadHandler(
    filename = function() paste0("Export_Data_Achats_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
    content = function(file) create_pdf_report(filtered_data(), file, "Rapport des Achats - Donnees Filtrees")
  )
  
  output$export_ajouter_excel <- downloadHandler(
    filename = function() paste0("Export_Ajouter_Achats_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) safe_excel_export(rv_data(), file)
  )
  
  output$export_ajouter_pdf <- downloadHandler(
    filename = function() paste0("Export_Ajouter_Achats_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
    content = function(file) create_pdf_report(rv_data(), file, "Rapport des Achats - Toutes les donnees")
  )
  
  output$export_excel_2 <- downloadHandler(
    filename = function() paste0("Export_Achats_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) safe_excel_export(filtered_data(), file)
  )
  
  output$export_pdf_2 <- downloadHandler(
    filename = function() paste0("Rapport_Achats_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
    content = function(file) create_pdf_report(filtered_data(), file, "Rapport des Achats")
  )
  
  # ===========================================================================
  # 25. USER MANAGEMENT (admin only)
  # ===========================================================================
  output$users_access_denied <- renderUI({
    if (can_admin()) return(NULL)
    tags$div(style = "color:#dc2626; padding: 10px 0;", "⛔ Acces reserve aux administrateurs")
  })
  
  output$table_users <- renderDT({
    req(can_admin())
    u <- users_rv()
    datatable(u[, c("username", "role", "created_at")], 
              rownames = FALSE, 
              options = list(pageLength = 10, dom = "tip"))
  })
  
  output$edit_user_select <- renderUI({
    req(can_admin())
    selectInput("eu_username", "👤 Utilisateur", choices = users_rv()$username)
  })
  
  output$delete_user_select <- renderUI({
    req(can_admin())
    choices <- setdiff(users_rv()$username, auth$username)
    selectInput("du_username", "👤 Utilisateur", choices = choices)
  })
  
  output$table_audit <- renderDT({
    req(can_admin())
    a <- audit_rv()
    if (is.null(a) || nrow(a) == 0) {
      return(datatable(
        data.frame(Message = "Aucune activité enregistrée"),
        options = list(dom = "t", pageLength = 1)
      ))
    }
    a <- a %>% arrange(desc(horodatage))
    datatable(a, rownames = FALSE,
              options = list(pageLength = 15, dom = "lrtip", order = list(list(0, "desc"))))
  })
  
  observeEvent(input$btn_add_user, {
    req(can_admin())
    
    uname <- trimws(input$nu_username %||% "")
    pw <- input$nu_password %||% ""
    role <- input$nu_role
    
    if (uname == "" || pw == "") {
      showNotification("❌ Nom d'utilisateur et mot de passe requis", type = "error")
      return()
    }
    
    u <- users_rv()
    if (uname %in% u$username) {
      showNotification("❌ Ce nom d'utilisateur existe deja", type = "error")
      return()
    }
    
    new_user <- data.frame(
      username = uname,
      password_hash = hash_password(pw),
      role = role,
      created_at = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    u <- bind_rows(u, new_user)
    users_rv(u)
    save_users(u)
    
    log_audit(auth$username, "ajout_utilisateur", paste(uname, "-", role))
    showNotification(paste0("✅ Utilisateur '", uname, "' ajoute"), type = "success")
    
    updateTextInput(session, "nu_username", value = "")
    updateTextInput(session, "nu_password", value = "")
  })
  
  observeEvent(input$btn_change_role, {
    req(can_admin())
    uname <- input$eu_username
    new_role <- input$eu_role
    if (is.null(uname) || uname == "") return()
    
    u <- users_rv()
    u$role[u$username == uname] <- new_role
    users_rv(u)
    save_users(u)
    
    if (identical(uname, auth$username)) auth$role <- new_role
    
    log_audit(auth$username, "changement_role", paste(uname, "->", new_role))
    showNotification(paste0("✅ Role de '", uname, "' mis a jour: ", new_role), type = "success")
  })
  
  observeEvent(input$btn_change_password, {
    req(can_admin())
    uname <- input$eu_username
    pw <- input$eu_password %||% ""
    if (is.null(uname) || uname == "" || pw == "") {
      showNotification("❌ Veuillez saisir un nouveau mot de passe", type = "error")
      return()
    }
    
    u <- users_rv()
    u$password_hash[u$username == uname] <- hash_password(pw)
    users_rv(u)
    save_users(u)
    
    log_audit(auth$username, "changement_mot_de_passe", uname)
    showNotification(paste0("✅ Mot de passe de '", uname, "' mis a jour"), type = "success")
    updateTextInput(session, "eu_password", value = "")
  })
  
  observeEvent(input$btn_delete_user, {
    req(can_admin())
    uname <- input$du_username
    if (is.null(uname) || uname == "") return()
    if (identical(uname, auth$username)) {
      showNotification("❌ Vous ne pouvez pas supprimer votre propre compte", type = "error")
      return()
    }
    
    u <- users_rv()
    u <- u[u$username != uname, , drop = FALSE]
    users_rv(u)
    save_users(u)
    
    log_audit(auth$username, "suppression_utilisateur", uname)
    showNotification(paste0("✅ Utilisateur '", uname, "' supprime"), type = "success")
  })
  
  # ===========================================================================
  # 26. REGISTRATION REQUESTS (admin only)
  # ===========================================================================
  
  output$table_requests <- renderDT({
    req(can_admin())
    
    req_data <- get_pending_requests()
    
    if (nrow(req_data) == 0) {
      return(datatable(
        data.frame(Message = "📭 Aucune demande en attente"),
        options = list(dom = "t", pageLength = 1, columnDefs = list(list(className = 'dt-center', targets = 0))),
        rownames = FALSE
      ))
    }
    
    # Add action buttons
    req_data$Actions <- paste0(
      '<div class="btn-group">',
      '<button class="btn btn-success btn-sm approve-btn" data-id="', req_data$id, '" data-role="tester">✅ Tester</button>',
      '<button class="btn btn-info btn-sm approve-btn" data-id="', req_data$id, '" data-role="viewer">👁️ Viewer</button>',
      '<button class="btn btn-danger btn-sm reject-btn" data-id="', req_data$id, '">❌ Rejeter</button>',
      '</div>'
    )
    
    display_data <- req_data %>%
      select(id, username, role_requested, requested_at, Actions)
    
    datatable(
      display_data,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = "lrtip",
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(className = 'dt-center', targets = 4)
        )
      )
    )
  })
  
  output$table_requests_history <- renderDT({
    req(can_admin())
    
    history <- get_request_history()
    
    if (nrow(history) == 0) {
      return(datatable(
        data.frame(Message = "📭 Aucun historique"),
        options = list(dom = "t", pageLength = 1),
        rownames = FALSE
      ))
    }
    
    history <- history %>%
      select(username, role_requested, status, approved_by, requested_at, approved_at)
    
    datatable(
      history,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "lrtip"
      )
    )
  })
  
  # Handle approve request
  observeEvent(input$approve_request, {
    req(can_admin())
    
    result <- approve_request(
      input$approve_request$id,
      auth$username,
      input$approve_request$role
    )
    
    if (result$success) {
      showNotification(result$message, type = "success")
      
      # Update users list
      users_rv(load_users())
      
      # Update pending badge
      pending_count <- get_pending_count()
      session$sendCustomMessage("update_badge", pending_count)
      
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Handle reject request
  observeEvent(input$reject_request, {
    req(can_admin())
    
    result <- reject_request(input$reject_request, auth$username)
    
    if (result$success) {
      showNotification(result$message, type = "warning")
      
      # Update pending badge
      pending_count <- get_pending_count()
      session$sendCustomMessage("update_badge", pending_count)
      
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Refresh requests
  observeEvent(input$refresh_requests, {
    # Just refresh the table
    output$table_requests <- renderDT({
      req(can_admin())
      
      req_data <- get_pending_requests()
      
      if (nrow(req_data) == 0) {
        return(datatable(
          data.frame(Message = "📭 Aucune demande en attente"),
          options = list(dom = "t", pageLength = 1),
          rownames = FALSE
        ))
      }
      
      req_data$Actions <- paste0(
        '<div class="btn-group">',
        '<button class="btn btn-success btn-sm approve-btn" data-id="', req_data$id, '" data-role="tester">✅ Tester</button>',
        '<button class="btn btn-info btn-sm approve-btn" data-id="', req_data$id, '" data-role="viewer">👁️ Viewer</button>',
        '<button class="btn btn-danger btn-sm reject-btn" data-id="', req_data$id, '">❌ Rejeter</button>',
        '</div>'
      )
      
      display_data <- req_data %>%
        select(id, username, role_requested, requested_at, Actions)
      
      datatable(
        display_data,
        rownames = FALSE,
        escape = FALSE,
        options = list(
          pageLength = 10,
          dom = "lrtip",
          columnDefs = list(
            list(targets = 0, visible = FALSE),
            list(className = 'dt-center', targets = 4)
          )
        )
      )
    })
    
    # Update badge
    pending_count <- get_pending_count()
    session$sendCustomMessage("update_badge", pending_count)
    
    showNotification("🔄 Demandes rafraîchies", type = "message")
  })
}