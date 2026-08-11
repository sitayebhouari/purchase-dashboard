# =============================================================================
# ui.R - Purchase Management Dashboard
# Version: 3.1.0 - With Registration Requests
# =============================================================================

ui <- dashboardPage(
  
  # ===========================================================================
  # HEADER
  # ===========================================================================
  dashboardHeader(
    title = tags$span("📊 Dashboard des Achats", style = "font-weight: bold; font-size: 18px;"),
    titleWidth = 280,
    tags$li(
      class = "dropdown",
      uiOutput("header_user_info")
    )
  ),
  
  # ===========================================================================
  # SIDEBAR
  # ===========================================================================
  dashboardSidebar(
    width = 280,
    
    sidebarMenu(
      id = "sidebar_menu",
      
      # Theme toggle
      div(
        style = "padding: 15px 15px 5px 15px;",
        awesomeRadio(
          inputId = "theme_toggle",
          label = "🎨 Thème",
          choices = c("☀️ Light Mode" = "Light", "🌙 Dark Mode" = "Dark"),
          selected = "Light",
          inline = TRUE,
          checkbox = FALSE
        )
      ),
      
      tags$hr(),
      
      # Filters (visible when logged in)
      div(
        id = "sidebar_filters",
        style = "padding: 5px 15px;",
        h5("🔍 Filtres", style = "font-weight: bold;"),
        
        pickerInput(
          inputId = "f_projet",
          label = "📌 Projet",
          choices = c("Tous" = "all", projets_liste),
          selected = "all",
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Tous les projets"
          ),
          multiple = FALSE
        ),
        
        pickerInput(
          inputId = "f_fournisseur",
          label = "🏢 Fournisseur",
          choices = c("Tous" = "all", fournisseurs_liste),
          selected = "all",
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Tous les fournisseurs"
          ),
          multiple = FALSE
        ),
        
        pickerInput(
          inputId = "f_annee",
          label = "📅 Année",
          choices = c("Toutes" = "all", annees_liste),
          selected = "all",
          options = list(
            `actions-box` = TRUE,
            `none-selected-text` = "Toutes les années"
          ),
          multiple = FALSE
        ),
        
        pickerInput(
          inputId = "f_mois",
          label = "📆 Mois",
          choices = c("Tous" = "all", MOIS_FR),
          selected = "all",
          options = list(
            `actions-box` = TRUE,
            `none-selected-text` = "Tous les mois"
          ),
          multiple = FALSE
        ),
        
        br(),
        
        actionButton(
          inputId = "reset_filters",
          label = "🔄 Réinitialiser",
          icon = icon("undo"),
          style = "width: 100%; background: #0891b2; color: white; font-weight: bold;"
        )
      ),
      
      tags$hr(id = "sidebar_hr_file"),
      
      # File management (admin + tester only)
      div(
        id = "sidebar_file",
        style = "padding: 5px 15px;",
        h5("📁 Fichier", style = "font-weight: bold;"),
        
        actionButton(
          inputId = "use_latest",
          label = "📂 Dernier fichier",
          icon = icon("sync"),
          style = "width: 100%; margin-bottom: 5px;"
        ),
        
        actionButton(
          inputId = "browse_file",
          label = "📂 Choisir un fichier",
          icon = icon("folder-open"),
          style = "width: 100%; margin-bottom: 5px;"
        ),
        
        uiOutput("current_file")
      ),
      
      tags$hr(),
      
      # Dynamic menu (depends on role)
      sidebarMenuOutput("dynamic_menu"),
      
      # Register link (visible when not logged in)
      div(
        id = "register_link",
        style = "padding: 15px; text-align: center;",
        actionLink(
          inputId = "go_to_register",
          label = "📝 Pas encore de compte ? S'inscrire",
          style = "color: #0891b2; font-weight: bold;"
        )
      )
    )
  ),
  
  # ===========================================================================
  # BODY
  # ===========================================================================
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        body { font-family: 'Segoe UI', Arial, sans-serif; }
        .content-wrapper { background-color: #f8fafc; }
        .box { border-radius: 10px; border: none; box-shadow: 0 2px 10px rgba(0,0,0,0.08); }
        .small-box { border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.08); }
        .small-box .inner h3 { font-weight: bold; font-size: 28px; }
        .nav-tabs-custom { border-radius: 10px; }

        #login_screen {
          max-width: 380px;
          margin: 80px auto;
          background: white;
          padding: 35px 30px;
          border-radius: 12px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.12);
        }

        #register_screen {
          max-width: 500px;
          margin: 40px auto;
        }

        .dark-mode .content-wrapper, .dark-mode .right-side { background-color: #111827 !important; }
        .dark-mode .box { background-color: #1f2937 !important; color: white !important; }
        .dark-mode .box-header { color: white !important; }
        .dark-mode .box-title { color: white !important; }
        .dark-mode .form-control, .dark-mode .selectize-input {
          background-color: #374151 !important; color: white !important; border-color: #4b5563 !important;
        }
        .dark-mode .main-sidebar { background-color: #111827 !important; }
        .dark-mode .main-header .logo { background-color: #111827 !important; }
        .dark-mode .main-header .navbar { background-color: #1f2937 !important; }
        .dark-mode .dataTables_wrapper { color: white !important; }
        .dark-mode .dataTable { color: white !important; }
        .dark-mode .dataTable th { background-color: #1f2937 !important; color: white !important; }
        .dark-mode .dataTable td { background-color: #1f2937 !important; color: white !important; }
        .dark-mode .dataTable tbody tr:hover { background-color: #374151 !important; }
        .dark-mode .bootstrap-select .dropdown-toggle {
          background-color: #374151 !important; border-color: #4b5563 !important; color: white !important;
        }
        .dark-mode .dropdown-menu { background-color: #374151 !important; }
        .dark-mode .dropdown-menu li a { color: white !important; }
        .dark-mode .dropdown-menu li a:hover { background-color: #1f2937 !important; }
        .dark-mode .nav-tabs-custom { background-color: #1f2937 !important; }
        .dark-mode .nav-tabs-custom .nav-tabs li a { color: white !important; }
        .dark-mode .nav-tabs-custom .nav-tabs li.active a {
          background-color: #1f2937 !important; border-top-color: #0891b2 !important; color: white !important;
        }
        .dark-mode .nav-tabs-custom .tab-content { background-color: #1f2937 !important; color: white !important; }
        
        .badge-pending {
          background-color: #dc3545;
          color: white;
          border-radius: 50%;
          padding: 2px 8px;
          font-size: 12px;
          margin-left: 5px;
        }
      ")),
      
      # JavaScript for handling buttons
      tags$script(HTML("
        $(document).on('click', '.approve-btn', function() {
          var id = $(this).data('id');
          var role = $(this).data('role');
          Shiny.setInputValue('approve_request', {id: id, role: role});
        });
        
        $(document).on('click', '.reject-btn', function() {
          var id = $(this).data('id');
          Shiny.setInputValue('reject_request', id);
        });
        
        Shiny.addCustomMessageHandler('update_badge', function(count) {
          var badge = $('.pending-badge');
          if (count > 0) {
            if (badge.length) {
              badge.text(count);
            } else {
              $('a[data-tab-name=\"requests\"]').append(' <span class=\"badge-pending pending-badge\">' + count + '</span>');
            }
          } else {
            badge.remove();
          }
        });
      "))
    ),
    
    # =========================================================================
    # LOGIN SCREEN
    # =========================================================================
    div(
      id = "login_screen",
      h3("🔐 Connexion", style = "text-align:center; margin-bottom: 20px;"),
      textInput("login_username", "👤 Nom d'utilisateur", placeholder = "Nom d'utilisateur"),
      passwordInput("login_password", "🔑 Mot de passe", placeholder = "Mot de passe"),
      uiOutput("login_error"),
      actionButton("login_btn", "Se connecter", class = "btn-primary",
                   style = "width: 100%; margin-top: 10px;"),
      br(), br(),
      div(
        style = "text-align: center;",
        actionLink("go_to_register_from_login", "📝 Pas encore de compte ? S'inscrire")
      )
    ),
    
    # =========================================================================
    # REGISTER SCREEN (public)
    # =========================================================================
    shinyjs::hidden(
      div(
        id = "register_screen",
        style = "max-width: 500px; margin: 40px auto;",
        h3("📝 Demander un compte", style = "text-align:center; margin-bottom: 20px;"),
        div(
          style = "background: white; padding: 30px; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.12);",
          p("Remplissez ce formulaire pour demander un compte. Un administrateur examinera votre demande.", 
            style = "color: #64748b;"),
          br(),
          textInput("reg_username", "👤 Nom d'utilisateur", 
                    placeholder = "Choisissez un nom d'utilisateur (min 3 caractères)"),
          passwordInput("reg_password", "🔑 Mot de passe", 
                        placeholder = "Choisissez un mot de passe (min 4 caractères)"),
          passwordInput("reg_password_confirm", "✅ Confirmer le mot de passe"),
          selectInput("reg_role", "📌 Rôle souhaité",
                      choices = c("Tester (peut ajouter des achats)" = "tester", 
                                  "Viewer (consultation uniquement)" = "viewer"),
                      selected = "viewer"),
          br(),
          uiOutput("register_message"),
          actionButton("btn_register", "📤 Envoyer la demande", 
                       class = "btn-success",
                       style = "width: 100%; font-size: 16px; padding: 10px;"),
          br(), br(),
          div(
            style = "text-align: center;",
            actionLink("go_to_login_from_register", "🔐 Déjà un compte ? Se connecter")
          )
        )
      )
    ),
    
    # =========================================================================
    # MAIN APP (hidden until logged in)
    # =========================================================================
    shinyjs::hidden(
      div(
        id = "main_app",
        
        tabItems(
          
          # =====================================================================
          # TAB 1: DASHBOARD
          # =====================================================================
          tabItem(
            tabName = "dashboard",
            fluidRow(
              valueBoxOutput("kpi_ht_dash", width = 3),
              valueBoxOutput("kpi_tva_dash", width = 3),
              valueBoxOutput("kpi_ttc_dash", width = 3),
              valueBoxOutput("kpi_four_dash", width = 3)
            ),
            fluidRow(
              valueBoxOutput("total_achats", width = 3),
              valueBoxOutput("nb_transactions", width = 3),
              valueBoxOutput("nb_fournisseurs", width = 3)
            ),
            fluidRow(
              box(title = "📈 Evolution des Achats par Mois", status = "primary", solidHeader = TRUE,
                  width = 8, height = "450px", plotlyOutput("plot_evolution", height = "400px")),
              box(title = "🍩 Repartition par Projet", status = "success", solidHeader = TRUE,
                  width = 4, height = "450px", plotlyOutput("plot_projet_donut", height = "400px"))
            ),
            fluidRow(
              box(title = "🏆 Top 10 Fournisseurs", status = "info", solidHeader = TRUE,
                  width = 12, height = "500px", plotlyOutput("plot_fournisseurs", height = "450px"))
            )
          ),
          
          # =====================================================================
          # TAB 2: MANAGEMENT
          # =====================================================================
          tabItem(
            tabName = "management",
            fluidRow(
              valueBoxOutput("mgmt_kpi_ttc", width = 3),
              valueBoxOutput("mgmt_kpi_ht", width = 3),
              valueBoxOutput("mgmt_kpi_avg", width = 3),
              valueBoxOutput("mgmt_kpi_growth", width = 3)
            ),
            fluidRow(
              valueBoxOutput("mgmt_kpi_transactions", width = 3),
              valueBoxOutput("mgmt_kpi_suppliers", width = 3),
              valueBoxOutput("mgmt_kpi_projects", width = 3),
              valueBoxOutput("mgmt_kpi_quantity", width = 3)
            ),
            fluidRow(
              box(title = "📊 Evolution Mensuelle par Annee", status = "primary", solidHeader = TRUE,
                  width = 8, height = "450px", plotlyOutput("mgmt_month_year", height = "400px")),
              box(title = "🍩 Part des Projets", status = "success", solidHeader = TRUE,
                  width = 4, height = "450px", plotlyOutput("mgmt_project_share", height = "400px"))
            ),
            fluidRow(
              box(title = "📊 Comparaison des Projets", status = "info", solidHeader = TRUE,
                  width = 8, height = "450px", plotlyOutput("mgmt_project_compare", height = "400px")),
              box(title = "📅 Analyse Trimestrielle", status = "warning", solidHeader = TRUE,
                  width = 4, height = "450px", plotlyOutput("mgmt_quarterly", height = "400px"))
            ),
            fluidRow(
              box(title = "🏆 Top 10 Fournisseurs", status = "primary", solidHeader = TRUE,
                  width = 7, height = "500px", plotlyOutput("mgmt_supplier_top", height = "450px")),
              box(title = "📊 Achat Moyen par Mois", status = "success", solidHeader = TRUE,
                  width = 5, height = "500px", plotlyOutput("mgmt_avg_month", height = "450px"))
            ),
            fluidRow(
              box(title = "📋 Resume par Projet", status = "info", solidHeader = TRUE,
                  width = 12, DTOutput("mgmt_project_table"))
            )
          ),
          
          # =====================================================================
          # TAB 3: DATA
          # =====================================================================
          tabItem(
            tabName = "data",
            fluidRow(
              valueBoxOutput("kpi_ht_data", width = 3),
              valueBoxOutput("kpi_tva_data", width = 3),
              valueBoxOutput("kpi_ttc_data", width = 3),
              valueBoxOutput("kpi_four_data", width = 3)
            ),
            fluidRow(
              box(
                title = "📋 Donnees Filtrees", status = "primary", solidHeader = TRUE, width = 12,
                div(
                  style = "margin-bottom: 15px;",
                  downloadButton("export_data_excel", "📥 Exporter Excel", class = "btn-success",
                                 style = "margin-right: 10px;"),
                  downloadButton("export_data_pdf", "📄 Exporter PDF", class = "btn-danger")
                ),
                DTOutput("table_data")
              )
            ),
            fluidRow(
              box(title = "🏢 Statistiques Fournisseurs", status = "info", solidHeader = TRUE,
                  width = 12, DTOutput("table_fournisseurs"))
            )
          ),
          
          # =====================================================================
          # TAB 4: ADD (admin + tester uniquement)
          # =====================================================================
          tabItem(
            tabName = "ajouter",
            fluidRow(
              box(
                title = "➕ Ajouter un Achat", status = "primary", solidHeader = TRUE, width = 6,
                dateInput("new_date", "📅 Date", value = Sys.Date(), format = "dd/mm/yyyy"),
                textInput("new_article", "📦 Article", placeholder = "Entrez le nom de l'article"),
                fluidRow(
                  column(width = 6, numericInput("new_quantite", "🔢 Quantite", value = 1, min = 0, step = 1)),
                  column(width = 6, textInput("new_unite", "📏 Unite", value = "U", placeholder = "ex: U, kg, m..."))
                ),
                numericInput("new_prix_unitaire", "💰 Prix Unitaire", value = 0, min = 0, step = 0.01),
                textInput("new_fournisseur", "🏢 Fournisseur", placeholder = "Entrez le nom du fournisseur"),
                selectInput("new_projet", "📌 Projet", choices = projets_liste,
                            selected = ifelse(length(projets_liste) > 0, projets_liste[1], "")),
                br(),
                actionButton("btn_ajouter", "✅ Ajouter l'achat", icon = icon("plus"), class = "btn-success",
                             style = "width: 100%; font-size: 16px; padding: 10px;")
              ),
              box(
                title = "💰 Apercu du Calcul", status = "success", solidHeader = TRUE, width = 6,
                uiOutput("apercu_calcul")
              )
            ),
            fluidRow(
              box(
                title = "📤 Exporter les Donnees", status = "info", solidHeader = TRUE, width = 12,
                downloadButton("export_ajouter_excel", "📥 Exporter Excel Complet", class = "btn-success",
                               style = "margin-right: 10px;"),
                downloadButton("export_ajouter_pdf", "📄 Exporter PDF", class = "btn-danger")
              )
            )
          ),
          
          # =====================================================================
          # TAB 5: REPORTS
          # =====================================================================
          tabItem(
            tabName = "reports",
            fluidRow(
              box(
                title = "📊 Rapport Excel", status = "success", solidHeader = TRUE, width = 6,
                p("Exportez toutes les donnees avec analyses detaillees:"),
                tags$ul(
                  tags$li("Donnees globales"), tags$li("Donnees par projet"),
                  tags$li("Resume des indicateurs"), tags$li("Statistiques fournisseurs"),
                  tags$li("Statistiques projets"), tags$li("Erreurs de validation")
                ),
                br(),
                downloadButton("export_excel_2", "📥 Telecharger Excel", class = "btn-success",
                               style = "width: 100%; font-size: 16px; padding: 10px;")
              ),
              box(
                title = "📄 Rapport PDF", status = "danger", solidHeader = TRUE, width = 6,
                p("Generez un rapport PDF professionnel:"),
                tags$ul(
                  tags$li("Resume des indicateurs"), tags$li("Evolution mensuelle"),
                  tags$li("Repartition des projets"), tags$li("Top fournisseurs"),
                  tags$li("Detail des transactions")
                ),
                br(),
                downloadButton("export_pdf_2", "📥 Telecharger PDF", class = "btn-danger",
                               style = "width: 100%; font-size: 16px; padding: 10px;")
              )
            )
          ),
          
          # =====================================================================
          # TAB 6: USERS (admin uniquement)
          # =====================================================================
          tabItem(
            tabName = "users",
            fluidRow(
              box(
                title = "👥 Utilisateurs", status = "primary", solidHeader = TRUE, width = 12,
                uiOutput("users_access_denied"),
                DTOutput("table_users")
              )
            ),
            fluidRow(
              box(
                title = "➕ Ajouter un utilisateur", status = "success", solidHeader = TRUE, width = 4,
                textInput("nu_username", "👤 Nom d'utilisateur"),
                passwordInput("nu_password", "🔑 Mot de passe"),
                selectInput("nu_role", "📌 Role", choices = ROLES, selected = "viewer"),
                actionButton("btn_add_user", "✅ Ajouter", icon = icon("user-plus"), class = "btn-success",
                             style = "width: 100%;")
              ),
              box(
                title = "✏️ Modifier un utilisateur", status = "warning", solidHeader = TRUE, width = 4,
                uiOutput("edit_user_select"),
                selectInput("eu_role", "📌 Nouveau role", choices = ROLES),
                actionButton("btn_change_role", "🔄 Changer le role", icon = icon("user-shield"),
                             style = "width: 100%; margin-bottom: 8px;"),
                passwordInput("eu_password", "🔑 Nouveau mot de passe"),
                actionButton("btn_change_password", "🔑 Changer le mot de passe", icon = icon("key"),
                             style = "width: 100%;")
              ),
              box(
                title = "🗑️ Supprimer un utilisateur", status = "danger", solidHeader = TRUE, width = 4,
                uiOutput("delete_user_select"),
                actionButton("btn_delete_user", "🗑️ Supprimer", icon = icon("user-times"), class = "btn-danger",
                             style = "width: 100%;")
              )
            ),
            fluidRow(
              box(
                title = "📋 Journal d'audit", status = "info", solidHeader = TRUE, width = 12,
                DTOutput("table_audit")
              )
            )
          ),
          
          # =====================================================================
          # TAB 7: REQUESTS (admin uniquement)
          # =====================================================================
          tabItem(
            tabName = "requests",
            fluidRow(
              box(
                title = "📋 Demandes d'inscription en attente",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                p("Voici la liste des personnes qui ont demandé un compte. Cliquez sur un bouton pour approuver ou rejeter la demande.",
                  style = "color: #64748b;"),
                br(),
                DTOutput("table_requests"),
                br(),
                div(
                  style = "text-align: center;",
                  actionButton("refresh_requests", "🔄 Rafraîchir", icon = icon("sync"),
                               class = "btn-info")
                )
              )
            ),
            fluidRow(
              box(
                title = "📜 Historique des demandes",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                DTOutput("table_requests_history")
              )
            )
          )
        )
      )
    )
  )
)