# Include packages
for (p in c("tidyverse", "shiny", "DT", "DiagrammeR", "here", "bslib")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Source Througput for model

source("Depmod/Throughput.R")



trimbos_theme <- bs_theme(
  version   = 5,
  bootswatch = "flatly",
  primary   = "#E11D2E",  # brand red
  secondary = "#0057B8",
  success   = "#16A34A",
  info      = "#0EA5E9",
  warning   = "#F59E0B",
  danger    = "#DC2626",
  light     = "#F8F9FB",
  dark      = "#0F172A",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font    = "ui-monospace",
  "border-radius"       = "0.6rem",
  "btn-border-radius"   = "0.6rem",
  "card-border-radius"  = "0.8rem"
) |>
  bs_add_rules("
    /* Cards */
    .card { border:1px solid var(--bs-border-color);
            border-radius:.8rem;
            box-shadow:0 1px 3px rgba(0,0,0,.06), 0 1px 2px rgba(0,0,0,.04); }
    .card-header { background:var(--bs-light); font-weight:600; letter-spacing:.2px; }

    /* Sidebar title pill */
    .sidebar h4 { background:var(--bs-primary); color:#fff; padding:.6rem .8rem;
                  border-radius:.5rem; margin:0 0 .75rem 0; font-size:1.05rem; }

    /* Inputs */
    .form-control, .form-select { border:1px solid var(--bs-border-color); border-radius:.6rem; }
    .form-control:focus, .form-select:focus {
      border-color:var(--bs-secondary); box-shadow:0 0 0 .25rem rgba(0,87,184,.15); }

    /* Tabs (inner) */
    .nav-tabs .nav-link { color:var(--bs-gray-600); font-weight:500; }
    .nav-tabs .nav-link.active {
      color:var(--bs-dark);
      border-color: var(--bs-border-color) var(--bs-border-color) transparent;
      background: var(--bs-light);
    }

    /* Mermaid labels */
    .mermaid .edgeLabel { background:rgba(255,255,255,.9); padding:2px 6px; border-radius:6px; font-size:.8rem; }

    /* Spacing helpers */
    .g-3 > [class^='col-'] { margin-bottom:1rem; } /* extra gap between stacked cols */
  ")

# ---------- UI ----------
ui <- page_navbar(
  title = tags$span(
    # drop a logo.png/svg into www/ and uncomment:
    tags$img(src = "Trimbos-logo.jpg", class = "me-2", style = "height:50px"),
    "DepMod"
  ),
  theme = trimbos_theme,
  
  # LEFT SIDEBAR
  sidebar = sidebar(
    open = TRUE, width = 320,
    h4("Settings"),
    card(
      card_body(
        numericInput("sim", "Simulations", 1000, 0, Inf, 1),
        numericInput("n_pat", "Target Population (n)", 10518000, min = 1, step = 1),
        numericInput("wtp_per_qaly", "WTP per QALY (€)", value = 50000, min = 0, step = 1000),
        hr(),
        actionButton("run_model", "Run model", class = "btn btn-primary w-100")
      )
    )
  ),
  
  # =================== TOP-LEVEL TABS ===================
  nav_panel("Input",
            # two-column layout: forms (left) and content (right)
            fluidRow(class = "g-3",
                     column(
                       width = 4,
                       card(
                         card_header("Case"),
                         card_body(
                           radioButtons("which_case", NULL, c("Base" = "base", "Alternative" = "alt"), inline = TRUE)
                         )
                       ),
                       card(
                         card_header("Quick actions"),
                         card_body(
                           # put any global input helpers here if you like
                           tags$small(class = "text-muted", "Use the tabs on the right to manage interventions.")
                         )
                       )
                     ),
                     column(
                       width = 8,
                       card(
                         card_header("Configure interventions"),
                         card_body(
                           navset_tab(id = "input_tabs",
                                      
                                      nav_panel("Prevention: Sub-clinical",
                                                br(),
                                                conditionalPanel("input.which_case == 'base'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("prev_sub_add_base", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("prev_sub_remove_base", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("prev_sub_first_base"), hr(),
                                                                 uiOutput("prev_sub_more_base")
                                                ),
                                                conditionalPanel("input.which_case == 'alt'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("prev_sub_add_alt", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("prev_sub_remove_alt", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("prev_sub_first_alt"), hr(),
                                                                 uiOutput("prev_sub_more_alt")
                                                )
                                      ),
                                      
                                      nav_panel("Treatment: Mild",
                                                br(),
                                                conditionalPanel("input.which_case == 'base'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("treat_mild_add_base", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("treat_mild_remove_base", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("treat_mild_first_base"),  hr(),
                                                                 uiOutput("treat_mild_second_base"), hr(),
                                                                 uiOutput("treat_mild_more_base")
                                                ),
                                                conditionalPanel("input.which_case == 'alt'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("treat_mild_add_alt", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("treat_mild_remove_alt", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("treat_mild_first_alt"),   hr(),
                                                                 uiOutput("treat_mild_second_alt"),  hr(),
                                                                 uiOutput("treat_mild_more_alt")
                                                )
                                      ),
                                      
                                      nav_panel("Treatment: Moderate",
                                                br(),
                                                conditionalPanel("input.which_case == 'base'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("treat_mod_add_base", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("treat_mod_remove_base", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("treat_mod_first_base"),  hr(),
                                                                 uiOutput("treat_mod_second_base"), hr(),
                                                                 uiOutput("treat_mod_more_base")
                                                ),
                                                conditionalPanel("input.which_case == 'alt'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("treat_mod_add_alt", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("treat_mod_remove_alt", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("treat_mod_first_alt"),   hr(),
                                                                 uiOutput("treat_mod_second_alt"),  hr(),
                                                                 uiOutput("treat_mod_more_alt")
                                                )
                                      ),
                                      
                                      nav_panel("Treatment: Severe",
                                                br(),
                                                conditionalPanel("input.which_case == 'base'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("treat_sev_add_base", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("treat_sev_remove_base", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("treat_sev_first_base"), hr(),
                                                                 uiOutput("treat_sev_second_base"), hr(),
                                                                 uiOutput("treat_sev_third_base"), hr(),
                                                                 uiOutput("treat_sev_more_base")
                                                ),
                                                conditionalPanel("input.which_case == 'alt'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("treat_sev_add_alt", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("treat_sev_remove_alt", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("treat_sev_first_alt"), hr(),
                                                                 uiOutput("treat_sev_second_alt"), hr(),
                                                                 uiOutput("treat_sev_third_alt"), hr(),
                                                                 uiOutput("treat_sev_more_alt")
                                                )
                                      ),
                                      
                                      nav_panel("Prevention: Recurrent",
                                                br(),
                                                conditionalPanel("input.which_case == 'base'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("prev_rec_add_base", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("prev_rec_remove_base", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("prev_rec_first_base"),  hr(),
                                                                 uiOutput("prev_rec_second_base"), hr(),
                                                                 uiOutput("prev_rec_third_base"),  hr(),
                                                                 uiOutput("prev_rec_more_base")
                                                ),
                                                conditionalPanel("input.which_case == 'alt'",
                                                                 div(class="d-flex gap-2 flex-wrap",
                                                                     actionButton("prev_rec_add_alt", "Add", class="btn btn-outline-primary"),
                                                                     actionButton("prev_rec_remove_alt", "Remove", class="btn btn-outline-secondary")),
                                                                 hr(),
                                                                 uiOutput("prev_rec_first_alt"),   hr(),
                                                                 uiOutput("prev_rec_second_alt"),  hr(),
                                                                 uiOutput("prev_rec_third_alt"),   hr(),
                                                                 uiOutput("prev_rec_more_alt")
                                                )
                                      )
                                      
                           ) # /navset_tab
                         )    # /card_body
                       )      # /card
                     )        # /column right
            )          # /row
  ),
  
  nav_panel("Parameters",
            # two responsive columns of parameter cards
            fluidRow(class = "g-3",
                     column(6,
                            card(
                              card_header("General"),
                              card_body(
                                numericInput("exces_mort", "Excess mortality", 1.65, 0, Inf, 0.01),
                                numericInput("retire_rate", "Retirement rate", 0.02128, 0, Inf, 0.00001),
                                numericInput("death_rate", "Death rate", 0.00198, 0, Inf, 0.00001),
                                numericInput("mean_duration_chron", "Mean duration of chronicity (years)", 3.96, 0, Inf, 0.01)
                              )
                            ),
                            card(
                              card_header("Discounting"),
                              card_body(
                                numericInput("discount_rate_daly_av", "Discount rate DALY averted", 0.015, 0, Inf, 0.01),
                                numericInput("discount_rate_cost", "Discount rate costs", 0.04, 0, Inf, 0.01)
                              )
                            )
                     ),
                     column(6,
                            card(
                              card_header("Relapse factors"),
                              card_body(
                                numericInput("incr_relapse_1", "Increased relapse 1", 0.50, 0, Inf, 0.01),
                                numericInput("incr_relapse_2", "Increased relapse 2", 0.70, 0, Inf, 0.01),
                                numericInput("incr_relapse_3", "Increased relapse 3", 0.90, 0, Inf, 0.01),
                                numericInput("incr_relapse_4", "Increased relapse 4", 0.95, 0, Inf, 0.01),
                                numericInput("incr_relapse_5", "Increased relapse 5", 0.99, 0, Inf, 0.01)
                              )
                            ),
                            card(
                              card_header("DW conversion"),
                              card_body(
                                numericInput("dw_conversion_fct", "DW conversion factor", 0.172),
                                numericInput("range_dw_converion_fct_low", "Range DW factor (low)", 0.154, 0, Inf, 0.001),
                                numericInput("range_dw_conversion_fct_high", "Range DW factor (high)", 0.190, 0, Inf, 0.001),
                                numericInput("scale_shape_gamma", "Scale/shape Gamma cost distribution", 6, 0, Inf, 1)
                              )
                            )
                     )
            )
  ),
  
  nav_panel("Transition tree",
            card(
              full_screen = TRUE,
              card_header(
                div(class="d-flex align-items-center justify-content-between flex-wrap gap-2",
                    span("Transition tree"),
                    div(class="d-flex gap-2",
                        actionButton("edit_tt", "Edit probabilities", class = "btn btn-outline-primary"),
                        downloadButton("download_tree", "Download", class = "btn btn-outline-secondary")
                    )
                )
              ),
              # Fillable body lets the widget expand nicely
              card_body(
                DiagrammeR::DiagrammeROutput("transition_tree", height = "650px")
              )
            )
  )
)

  


server <- function(input, output, session) {
  
  # Base case
  
  # Get reactive values for the different counts
  rv_base <- reactiveValues(
    prev_sub_count_base = 1,
    mild_count_base     = 2,
    mod_count_base      = 2,
    sev_count_base      = 3,
    prev_rec_count_base = 3
  )
  
  # Prevention: Sub-clinical depression
  
  observeEvent(input$prev_sub_add_base, {
    rv_base$prev_sub_count_base <- rv_base$prev_sub_count_base + 1
  })
  
  observeEvent(input$prev_sub_remove_base, {
    rv_base$prev_sub_count_base <- max(0, rv_base$prev_sub_count_base - 1)
  })
  
  # Default intervention
  output$prev_sub_first_base <- renderUI({
    if (rv_base$prev_sub_count_base < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_sub_cov_base_1", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_sub_adh_base_1", "Adherence (%)",56,  0, 100, 0.1),
      numericInput("prev_sub_1_RR_base_1", "1 - RR (%)", 21,  0, 100, 0.1),
      numericInput("prev_sub_n_base_1", "n", 30,  0, Inf, 1),
      numericInput("prev_sub_hc_cost_base_1","Healthcare costs (€)",  160,  0, Inf, 1),
      numericInput("prev_sub_soc_cost_base_1", "Societal costs (€)",    297,  0, Inf, 1)
    )
  })
  
  # Other possible interventions
  output$prev_sub_more_base <- renderUI({
    if (rv_base$prev_sub_count_base <= 1) return(NULL)
    lapply(2:rv_base$prev_sub_count_base, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("prev_sub_cov_base_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_sub_adh_base_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_sub_eff_base_", i), "Effectiveness (1 - RR)", 0, 0, 1, 0.01),
        numericInput(paste0("prev_sub_n_base_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("prev_sub_hc_cost_base_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("prev_sub_soc_cost_base_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  
  # NULL-coalescing helper to build reactive dataframes
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # ---------- central defaults + helpers (used by all DF reactives) ----------
  .defaults <- list(
    base = list(
      prev_sub = list(i1 = list(cov=0.0, adh=56, rr=21, n=30, hc=160, soc=297)),
      mild = list(
        i1 = list(cov=2.0,  adh=43, d=0.33, n=30, hc=265, soc=441),
        i2 = list(cov=17.0, adh=56, d=0.51, n=30, hc=962, soc=1491)
      ),
      mod = list(
        i1 = list(cov=2.0,  adh=43, d=0.33, n=30, hc=265, soc=441),
        i2 = list(cov=17.0, adh=56, d=0.51, n=30, hc=962, soc=1491)
      ),
      sev = list(
        i1 = list(cov=18.0, adh=68, d=0.51, n=30, hc=1984, soc=1652),
        i2 = list(cov=20.0, adh=44, d=0.30, n=30, hc=607,  soc=358),
        i3 = list(cov=20.0, adh=56, d=0.51, n=30, hc=679,  soc=415)
      ),
      prev_rec = list(
        i1 = list(cov=0.0, adh=42, rr=25, n=30, hc=1002, soc=680),
        i2 = list(cov=0.0, adh=68, rr=34, n=30, hc=361,  soc=540),
        i3 = list(cov=0.0, adh=56, rr=32, n=30, hc=429,  soc=537)
      )
    )
  )
  .defaults$alt <- .defaults$base
  
  .default_for_id <- function(id) {
    # prev_sub/prev_rec:
    m1 <- regexec("^(prev_sub|prev_rec)_(cov|adh|1_RR|eff|n|hc_cost|soc_cost)_(base|alt)_(\\d+)$", id)
    r1 <- regmatches(id, m1)[[1]]
    if (length(r1)) {
      block <- r1[2]; field <- r1[3]; case <- r1[4]; i <- paste0("i", as.integer(r1[5]))
      key <- switch(field, "1_RR"="rr", "hc_cost"="hc", "soc_cost"="soc", field)
      item <- .defaults[[case]][[block]][[i]]
      if (is.null(item) || is.null(item[[key]])) return(0)
      return(item[[key]])
    }
    # mild/mod/sev:
    m2 <- regexec("^(mild|mod|sev)_(cov|adh|d|n|hc_cost|soc_cost)_(base|alt)_(\\d+)$", id)
    r2 <- regmatches(id, m2)[[1]]
    if (length(r2)) {
      block <- r2[2]; field <- r2[3]; case <- r2[4]; i <- paste0("i", as.integer(r2[5]))
      key <- switch(field, "hc_cost"="hc", "soc_cost"="soc", field)
      item <- .defaults[[case]][[block]][[i]]
      if (is.null(item) || is.null(item[[key]])) return(0)
      return(item[[key]])
    }
    0
  }
  
  get_num_with_defaults <- function(id, default = NULL) {
    dv <- if (is.null(default)) .default_for_id(id) else default
    val <- input[[id]]
    if (is.null(val) || is.na(val)) as.numeric(dv) else as.numeric(val)
  }
  # ---------- /NEW ----------
  
  
  # Reactive dataframe
  
  df_prev_sub_base <- reactive({
    n_int <- rv_base$prev_sub_count_base %||% 0
    
    # If zero interventions, return empty DF with correct columns
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        `1-RR` = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW: use helper that falls back to defaults if inputs don't exist yet
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    # Coverage & adherence are 0–100 in BOTH first and later interventions -> store as proportions
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_cov_base_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_adh_base_", i)), numeric(1)) / 100
    
    # 1-RR: first uses % with id prev_sub_1_RR_base_1, others use proportion with id prev_sub_eff_base_i
    rr_vec <- vapply(seq_len(n_int), function(i) {
      if (i == 1) {
        # percent -> proportion
        get_num("prev_sub_1_RR_base_1") / 100
      } else {
        # already a proportion 0–1
        get_num(paste0("prev_sub_eff_base_", i))
      }
    }, numeric(1))
    
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_n_base_", i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_hc_cost_base_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_soc_cost_base_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      `1-RR` = rr_vec,
      n = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs` = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  prev_sub_base_covered <- reactive({
    sum(df_prev_sub_base()$cov, na.rm = TRUE)
  })
  
  prev_sub_base_effectivity <- reactive({
    sum(df_prev_sub_base()$cov * df_prev_sub_base()$adh)
  })
  
  
  # Treatment: Mild depression
  
  observeEvent(input$treat_mild_add_base, {
    rv_base$mild_count_base <- rv_base$mild_count_base + 1
  })
  
  observeEvent(input$treat_mild_remove_base, {
    rv_base$mild_count_base <- max(0, rv_base$mild_count_base - 1)
  })
  
  # Default intervention
  ## Intervention 1
  output$treat_mild_first_base <- renderUI({
    if (rv_base$mild_count_base < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("mild_cov_base_1", "Coverage (%)", 2.0, 0, 100, 0.1),
      numericInput("mild_adh_base_1", "Adherence (%)",43.0,  0, 100, 0.1),
      numericInput("mild_d_base_1", "d", 0.33,  0, Inf, 0.01),
      numericInput("mild_n_base_1", "n", 30,  0, Inf, 1),
      numericInput("mild_hc_cost_base_1","Healthcare costs (€)",  265,  0, Inf, 1),
      numericInput("mild_soc_cost_base_1", "Societal costs (€)",    441,  0, Inf, 1)
    )
  })
  
  ## Intervention 2
  output$treat_mild_second_base <- renderUI({
    if(rv_base$mild_count_base < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("mild_cov_base_2", "Coverage (%)", 17.0, 0, 100, 0.1),
      numericInput("mild_adh_base_2", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("mild_d_base_2", "d", 0.51,  0, Inf, 0.01),
      numericInput("mild_n_base_2", "n", 30,  0, Inf, 1),
      numericInput("mild_hc_cost_base_2","Healthcare costs (€)",  962,  0, Inf, 1),
      numericInput("mild_soc_cost_base_2", "Societal costs (€)",    1491,  0, Inf, 1)
    )
  })
  
  
  # Other possible interventions
  output$treat_mild_more_base <- renderUI({
    if (rv_base$mild_count_base < 3) return(NULL)
    lapply(3:rv_base$mild_count_base, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("mild_cov_base_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mild_adh_base_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mild_d_base_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("mild_n_base_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("mild_hc_cost_base_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("mild_soc_cost_base_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Default intervention (duplicate in your source—left as-is)
  output$treat_mild_first_base <- renderUI({
    if (rv_base$mild_count_base < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("mild_cov_base_1", "Coverage (%)", 2.0, 0, 100, 0.1),
      numericInput("mild_adh_base_1", "Adherence (%)",43.0,  0, 100, 0.1),
      numericInput("mild_d_base_1", "d", 0.33,  0, Inf, 0.01),
      numericInput("mild_n_base_1", "n", 30,  0, Inf, 1),
      numericInput("mild_hc_cost_base_1","Healthcare costs (€)",  265,  0, Inf, 1),
      numericInput("mild_soc_cost_base_1", "Societal costs (€)",    441,  0, Inf, 1)
    )
  })
  
  output$treat_mild_second_base <- renderUI({
    if(rv_base$mild_count_base < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("mild_cov_base_2", "Coverage (%)", 17.0, 0, 100, 0.1),
      numericInput("mild_adh_base_2", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("mild_d_base_2", "d", 0.51,  0, Inf, 0.01),
      numericInput("mild_n_base_2", "n", 30,  0, Inf, 1),
      numericInput("mild_hc_cost_base_2","Healthcare costs (€)",  962,  0, Inf, 1),
      numericInput("mild_soc_cost_base_2", "Societal costs (€)",    1491,  0, Inf, 1)
    )
  })
  
  output$treat_mild_more_base <- renderUI({
    if (rv_base$mild_count_base < 3) return(NULL)
    lapply(3:rv_base$mild_count_base, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("mild_cov_base_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mild_adh_base_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mild_d_base_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("mild_n_base_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("mild_hc_cost_base_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("mild_soc_cost_base_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Reactive dataframe
  
  df_treat_mild_base <- reactive({
    n_int <- rv_base$mild_count_base %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        d = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_cov_base_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_adh_base_", i)), numeric(1)) / 100
    d_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_d_base_",   i)), numeric(1))
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_n_base_",   i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_hc_cost_base_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_soc_cost_base_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      d   = d_vec,
      n   = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs`   = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  treat_mild_base_covered <- reactive({
    sum(df_treat_mild_base()$cov, na.rm = TRUE)
  })
  
  treat_mild_base_effectivity <- reactive({
    sum(df_treat_mild_base()$cov * df_treat_mild_base()$adh)
  })
  
  treat_mild_base_profilactic <- reactive({
    sum(df_treat_mild_base()$cov * df_treat_mild_base()$adh, na.rm = TRUE) * 0.25
  })
  
  
  
  # Treatment: Moderate depression
  
  observeEvent(input$treat_mod_add_base, {
    rv_base$mod_count_base <- rv_base$mod_count_base + 1
  })
  
  observeEvent(input$treat_mod_remove_base, {
    rv_base$mod_count_base <- max(0, rv_base$mod_count_base - 1)
  })
  
  # Default intervention
  output$treat_mod_first_base <- renderUI({
    if (rv_base$mod_count_base < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("mod_cov_base_1", "Coverage (%)", 2.0, 0, 100, 0.1),
      numericInput("mod_adh_base_1", "Adherence (%)", 43.0,  0, 100, 0.1),
      numericInput("mod_d_base_1", "d", 0.33,  0, Inf, 0.01),
      numericInput("mod_n_base_1", "n", 30,  0, Inf, 1),
      numericInput("mod_hc_cost_base_1","Healthcare costs (€)",  248,  0, Inf, 1),
      numericInput("mod_soc_cost_base_1", "Societal costs (€)",    441,  0, Inf, 1)
    )
  })
  
  output$treat_mod_second_base <- renderUI({
    if(rv_base$mod_count_base < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("mod_cov_base_2", "Coverage (%)", 16.0, 0, 100, 0.1),
      numericInput("mod_adh_base_2", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("mod_d_base_2", "d", 0.51,  0, Inf, 0.01),
      numericInput("mod_n_base_2", "n", 30,  0, Inf, 1),
      numericInput("mod_hc_cost_base_2","Healthcare costs (€)",  901,  0, Inf, 1),
      numericInput("mod_soc_cost_base_2", "Societal costs (€)",    1491,  0, Inf, 1)
    )
  })
  
  # Other possible interventions
  output$treat_mod_more_base <- renderUI({
    if (rv_base$mod_count_base < 3) return(NULL)
    lapply(3 :rv_base$mod_count_base, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("mod_cov_base_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mod_adh_base_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mod_d_base_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("mod_n_base_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("mod_hc_cost_base_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("mod_soc_cost_base_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Reactive dataframe
  
  df_treat_mod_base <- reactive({
    n_int <- rv_base$mod_count_base %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        d = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_cov_base_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_adh_base_", i)), numeric(1)) / 100
    d_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_d_base_",   i)), numeric(1))
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_n_base_",   i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_hc_cost_base_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_soc_cost_base_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      d   = d_vec,
      n   = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs`   = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  treat_mod_base_covered <- reactive({
    sum(df_treat_mod_base()$cov, na.rm = TRUE)
  })
  
  treat_mod_base_effectivity <- reactive({
    sum(df_treat_mod_base()$cov * df_treat_mod_base()$adh)
  })
  
  treat_mod_base_profilactic <- reactive({
    sum(df_treat_mod_base()$cov * df_treat_mod_base()$adh, na.rm = TRUE) * 0.25
  })
  
  
  # Treatment: Severe depression
  
  observeEvent(input$treat_sev_add_base, {
    rv_base$sev_count_base <- rv_base$sev_count_base + 1
  })
  
  observeEvent(input$treat_sev_remove_base, {
    rv_base$sev_count_base <- max(0, rv_base$sev_count_base - 1)
  })
  
  # Default intervention
  output$treat_sev_first_base <- renderUI({
    if (rv_base$sev_count_base < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("sev_cov_base_1", "Coverage (%)", 18.0, 0, 100, 0.1),
      numericInput("sev_adh_base_1", "Adherence (%)", 68.0,  0, 100, 0.1),
      numericInput("sev_d_base_1", "d", 0.51,  0, Inf, 0.01),
      numericInput("sev_n_base_1", "n", 30,  0, Inf, 1),
      numericInput("sev_hc_cost_base_1","Healthcare costs (€)",  1984,  0, Inf, 1),
      numericInput("sev_soc_cost_base_1", "Societal costs (€)",    1652,  0, Inf, 1)
    )
  })
  
  output$treat_sev_second_base <- renderUI({
    if(rv_base$sev_count_base < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("sev_cov_base_2", "Coverage (%)", 20.0, 0, 100, 0.1),
      numericInput("sev_adh_base_2", "Adherence (%)",44.0,  0, 100, 0.1),
      numericInput("sev_d_base_2", "d", 0.30,  0, Inf, 0.01),
      numericInput("sev_n_base_2", "n", 30,  0, Inf, 1),
      numericInput("sev_hc_cost_base_2","Healthcare costs (€)",  607,  0, Inf, 1),
      numericInput("sev_soc_cost_base_2", "Societal costs (€)",    358,  0, Inf, 1)
    )
  })
  
  output$treat_sev_third_base <- renderUI({
    if(rv_base$sev_count_base < 3) return(NULL)
    tagList(
      h4("Intervention 3"),
      numericInput("sev_cov_base_3", "Coverage (%)", 20.0, 0, 100, 0.1),
      numericInput("sev_adh_base_3", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("sev_d_base_3", "d", 0.51,  0, Inf, 0.01),
      numericInput("sev_n_base_3", "n", 30,  0, Inf, 1),
      numericInput("sev_hc_cost_base_3","Healthcare costs (€)",  679,  0, Inf, 1),
      numericInput("sev_soc_cost_base_3", "Societal costs (€)",    415,  0, Inf, 1)
    )
  })
  
  
  # Other possible interventions
  output$treat_sev_more_base <- renderUI({
    if (rv_base$sev_count_base < 4) return(NULL)
    lapply(4:rv_base$sev_count_base, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("sev_cov_base_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("sev_adh_base_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("sev_d_base_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("sev_n_base_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("sev_hc_cost_base_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("sev_soc_cost_base_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Reactive dataframe
  
  df_treat_sev_base <- reactive({
    n_int <- rv_base$sev_count_base %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        d = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_cov_base_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_adh_base_", i)), numeric(1)) / 100
    d_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_d_base_",   i)), numeric(1))
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_n_base_",   i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_hc_cost_base_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_soc_cost_base_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      d   = d_vec,
      n   = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs`   = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  treat_sev_base_covered <- reactive({
    sum(df_treat_sev_base()$cov, na.rm = TRUE)
  })
  
  treat_sev_base_effectivity <- reactive({
    sum(df_treat_sev_base()$cov * df_treat_sev_base()$adh)
  })
  
  treat_sev_base_profilactic <- reactive({
    sum(df_treat_sev_base()$cov * df_treat_sev_base()$adh, na.rm = TRUE) * 0.25
  })
  
  
  # Prevention: Recurrent depression
  
  observeEvent(input$prev_rec_add_base, {
    rv_base$prev_rec_count_base <- rv_base$prev_rec_count_base + 1
  })
  
  observeEvent(input$prev_rec_remove_base, {
    rv_base$prev_rec_count_base <- max(0, rv_base$prev_rec_count_base - 1)
  })
  
  # Default intervention
  output$prev_rec_first_base <- renderUI({
    if (rv_base$prev_rec_count_base < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_rec_cov_base_1", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_rec_adh_base_1", "Adherence (%)", 42.0,  0, 100, 0.1),
      numericInput("prev_rec_1_RR_base_1", "1 - RR (%)", 25.0,  0, 100, 0.1),
      numericInput("prev_rec_n_base_1", "n", 30,  0, Inf, 1),
      numericInput("prev_rec_hc_cost_base_1","Healthcare costs (€)",  1002,  0, Inf, 1),
      numericInput("prev_rec_soc_cost_base_1", "Societal costs (€)",    680,  0, Inf, 1)
    )
  })
  
  output$prev_rec_second_base <- renderUI({
    if (rv_base$prev_rec_count_base < 2) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_rec_cov_base_2", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_rec_adh_base_2", "Adherence (%)", 68.0,  0, 100, 0.1),
      numericInput("prev_rec_1_RR_base_2", "1 - RR (%)", 34.0,  0, 100, 0.1),
      numericInput("prev_rec_n_base_2", "n", 30,  0, Inf, 1),
      numericInput("prev_rec_hc_cost_base_2","Healthcare costs (€)",  361,  0, Inf, 1),
      numericInput("prev_rec_soc_cost_base_2", "Societal costs (€)",    540,  0, Inf, 1)
    )
  })
  
  output$prev_rec_third_base <- renderUI({
    if (rv_base$prev_rec_count_base < 3) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_rec_cov_base_3", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_rec_adh_base_3", "Adherence (%)", 56.0,  0, 100, 0.1),
      numericInput("prev_rec_1_RR_base_3", "1 - RR (%)", 32.0,  0, 100, 0.1),
      numericInput("prev_rec_n_base_3", "n", 30,  0, Inf, 1),
      numericInput("prev_rec_hc_cost_base_3","Healthcare costs (€)",  429,  0, Inf, 1),
      numericInput("prev_rec_soc_cost_base_3", "Societal costs (€)",    537,  0, Inf, 1)
    )
  })
  
  
  # Other possible interventions
  output$prev_rec_more_base <- renderUI({
    if (rv_base$prev_rec_count_base < 4) return(NULL)
    lapply(2:rv_base$prev_rec_count_base, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("prev_rec_cov_base_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_rec_adh_base_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_rec_eff_base_", i), "Effectiveness (1 - RR)", 0, 0, 1, 0.01),
        numericInput(paste0("prev_rec_n_base_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("prev_rec_hc_cost_base_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("prev_rec_soc_cost_base_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  
  # Reactive dataframe
  
  df_prev_rec_base <- reactive({
    n_int <- rv_base$prev_rec_count_base %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        `1-RR` = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_cov_base_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_adh_base_", i)), numeric(1)) / 100
    
    rr_vec <- vapply(seq_len(n_int), function(i) {
      get_num(paste0("prev_rec_1_RR_base_", i)) / 100
    }, numeric(1))
    
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_n_base_", i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_hc_cost_base_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_soc_cost_base_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      `1-RR` = rr_vec,
      n = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs` = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  prev_rec_base_covered <- reactive({
    sum(df_prev_rec_base()$cov, na.rm = TRUE)
  })
  
  prev_rec_base_effectivity <- reactive({
    sum(df_prev_rec_base()$cov * df_prev_rec_base()$adh)
  })
  
  
  
  # Alternative case
  
  # Get reactive values for the different counts
  rv_alt <- reactiveValues(
    prev_sub_count_alt = 1,
    mild_count_alt     = 2,
    mod_count_alt      = 2,
    sev_count_alt      = 3,
    prev_rec_count_alt = 3
  )
  
  # Prevention: Sub-clinical depression
  
  observeEvent(input$prev_sub_add_alt, {
    rv_alt$prev_sub_count_alt <- rv_alt$prev_sub_count_alt + 1
  })
  
  observeEvent(input$prev_sub_remove_alt, {
    rv_alt$prev_sub_count_alt <- max(0, rv_alt$prev_sub_count_alt - 1)
  })
  
  # Default intervention
  output$prev_sub_first_alt <- renderUI({
    if (rv_alt$prev_sub_count_alt < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_sub_cov_alt_1", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_sub_adh_alt_1", "Adherence (%)",56,  0, 100, 0.1),
      numericInput("prev_sub_1_RR_alt_1", "1 - RR (%)", 21,  0, 100, 0.1),
      numericInput("prev_sub_n_alt_1", "n", 30,  0, Inf, 1),
      numericInput("prev_sub_hc_cost_alt_1","Healthcare costs (€)",  160,  0, Inf, 1),
      numericInput("prev_sub_soc_cost_alt_1", "Societal costs (€)",    297,  0, Inf, 1)
    )
  })
  
  # Other possible interventions
  output$prev_sub_more_alt <- renderUI({
    if (rv_alt$prev_sub_count_alt <= 1) return(NULL)
    lapply(2:rv_alt$prev_sub_count_alt, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("prev_sub_cov_alt_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_sub_adh_alt_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_sub_eff_alt_", i), "Effectiveness (1 - RR)", 0, 0, 1, 0.01),
        numericInput(paste0("prev_sub_n_alt_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("prev_sub_hc_cost_alt_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("prev_sub_soc_cost_alt_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  
  # Reactive dataframe
  
  df_prev_sub_alt <- reactive({
    n_int <- rv_alt$prev_sub_count_alt %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        `1-RR` = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_cov_alt_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_adh_alt_", i)), numeric(1)) / 100
    
    rr_vec <- vapply(seq_len(n_int), function(i) {
      if (i == 1) {
        get_num("prev_sub_1_RR_alt_1") / 100
      } else {
        get_num(paste0("prev_sub_eff_alt_", i))
      }
    }, numeric(1))
    
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_n_alt_", i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_hc_cost_alt_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_sub_soc_cost_alt_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      `1-RR` = rr_vec,
      n = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs` = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  prev_sub_alt_covered <- reactive({
    sum(df_prev_sub_alt()$cov, na.rm = TRUE)
  })
  
  prev_sub_alt_effectivity <- reactive({
    sum(df_prev_sub_alt()$cov * df_prev_sub_alt()$adh)
  })
  
  # Treatment: Mild depression
  
  observeEvent(input$treat_mild_add_alt, {
    rv_alt$mild_count_alt <- rv_alt$mild_count_alt + 1
  })
  
  observeEvent(input$treat_mild_remove_alt, {
    rv_alt$mild_count_alt <- max(0, rv_alt$mild_count_alt - 1)
  })
  
  # Default intervention
  output$treat_mild_first_alt <- renderUI({
    if (rv_alt$mild_count_alt < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("mild_cov_alt_1", "Coverage (%)", 2.0, 0, 100, 0.1),
      numericInput("mild_adh_alt_1", "Adherence (%)",43.0,  0, 100, 0.1),
      numericInput("mild_d_alt_1", "d", 0.33,  0, Inf, 0.01),
      numericInput("mild_n_alt_1", "n", 30,  0, Inf, 1),
      numericInput("mild_hc_cost_alt_1","Healthcare costs (€)",  265,  0, Inf, 1),
      numericInput("mild_soc_cost_alt_1", "Societal costs (€)",    441,  0, Inf, 1)
    )
  })
  
  output$treat_mild_second_alt <- renderUI({
    if(rv_alt$mild_count_alt < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("mild_cov_alt_2", "Coverage (%)", 17.0, 0, 100, 0.1),
      numericInput("mild_adh_alt_2", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("mild_d_alt_2", "d", 0.51,  0, Inf, 0.01),
      numericInput("mild_n_alt_2", "n", 30,  0, Inf, 1),
      numericInput("mild_hc_cost_alt_2","Healthcare costs (€)",  962,  0, Inf, 1),
      numericInput("mild_soc_cost_alt_2", "Societal costs (€)",    1491,  0, Inf, 1)
    )
  })
  
  
  # Other possible interventions
  output$treat_mild_more_alt <- renderUI({
    if (rv_alt$mild_count_alt < 3) return(NULL)
    lapply(3:rv_alt$mild_count_alt, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("mild_cov_alt_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mild_adh_alt_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mild_d_alt_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("mild_n_alt_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("mild_hc_cost_alt_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("mild_soc_cost_alt_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Reactive dataframe
  
  df_treat_mild_alt <- reactive({
    n_int <- rv_alt$mild_count_alt %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        d = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_cov_alt_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_adh_alt_", i)), numeric(1)) / 100
    d_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_d_alt_",   i)), numeric(1))
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_n_alt_",   i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_hc_cost_alt_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mild_soc_cost_alt_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      d   = d_vec,
      n   = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs`   = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  treat_mild_alt_covered <- reactive({
    sum(df_treat_mild_alt()$cov, na.rm = TRUE)
  })
  
  treat_mild_alt_effectivity <- reactive({
    sum(df_treat_mild_alt()$cov * df_treat_mild_alt()$adh)
  })
  
  treat_mild_alt_profilactic <- reactive({
    sum(df_treat_mild_alt()$cov * df_treat_mild_alt()$adh, na.rm = TRUE) * 0.25
  })
  
  
  # Treatment: Moderate depression
  
  observeEvent(input$treat_mod_add_alt, {
    rv_alt$mod_count_alt <- rv_alt$mod_count_alt + 1
  })
  
  observeEvent(input$treat_mod_remove_alt, {
    rv_alt$mod_count_alt <- max(0, rv_alt$mod_count_alt - 1)
  })
  
  # Default intervention
  output$treat_mod_first_alt <- renderUI({
    if (rv_alt$mod_count_alt < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("mod_cov_alt_1", "Coverage (%)", 2.0, 0, 100, 0.1),
      numericInput("mod_adh_alt_1", "Adherence (%)", 43.0,  0, 100, 0.1),
      numericInput("mod_d_alt_1", "d", 0.33,  0, Inf, 0.01),
      numericInput("mod_n_alt_1", "n", 30,  0, Inf, 1),
      numericInput("mod_hc_cost_alt_1","Healthcare costs (€)",  248,  0, Inf, 1),
      numericInput("mod_soc_cost_alt_1", "Societal costs (€)",    441,  0, Inf, 1)
    )
  })
  
  output$treat_mod_second_alt <- renderUI({
    if(rv_alt$mod_count_alt < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("mod_cov_alt_2", "Coverage (%)", 16.0, 0, 100, 0.1),
      numericInput("mod_adh_alt_2", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("mod_d_alt_2", "d", 0.51,  0, Inf, 0.01),
      numericInput("mod_n_alt_2", "n", 30,  0, Inf, 1),
      numericInput("mod_hc_cost_alt_2","Healthcare costs (€)",  901,  0, Inf, 1),
      numericInput("mod_soc_cost_alt_2", "Societal costs (€)",    1491,  0, Inf, 1)
    )
  })
  
  # Other possible interventions
  output$treat_mod_more_alt <- renderUI({
    if (rv_alt$mod_count_alt < 3) return(NULL)
    lapply(3 :rv_alt$mod_count_alt, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("mod_cov_alt_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mod_adh_alt_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("mod_d_alt_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("mod_n_alt_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("mod_hc_cost_alt_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("mod_soc_cost_alt_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Reactive dataframe
  
  df_treat_mod_alt <- reactive({
    n_int <- rv_alt$mod_count_alt %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        d = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_cov_alt_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_adh_alt_", i)), numeric(1)) / 100
    d_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_d_alt_",   i)), numeric(1))
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_n_alt_",   i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_hc_cost_alt_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("mod_soc_cost_alt_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      d   = d_vec,
      n   = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs`   = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  treat_mod_alt_covered <- reactive({
    sum(df_treat_mod_alt()$cov, na.rm = TRUE)
  })
  
  treat_mod_alt_effectivity <- reactive({
    sum(df_treat_mod_alt()$cov * df_treat_mod_alt()$adh)
  })
  
  treat_mod_alt_profilactic <- reactive({
    sum(df_treat_mod_alt()$cov * df_treat_mod_alt()$adh, na.rm = TRUE) * 0.25
  })
  
  # Treatment: Severe depression
  
  observeEvent(input$treat_sev_add_alt, {
    rv_alt$sev_count_alt <- rv_alt$sev_count_alt + 1
  })
  
  observeEvent(input$treat_sev_remove_alt, {
    rv_alt$sev_count_alt <- max(0, rv_alt$sev_count_alt - 1)
  })
  
  # Default intervention
  output$treat_sev_first_alt <- renderUI({
    if (rv_alt$sev_count_alt < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("sev_cov_alt_1", "Coverage (%)", 18.0, 0, 100, 0.1),
      numericInput("sev_adh_alt_1", "Adherence (%)", 68.0,  0, 100, 0.1),
      numericInput("sev_d_alt_1", "d", 0.51,  0, Inf, 0.01),
      numericInput("sev_n_alt_1", "n", 30,  0, Inf, 1),
      numericInput("sev_hc_cost_alt_1","Healthcare costs (€)",  1984,  0, Inf, 1),
      numericInput("sev_soc_cost_alt_1", "Societal costs (€)",    1652,  0, Inf, 1)
    )
  })
  
  output$treat_sev_second_alt <- renderUI({
    if(rv_alt$sev_count_alt < 2) return(NULL)
    tagList(
      h4("Intervention 2"),
      numericInput("sev_cov_alt_2", "Coverage (%)", 20.0, 0, 100, 0.1),
      numericInput("sev_adh_alt_2", "Adherence (%)",44.0,  0, 100, 0.1),
      numericInput("sev_d_alt_2", "d", 0.30,  0, Inf, 0.01),
      numericInput("sev_n_alt_2", "n", 30,  0, Inf, 1),
      numericInput("sev_hc_cost_alt_2","Healthcare costs (€)",  607,  0, Inf, 1),
      numericInput("sev_soc_cost_alt_2", "Societal costs (€)",    358,  0, Inf, 1)
    )
  })
  
  output$treat_sev_third_alt <- renderUI({
    if(rv_alt$sev_count_alt < 3) return(NULL)
    tagList(
      h4("Intervention 3"),
      numericInput("sev_cov_alt_3", "Coverage (%)", 20.0, 0, 100, 0.1),
      numericInput("sev_adh_alt_3", "Adherence (%)",56.0,  0, 100, 0.1),
      numericInput("sev_d_alt_3", "d", 0.51,  0, Inf, 0.01),
      numericInput("sev_n_alt_3", "n", 30,  0, Inf, 1),
      numericInput("sev_hc_cost_alt_3","Healthcare costs (€)",  679,  0, Inf, 1),
      numericInput("sev_soc_cost_alt_3", "Societal costs (€)",    415,  0, Inf, 1)
    )
  })
  
  
  # Other possible interventions
  output$treat_sev_more_alt <- renderUI({
    if (rv_alt$sev_count_alt < 4) return(NULL)
    lapply(4:rv_alt$sev_count_alt, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("sev_cov_alt_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("sev_adh_alt_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("sev_d_alt_", i), "d", 0, 0, 1, 0.01),
        numericInput(paste0("sev_n_alt_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("sev_hc_cost_alt_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("sev_soc_cost_alt_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  
  # Reactive dataframe
  
  df_treat_sev_alt <- reactive({
    n_int <- rv_alt$sev_count_alt %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        d = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_cov_alt_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_adh_alt_", i)), numeric(1)) / 100
    d_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_d_alt_",   i)), numeric(1))
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_n_alt_",   i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_hc_cost_alt_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("sev_soc_cost_alt_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      d   = d_vec,
      n   = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs`   = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  treat_sev_alt_covered <- reactive({
    sum(df_treat_sev_alt()$cov, na.rm = TRUE)
  })
  
  treat_sev_alt_effectivity <- reactive({
    sum(df_treat_sev_alt()$cov * df_treat_sev_alt()$adh)
  })
  
  treat_sev_alt_profilactic <- reactive({
    sum(df_treat_sev_alt()$cov * df_treat_sev_alt()$adh, na.rm = TRUE) * 0.25
  })
  
  
  # Prevention: Recurrent depression
  
  observeEvent(input$prev_rec_add_alt, {
    rv_alt$prev_rec_count_alt <- rv_alt$prev_rec_count_alt + 1
  })
  
  observeEvent(input$prev_rec_remove_alt, {
    rv_alt$prev_rec_count_alt <- max(0, rv_alt$prev_rec_count_alt - 1)
  })
  
  # Default intervention
  output$prev_rec_first_alt <- renderUI({
    if (rv_alt$prev_rec_count_alt < 1) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_rec_cov_alt_1", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_rec_adh_alt_1", "Adherence (%)", 42.0,  0, 100, 0.1),
      numericInput("prev_rec_1_RR_alt_1", "1 - RR (%)", 25.0,  0, 100, 0.1),
      numericInput("prev_rec_n_alt_1", "n", 30,  0, Inf, 1),
      numericInput("prev_rec_hc_cost_alt_1","Healthcare costs (€)",  1002,  0, Inf, 1),
      numericInput("prev_rec_soc_cost_alt_1", "Societal costs (€)",    680,  0, Inf, 1)
    )
  })
  
  output$prev_rec_second_alt <- renderUI({
    if (rv_alt$prev_rec_count_alt < 2) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_rec_cov_alt_2", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_rec_adh_alt_2", "Adherence (%)", 68.0,  0, 100, 0.1),
      numericInput("prev_rec_1_RR_alt_2", "1 - RR (%)", 34.0,  0, 100, 0.1),
      numericInput("prev_rec_n_alt_2", "n", 30,  0, Inf, 1),
      numericInput("prev_rec_hc_cost_alt_2","Healthcare costs (€)",  361,  0, Inf, 1),
      numericInput("prev_rec_soc_cost_alt_2", "Societal costs (€)",    540,  0, Inf, 1)
    )
  })
  
  output$prev_rec_third_alt <- renderUI({
    if (rv_alt$prev_rec_count_alt < 3) return(NULL)
    tagList(
      h4("Intervention 1"),
      numericInput("prev_rec_cov_alt_3", "Coverage (%)", 0.0, 0, 100, 0.1),
      numericInput("prev_rec_adh_alt_3", "Adherence (%)", 56.0,  0, 100, 0.1),
      numericInput("prev_rec_1_RR_alt_3", "1 - RR (%)", 32.0,  0, 100, 0.1),
      numericInput("prev_rec_n_alt_3", "n"
                   , 30,  0, Inf, 1),
      numericInput("prev_rec_hc_cost_alt_3","Healthcare costs (€)",  429,  0, Inf, 1),
      numericInput("prev_rec_soc_cost_alt_3", "Societal costs (€)",    537,  0, Inf, 1)
    )
  })
  
  
  # Other possible interventions
  output$prev_rec_more_alt <- renderUI({
    if (rv_alt$prev_rec_count_alt < 4) return(NULL)
    lapply(2:rv_alt$prev_rec_count_alt, function(i) {
      tagList(
        h4(paste("Intervention", i)),
        numericInput(paste0("prev_rec_cov_alt_", i), "Coverage (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_rec_adh_alt_", i), "Adherence (%)", 0, 0, 100, 0.1),
        numericInput(paste0("prev_rec_eff_alt_", i), "Effectiveness (1 - RR)", 0, 0, 1, 0.01),
        numericInput(paste0("prev_rec_n_alt_",   i), "Population (n)", 0, 0, Inf, 1),
        numericInput(paste0("prev_rec_hc_cost_alt_",  i), "Healthcare costs (€)", 0, 0, Inf, 1),
        numericInput(paste0("prev_rec_soc_cost_alt_", i), "Societal costs (€)", 0, 0, Inf, 1),
        hr()
      )
    })
  })
  
  # Reactive dataframe
  
  df_prev_rec_alt <- reactive({
    n_int <- rv_alt$prev_rec_count_alt %||% 0
    
    if (n_int < 1) {
      return(data.frame(
        Intervention = character(0),
        cov = numeric(0),
        adh = numeric(0),
        `1-RR` = numeric(0),
        n = numeric(0),
        `healthcare costs` = numeric(0),
        `societal costs` = numeric(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # NEW:
    get_num <- function(id, default = NULL) get_num_with_defaults(id, default)
    
    cov_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_cov_alt_", i)), numeric(1)) / 100
    adh_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_adh_alt_", i)), numeric(1)) / 100
    
    rr_vec <- vapply(seq_len(n_int), function(i) {
      get_num(paste0("prev_rec_1_RR_alt_", i)) / 100
    }, numeric(1))
    
    n_vec   <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_n_alt_", i)), numeric(1))
    hc_vec  <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_hc_cost_alt_",  i)), numeric(1))
    soc_vec <- vapply(seq_len(n_int), function(i) get_num(paste0("prev_rec_soc_cost_alt_", i)), numeric(1))
    
    data.frame(
      Intervention = paste("Intervention", seq_len(n_int)),
      cov = cov_vec,
      adh = adh_vec,
      `1-RR` = rr_vec,
      n = n_vec,
      `healthcare costs` = hc_vec,
      `societal costs` = soc_vec,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  prev_rec_alt_covered <- reactive({
    sum(df_prev_rec_alt()$cov, na.rm = TRUE)
  })
  
  prev_rec_alt_effectivity <- reactive({
    sum(df_prev_rec_alt()$cov * df_prev_rec_alt()$adh)
  })
  
  
  # Transition matrix
  # 1) Define vals BEFORE any use in the modal
  vals <- reactiveValues(
    pop_inc = 0.0128,
    
    inc_mild = 0.30,  inc_mod = 0.47,  inc_sev = 0.23,
    
    mild_rec = 0.60,  mild_part = 0.37,  mild_chr = 0.03,
    mod_rec  = 0.48,  mod_part  = 0.50,  mod_chr  = 0.03,
    sev_rec  = 0.51,  sev_part  = 0.46,  sev_chr  = 0.03,
    
    mildrec_one = 0.46, mildrec_rel = 0.54,
    mildpart_one = 0.20, mildpart_rel = 0.80,
    modrec_one  = 0.49, modrec_rel  = 0.51,
    modpart_one = 0.25, modpart_rel = 0.75,
    sevrec_one  = 0.49, sevrec_rel  = 0.51,
    sevpart_one = 0.25, sevpart_rel = 0.75,
    
    # Chronic -> End; your data had zeros—keeping that default
    mild_chr_end = 0.0,
    mod_chr_end  = 0.0,
    sev_chr_end  = 0.0
  )
  
  ## ---------- 2) Modal for editing ----------
  observeEvent(input$edit_tt, {
    showModal(modalDialog(
      title = "Edit transition probabilities (0–1)",
      fluidRow(
        # --- Incidence ---
        column(3,
               strong("Incidence"),
               numericInput("pop_inc", "Population → Incidence", 
                            value = isolate(vals$pop_inc), min=0, max=1, step=1e-6),
               tags$hr(),
               numericInput("inc_mild", "Incidence → Mild",     
                            value = isolate(vals$inc_mild), min=0, max=1, step=1e-6),
               numericInput("inc_mod",  "Incidence → Moderate", 
                            value = isolate(vals$inc_mod),  min=0, max=1, step=1e-6),
               numericInput("inc_sev",  "Incidence → Severe",   
                            value = isolate(vals$inc_sev),  min=0, max=1, step=1e-6)
        ),
        
        # --- Mild ---
        column(3,
               strong("Mild"),
               numericInput("mild_rec",  "Mild → Recovery", value = isolate(vals$mild_rec),  min=0, max=1, step=1e-6),
               numericInput("mild_part", "Mild → Partial",  value = isolate(vals$mild_part), min=0, max=1, step=1e-6),
               numericInput("mild_chr",  "Mild → Chronic",  value = isolate(vals$mild_chr),  min=0, max=1, step=1e-6),
               tags$hr(),
               numericInput("mildrec_one", "Recovery(Mild) → One",     value = isolate(vals$mildrec_one), min=0, max=1, step=1e-6),
               numericInput("mildrec_rel", "Recovery(Mild) → Relapse", value = isolate(vals$mildrec_rel), min=0, max=1, step=1e-6),
               numericInput("mildpart_one","Partial(Mild) → One",      value = isolate(vals$mildpart_one), min=0, max=1, step=1e-6),
               numericInput("mildpart_rel","Partial(Mild) → Relapse",  value = isolate(vals$mildpart_rel), min=0, max=1, step=1e-6)
        ),
        
        # --- Moderate ---
        column(3,
               strong("Moderate"),
               numericInput("mod_rec",  "Moderate → Recovery", value = isolate(vals$mod_rec),  min=0, max=1, step=1e-6),
               numericInput("mod_part", "Moderate → Partial",  value = isolate(vals$mod_part), min=0, max=1, step=1e-6),
               numericInput("mod_chr",  "Moderate → Chronic",  value = isolate(vals$mod_chr),  min=0, max=1, step=1e-6),
               tags$hr(),
               numericInput("modrec_one", "Recovery(Mod) → One",     value = isolate(vals$modrec_one), min=0, max=1, step=1e-6),
               numericInput("modrec_rel", "Recovery(Mod) → Relapse", value = isolate(vals$modrec_rel), min=0, max=1, step=1e-6),
               numericInput("modpart_one","Partial(Mod) → One",      value = isolate(vals$modpart_one), min=0, max=1, step=1e-6),
               numericInput("modpart_rel","Partial(Mod) → Relapse",  value = isolate(vals$modpart_rel), min=0, max=1, step=1e-6)
        ),
        
        # --- Severe ---
        column(3,
               strong("Severe"),
               numericInput("sev_rec",  "Severe → Recovery", value = isolate(vals$sev_rec),  min=0, max=1, step=1e-6),
               numericInput("sev_part", "Severe → Partial",  value = isolate(vals$sev_part), min=0, max=1, step=1e-6),
               numericInput("sev_chr",  "Severe → Chronic",  value = isolate(vals$sev_chr),  min=0, max=1, step=1e-6),
               tags$hr(),
               numericInput("sevrec_one", "Recovery(Sev) → One",     value = isolate(vals$sevrec_one), min=0, max=1, step=1e-6),
               numericInput("sevrec_rel", "Recovery(Sev) → Relapse", value = isolate(vals$sevrec_rel), min=0, max=1, step=1e-6),
               numericInput("sevpart_one","Partial(Sev) → One",      value = isolate(vals$sevpart_one), min=0, max=1, step=1e-6),
               numericInput("sevpart_rel","Partial(Sev) → Relapse",  value = isolate(vals$sevpart_rel), min=0, max=1, step=1e-6)
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_tt", "Save", class = "btn-primary")
      ),
      size = "l"
    ))
  })
  
  
  ## ---------- 3) Save back into vals so the plot updates ----------
  observeEvent(input$save_tt, {
    for (nm in names(reactiveValuesToList(vals))) {
      if (!is.null(input[[nm]])) vals[[nm]] <- input[[nm]]
    }
    removeModal()
  })
  
  ## ---------- 4) Build full Mermaid from vals ----------
  lab <- function(p) sprintf("%.1f %%", 100 * as.numeric(p))
  
  mermaid_text <- reactive({
    v <- reactiveValuesToList(vals)  # makes this reactive on any change
    
    paste(
      "graph LR",
      # Nodes (IDs -> labels)
      'Pop["Target population"]',
      'Inc["Yearly incidence (no history)"]',
      'Mild["Mild"]', 'Mod["Moderate"]', 'Sev["Severe"]',
      
      'RecM["Recovery (Mild)"]',   'ParM["Partial (Mild)"]',   'ChrM["Chronic (Mild)"]',
      'RecMo["Recovery (Moderate)"]','ParMo["Partial (Moderate)"]','ChrMo["Chronic (Moderate)"]',
      'RecS["Recovery (Severe)"]', 'ParS["Partial (Severe)"]', 'ChrS["Chronic (Severe)"]',
      
      'OneM["One occurrence"]', 'RelM["Relapse"]',
      'OnePM["One occurence"]', 'RelPM["Relapse"]',
      'OneMo["One occurrence"]','RelMo["Relapse"]',
      'OnePMo["One occurrence"]','RelPMo["Relapse"]',
      'OneS["One occurrence"]', 'RelS["Relapse"]',
      'OnePS["One occurrence"]', 'RelPS["Relapse"]',
      
      # Edges
      sprintf("Pop  -->|%s| Inc",  lab(v$pop_inc)),
      
      sprintf("Inc  -->|%s| Mild", lab(v$inc_mild)),
      sprintf("Inc  -->|%s| Mod",  lab(v$inc_mod)),
      sprintf("Inc  -->|%s| Sev",  lab(v$inc_sev)),
      
      sprintf("Mild -->|%s| RecM", lab(v$mild_rec)),
      sprintf("Mild -->|%s| ParM", lab(v$mild_part)),
      sprintf("Mild -->|%s| ChrM", lab(v$mild_chr)),
      
      sprintf("Mod  -->|%s| RecMo", lab(v$mod_rec)),
      sprintf("Mod  -->|%s| ParMo", lab(v$mod_part)),
      sprintf("Mod  -->|%s| ChrMo", lab(v$mod_chr)),
      
      sprintf("Sev  -->|%s| RecS",  lab(v$sev_rec)),
      sprintf("Sev  -->|%s| ParS",  lab(v$sev_part)),
      sprintf("Sev  -->|%s| ChrS",  lab(v$sev_chr)),
      
      # Recovery/Partial branches: Mild
      sprintf("RecM -->|%s| OneM", lab(v$mildrec_one)),
      sprintf("RecM -->|%s| RelM", lab(v$mildrec_rel)),
      sprintf("ParM -->|%s| OnePM", lab(v$mildpart_one)),
      sprintf("ParM -->|%s| RelPM", lab(v$mildpart_rel)),
      
      # Recovery/Partial branches: Moderate
      sprintf("RecMo -->|%s| OneMo", lab(v$modrec_one)),
      sprintf("RecMo -->|%s| RelMo", lab(v$modrec_rel)),
      sprintf("ParMo -->|%s| OnePMo", lab(v$modpart_one)),
      sprintf("ParMo -->|%s| RelPMo", lab(v$modpart_rel)),
      
      # Recovery/Partial branches: Severe
      sprintf("RecS -->|%s| OneS", lab(v$sevrec_one)),
      sprintf("RecS -->|%s| RelS", lab(v$sevrec_rel)),
      sprintf("ParS -->|%s| OnePS", lab(v$sevpart_one)),
      sprintf("ParS -->|%s| RelPS", lab(v$sevpart_rel)),
      
      
      # Classes (pastel fill, darker stroke & text)
      "classDef source   fill:#CCFBF1,stroke:#14B8A6,stroke-width:1px,color:#115E59;",
      "classDef incidence fill:#F3E8FF,stroke:#7C3AED,stroke-width:1px,color:#4C1D95;",
      
      "classDef mild     fill:#E8F5E9,stroke:#2E7D32,stroke-width:1px,color:#1B5E20;",
      "classDef mod      fill:#FFF8E1,stroke:#F59E0B,stroke-width:1px,color:#7C2D12;",
      "classDef sev      fill:#FDECEA,stroke:#C62828,stroke-width:1px,color:#7F1D1D;",
      
      "classDef recovery fill:#E6FFED,stroke:#22C55E,stroke-width:1px,color:#166534;",
      "classDef partial  fill:#FFF7ED,stroke:#F97316,stroke-width:1px,color:#9A3412;",
      "classDef chronic  fill:#F3F4F6,stroke:#6B7280,stroke-width:1px,color:#111827;",
      
      "classDef one      fill:#EFF6FF,stroke:#3B82F6,stroke-width:1px,color:#1E40AF;",
      "classDef relapse  fill:#FEF3C7,stroke:#F59E0B,stroke-width:1px,color:#92400E;",
      
      # (Optional) default link look
      "linkStyle default stroke:#94A3B8,stroke-width:1px,opacity:0.6;",
      
      # --- Assign classes to nodes ---
      "class Pop source;",
      "class Inc incidence;",
      
      "class Mild mild;  class Mod mod;  class Sev sev;",
      
      "class RecM,RecMo,RecS recovery;",
      "class ParM,ParMo,ParS partial;",
      "class ChrM,ChrMo,ChrS chronic;",
      
      "class OneM,OnePM,OneMo,OnePMo,OneS,OnePS one;",
      "class RelM,RelPM,RelMo,RelPMo,RelS,RelPS relapse;",
      sep = "\n")
  })
  
  ## ---------- 5) Render ----------
  output$transition_tree <- DiagrammeR::renderDiagrammeR({
    DiagrammeR::mermaid(mermaid_text())  # keep 'graph LR' for DiagrammeR compatibility
  })
  
  
  ce_draws <- reactiveVal(NULL)           
  wtp      <- reactive({ as.numeric(input$wtp_per_qaly) })
  
  # Incrementals for plots
  ce_df <- reactive({
    d <- ce_draws(); req(d)
    data.frame(dE = d$delta_QALYs, dC = d$delta_cost)
  })
  
  # Summary stats
  ce_stats <- reactive({
    df <- ce_df()
    m_dC <- mean(df$dC, na.rm = TRUE)
    m_dE <- mean(df$dE, na.rm = TRUE)
    sd_dC <- sd(df$dC, na.rm = TRUE)
    sd_dE <- sd(df$dE, na.rm = TRUE)
    icer <- if (isTRUE(all.equal(m_dE, 0))) NA_real_ else m_dC / m_dE
    pr_ce <- mean(wtp() * df$dE - df$dC > 0, na.rm = TRUE)
    list(m_dC = m_dC, sd_dC = sd_dC, m_dE = m_dE, sd_dE = sd_dE, icer = icer, pr_ce = pr_ce)
  })
  
  # CE plane
  output$plot_ce_plane <- renderPlot({
    df <- ce_df(); λ <- wtp()
    ggplot2::ggplot(df, ggplot2::aes(x = dE, y = dC)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "#94A3B8") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, color = "#94A3B8") +
      ggplot2::geom_point(alpha = 0.35, size = 1.8, color = "#E11D2E") +
      ggplot2::geom_abline(slope = λ, intercept = 0, color = "#0057B8", linewidth = 0.6) +
      ggplot2::labs(x = "Incremental effects (ΔQALY)", y = "Incremental costs (Δ€)",
                    subtitle = sprintf("WTP line: y = %.0f · x", λ))
  })
  
  # Summary table
  output$tbl_ce_summary <- renderTable({
    s <- ce_stats()
    data.frame(
      `Mean ΔCost (€)` = round(s$m_dC, 2),
      `Mean ΔQALY`     = round(s$m_dE, 6),
      `SD ΔCost (€)`  =  round(s$sd_dC, 2),
      `SD ΔQALY`      = round(s$sd_dE, 6),
      `ICER (€/QALY)`  = if (is.na(s$icer)) "NA" else format(round(s$icer, 2), big.mark = ","),
      `Pr(CE at WTP)`  = sprintf("%.1f%%", 100 * s$pr_ce),
      check.names = FALSE
    )
  }, bordered = TRUE, striped = TRUE, hover = TRUE, align = "c")
  
  # CEAC
  ceac_df <- reactive({
    df <- ce_df()
    W <- seq(0, 100000, by = 1000)
    prob <- vapply(W, function(w) mean(w * df$dE - df$dC > 0), numeric(1))
    data.frame(WTP = W, ProbCE = prob)
  })
  output$plot_ceac <- renderPlot({
    d <- ceac_df()
    ggplot2::ggplot(d, ggplot2::aes(x = WTP, y = ProbCE)) +
      ggplot2::geom_line(linewidth = 0.9, color = "#E11D2E") +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", color = "#94A3B8") +
      ggplot2::geom_vline(xintercept = wtp(), linetype = "dashed", color = "#0057B8") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      ggplot2::labs(x = "Willingness to pay (€/QALY)", y = "Pr(cost-effective)",
                    subtitle = sprintf("Current WTP: €%s", format(wtp(), big.mark = ",")))
  })
  
  # Quadrant shares
  output$tbl_quadrants <- renderTable({
    df <- ce_df()
    quad <- ifelse(df$dE >= 0 & df$dC >= 0, "NE (more effective, more costly)",
                   ifelse(df$dE >= 0 & df$dC <  0, "SE (more effective, less costly)",
                          ifelse(df$dE <  0 & df$dC >= 0, "NW (less effective, more costly)",
                                 "SW (less effective, less costly)")))
    prop <- round(100 * prop.table(table(quad)), 1)
    data.frame(Quadrant = names(prop), Share = paste0(prop, " %"), check.names = FALSE)
  }, bordered = TRUE, striped = TRUE, align = "l")
  
  # Your full draws table (DT)
  output$def_df_table <- DT::renderDT({
    d <- ce_draws(); req(d)
    DT::datatable(d, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$download_def_df <- downloadHandler(
    filename = function() paste0("ce_draws-", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(ce_draws(), file, row.names = FALSE)
  )
  
  # THE MODEL
  
  
  first_part_model_trans <- reactiveVal(NULL)
  def_table <- reactiveVal(NULL)
  
  observeEvent(input$run_model, {
    
    # (optional) show a simple "running" popup
    showModal(modalDialog("Running model…", easyClose = FALSE, footer = NULL))
    
    # Build args safely; req() ensures they exist; as.numeric() avoids type hiccups
    args_first <- list(
      death_rate           = req(as.numeric(input$death_rate)),
      retirement_rate      = req(as.numeric(input$retire_rate)),
      excess_mortality     = req(as.numeric(input$exces_mort)),
      increased_relapse_1  = req(as.numeric(input$incr_relapse_1)),
      increased_relapse_2  = req(as.numeric(input$incr_relapse_2)),
      increased_relapse_3  = req(as.numeric(input$incr_relapse_3)),
      increased_relapse_4  = req(as.numeric(input$incr_relapse_4)),
      increased_relapse_5  = req(as.numeric(input$incr_relapse_5)),
      mean_dur_chron       = req(as.numeric(input$mean_duration_chron)),
      
      # make this robust too (and numeric)
      leavemodel = req({
        (1 - as.numeric(input$death_rate) * as.numeric(input$exces_mort)) *
          (1 - as.numeric(input$retire_rate))
      }),
      
      incidence_no_history = req(as.numeric(vals$pop_inc)),
      pmild                = req(as.numeric(vals$inc_mild)),
      pmoderate            = req(as.numeric(vals$inc_mod)),
      psevere              = req(as.numeric(vals$inc_sev)),
      
      mildrecovery         = req(as.numeric(vals$mild_rec)),
      mildpartial          = req(as.numeric(vals$mild_part)),
      mildchronic          = req(as.numeric(vals$mild_chr)),
      
      moderaterecovery     = req(as.numeric(vals$mod_rec)),
      moderatepartial      = req(as.numeric(vals$mod_part)),
      moderatechronic      = req(as.numeric(vals$mod_chr)),
      
      severerecovery       = req(as.numeric(vals$sev_rec)),
      severepartial        = req(as.numeric(vals$sev_part)),
      severechronic        = req(as.numeric(vals$sev_chr)),
      
      mildrecoverycured      = req(as.numeric(vals$mildrec_one)),
      mildrecoveryrelapse    = req(as.numeric(vals$mildrec_rel)),
      mildpartialcured       = req(as.numeric(vals$mildpart_one)),
      mildpartialrelapse     = req(as.numeric(vals$mildpart_rel)),
      
      moderaterecoverycured  = req(as.numeric(vals$modrec_one)),
      moderaterecoveryrelapse= req(as.numeric(vals$modrec_rel)),
      moderatepartialcured   = req(as.numeric(vals$modpart_one)),
      moderatepartialrelapse = req(as.numeric(vals$modpart_rel)),
      
      severerecoverycured    = req(as.numeric(vals$sevrec_one)),
      severerecoveryrelapse  = req(as.numeric(vals$sevrec_rel)),
      severepartialcured     = req(as.numeric(vals$sevpart_one)),
      severepartialrelapse   = req(as.numeric(vals$sevpart_rel))
    )
    
    
    # Run your function with named args
    tm <- do.call(func_first_part_model, args_first)

    
    # Run the second part of the model, first for base
    res_base <- fun_sim_model(
      transition_matrix        = tm,  
      sim_runs                 = req(as.integer(input$sim)),
      total_population         = req(as.integer(input$n_pat)),
      df_prev_sub              = df_prev_sub_base(),
      df_tr_mild               = df_treat_mild_base(),
      df_tr_mod                = df_treat_mod_base(),
      df_tr_sev                = df_treat_sev_base(),
      df_prev_rec              = df_prev_rec_base(),
      dw_conversion_fact        = req(as.numeric(input$dw_conversion_fct)),   
      tr_mild_profilactic      = treat_mild_base_profilactic(),              
      tr_mod_profilactic       = treat_mod_base_profilactic(),
      tr_sev_profilactic       = treat_sev_base_profilactic(),
      discount_rate_daly       = req(as.numeric(input$discount_rate_daly_av)),
      scale_shape_gamma_cost   = req(as.numeric(input$scale_shape_gamma)),     
      disc_rate_cost           = req(as.numeric(input$discount_rate_cost)),
      increased_relapse_1      = req(as.numeric(input$incr_relapse_1)),
      increased_relapse_2      = req(as.numeric(input$incr_relapse_2)),
      increased_relapse_3      = req(as.numeric(input$incr_relapse_3)),
      increased_relapse_4      = req(as.numeric(input$incr_relapse_4)),
      increased_relapse_5      = req(as.numeric(input$incr_relapse_5)),
      leavemodel               = (1 - input$death_rate * input$exces_mort) * (1 - input$retire_rate),
      mean_dur_chron           =  req(as.numeric(input$mean_duration_chron)),
      incidence_no_history = req(as.numeric(vals$pop_inc)),
      pmild = req(as.numeric(vals$inc_mild)),
      pmoderate = req(as.numeric(vals$inc_mod)),
      psevere = req(as.numeric(vals$inc_sev)),
      mildrecovery = req(as.numeric(vals$mild_rec)),
      mildpartial = req(as.numeric(vals$mild_part)),
      mildchronic = req(as.numeric(vals$mild_chr)),
      moderaterecovery = req(as.numeric(vals$mod_rec)),
      moderatepartial = req(as.numeric(vals$mod_part)),
      moderatechronic = req(as.numeric(vals$mod_chr)),
      severerecovery = req(as.numeric(vals$sev_rec)),
      severepartial = req(as.numeric(vals$sev_part)),
      severechronic = req(as.numeric(vals$sev_chr)),
      mildrecoverycured = req(as.numeric(vals$mildrec_one)),
      mildrecoveryrelapse = req(as.numeric(vals$mildrec_rel)),
      mildpartialcured = req(as.numeric(vals$mildpart_one)),
      mildpartialrelapse = req(as.numeric(vals$mildpart_rel)),
      moderaterecoverycured = req(as.numeric(vals$modrec_one)),
      moderaterecoveryrelapse = req(as.numeric(vals$modrec_rel)),
      moderatepartialcured = req(as.numeric(vals$modpart_one)),
      moderatepartialrelapse = req(as.numeric(vals$modpart_rel)),
      severerecoverycured = req(as.numeric(vals$sevrec_one)),
      severerecoveryrelapse = req(as.numeric(vals$sevrec_rel)),
      severepartialcured =  req(as.numeric(vals$sevpart_one)),
      severepartialrelapse = req(as.numeric(vals$sevpart_rel))
    )
    
    res_alt <- fun_sim_model(
      transition_matrix        = tm,  
      sim_runs                 = req(as.integer(input$sim)),
      total_population         = req(as.integer(input$n_pat)),
      df_prev_sub              = df_prev_sub_alt(),
      df_tr_mild               = df_treat_mild_alt(),
      df_tr_mod                = df_treat_mod_alt(),
      df_tr_sev                = df_treat_sev_alt(),
      df_prev_rec              = df_prev_rec_alt(),
      dw_conversion_fact        = req(as.numeric(input$dw_conversion_fct)),   
      tr_mild_profilactic      = treat_mild_alt_profilactic(),              
      tr_mod_profilactic       = treat_mod_alt_profilactic(),
      tr_sev_profilactic       = treat_sev_alt_profilactic(),
      discount_rate_daly       = req(as.numeric(input$discount_rate_daly_av)),
      scale_shape_gamma_cost   = req(as.numeric(input$scale_shape_gamma)),     
      disc_rate_cost           = req(as.numeric(input$discount_rate_cost)),
      increased_relapse_1      =    req(as.numeric(input$incr_relapse_1)),
      increased_relapse_2      = req(as.numeric(input$incr_relapse_2)),
      increased_relapse_3      = req(as.numeric(input$incr_relapse_3)),
      increased_relapse_4      = req(as.numeric(input$incr_relapse_4)),
      increased_relapse_5      = req(as.numeric(input$incr_relapse_5)),
      leavemodel           = (1 - input$death_rate * input$exces_mort) * (1 - input$retire_rate),
      mean_dur_chron          =  req(as.numeric(input$mean_duration_chron)),
      incidence_no_history = req(as.numeric(vals$pop_inc)),
      pmild = req(as.numeric(vals$inc_mild)),
      pmoderate = req(as.numeric(vals$inc_mod)),
      psevere = req(as.numeric(vals$inc_sev)),
      mildrecovery = req(as.numeric(vals$mild_rec)),
      mildpartial = req(as.numeric(vals$mild_part)),
      mildchronic = req(as.numeric(vals$mild_chr)),
      moderaterecovery = req(as.numeric(vals$mod_rec)),
      moderatepartial = req(as.numeric(vals$mod_part)),
      moderatechronic = req(as.numeric(vals$mod_chr)),
      severerecovery = req(as.numeric(vals$sev_rec)),
      severepartial = req(as.numeric(vals$sev_part)),
      severechronic = req(as.numeric(vals$sev_chr)),
      mildrecoverycured = req(as.numeric(vals$mildrec_one)),
      mildrecoveryrelapse = req(as.numeric(vals$mildrec_rel)),
      mildpartialcured = req(as.numeric(vals$mildpart_one)),
      mildpartialrelapse = req(as.numeric(vals$mildpart_rel)),
      moderaterecoverycured = req(as.numeric(vals$modrec_one)),
      moderaterecoveryrelapse = req(as.numeric(vals$modrec_rel)),
      moderatepartialcured = req(as.numeric(vals$modpart_one)),
      moderatepartialrelapse = req(as.numeric(vals$modpart_rel)),
      severerecoverycured = req(as.numeric(vals$sevrec_one)),
      severerecoveryrelapse = req(as.numeric(vals$sevrec_rel)),
      severepartialcured =  req(as.numeric(vals$sevpart_one)),
      severepartialrelapse = req(as.numeric(vals$sevpart_rel))
    )
    
  
    
    # 4) Avoid cbind of big matrices; compute incrementals directly
    cost_base  <- as.numeric(res_base[, "Cost"])
    qaly_base  <- as.numeric(res_base[, "QALYs"])
    cost_alt   <- as.numeric(res_alt[,  "Cost"])
    qaly_alt   <- as.numeric(res_alt[,  "QALYs"])
    
    d_cost  <- cost_alt - cost_base
    d_qaly  <- qaly_alt - qaly_base
    icer    <- d_cost / d_qaly; icer[!is.finite(icer)] <- NA_real_
    lambda  <- as.numeric(input$wtp_per_qaly)
    inc_nb  <- lambda * d_qaly - d_cost
    quad    <- ifelse(d_cost > 0 & d_qaly < 0, 1L,
                      ifelse(d_cost > 0 & d_qaly > 0, 2L,
                             ifelse(d_cost < 0 & d_qaly > 0, 3L,
                                    ifelse(d_cost < 0 & d_qaly < 0, 4L, NA_integer_))))
    
    # Build a lightweight data.frame (no intermediate def_array)
    def_df <- data.frame(
      Cost_base   = cost_base,
      QALYs_base  = qaly_base,
      Cost_alt    = cost_alt,
      QALYs_alt   = qaly_alt,
      delta_cost  = d_cost,
      delta_QALYs = d_qaly,
      ICER        = icer,
      NB          = inc_nb,
      quadrant    = quad,
      check.names = FALSE
    )
    
    ce_draws(def_df)   
    
    isolate(ce_draws(def_df))
    
    removeModal()
    
    showModal(modalDialog(
      title = "Cost-effectiveness results",
      size  = "l",
      easyClose = TRUE,
      bslib::navset_tab(
        # Tab 1: CE plane + summary
        bslib::nav_panel("CE plane & summary",
                         bslib::card(
                           bslib::card_header("Cost-effectiveness plane"),
                           bslib::card_body(plotOutput("plot_ce_plane", height = 380))
                         ),
                         bslib::card(
                           bslib::card_header("Summary (incrementals)"),
                           bslib::card_body(tableOutput("tbl_ce_summary"))
                         )
        ),
        # Tab 2: Uncertainty
        bslib::nav_panel("Uncertainty",
                         bslib::card(
                           bslib::card_header("ICER acceptability curve"),
                           bslib::card_body(plotOutput("plot_ceac", height = 360))
                         ),
                         bslib::card(
                           bslib::card_header("Quadrant distribution"),
                           bslib::card_body(tableOutput("tbl_quadrants"))
                         )
        ),
        # Tab 3: Your existing draws table
        bslib::nav_panel("Draws table",
                         bslib::card(
                           bslib::card_header(
                             div(class="d-flex align-items-center justify-content-between w-100",
                                 span("Simulated draws"),
                                 downloadButton("download_def_df", "Download CSV", class = "btn btn-outline-secondary"))
                           ),
                           bslib::card_body(DT::DTOutput("def_df_table"))
                         )
        )
      )
    ))
  })
  
  
  
  
  
    
  
}

                  
                  
  
                        
                        

# Define server logic required to draw a histogram


# Run the application 
shinyApp(ui = ui, server = server)

