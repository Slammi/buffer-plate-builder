# Buffer Plate Builder: DoE design + stocks -> per-well pipetting volumes.
# Run with shiny::runApp() from this folder. Core logic lives in R/ (sourced
# automatically by Shiny) and is tested in tests/.

library(shiny)
library(bslib)
library(rhandsontable)
library(DT)
library(ggplot2)

if (!exists("build_plate")) {
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
}

EXAMPLE_DESIGN <- file.path("tests", "fixtures", "ASR4_design_PEG24.csv")
EXAMPLE_STOCKS <- file.path("tests", "fixtures", "ASR4_stocks.csv")

# Round numeric columns for display.
round_df <- function(df, digits = 2) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  df
}

dt <- function(df, pct = NULL, digits = 2, page = 25, ...) {
  out <- datatable(round_df(df, 6), rownames = FALSE, filter = "top",
                   options = list(pageLength = page, scrollX = TRUE), ...)
  num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))],
                 c(pct, "run", "replicate", "plate", "row", "col", "step", "wells",
                   "destination_plate", "runs", "plates", "ok", "over_volume", "below_min"))
  if (length(num)) out <- formatRound(out, num, digits)
  if (length(pct)) out <- formatPercentage(out, intersect(pct, names(df)), 1)
  if ("status" %in% names(df)) {
    out <- formatStyle(out, "status", backgroundColor = styleEqual(
      c("OK", "OVER VOLUME", "BELOW MIN"), c("#d9f2d9", "#f8d0d0", "#fde5c0")))
  }
  out
}

ui <- page_sidebar(
  title = "Buffer Plate Builder",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 330,
    accordion(
      open = c("Design", "Settings"),
      accordion_panel(
        "Design",
        fileInput("design_file", "Design CSV", accept = ".csv"),
        actionButton("load_example", "Load ASR4 example", class = "btn-sm btn-outline-secondary mb-2"),
        radioButtons("orientation", "Orientation",
                     c("Auto-detect" = "auto", "Runs in rows" = "runs_in_rows",
                       "Runs in columns" = "runs_in_columns"), inline = FALSE),
        uiOutput("design_info"),
        selectizeInput("components", "Columns that are components", choices = NULL,
                       multiple = TRUE, options = list(plugins = list("remove_button")))
      ),
      accordion_panel(
        "Stocks file",
        fileInput("stocks_file", "Load stock CSV", accept = ".csv"),
        downloadButton("dl_stocks", "Save stock CSV", class = "btn-sm")
      ),
      accordion_panel(
        "Settings",
        numericInput("final_volume", "Final well volume (µL)", 100, min = 1),
        numericInput("fixed_volume", "Fixed volume per well (µL)", 0, min = 0),
        helpText("Enzyme / template / master mix, anything common not in the stock table."),
        numericInput("min_volume", "Minimum pipettable volume (µL)", 0.5, min = 0, step = 0.1),
        numericInput("overage", "Stock overage (fraction)", 0.15, min = 0, step = 0.05),
        numericInput("dead_volume", "Dead volume per reservoir (µL)", 20, min = 0)
      ),
      accordion_panel(
        "Plate layout",
        radioButtons("plate_format", "Plate format", c("96 (8 × 12)" = 96, "384 (16 × 24)" = 384), inline = TRUE),
        radioButtons("fill", "Fill direction", c("Row-wise (A1, A2 …)" = "row", "Column-wise (A1, B1 …)" = "column")),
        numericInput("replicates", "Replicates per run", 1, min = 1, step = 1),
        checkboxInput("exclude_edges", "Exclude edge wells", FALSE),
        textInput("reserved_wells", "Reserved / control wells", placeholder = "e.g. H11, H12"),
        checkboxInput("randomize", "Randomize run order", FALSE),
        conditionalPanel("input.randomize", numericInput("seed", "Seed", 1, step = 1))
      )
    ),
    downloadButton("dl_excel", "Download Excel workbook", class = "btn-primary")
  ),
  navset_card_tab(
    id = "tabs",
    nav_panel(
      "Stocks",
      div(class = "alert alert-warning py-2",
          strong("Units are not converted."),
          " Each stock (and final_conc for constants) must be in the same units as the design values for that component."),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Stock table (edit live; right-click to add/remove rows)"),
             rHandsontableOutput("stocks_hot"),
             div(class = "mt-2", actionButton("add_constant", "Add constant component", class = "btn-sm"))),
        card(card_header("Per-component check"), DTOutput("component_check"))
      ),
      uiOutput("stock_problems")
    ),
    nav_panel(
      "Feasibility",
      uiOutput("value_boxes"),
      card(card_header("Feasibility rule"), uiOutput("rule")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Failing wells"), DTOutput("failing")),
        card(card_header("Minimum stock per component (others held fixed)"), DTOutput("min_stocks"))
      ),
      card(
        card_header("Optimiser: stocks within max_stock_conc that make every well fit"),
        p(class = "text-muted small",
          "Stocks are only made more concentrated, never above max_stock_conc (no max = unlimited), ",
          "changing as little as possible. Working stocks are ignored."),
        selectizeInput("hold", "Keep these stocks unchanged", choices = NULL, multiple = TRUE),
        actionButton("optimise", "Run optimiser", class = "btn-sm btn-primary"),
        uiOutput("opt_message"),
        DTOutput("opt_table"),
        uiOutput("opt_apply")
      )
    ),
    nav_panel(
      "Plate maps",
      layout_columns(col_widths = c(4, 8),
                     selectInput("map_value", "Show", choices = NULL),
                     div(class = "small text-muted mt-4",
                         "Red outline = OVER VOLUME, orange = BELOW MIN. Blank wells are unused.")),
      plotOutput("plate_map", height = "auto")
    ),
    nav_panel(
      "Volumes",
      div(class = "mb-2", downloadButton("dl_volumes", "Download volumes CSV", class = "btn-sm")),
      DTOutput("volumes")
    ),
    nav_panel(
      "Stock prep",
      p(class = "text-muted small", "Total to prepare = total dispensed × (1 + overage) + dead volume."),
      DTOutput("stock_prep")
    ),
    nav_panel(
      "Picklist",
      selectizeInput("addition_order", "Order of addition (drag to reorder)", choices = NULL,
                     multiple = TRUE, width = "100%",
                     options = list(plugins = list("drag_drop"))),
      checkboxInput("include_failed", "Include OVER VOLUME wells", FALSE),
      div(class = "mb-2", downloadButton("dl_picklist", "Download picklist CSV", class = "btn-sm")),
      DTOutput("picklist")
    )
  )
)

server <- function(input, output, session) {
  design_source <- reactiveVal(NULL)
  stocks_seed <- reactiveVal(NULL)   # what the stock grid is (re)rendered from
  optimised <- reactiveVal(NULL)

  observeEvent(input$design_file, design_source(input$design_file$datapath))
  observeEvent(input$load_example, {
    design_source(EXAMPLE_DESIGN)
    stocks_seed(read_stocks(EXAMPLE_STOCKS))
  })

  design <- reactive({
    req(design_source())
    tryCatch(read_design(design_source(), input$orientation),
             error = function(e) structure(list(error = conditionMessage(e)), class = "design_error"))
  })

  observeEvent(design(), {
    d <- design()
    if (inherits(d, "design_error")) {
      updateSelectizeInput(session, "components", choices = character(), selected = character())
    } else {
      updateSelectizeInput(session, "components", choices = d$columns, selected = d$components)
    }
  })

  output$design_info <- renderUI({
    d <- design()
    if (inherits(d, "design_error")) return(div(class = "text-danger small", d$error))
    det <- c(runs_in_rows = "runs in rows", runs_in_columns = "runs in columns")
    div(class = "small text-muted mb-2",
        sprintf("%d runs. Detected: %s.", nrow(design_matrix(d, d$components)),
                det[[d$detected_orientation]]),
        if (length(setdiff(d$columns, d$components)))
          tagList(br(), "Excluded (responses / IDs): ", paste(setdiff(d$columns, d$components), collapse = ", ")))
  })

  components <- reactive({
    d <- design()
    req(!inherits(d, "design_error"), length(input$components) > 0)
    intersect(input$components, d$columns)
  })

  # Current stock table: the latest seed, then every edit made in the grid.
  current_stocks <- reactiveVal(NULL)
  observeEvent(stocks_seed(), current_stocks(stocks_seed()))
  observeEvent(input$stocks_hot, current_stocks(normalise_stocks(hot_to_r(input$stocks_hot))))
  stocks <- reactive(req(current_stocks()))

  # Re-seed the grid when the component set changes, keeping matching rows.
  observeEvent(components(), {
    existing <- isolate(current_stocks())
    seeded <- default_stocks(components(), existing)
    if (is.null(existing) || !identical(seeded$component, existing$component)) stocks_seed(seeded)
  })

  observeEvent(input$stocks_file, {
    loaded <- read_stocks(input$stocks_file$datapath)
    stocks_seed(if (is.null(design_source())) loaded else default_stocks(isolate(components()), loaded))
  })

  observeEvent(input$add_constant, {
    s <- stocks()
    new <- normalise_stocks(data.frame(component = paste0("Constant", nrow(s) + 1), role = "constant"))
    stocks_seed(rbind(s, new))
  })

  output$stocks_hot <- renderRHandsontable({
    s <- req(stocks_seed())
    rhandsontable(s, rowHeaders = NULL, stretchH = "all", height = max(200, 32 * nrow(s) + 40)) |>
      hot_col("role", type = "dropdown", source = c("factor", "constant"), strict = TRUE) |>
      hot_col(c("stock_conc", "final_conc", "working_stock_conc", "max_stock_conc"), format = "0.[0000]") |>
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })

  settings <- reactive({
    list(
      final_volume = req(input$final_volume), fixed_volume = input$fixed_volume %||% 0,
      min_volume = input$min_volume %||% 0, overage = input$overage %||% 0,
      dead_volume = input$dead_volume %||% 0, plate_format = as.integer(input$plate_format),
      fill = input$fill, exclude_edges = input$exclude_edges,
      reserved_wells = input$reserved_wells %||% "", randomize = input$randomize,
      seed = input$seed %||% 1, replicates = max(1L, as.integer(input$replicates %||% 1))
    )
  })

  result <- reactive({
    d <- design()
    req(!inherits(d, "design_error"))
    tryCatch(build_plate(d, stocks(), settings(), components = components()),
             error = function(e) structure(list(error = conditionMessage(e)), class = "build_error"))
  })

  res <- reactive({
    r <- result()
    validate(need(!inherits(r, "build_error"), if (inherits(r, "build_error")) r$error))
    r
  })

  # ---- Stocks tab
  output$component_check <- renderDT({
    s <- stocks()
    probs <- validate_stocks(s, components())
    r <- result()
    tab <- data.frame(component = s$component, role = s$role, stringsAsFactors = FALSE)
    if (!inherits(r, "build_error")) {
      cv <- r$concentrations
      max_share <- tapply(cv$volume, cv$component, max) / r$settings$final_volume
      ms <- r$min_stocks
      i <- match(tolower(tab$component), tolower(ms$component))
      tab$max_share <- unname(max_share[ms$component[i]])
      tab$min_stock <- ms$min_stock_needed[i]
      tab$check <- ifelse(is.na(i), "",
                   ifelse(ms$exceeds_max[i], "min stock > max",
                   ifelse(ms$fits_now[i] | is.na(ms$min_stock_needed[i]), "OK",
                   ifelse(is.infinite(ms$min_stock_needed[i]), "can't fix alone", "stock too dilute"))))
    }
    tab$problem <- vapply(tolower(tab$component), function(cmp) {
      paste(probs$problem[tolower(probs$component) == cmp], collapse = "; ")
    }, character(1))
    out <- datatable(round_df(tab, 4), rownames = FALSE,
                     colnames = c("component", "role", if (!inherits(r, "build_error"))
                       c("max % of well", "min stock", "check"), "problem"),
                     options = list(dom = "t", pageLength = 100, ordering = FALSE))
    if ("max_share" %in% names(tab)) {
      out <- formatPercentage(out, "max_share", 1) |>
        formatSignif("min_stock", 4) |>
        formatStyle("check", color = styleEqual(c("OK"), c("green"), default = "firebrick"))
    }
    out
  })

  output$stock_problems <- renderUI({
    r <- result()
    if (inherits(r, "build_error")) div(class = "alert alert-danger", style = "white-space: pre-wrap", r$error)
  })

  # ---- Feasibility tab
  output$value_boxes <- renderUI({
    sm <- res()$summary
    layout_column_wrap(
      width = "170px", fill = FALSE, heights_equal = "row",
      value_box("OK wells", sm$ok, theme = "success"),
      value_box("OVER VOLUME", sm$over_volume, theme = if (sm$over_volume) "danger" else "secondary"),
      value_box("BELOW MIN", sm$below_min, theme = if (sm$below_min) "warning" else "secondary"),
      value_box("Worst well, used before water", sprintf("%.1f%%", 100 * sm$worst_fraction),
                p(sprintf("Run %d", sm$worst_run)),
                theme = if (sm$worst_fraction > 1) "danger" else "primary"),
      value_box("Headroom in most crowded well",
                sprintf("%.1f µL", sm$headroom_uL),
                p(sprintf("%.1f%% of the well; %d well(s) on %d plate(s)",
                          100 * sm$headroom_fraction, sm$wells, sm$plates)),
                theme = if (sm$headroom_uL < 0) "danger" else "info")
    )
  })

  output$rule <- renderUI({
    r <- res()
    s <- r$settings
    tagList(
      p("A well fits only if", tags$code("Σ (C_i / Stock_i) + fixed / V_total ≤ 1"), "."),
      p("V_total cancels out: changing the final well volume never makes a well fit. ",
        "Only more concentrated stocks, a smaller fixed volume or narrower design ranges help."),
      p(sprintf("Here: fixed / V_total = %.1f%%; worst well Σ = %.1f%%.",
                100 * s$fixed_volume / s$final_volume, 100 * r$summary$worst_fraction))
    )
  })

  output$failing <- renderDT({
    f <- res()$failing
    dt(f, pct = c("fraction_used", "largest_share"), page = 10)
  })

  output$min_stocks <- renderDT({
    m <- res()$min_stocks
    m <- m[, c("component", "units", "stock_conc", "min_stock_needed", "max_stock_conc",
               "fits_now", "exceeds_max", "wells_blocked_by_others")]
    datatable(m, rownames = FALSE, options = list(dom = "t", pageLength = 100, scrollX = TRUE)) |>
      formatSignif(c("stock_conc", "min_stock_needed", "max_stock_conc"), 4) |>
      formatStyle("fits_now", color = styleEqual(c(TRUE, FALSE), c("green", "firebrick")))
  })

  observeEvent(stocks(), {
    updateSelectizeInput(session, "hold", choices = stocks()$component,
                         selected = intersect(isolate(input$hold), stocks()$component))
  })
  observeEvent(list(stocks(), settings(), components()), optimised(NULL), ignoreInit = TRUE)
  observeEvent(input$optimise, {
    optimised(tryCatch(optimise_stocks(res(), hold = input$hold),
                       error = function(e) list(feasible = FALSE, message = conditionMessage(e), stocks = NULL)))
  })
  output$opt_message <- renderUI({
    o <- req(optimised())
    div(class = paste("alert mt-2", if (o$feasible) "alert-success" else "alert-danger"), o$message)
  })
  output$opt_table <- renderDT({
    o <- req(optimised())
    req(o$feasible)
    datatable(o$stocks, rownames = FALSE, options = list(dom = "t", pageLength = 100)) |>
      formatSignif(c("stock_conc", "max_stock_conc", "suggested_stock", "suggested_rounded"), 4) |>
      formatStyle("changed", fontWeight = styleEqual(TRUE, "bold"))
  })
  output$opt_apply <- renderUI({
    o <- req(optimised())
    if (isTRUE(o$feasible)) actionButton("apply_opt", "Apply rounded stocks to the stock table", class = "btn-sm mt-2")
  })
  observeEvent(input$apply_opt, {
    o <- req(optimised())
    s <- stocks()
    i <- match(tolower(s$component), tolower(o$stocks$component))
    s$stock_conc[!is.na(i)] <- o$stocks$suggested_rounded[i[!is.na(i)]]
    stocks_seed(s)
    optimised(NULL)
  })

  # ---- Plate maps
  observeEvent(res(), {
    ch <- plate_map_choices(res())
    names(ch) <- ifelse(ch == "fraction_used", "% used before water",
                 ifelse(ch == "water", "Water", ifelse(ch == "fixed", "Fixed mix", ch)))
    sel <- if (isTRUE(input$map_value %in% ch)) input$map_value else ch[[1]]
    updateSelectInput(session, "map_value", choices = ch, selected = sel)
  })
  map_height <- reactive({
    r <- res()
    per_plate <- if (r$settings$plate_format == 384) 560 else 420
    per_plate * max(r$volumes$plate)
  })
  output$plate_map <- renderPlot({
    r <- res()
    req(input$map_value %in% plate_map_choices(r))
    plot_plate_map(r, input$map_value)
  }, height = function() map_height())

  # ---- Volumes / stock prep / picklist
  output$volumes <- renderDT({
    v <- res()$volumes
    dt(v[, setdiff(names(v), c("row", "col"))], pct = "fraction_used")
  })
  output$stock_prep <- renderDT(dt(res()$stock_prep, page = 50))

  observeEvent(res(), {
    current <- isolate(input$addition_order)
    def <- res()$addition_order
    if (!setequal(current, def)) updateSelectizeInput(session, "addition_order", choices = def, selected = def)
  })
  addition_order <- reactive({
    def <- res()$addition_order
    ord <- input$addition_order
    c(intersect(ord, def), setdiff(def, ord))
  })
  picklist_table <- reactive(picklist(res()$transfers, addition_order(), input$include_failed))
  output$picklist <- renderDT(dt(picklist_table(), page = 50))

  # ---- Downloads
  output$dl_stocks <- downloadHandler("stocks.csv", function(file) write_stocks(stocks(), file))
  output$dl_volumes <- downloadHandler("volumes.csv", function(file) {
    utils::write.csv(res()$volumes, file, row.names = FALSE)
  })
  output$dl_picklist <- downloadHandler("picklist.csv", function(file) {
    utils::write.csv(picklist_table(), file, row.names = FALSE)
  })
  output$dl_excel <- downloadHandler(
    function() paste0("buffer_plate_", format(Sys.Date()), ".xlsx"),
    function(file) {
      r <- res()
      r$stock_prep <- r$stock_prep[match(addition_order(), r$stock_prep$source), ]
      export_excel(r, file, picklist_table())
    }
  )
}

shinyApp(ui, server)
