# Buffer Plate Builder: DoE design + stocks -> per-well pipetting volumes.
# Run with shiny::runApp() from this folder. Core logic lives in R/ (sourced
# automatically by Shiny) and is tested in tests/.
#
# The Inputs page mirrors the Inputs tab of the ASR4 Excel calculator: yellow
# cells with blue text are inputs, everything else is calculated.

library(shiny)
library(bslib)
library(DT)
library(ggplot2)

if (!exists("build_plate")) {
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
}

EXAMPLE_DESIGN <- file.path("tests", "fixtures", "ASR4_design_PEG24.csv")
EXAMPLE_STOCKS <- file.path("tests", "fixtures", "ASR4_stocks.csv")

XL_CSS <- "
.xl-title { font-size: 1.35rem; font-weight: 700; margin: 0; }
.xl-sub { color: #595959; font-style: italic; font-size: .9rem; margin-bottom: 1rem; }
.xl-section { font-size: 1.05rem; font-weight: 700; margin: 1.2rem 0 .4rem; }
table.xl { border-collapse: collapse; font-size: .85rem; margin-bottom: .5rem;
  width: auto !important; align-self: flex-start; }
.xl-wrap { align-self: flex-start; max-width: 100%; overflow-x: auto; }
table.xl th, table.xl td { border: 1px solid #BFBFBF; padding: 2px 6px; vertical-align: middle; }
table.xl th { background: #D9E1F2; font-weight: 700; white-space: normal; text-align: left; max-width: 9rem; }
table.xl td.name { font-weight: 700; white-space: nowrap; }
table.xl td.calc { text-align: right; white-space: nowrap; }
table.xl td.check { white-space: nowrap; }
table.xl td.in { background: #FFFF99; padding: 0; }
table.xl td.ok { background: #C6EFCE; }
table.xl td.warn { background: #FFE699; }
table.xl td.bad { background: #FFC7CE; }
table.xl-set td { border: none; padding: 2px 8px 2px 0; }
table.xl-set td.in { border: 1px solid #BFBFBF; }
table.xl .form-group { margin: 0; width: auto !important; }
table.xl input.form-control { background: #FFFF99; color: #0000FF; border: none; border-radius: 0;
  padding: 1px 4px; height: auto; min-height: 0; font-size: .85rem; box-shadow: none; }
table.xl input.num { width: 6.5rem; text-align: right; }
table.xl input.txt { width: 6rem; }
table.xl input.wide { width: 12rem; }
"

# Pushes calculated cell values into the Inputs tables without re-rendering
# (and so without stealing focus from) the input cells.
XL_JS <- "
var xlLast = [];
function xlApply() {
  xlLast.forEach(function(c) {
    var el = document.getElementById(c.id);
    if (el) { el.textContent = c.text; el.className = c.cls; }
  });
}
Shiny.addCustomMessageHandler('calc-cells', function(cells) { xlLast = cells; xlApply(); });
$(document).on('shiny:value', function(e) {
  if (e.name === 'factor_table' || e.name === 'constant_table') setTimeout(xlApply, 0);
});
"

# Table headers without underscores ("min_stock_needed" -> "min stock needed").
# Formatting still refers to the original column names.
pretty_names <- function(x) sub(" uL$", " (µL)", gsub("_", " ", x))

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
      c("OK", "OVER VOLUME", "BELOW MIN"), c("#C6EFCE", "#FFC7CE", "#FFE699")))
  }
  out
}

# Number formatting for calculated cells.
fmt_g <- function(x) ifelse(is.na(x) | !is.finite(x), "", format(signif(x, 6), trim = TRUE, drop0trailing = TRUE))
fmt_vol <- function(x) ifelse(is.na(x) | !is.finite(x), "", sprintf("%.3f", x))
fmt_pct <- function(x) ifelse(is.na(x) | !is.finite(x), "", sprintf("%.1f%%", 100 * x))

# A yellow input cell.
in_num <- function(id, value) {
  tags$td(class = "in", tagAppendAttributes(
    numericInput(id, NULL, if (is.finite(value)) value else NA),
    .cssSelector = "input", class = "num"))
}
in_txt <- function(id, value, cls = "txt") {
  tags$td(class = "in", tagAppendAttributes(
    textInput(id, NULL, value), .cssSelector = "input", class = cls))
}
calc_td <- function(id, cls = "calc") tags$td(id = id, class = cls)

setting_row <- function(id, label, value, step = NA) {
  tags$tr(tags$td(label),
          tags$td(class = "in", tagAppendAttributes(
            numericInput(id, NULL, value, step = step),
            .cssSelector = "input", class = "num")))
}

ui <- page_sidebar(
  title = "Buffer Plate Builder",
  # Let the page scroll instead of squeezing the cards into the window height
  # (a fillable page collapsed the optimiser table to 0 px in small windows).
  fillable = FALSE,
  theme = bs_theme(version = 5, base_font = c("Calibri", "Segoe UI", "Arial", "sans-serif")),
  tags$head(tags$style(HTML(XL_CSS)), tags$script(HTML(XL_JS))),
  sidebar = sidebar(
    width = 320,
    accordion(
      open = "Design",
      accordion_panel(
        "Design",
        fileInput("design_file", "Design CSV", accept = ".csv"),
        actionButton("load_example", "Load ASR4 example", class = "btn-sm btn-outline-secondary mb-2"),
        radioButtons("orientation", "Orientation",
                     c("Auto-detect" = "auto", "Runs in rows" = "runs_in_rows",
                       "Runs in columns" = "runs_in_columns")),
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
      "Inputs",
      div(class = "xl-title", textOutput("page_title", inline = TRUE)),
      div(class = "xl-sub",
          "Yellow cells with blue text are inputs - edit these. Everything else is calculated. ",
          "Units are not converted: each stock must be in the same units as the design."),
      tags$table(
        class = "xl xl-set",
        setting_row("final_volume", "Final reaction volume per well (µL)", 100),
        setting_row("fixed_volume", "Fixed volume per well: enzyme / template / master mix etc. (µL)", 0),
        setting_row("min_volume", "Minimum pipettable volume (µL)", 0.5, step = 0.1),
        setting_row("overage", "Stock overage (fraction, 0.15 = 15%)", 0.15, step = 0.05),
        setting_row("dead_volume", "Dead volume per source reservoir (µL)", 20)
      ),
      div(class = "xl-section", "Design factors"),
      div(class = "xl-wrap", uiOutput("factor_table")),
      div(class = "xl-section", "Constant components (same concentration in every well)"),
      div(class = "xl-wrap", uiOutput("constant_table")),
      div(class = "small text-muted mb-1", "To remove a constant, clear its name."),
      div(actionButton("add_constant", "Add constant component", class = "btn-sm btn-outline-secondary")),
      div(class = "xl-section", "Feasibility summary"),
      div(class = "xl-wrap", uiOutput("feasibility_summary"))
    ),
    nav_panel(
      "Diagnostics",
      card(card_header("Feasibility rule"), uiOutput("rule")),
      card(
        card_header("Stocks causing failures"),
        p(class = "text-muted small",
          "Red: the largest volume in over-volume runs (water < 0). Yellow: below the minimum ",
          "pipettable volume in some runs. \"Runs below min\" counts every run where that stock is ",
          "too small, including over-volume runs, so it can be higher than the Inputs summary, ",
          "which counts each run once."),
        checkboxInput("problems_only", "Show only stocks with problems", TRUE),
        DTOutput("stock_problems")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Failing wells"), DTOutput("failing")),
        card(card_header("Minimum stock per component (others held fixed)"), DTOutput("min_stocks"))
      ),
      card(
        card_header("Optimiser: stocks within max stock that make every well fit"),
        p(class = "text-muted small",
          "Stocks are only made more concentrated, never above the max stock (no max = unlimited), ",
          "changing as little as possible. Working stocks are ignored."),
        uiOutput("hold_ui"),
        actionButton("optimise", "Run optimiser", class = "btn-sm btn-primary"),
        uiOutput("opt_message"),
        uiOutput("opt_table_ui"),
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
    ),
    nav_panel(
      "Dragonfly",
      p(class = "text-muted small",
        "Dispense file for the SPT Labtech Dragonfly: one block of well volumes (µL) per source, ",
        "water as \"Diluent\", in the order of addition set on the Picklist tab. ",
        "The fixed mix is not included."),
      layout_columns(
        col_widths = c(4, 8),
        div(
          uiOutput("dfd_plate_ui"),
          checkboxInput("dfd_fill_unused", "Fill wells without a run with diluent (final volume)", TRUE),
          checkboxInput("dfd_include_failed", "Dispense OVER VOLUME wells anyway (no diluent)", FALSE),
          downloadButton("dl_dfd", "Download Dragonfly file", class = "btn-primary")
        ),
        uiOutput("dfd_warning")
      ),
      div(class = "mt-3", tags$strong("Preview")),
      verbatimTextOutput("dfd_preview")
    )
  )
)

server <- function(input, output, session) {
  design_source <- reactiveVal(NULL)
  design_name <- reactiveVal(NULL)
  optimised <- reactiveVal(NULL)

  # ---- Stock state
  # stock_state holds the current stock table. The Inputs tables are rebuilt
  # from it only when reseed() is called (new design, file load, add row,
  # optimiser); ordinary edits flow back from the input cells. Input ids carry
  # a version number so stale values from an earlier table are never read.
  stock_state <- reactiveVal(normalise_stocks(data.frame(component = character())))
  skeleton <- reactiveVal(list(version = 0, factors = character(), n_const = 0))
  reseed <- function(df) {
    df <- normalise_stocks(df)
    stock_state(df)
    old <- isolate(skeleton())
    skeleton(list(version = old$version + 1,
                  factors = df$component[df$role == "factor"],
                  n_const = sum(df$role == "constant")))
  }
  cell_id <- function(kind, field, i, v = skeleton()$version) sprintf("%s_%s_%d_%d", kind, field, v, i)
  stocks <- reactive(stock_state())

  observeEvent(input$design_file, {
    design_source(input$design_file$datapath)
    design_name(input$design_file$name)
  })
  observeEvent(input$load_example, {
    design_source(EXAMPLE_DESIGN)
    design_name(basename(EXAMPLE_DESIGN))
    reseed(read_stocks(EXAMPLE_STOCKS))
  })

  output$page_title <- renderText({
    nm <- design_name()
    paste0(if (is.null(nm)) "Buffer screen" else paste(tools::file_path_sans_ext(nm), "buffer screen"),
           " - volume calculator")
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

  dmat <- reactive(tryCatch(design_matrix(design(), components()), error = function(e) NULL))

  # One factor row per design component, keeping stocks already entered.
  observeEvent(components(), {
    seeded <- default_stocks(components(), stock_state())
    if (!identical(seeded$component[seeded$role == "factor"], isolate(skeleton())$factors)) reseed(seeded)
  })

  observeEvent(input$stocks_file, {
    loaded <- read_stocks(input$stocks_file$datapath)
    comps <- tryCatch(isolate(components()), error = function(e) NULL)
    reseed(if (is.null(comps)) loaded else default_stocks(comps, loaded))
  })

  observeEvent(input$add_constant, {
    s <- stock_state()
    reseed(rbind(s, normalise_stocks(data.frame(component = "New constant", role = "constant"))))
  })

  # ---- Inputs tables (structure only; values are pushed by the observer below)
  output$factor_table <- renderUI({
    sk <- skeleton()
    st <- isolate(stock_state())
    fac <- st[st$role == "factor", , drop = FALSE]
    if (!nrow(fac)) return(div(class = "text-muted", "Load a design CSV (sidebar) to list its factors here."))
    v <- sk$version
    head <- c("Component", "Units (as in design)", "Stock conc. (same units as design)", "Design min",
              "Design max", "Vol. at design max (µL)", "Fraction of well at max",
              "Smallest non-zero level", "Vol. at smallest level (µL)", "Pipetting check",
              "Working stock (optional)", "Max stock (optional)", "Stock notes")
    rows <- lapply(seq_len(nrow(fac)), function(i) {
      calc <- function(col, cls = "calc") calc_td(cell_id("fc", col, i, v), cls)
      tags$tr(
        tags$td(class = "name", fac$component[i]),
        in_txt(cell_id("f", "units", i, v), fac$units[i]),
        in_num(cell_id("f", "stock", i, v), fac$stock_conc[i]),
        calc("min"), calc("max"), calc("volmax"), calc("fracmax"),
        calc("small"), calc("volsmall"), calc("check", "check"),
        in_num(cell_id("f", "work", i, v), fac$working_stock_conc[i]),
        in_num(cell_id("f", "maxst", i, v), fac$max_stock_conc[i]),
        in_txt(cell_id("f", "notes", i, v), fac$notes[i], "wide")
      )
    })
    tags$table(class = "xl", tags$thead(tags$tr(lapply(head, tags$th))), tags$tbody(rows))
  })

  output$constant_table <- renderUI({
    sk <- skeleton()
    st <- isolate(stock_state())
    cst <- st[st$role == "constant", , drop = FALSE]
    v <- sk$version
    head <- c("Component", "Units", "Stock conc.", "Final conc. in well", "Vol. per well (µL)",
              "Fraction of well", "Pipetting check", "Stock notes")
    rows <- lapply(seq_len(nrow(cst)), function(i) {
      tags$tr(
        in_txt(cell_id("c", "name", i, v), cst$component[i], "wide"),
        in_txt(cell_id("c", "units", i, v), cst$units[i]),
        in_num(cell_id("c", "stock", i, v), cst$stock_conc[i]),
        in_num(cell_id("c", "final", i, v), cst$final_conc[i]),
        calc_td(cell_id("cc", "vol", i, v)), calc_td(cell_id("cc", "frac", i, v)),
        calc_td(cell_id("cc", "check", i, v), "check"),
        in_txt(cell_id("c", "notes", i, v), cst$notes[i], "wide")
      )
    })
    if (!length(rows)) rows <- list(tags$tr(tags$td(colspan = length(head), class = "text-muted", "None")))
    tags$table(class = "xl", tags$thead(tags$tr(lapply(head, tags$th))), tags$tbody(rows))
  })

  # Read the input cells back into stock_state.
  observe({
    sk <- skeleton()
    st <- isolate(stock_state())
    fac <- st[st$role == "factor", , drop = FALSE]
    get <- function(kind, field, i) input[[cell_id(kind, field, i, sk$version)]]
    num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)

    ids <- c(unlist(lapply(seq_along(sk$factors), function(i)
      c(cell_id("f", "units", i, sk$version), cell_id("f", "stock", i, sk$version)))),
      unlist(lapply(seq_len(sk$n_const), function(i) cell_id("c", "name", i, sk$version))))
    # Wait until every cell of this version has reported a value.
    if (any(vapply(ids, function(id) is.null(input[[id]]), logical(1)))) return()

    if (nrow(fac)) {
      for (i in seq_len(nrow(fac))) {
        fac$units[i] <- get("f", "units", i) %||% ""
        fac$stock_conc[i] <- num(get("f", "stock", i))
        fac$working_stock_conc[i] <- num(get("f", "work", i))
        fac$max_stock_conc[i] <- num(get("f", "maxst", i))
        fac$notes[i] <- get("f", "notes", i) %||% ""
      }
    }
    cst <- if (sk$n_const) {
      do.call(rbind, lapply(seq_len(sk$n_const), function(i) data.frame(
        component = get("c", "name", i) %||% "", role = "constant",
        units = get("c", "units", i) %||% "", stock_conc = num(get("c", "stock", i)),
        final_conc = num(get("c", "final", i)), notes = get("c", "notes", i) %||% "",
        stringsAsFactors = FALSE)))
    } else NULL
    new <- normalise_stocks(rbind(normalise_stocks(fac), if (!is.null(cst)) normalise_stocks(cst)))
    if (!identical(new, isolate(stock_state()))) stock_state(new)
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
  safe_settings <- reactive(tryCatch(resolve_settings(settings()), error = function(e) NULL))

  # Push the calculated cells.
  observe({
    sk <- skeleton()
    s <- req(safe_settings())
    st <- stocks()
    v <- sk$version
    cells <- list()
    add <- function(id, text, cls = "calc") cells[[length(cells) + 1]] <<- list(id = id, text = text, cls = cls)

    dm <- dmat()
    if (!is.null(dm) && length(sk$factors) && all(sk$factors %in% names(dm))) {
      fc <- factor_checks(dm[, c("run", sk$factors)], st, s)
      for (i in seq_len(nrow(fc))) {
        add(cell_id("fc", "min", i, v), fmt_g(fc$design_min[i]))
        add(cell_id("fc", "max", i, v), fmt_g(fc$design_max[i]))
        add(cell_id("fc", "volmax", i, v), fmt_vol(fc$vol_at_max[i]))
        add(cell_id("fc", "fracmax", i, v), fmt_pct(fc$fraction_at_max[i]))
        add(cell_id("fc", "small", i, v), fmt_g(fc$smallest_level[i]))
        add(cell_id("fc", "volsmall", i, v), fmt_vol(fc$vol_at_smallest[i]))
        add(cell_id("fc", "check", i, v), fc$check[i], paste("check", fc$status[i]))
      }
    }
    for (i in seq_len(sk$n_const)) {
      row <- data.frame(component = "x", role = "constant",
                        stock_conc = input[[cell_id("c", "stock", i, v)]] %||% NA,
                        final_conc = input[[cell_id("c", "final", i, v)]] %||% NA)
      blank <- !nzchar(trimws(input[[cell_id("c", "name", i, v)]] %||% ""))
      cc <- constant_checks(row, s)
      add(cell_id("cc", "vol", i, v), if (blank) "" else fmt_vol(cc$vol_per_well))
      add(cell_id("cc", "frac", i, v), if (blank) "" else fmt_pct(cc$fraction_of_well))
      add(cell_id("cc", "check", i, v), if (blank) "(removed)" else cc$check,
          if (blank) "check" else paste("check", cc$status))
    }
    session$sendCustomMessage("calc-cells", cells)
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

  output$feasibility_summary <- renderUI({
    r <- tryCatch(result(), error = function(e) NULL)
    ok <- !is.null(r) && !inherits(r, "build_error")
    s <- safe_settings()
    sum_max <- if (ok) {
      fc <- factor_checks(r$design_matrix, r$stocks, s)
      sum_fraction_at_max(fc, constant_checks(r$stocks, s), s)
    } else NA
    runs <- if (ok) r$runs else NULL
    val <- function(x, cls = "", fmt = fmt_g) tags$td(class = paste("calc", cls), style = "font-weight:700",
                                                      if (ok) fmt(x) else "-")
    n_over <- if (ok) sum(runs$status == "OVER VOLUME") else NA
    n_below <- if (ok) sum(runs$status == "BELOW MIN") else NA
    worst <- if (ok) r$summary$worst_fraction else NA
    tagList(
      tags$table(
        class = "xl xl-set",
        tags$tr(tags$td("Runs in design"), val(if (ok) nrow(runs) else NA)),
        tags$tr(tags$td("Runs that do NOT fit (water < 0)"), val(n_over, if (isTRUE(n_over > 0)) "bad")),
        tags$tr(tags$td("Runs with a volume below min pipettable"), val(n_below, if (isTRUE(n_below > 0)) "warn")),
        tags$tr(tags$td("Runs OK"), val(if (ok) sum(runs$status == "OK") else NA)),
        tags$tr(tags$td("Worst-case well: fraction of volume used before water"),
                val(worst, if (isTRUE(worst > 1)) "bad", fmt_pct)),
        tags$tr(tags$td("Sum of all factor fractions at max (+ constant + fixed vol.)"),
                val(sum_max, if (isTRUE(sum_max > 1)) "bad", fmt_pct))
      ),
      if (!is.null(r) && inherits(r, "build_error"))
        div(class = "alert alert-danger py-2 mt-2", style = "white-space: pre-wrap", r$error),
      if (ok && r$settings$replicates > 1)
        div(class = "small text-muted", sprintf("Counts are per run; each run is in %d wells.", r$settings$replicates))
    )
  })

  # ---- Diagnostics tab
  output$rule <- renderUI({
    r <- res()
    s <- r$settings
    tagList(
      p("A well fits only if", tags$code("Σ (C_i / Stock_i) + fixed / V_total ≤ 1"), "."),
      p("V_total cancels out: changing the final well volume never makes a well fit. ",
        "Only more concentrated stocks, a smaller fixed volume or narrower design ranges help."),
      p(sprintf("Here: fixed / V_total = %.1f%%; worst well Σ = %.1f%% (run %d); headroom %.1f µL.",
                100 * s$fixed_volume / s$final_volume, 100 * r$summary$worst_fraction,
                r$summary$worst_run, r$summary$headroom_uL))
    )
  })

  # Tables use server = FALSE: rows are sent with the table instead of
  # fetched by a later request, which the in-browser (Shinylive) build needs.
  output$stock_problems <- renderDT(server = FALSE, {
    p <- stock_problems(res())
    if (isTRUE(input$problems_only)) p <- p[p$status != "ok", , drop = FALSE]
    validate(need(nrow(p) > 0, "No stock causes a failing run."))
    datatable(p, rownames = FALSE, colnames = pretty_names(names(p)),
              options = list(dom = "t", pageLength = 100, scrollX = TRUE, ordering = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = match("status", names(p)) - 1)))) |>
      formatSignif(c("stock_conc", "min_stock_to_fit"), 4) |>
      formatRound("smallest_volume_uL", 3) |>
      formatStyle("source", "status", fontWeight = "bold",
                  backgroundColor = styleEqual(c("bad", "warn", "ok"), c("#FFC7CE", "#FFE699", "#FFFFFF"))) |>
      formatStyle("problem", "status",
                  backgroundColor = styleEqual(c("bad", "warn", "ok"), c("#FFC7CE", "#FFE699", "#C6EFCE")))
  })

  output$failing <- renderDT(server = FALSE, {
    f <- res()$failing
    dt(f, pct = c("fraction_used", "largest_share"), page = 10, colnames = pretty_names(names(f)))
  })

  output$min_stocks <- renderDT(server = FALSE, {
    m <- res()$min_stocks
    m <- m[, c("component", "units", "stock_conc", "min_stock_needed", "max_stock_conc",
               "fits_now", "exceeds_max", "wells_blocked_by_others")]
    datatable(m, rownames = FALSE, colnames = pretty_names(names(m)),
              options = list(dom = "t", pageLength = 100, scrollX = TRUE)) |>
      formatSignif(c("stock_conc", "min_stock_needed", "max_stock_conc"), 4) |>
      formatStyle("fits_now", color = styleEqual(c(TRUE, FALSE), c("green", "firebrick")))
  })

  # The hold list is rendered (not updated) from the component names, so it is
  # never left empty by an update that reaches the browser before the input
  # is ready. It re-renders only when the names change, not on every edit.
  component_names <- reactiveVal(character())
  observeEvent(stocks(), {
    if (!identical(stocks()$component, component_names())) component_names(stocks()$component)
  })
  output$hold_ui <- renderUI({
    nm <- component_names()
    selectizeInput("hold", "Keep these stocks unchanged", choices = nm, multiple = TRUE,
                   selected = intersect(isolate(input$hold), nm))
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
  # Only take up space when there is a feasible suggestion to show.
  output$opt_table_ui <- renderUI(if (isTRUE(optimised()$feasible)) DTOutput("opt_table"))
  output$opt_table <- renderDT(server = FALSE, {
    o <- req(optimised())
    req(o$feasible)
    datatable(o$stocks, rownames = FALSE, colnames = pretty_names(names(o$stocks)),
              options = list(dom = "t", pageLength = 100)) |>
      formatSignif(c("stock_conc", "max_stock_conc", "suggested_stock", "suggested_rounded"), 4) |>
      formatStyle("changed", fontWeight = styleEqual(TRUE, "bold"))
  })
  output$opt_apply <- renderUI({
    o <- req(optimised())
    if (isTRUE(o$feasible)) actionButton("apply_opt", "Apply rounded stocks to the Inputs page", class = "btn-sm mt-2")
  })
  observeEvent(input$apply_opt, {
    o <- req(optimised())
    req(isTRUE(o$feasible))
    new <- o$stocks$suggested_rounded
    if (any(!is.finite(new) | new <= 0)) {
      showNotification("Not applied: the suggestion contains a stock that is not a finite positive number.",
                       type = "error")
      return()
    }
    s <- stocks()
    i <- match(tolower(s$component), tolower(o$stocks$component))
    s$stock_conc[!is.na(i)] <- new[i[!is.na(i)]]
    reseed(s)
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
  output$volumes <- renderDT(server = FALSE, {
    v <- res()$volumes
    dt(v[, setdiff(names(v), c("row", "col"))], pct = "fraction_used")
  })
  output$stock_prep <- renderDT(server = FALSE, dt(res()$stock_prep, page = 50))

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
  output$picklist <- renderDT(server = FALSE, dt(picklist_table(), page = 50))

  # ---- Dragonfly dispense file
  n_plates <- reactive(max(res()$volumes$plate))
  output$dfd_plate_ui <- renderUI({
    n <- n_plates()
    if (n > 1) selectInput("dfd_plate", "Plate (one file per plate)", seq_len(n),
                           selected = min(as.integer(isolate(input$dfd_plate) %||% 1), n))
  })
  dfd_plate <- reactive(min(as.integer(input$dfd_plate %||% 1), n_plates()))
  dfd_lines <- reactive({
    dragonfly_dfd(res(), plate = dfd_plate(), order = addition_order(),
                  include_failed = isTRUE(input$dfd_include_failed),
                  fill_unused = isTRUE(input$dfd_fill_unused))
  })
  output$dfd_warning <- renderUI({
    v <- res()$volumes
    v <- v[v$plate == dfd_plate(), ]
    n_over <- sum(v$status == "OVER VOLUME")
    n_below <- sum(v$status == "BELOW MIN")
    tagList(
      if (n_over) div(class = "alert alert-danger py-2", sprintf(
        "%d well(s) on this plate are OVER VOLUME and %s. Fix them on the Inputs page before dispensing.",
        n_over, if (isTRUE(input$dfd_include_failed)) "are dispensed without diluent (they will overflow)"
        else "are left empty in this file")),
      if (n_below) div(class = "alert alert-warning py-2", sprintf(
        "%d well(s) on this plate have a volume below the minimum pipettable volume.", n_below)),
      if (!n_over && !n_below) div(class = "alert alert-success py-2", "All wells on this plate fit.")
    )
  })
  output$dfd_preview <- renderText(paste(dfd_lines(), collapse = "\n"))
  output$dl_dfd <- downloadHandler(
    function() {
      nm <- design_name()
      base <- if (is.null(nm)) "Buffer screen" else tools::file_path_sans_ext(nm)
      paste0(base, " DFD", if (n_plates() > 1) paste0(" plate ", dfd_plate()), ".csv")
    },
    function(file) write_dfd(dfd_lines(), file)
  )

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
