# Helpers for app workflow tests with shiny::testServer().
#
# testServer() runs the app's server function without a browser. The Inputs
# tables are built in the browser, so tests must "echo" their cells back, as
# the browser would after drawing them. Helpers take the testServer
# environment (default: the caller's) so they can reach session, input and
# the server's own reactives.

app_object <- function() {
  suppressMessages(shiny::shinyAppDir(root))
}

# Settings inputs as the browser sends them at start-up.
app_defaults <- function(env = parent.frame()) {
  session <- get("session", envir = env)
  session$setInputs(orientation = "auto", final_volume = 100, fixed_volume = 0, min_volume = 0.5,
                    overage = 0.15, dead_volume = 20, plate_format = "96", fill = "row",
                    exclude_edges = FALSE, reserved_wells = "", randomize = FALSE, seed = 1,
                    replicates = 1, include_failed = FALSE, problems_only = TRUE,
                    dfd_fill_unused = TRUE, dfd_include_failed = FALSE)
}

# Report every Inputs cell of the current table version, as the browser does
# after (re)drawing the Inputs page.
echo_cells <- function(env = parent.frame()) {
  session <- get("session", envir = env)
  st <- get("stock_state", envir = env)()
  cell_id <- get("cell_id", envir = env)
  fac <- st[st$role == "factor", ]
  cst <- st[st$role == "constant", ]
  a <- list()
  for (i in seq_len(nrow(fac))) {
    a[[cell_id("f", "units", i)]] <- fac$units[i]
    a[[cell_id("f", "stock", i)]] <- fac$stock_conc[i]
    a[[cell_id("f", "work", i)]] <- fac$working_stock_conc[i]
    a[[cell_id("f", "maxst", i)]] <- fac$max_stock_conc[i]
    a[[cell_id("f", "notes", i)]] <- fac$notes[i]
  }
  for (i in seq_len(nrow(cst))) {
    a[[cell_id("c", "name", i)]] <- cst$component[i]
    a[[cell_id("c", "units", i)]] <- cst$units[i]
    a[[cell_id("c", "stock", i)]] <- cst$stock_conc[i]
    a[[cell_id("c", "final", i)]] <- cst$final_conc[i]
    a[[cell_id("c", "notes", i)]] <- cst$notes[i]
  }
  if (length(a)) do.call(session$setInputs, a)
}

# Load the ASR4 example and echo its Inputs cells.
load_example <- function(env = parent.frame()) {
  session <- get("session", envir = env)
  app_defaults(env)
  session$setInputs(load_example = 1)
  session$setInputs(components = get("design", envir = env)()$components)
  echo_cells(env)
}

# Type a value into a factor's cell ("stock", "work", "maxst", "units", "notes").
type_factor <- function(component, field, value, env = parent.frame()) {
  session <- get("session", envir = env)
  i <- which(get("skeleton", envir = env)()$factors == component)
  if (!length(i)) stop("No factor row for ", component)
  do.call(session$setInputs, stats::setNames(list(value), get("cell_id", envir = env)("f", field, i)))
}

# Click an action button (each click increments its value, as in the browser).
click <- function(id, env = parent.frame()) {
  session <- get("session", envir = env)
  input <- get("input", envir = env)
  do.call(session$setInputs, stats::setNames(list((shiny::isolate(input[[id]]) %||% 0) + 1), id))
}

run_counts <- function(r) {
  c(over = sum(r$runs$status == "OVER VOLUME"), below = sum(r$runs$status == "BELOW MIN"),
    ok = sum(r$runs$status == "OK"))
}

html_text <- function(html) {
  txt <- gsub("<[^>]+>", " ", html)
  txt <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", txt, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  gsub("[[:space:]]+", " ", txt)
}
