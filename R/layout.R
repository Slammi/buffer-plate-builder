# Plate geometry and assignment of runs to wells.

DEFAULT_SETTINGS <- list(
  final_volume = 100,      # µL per well
  fixed_volume = 0,        # µL per well of common mix (enzyme/template/master mix)
  min_volume = 0.5,        # minimum pipettable volume, µL
  overage = 0.15,          # stock overage, fraction
  dead_volume = 20,        # dead volume per source reservoir, µL
  plate_format = 96,       # 96 or 384
  fill = "row",            # "row" (A1, A2 ... A12, B1 ...) or "column" (A1, B1 ... H1, A2 ...)
  exclude_edges = FALSE,
  reserved_wells = character(),
  randomize = FALSE,
  seed = 1,
  replicates = 1
)

# Number settings: label shown in messages, and whether 0 is allowed.
SETTING_LIMITS <- list(
  final_volume = list(label = "Final reaction volume per well (µL)", zero_ok = FALSE),
  fixed_volume = list(label = "Fixed volume per well (µL)", zero_ok = TRUE),
  min_volume = list(label = "Minimum pipettable volume (µL)", zero_ok = TRUE),
  overage = list(label = "Stock overage (fraction)", zero_ok = TRUE),
  dead_volume = list(label = "Dead volume per source reservoir (µL)", zero_ok = TRUE),
  replicates = list(label = "Replicates per run", zero_ok = FALSE)
)

#' Fill in defaults for any settings not given, and check the rest. A cleared
#' input arrives as NA and is rejected with a plain-language message.
resolve_settings <- function(settings = list()) {
  unknown <- setdiff(names(settings), names(DEFAULT_SETTINGS))
  if (length(unknown)) stop("Unknown settings: ", paste(unknown, collapse = ", "))
  s <- utils::modifyList(DEFAULT_SETTINGS, settings)
  for (nm in names(SETTING_LIMITS)) {
    v <- suppressWarnings(as.numeric(s[[nm]]))
    lim <- SETTING_LIMITS[[nm]]
    ok <- length(v) == 1 && is.finite(v) && (if (lim$zero_ok) v >= 0 else v > 0)
    if (!ok) stop(sprintf("%s must be a number %s 0.", lim$label, if (lim$zero_ok) "of at least" else "above"),
                  call. = FALSE)
    s[[nm]] <- v
  }
  if (s$fixed_volume >= s$final_volume) {
    stop("Fixed volume per well must be less than the final reaction volume.", call. = FALSE)
  }
  s$plate_format <- as.integer(s$plate_format)
  s$fill <- match.arg(s$fill, c("row", "column"))
  s$replicates <- as.integer(s$replicates)
  if (!isTRUE(s$plate_format %in% c(96L, 384L))) stop("plate_format must be 96 or 384")
  if (isTRUE(s$randomize) && !(length(s$seed) == 1 && is.finite(suppressWarnings(as.numeric(s$seed))))) {
    stop("Seed must be a number when randomizing the run order.", call. = FALSE)
  }
  s
}

plate_dims <- function(format) {
  switch(as.character(format),
         "96" = c(rows = 8L, cols = 12L),
         "384" = c(rows = 16L, cols = 24L),
         stop("plate_format must be 96 or 384"))
}

well_name <- function(row, col) paste0(LETTERS[row], col)

#' Normalise well names: upper case, no leading zeros ("a01" -> "A1").
normalise_wells <- function(wells) {
  wells <- toupper(trimws(unlist(strsplit(as.character(wells), "[,; ]+"))))
  wells <- wells[wells != ""]
  ok <- grepl("^[A-P][0-9]{1,2}$", wells)
  if (any(!ok)) stop("Not well names: ", paste(wells[!ok], collapse = ", "))
  unique(paste0(substr(wells, 1, 1), as.integer(substring(wells, 2))))
}

#' All wells of a plate in fill order, with `available` = not an edge (when
#' excluded) and not reserved.
plate_wells <- function(format = 96, fill = "row", exclude_edges = FALSE,
                        reserved = character()) {
  d <- plate_dims(format)
  grid <- expand.grid(col = seq_len(d[["cols"]]), row = seq_len(d[["rows"]]))
  if (fill == "column") grid <- grid[order(grid$col, grid$row), ]
  grid <- data.frame(well = well_name(grid$row, grid$col), row = grid$row, col = grid$col,
                     stringsAsFactors = FALSE)
  edge <- grid$row %in% c(1, d[["rows"]]) | grid$col %in% c(1, d[["cols"]])
  grid$available <- !(exclude_edges & edge) & !grid$well %in% normalise_wells(reserved)
  rownames(grid) <- NULL
  grid
}

# Run code with a fixed seed without disturbing the caller's RNG state.
with_seed <- function(seed, code) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()))
  set.seed(seed)
  code
}

#' Assign runs (x replicates) to wells, spilling onto further plates as needed.
#'
#' Replicates of a run are placed next to each other unless the order is
#' randomized, in which case every run x replicate is shuffled.
layout_runs <- function(n_runs, settings = list()) {
  s <- resolve_settings(settings)
  units <- expand.grid(replicate = seq_len(s$replicates), run = seq_len(n_runs))[, c("run", "replicate")]
  if (s$randomize) units <- units[with_seed(s$seed, sample(nrow(units))), ]

  wells <- plate_wells(s$plate_format, s$fill, s$exclude_edges, s$reserved_wells)
  wells <- wells[wells$available, ]
  if (!nrow(wells)) stop("No wells available on the plate after exclusions.")

  n <- nrow(units)
  pos <- seq_len(n) - 1
  slot <- wells[pos %% nrow(wells) + 1, c("well", "row", "col")]
  out <- data.frame(run = units$run, replicate = units$replicate,
                    plate = pos %/% nrow(wells) + 1, slot, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}
