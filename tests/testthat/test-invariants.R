# Rules that must hold for any design, checked on many random cases.
# Each case comes from a fixed seed, so a failure names the seed to reproduce:
#   random_case(<seed>)

random_case <- function(seed) {
  with_seed(seed, {
    n_comp <- sample(1:6, 1)
    n_runs <- sample(c(1, 2, 5, 12, 40, 96, 97, 150), 1)
    comps <- paste0("C", seq_len(n_comp))
    # Each component has 2-4 levels including 0; some components are always 0.
    design <- vapply(comps, function(cmp) {
      top <- signif(stats::runif(1, 1, 500), 2)
      lv <- if (stats::runif(1) < 0.1) 0 else unique(c(0, top * sort(sample(c(0.1, 0.25, 0.5, 1), sample(1:3, 1)))))
      sample(lv, n_runs, replace = TRUE)
    }, numeric(n_runs))
    design <- matrix(design, nrow = n_runs, dimnames = list(NULL, comps))
    raw <- rbind(comps, apply(design, 2, format, scientific = FALSE, trim = TRUE))
    raw <- as.data.frame(matrix(raw, nrow = n_runs + 1), stringsAsFactors = FALSE)

    # Stocks: usually 5-40x the top level, sometimes too dilute (over volume).
    top <- apply(design, 2, max)
    stock <- signif(pmax(top, 1) * sample(c(1.5, 5, 10, 20, 40), n_comp, replace = TRUE), 3)
    stocks <- data.frame(component = comps, role = "factor", units = "mM", stock_conc = stock,
                         working_stock_conc = ifelse(stats::runif(n_comp) < 0.3, signif(stock / 20, 3), NA),
                         max_stock_conc = ifelse(stats::runif(n_comp) < 0.5, stock * sample(c(1, 2, 5), n_comp, TRUE), NA))
    if (stats::runif(1) < 0.5) {
      stocks <- rbind(stocks, data.frame(component = "Buffer", role = "constant", units = "mM",
                                         stock_conc = 1000, working_stock_conc = NA, max_stock_conc = NA,
                                         final_conc = sample(c(10, 25, 50), 1))[, c(names(stocks), "final_conc")][, names(stocks)])
      stocks$final_conc <- c(rep(NA, n_comp), sample(c(10, 25, 50), 1))
    }

    fmt <- sample(c(96, 384), 1)
    settings <- list(
      final_volume = sample(c(20, 50, 100), 1), fixed_volume = sample(c(0, 0, 2, 5), 1),
      min_volume = sample(c(0.1, 0.2, 0.5, 1), 1), overage = sample(c(0, 0.1, 0.15), 1),
      dead_volume = sample(c(0, 20), 1), plate_format = fmt, fill = sample(c("row", "column"), 1),
      exclude_edges = stats::runif(1) < 0.3, randomize = stats::runif(1) < 0.3, seed = seed,
      replicates = sample(1:3, 1),
      reserved_wells = if (stats::runif(1) < 0.3) paste(sample(plate_wells(fmt)$well, 3), collapse = ",") else ""
    )
    list(seed = seed, design = read_design(raw), stocks = stocks, settings = settings)
  })
}

build_case <- function(cs) suppressWarnings(build_plate(cs$design, cs$stocks, cs$settings))

# Set QUICK_TESTS=1 for a faster run with fewer random cases.
SEEDS <- if (nzchar(Sys.getenv("QUICK_TESTS"))) 1:20 else 1:120
# Build every case once; tests below reuse them. Errors are kept, not thrown.
CASES <- lapply(SEEDS, function(seed) {
  cs <- random_case(seed)
  list(seed = seed, cs = cs, r = tryCatch(build_case(cs), error = function(e) e))
})
built <- function(n = length(CASES)) {
  Filter(function(x) !inherits(x$r, "error"), CASES[seq_len(min(n, length(CASES)))])
}

test_that("random cases build without errors", {
  for (x in CASES) {
    expect_false(inherits(x$r, "error"),
                 info = paste("seed", x$seed, if (inherits(x$r, "error")) conditionMessage(x$r)))
  }
})

test_that("well volumes add up and the status matches the volumes", {
  for (x in built()) {
    seed <- x$seed; r <- x$r
    s <- r$settings
    v <- r$volumes
    sources <- setdiff(names(v), c("run", "replicate", "plate", "well", "row", "col", "fixed", "water",
                                   "total", "fraction_used", "status", "largest_component", "below_min_sources"))
    comp_sum <- rowSums(v[, sources, drop = FALSE])
    info <- paste("seed", seed)
    # Components + fixed + water is always exactly the final volume.
    expect_equal(comp_sum + v$fixed + v$water, rep(s$final_volume, nrow(v)), tolerance = 1e-9, info = info)
    expect_equal(v$fraction_used, (comp_sum + v$fixed) / s$final_volume, tolerance = 1e-9, info = info)
    # OVER VOLUME exactly when water < 0.
    expect_equal(v$status == "OVER VOLUME", v$water < -1e-9, info = info)
    # BELOW MIN exactly when a non-zero volume (components, water, fixed) is under the minimum.
    vols <- cbind(as.matrix(v[, sources, drop = FALSE]), water = v$water, fixed = v$fixed)
    small <- rowSums(vols > 1e-9 & vols < s$min_volume - 1e-9) > 0
    expect_equal(v$status == "BELOW MIN", small & v$water >= -1e-9, info = info)
    # OK wells hold exactly the final volume.
    expect_equal(v$total[v$status == "OK"], rep(s$final_volume, sum(v$status == "OK")), tolerance = 1e-9, info = info)
  }
})

test_that("every run is placed the right number of times in distinct, allowed wells", {
  for (x in built()) {
    seed <- x$seed; cs <- x$cs
    r <- x$r
    v <- r$volumes
    s <- r$settings
    info <- paste("seed", seed)
    n_runs <- nrow(r$design_matrix)
    expect_equal(as.vector(table(factor(v$run, levels = seq_len(n_runs)))), rep(s$replicates, n_runs), info = info)
    expect_false(anyDuplicated(paste(v$plate, v$well)) > 0, info = info)
    allowed <- plate_wells(s$plate_format, s$fill, s$exclude_edges, s$reserved_wells)
    expect_true(all(v$well %in% allowed$well[allowed$available]), info = info)
    # Plates fill in order: every plate but the last is full.
    per_plate <- table(v$plate)
    if (length(per_plate) > 1) {
      expect_true(all(per_plate[-length(per_plate)] == sum(allowed$available)), info = info)
    }
  }
})

test_that("picklist, stock prep and Dragonfly files agree with the volumes", {
  for (x in built(60)) {
    seed <- x$seed; r <- x$r
    info <- paste("seed", seed)
    # Picklist (all wells) totals = stock prep totals, per source.
    pk <- picklist(r$transfers, r$addition_order, include_failed = TRUE)
    pk_tot <- tapply(pk$volume_uL, pk$source, sum)
    prep <- stats::setNames(r$stock_prep$total_dispensed_uL, r$stock_prep$source)
    used <- names(prep)[prep > 0]
    expect_equal(as.vector(pk_tot[used]), as.vector(prep[used]), tolerance = 1e-3, info = info)

    # Each Dragonfly block matches the volumes table (failed wells left empty).
    for (p in unique(r$volumes$plate)) {
      f <- tempfile(fileext = ".csv")
      write_dfd(dragonfly_dfd(r, plate = p), f)
      dfd <- read_dfd(f)
      v <- r$volumes[r$volumes$plate == p & r$volumes$status != "OVER VOLUME", ]
      for (src in setdiff(r$addition_order, "Fixed mix")) {
        got <- dfd[dfd$source == dfd_label(src, r), ]
        got <- got$volume[match(paste(v$row, v$col), paste(got$row, got$col))]
        want <- if (src == "Water") pmax(v$water, 0) else v[[src]]
        expect_equal(got, want, tolerance = 1e-5, info = paste(info, "plate", p, src))
      }
    }
  }
})

test_that("optimiser suggestions stay within limits and make every well fit", {
  for (x in built(60)) {
    seed <- x$seed; cs <- x$cs
    r <- x$r
    o <- optimise_stocks(r)
    info <- paste("seed", seed)
    if (!o$feasible) {
      expect_true(all(is.na(o$stocks$suggested_rounded)), info = info)
      next
    }
    st <- o$stocks
    expect_true(all(st$suggested_rounded >= st$stock_conc - 1e-9), info = info)
    has_max <- !is.na(st$max_stock_conc)
    expect_true(all(st$suggested_rounded[has_max] <= st$max_stock_conc[has_max] + 1e-9), info = info)
    new_stocks <- cs$stocks
    new_stocks$stock_conc <- st$suggested_rounded[match(new_stocks$component, st$component)]
    r2 <- suppressWarnings(build_plate(cs$design, new_stocks, cs$settings))
    expect_equal(r2$summary$over_volume, 0, info = info)
    # The worst well matches what the optimiser promised...
    expect_equal(r2$summary$worst_fraction, o$worst_fraction, tolerance = 1e-9, info = info)
    # ...and no well that was pipettable becomes BELOW MIN because of a stock change.
    newly_small <- r2$runs$below_min & !r$runs$below_min & !r$runs$over_volume
    expect_false(any(newly_small), info = info)
  }
})

test_that("diagnostics tables build for every random case", {
  for (x in built(60)) {
    seed <- x$seed; r <- x$r
    info <- paste("seed", seed)
    p <- stock_problems(r)
    expect_true(all(p$status %in% c("ok", "warn", "bad")), info = info)
    expect_equal(sum(p$status == "bad") > 0, r$summary$over_volume > 0, info = info)
    fc <- factor_checks(r$design_matrix, r$stock_table, r$settings)
    expect_equal(nrow(fc), ncol(r$design_matrix) - 1, info = info)
  }
})
