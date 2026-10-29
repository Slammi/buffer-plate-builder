# App workflows, run through the server code with shiny::testServer().

test_that("loading the example fills the Inputs page and the summary", {
  shiny::testServer(app_object(), {
    load_example()
    expect_equal(skeleton()$factors, c("MgCl2", "Kcl", "PEG8000", "Betaine", "Trehalose",
                                       "TCEP", "CaCl2", "Tween20"))
    expect_equal(skeleton()$n_const, 1)
    expect_equal(run_counts(result()), c(over = 18, below = 0, ok = 70))
    summary <- html_text(output$feasibility_summary$html)
    expect_match(summary, "Runs that do NOT fit \\(water < 0\\) 18")
    expect_match(summary, "122.4%")
  })
})

test_that("editing a stock cell recalculates everything", {
  shiny::testServer(app_object(), {
    load_example()
    type_factor("PEG8000", "stock", 64)
    expect_equal(stock_state()$stock_conc[stock_state()$component == "PEG8000"], 64)
    expect_equal(run_counts(result())[["over"]], 0)
    type_factor("PEG8000", "notes", "50% w/v in water")
    expect_equal(stock_state()$notes[stock_state()$component == "PEG8000"], "50% w/v in water")
  })
})

test_that("optimise, apply, edit and optimise again keeps working", {
  shiny::testServer(app_object(), {
    load_example()
    type_factor("Betaine", "stock", 1000)
    click("optimise")
    expect_true(optimised()$feasible)
    first_peg <- optimised()$stocks$suggested_rounded[optimised()$stocks$component == "PEG8000"]
    expect_gt(first_peg, 40)

    click("apply_opt")
    expect_null(optimised())
    expect_equal(stock_state()$stock_conc[stock_state()$component == "PEG8000"], first_peg)
    expect_equal(skeleton()$version, 2)
    echo_cells()                                   # the Inputs page is redrawn
    expect_equal(stock_state()$stock_conc[stock_state()$component == "PEG8000"], first_peg)

    type_factor("Betaine", "stock", 100)
    expect_gt(run_counts(result())[["over"]], 0)
    click("optimise")
    expect_true(optimised()$feasible)
    expect_gt(optimised()$stocks$suggested_rounded[optimised()$stocks$component == "PEG8000"], first_peg)
    click("optimise")                              # a third run works too
    expect_true(optimised()$feasible)
  })
})

test_that("an infeasible optimiser run explains itself and offers no Apply", {
  shiny::testServer(app_object(), {
    load_example()
    type_factor("PEG8000", "stock", 24)            # PEG alone fills those wells
    session$setInputs(hold = "PEG8000")
    click("optimise")
    expect_false(optimised()$feasible)
    expect_match(output$opt_message$html, "PEG8000 is held at 24")
    expect_null(output$opt_table_ui$html)
    expect_null(output$opt_apply$html)
  })
})

test_that("constants can be added and removed by clearing the name", {
  shiny::testServer(app_object(), {
    load_example()
    click("add_constant")
    expect_equal(skeleton()$n_const, 2)
    expect_true(inherits(result(), "build_error"))   # new constant has no values yet
    echo_cells()
    do.call(session$setInputs, stats::setNames(list(""), cell_id("c", "name", 2)))
    expect_false("New constant" %in% stock_state()$component)
    expect_false(inherits(result(), "build_error"))
  })
})

test_that("cleared settings show a message and the session keeps working", {
  shiny::testServer(app_object(), {
    load_example()
    for (nm in c("min_volume", "fixed_volume", "overage", "dead_volume", "final_volume", "replicates")) {
      do.call(session$setInputs, stats::setNames(list(NA), nm))
      expect_true(inherits(result(), "build_error"))
      expect_match(html_text(output$feasibility_summary$html), "must be a number")
    }
    app_defaults()
    expect_equal(run_counts(result())[["over"]], 18)
  })
})

test_that("deselecting a design column drops it from the stocks and the plate", {
  shiny::testServer(app_object(), {
    load_example()
    session$setInputs(components = setdiff(design()$components, "Tween20"))
    expect_false("Tween20" %in% skeleton()$factors)
    echo_cells()
    expect_false("Tween20" %in% names(result()$volumes))
    expect_equal(nrow(result()$volumes), 88)
  })
})

test_that("stock CSV download and upload round-trip", {
  shiny::testServer(app_object(), {
    load_example()
    type_factor("PEG8000", "stock", 55)
    saved <- output$dl_stocks
    expect_equal(read_stocks(saved)$stock_conc[read_stocks(saved)$component == "PEG8000"], 55)

    type_factor("PEG8000", "stock", 40)
    session$setInputs(stocks_file = list(datapath = saved, name = "stocks.csv"))
    expect_equal(stock_state()$stock_conc[stock_state()$component == "PEG8000"], 55)
  })
})

test_that("downloads produce valid files", {
  shiny::testServer(app_object(), {
    load_example()
    dfd <- output$dl_dfd
    expect_equal(readLines(dfd, n = 1), "version, '1.2")
    expect_equal(nrow(read_dfd(dfd)), 96 * length(result()$addition_order))

    pick <- utils::read.csv(output$dl_picklist)
    expect_equal(names(pick), c("step", "source", "destination_plate", "destination_well", "volume_uL"))
    expect_gt(nrow(pick), 0)

    xl <- output$dl_excel
    expect_true("Volumes" %in% openxlsx::getSheetNames(xl))
    expect_equal(nrow(openxlsx::read.xlsx(xl, "Volumes")), 88)
  })
})

test_that("plate layout options flow through to the result", {
  shiny::testServer(app_object(), {
    load_example()
    session$setInputs(plate_format = "384", replicates = 2, exclude_edges = TRUE,
                      reserved_wells = "B2, B3")
    v <- result()$volumes
    expect_equal(nrow(v), 176)
    expect_false(any(v$well %in% c("B2", "B3")))
    expect_false(any(v$row %in% c(1, 16) | v$col %in% c(1, 24)))
  })
})
