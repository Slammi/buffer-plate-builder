# Buffer Plate Builder (R + Shiny)

## Goal
An R Shiny app that turns a DoE buffer-screen design (CSV) into exact per-well pipetting volumes for a 96- or 384-well plate. The user defines and edits stock concentrations interactively, and the app shows instantly whether every well fits and which components cause problems.

This replaces an Excel calculator (ASR4_Volume_Calculator). The Excel version had a fixed number of components and one plate size. This app must handle **any number of components** and **either plate format**.

## Build approach
1. Write the core logic as plain, tested R functions in `R/` with no Shiny code. Test them with `testthat` against the fixtures in `tests/fixtures/`.
2. Build the Shiny app (`app.R`) on top of those functions.
3. Keep the code runnable without the UI too, e.g. `build_plate(design_csv, stocks_csv, settings)` returning all outputs.

Suggested packages: `shiny`, `bslib`, `rhandsontable` (editable stock table), `dplyr`/`tidyr`, `readr`, `ggplot2` (plate heatmaps), `openxlsx` (Excel export), `testthat`. Use `renv` for package management.

## Inputs

### Design CSV
- Default orientation is one row per run and one column per component, as in `tests/fixtures/ASR4_design_PEG24.csv`.
- Also accept the transposed layout (one row per component, runs across), as in `tests/fixtures/ASR4_design_PEG30_transposed.csv`. Auto-detect the orientation and let the user override it.
- Component names come from the headers or first column. Strip whitespace.
- Some columns are responses, not components (e.g. Transcript Number, Library Yield, Number of Genes Detected). They are empty or non-numeric. Exclude them automatically, and let the user toggle which columns count as components.
- Run order is the row order unless randomization is turned on.

### Stock table (editable in the UI, saved/loaded as CSV)
Columns:
- `component`
- `role`: `factor` (varies per the design) or `constant` (same final concentration in every well, e.g. Tris-HCl)
- `units`
- `stock_conc`
- `final_conc`: constant components only
- optional `working_stock_conc`: a diluted stock to use for low levels that fall below the minimum pipettable volume
- optional `max_stock_conc`: solubility or practical limit, used by the stock suggestions

Rules:
- On loading a design, pre-populate a row for each factor. Users can add constant components.
- **Units are not converted.** The stock must be in the same units as the design values. Show this clearly in the UI.

### Settings
| Setting | Default |
|---|---|
| Final well volume (µL) | 100 |
| Fixed volume per well (µL; enzyme/template/master mix, anything common not otherwise listed) | 0 |
| Minimum pipettable volume (µL) | 0.5 |
| Stock overage (fraction) | 0.15 |
| Dead volume per source reservoir (µL) | 20 |
| Plate format | 96 (8×12) or 384 (16×24) |
| Fill direction | row-wise (left to right, A1, A2 … A12, B1 …) as default; column-wise as an option |

Further layout options:
- reserved/control wells (a user-selected set)
- exclude edge wells
- randomize run order (with a seed)
- number of replicates per run
- if the runs exceed the available wells, spill onto plate 2, 3, …

## Calculations
- Factor volume: `V_i = C_final_i / C_stock_i * V_total`
- Constant volume: `final_conc / stock_conc * V_total`
- Water: `V_total - fixed - sum(all component volumes)`
- A well **fails (OVER VOLUME)** if water < 0.
- A well is **flagged (BELOW MIN)** if any non-zero volume, including water, is under the minimum pipettable volume. If a working stock is defined, use it for levels that would otherwise fall below the minimum.
- Report the fraction of the well used before water for each well, plus the worst case.

## Constraint diagnostics
These are the main value over the spreadsheet:
- **Feasibility rule.** A well fits only if `sum(C_i / Stock_i) + fixed / V_total <= 1`. Changing V_total does not help; only stocks, fixed volume or ranges do. Show this clearly.
- **Failing wells.** For each failing well, show how much it overflows and which component takes the largest share of the volume.
- **Headroom.** Show the remaining headroom in the most crowded well, i.e. how much more could fit.
- **Minimum stocks.** For each component, calculate the minimum stock needed for all wells to fit, holding the other stocks fixed. Flag it if this exceeds `max_stock_conc`.
- **Optional optimiser.** Use `lpSolve` or `nloptr` to find a set of stocks within the max limits that makes every well fit, if one exists.

## Outputs
- **Plate maps.** A ggplot heatmap per component (µL per well), selectable in the UI. Failing wells are outlined red and below-minimum wells orange.
- **Volumes table.** Run, plate, well, volume per component, fixed, water, total, status.
- **Stock preparation table.** Total dispensed + overage + dead volume per stock, and the number of wells receiving it.
- **Picklist CSV.** Columns: step, source, destination plate, destination well, volume. Drop zero volumes. Order of addition: water → constant buffers → salts → other additives → viscous components (PEG) → fixed mix last. Make the order editable.
- **Excel export.** One workbook with all of the above. **Lesson from the Excel version:** make sure only one sheet is selected when saving. Grouped tabs caused edits to overwrite the design sheet.

## UI layout (Shiny)
- **Sidebar:** upload the design CSV, upload/download the stock CSV, settings, plate format.
- **Tab "Stocks":** the editable `rhandsontable`, with the per-component check next to each row. Edits recalculate live.
- **Tab "Feasibility":** summary counts, worst-case fraction, failing-wells table, minimum-stock suggestions.
- **Tab "Plate maps":** a component selector and the heatmap.
- **Tab "Volumes" / "Picklist":** tables plus download buttons.

## Regression test: must match the Excel calculator
Fixtures: `tests/fixtures/ASR4_design_PEG24.csv` + `tests/fixtures/ASR4_stocks.csv`
Settings: 100 µL final, 0 µL fixed, 0.5 µL minimum, 15% overage, 20 µL dead volume, 96-well, row-wise fill.

Expected:
- 88 runs → Run 1 = A1, Run 12 = A12, Run 13 = B1, Run 88 = H4
- 70 OK, 18 OVER VOLUME, 0 BELOW MIN
- Worst-case fraction used before water = 122.4%
- Run 1 (A1) volumes, µL:
  | MgCl2 | Kcl | PEG8000 | Betaine | Trehalose | TCEP | CaCl2 | Tween20 | Tris | Water |
  |---|---|---|---|---|---|---|---|---|---|
  | 2.4 | 12.5 | 0 | 2 | 2 | 4 | 0 | 1 | 25 | 51.1 |
- Total dispensed: Kcl 1100 µL, PEG8000 2797.5 µL, Tris 2200 µL; water (positive wells only) 2303.9 µL

Notes on the fixtures:
- `ASR4_design_PEG30_transposed.csv` is the original design (PEG max 30%) in transposed form. Use it to test orientation detection. Its volumes will differ from the PEG24 expectations.
- Design header uses `Kcl` (not `KCl`). Matching between design and stock table should be case-insensitive.
