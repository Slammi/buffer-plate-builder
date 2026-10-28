# Build a Shinylive (in-browser R) version of the app into _site/.
#
#   Rscript build_shinylive.R            # build
#   Rscript build_shinylive.R serve      # build, then serve at http://localhost:8008
#
# The site is plain static files: host the _site folder on any web server
# (e.g. GitHub Pages or an internal server). It cannot be opened by
# double-clicking index.html. Users need no R: the app runs in their browser
# and their CSV files never leave their computer.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- "_site"

# Stage only what the app needs, so nothing else (.git, zips, tests) is published.
stage <- file.path(tempdir(), "buffer-plate-builder")
unlink(stage, recursive = TRUE)
dir.create(file.path(stage, "tests"), recursive = TRUE)
file.copy("app.R", stage)
file.copy("R", stage, recursive = TRUE)
file.copy(file.path("tests", "fixtures"), file.path(stage, "tests"), recursive = TRUE)

unlink(out_dir, recursive = TRUE)
shinylive::export(stage, out_dir, template_params = list(title = "Buffer Plate Builder"))

if ("serve" %in% args) {
  message("Serving ", normalizePath(out_dir), " at http://localhost:8008 (Ctrl+C to stop)")
  httpuv::runStaticServer(out_dir, port = 8008, browse = FALSE)
}
