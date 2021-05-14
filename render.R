source("render_functions.R")
if (interactive()) {
  checklist::check_filename("source")
  lintr::lint_dir("source", pattern = "\\.[Rr](md)?$")
}
render_all()
