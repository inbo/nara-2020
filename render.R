source("render_functions.R")
if (interactive()) {
  checklist::check_filename("source")
  checklist::check_lintr("source")
}
render_all()
