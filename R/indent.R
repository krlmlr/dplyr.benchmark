indent_code <- function(code) {
  formatR::tidy_source(
    comment = TRUE, blank = TRUE, arrow = FALSE, brace.newline = FALSE,
    indent = 2, output = FALSE, text = code, width.cutoff = 80)$text.tidy
}
