.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tidygeocoder <- list(
    tidygeocoder.progress_bar = TRUE,
    tidygeocoder.verbose = FALSE,
    tidygeocoder.quiet = FALSE,
    tidygeocoder.full_results = FALSE,
    tidygeocoder.flatten = FALSE
  )
  toset <- !(names(op.tidygeocoder) %in% names(op))
  if (any(toset)) options(op.tidygeocoder[toset])
  
  invisible()
}