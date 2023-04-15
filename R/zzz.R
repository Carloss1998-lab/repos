.onAttach <- function( libname, pkgname ) {
  packageStartupMessage(
    paste0( "\nPlease cite the acar package as:\n"),
    domain = NULL,  appendLF = TRUE )
}

.onLoad <- function(libname, pkgname) {
  ## max rows and columns to output when printing matrices/vectors
  options(max.rows = 20L,
          max.cols = 7L)
}

.onUnload <- function(libpath) {
  .Options$max.rows <- NULL
  .Options$max.cols <- NULL
}
