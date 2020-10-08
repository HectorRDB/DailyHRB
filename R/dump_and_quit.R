#' dump_and_quit Function
#'
#' From https://adv-r.hadley.nz/debugging.html#alternatives
#'
#' @export
#' @return Invisible NULL
#' @importFrom utils dump.frames
#'

dump_and_quit <- function(file = "last.dump") {
  # Save debugging info to file last.dump.rda
  dump.frames(dumptp = file, to.file = TRUE, include.GlobalEnv = TRUE)
  # Quit R with error status
  q(status = 1)
}
