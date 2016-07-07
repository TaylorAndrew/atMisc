#' tdate
#'
#' tdate returns a character string of the current day's date. It is a convenience wrapper for `format(Sys.Date())`.
#'
#' @param format Character string providing formatting instructions to format()
#'
#' @return character string of the current day's date
#' @export
#'
#' @examples
#' #tdate()
#' #tdate("%Y%m%d")
#' #tdate("%A, %B %d, %Y")
tdate <- function(format = "%Y%B%d")
  format(Sys.Date(), format)
