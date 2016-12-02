#' letters2numbers
#'
#' letters2numbers returns the number(s) corresponding to a letter or set of letters. Useful when a researcher tells you he is interested in column k, bc, and dd from an excel sheet.
#'
#' @param letterlist A letter or vector of letters to get column numbers for.

#'
#' @return numeric vector useful for getting variable names via indexing
#' @export
#'
#' @examples
#' #indexLocation <- letters2numbers(c('k', 'bc', 'dd'))
#' #actualNames <- names(df)[indexLocation]
letters2numbers <- function (letterlist) {
    eg <- expand.grid(letters[1:26], letters[1:26])
    j <- c(letters[1:26], paste0(eg[, 2], eg[, 1]))
    conversionTable <- data.frame(ColNumber = 1:702, ColLetter = j)
    nums <- subset(conversionTable, conversionTable$ColLetter %in%
        tolower(letterlist))
    orig = data.frame(ColLetter = letterlist)
    out <- suppressWarnings(dplyr::left_join(orig, nums, by = 'ColLetter')[[2]])
    return(out)
}
