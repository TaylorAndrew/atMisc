#' qprint
#'
#' qprint prints data.frame, matrix, and table objects to word documents.
#'
#' @param table Either a data.frame, matrix, or table R object
#' @param filename Name of the output word document
#' @param tableHeading Heading for the table in the output word document
#' @param row.names If TRUE, the row.names of the input object is retained for the output document
#' @param fontsize Fontsize for the output document
#' @param column.margins Width of the columns in the output table
#'
#' @return NULL
#' @export
#'
#' @examples
#' #qprint(table = OutputTable, filename = "OutputFileName.doc")
qprint      <- function(table = FALSE,
                        filename = "RPrintout.doc",
                        tableHeading = "Table X",
                        row.names = FALSE,
                        fontsize = 10,
                        column.margins = 1) {
  #Get package and load
  type <- class(table)
  tabtypes <- c("table", "data.frame", "matrix")
  match <- type %in% tabtypes
  if (match != 1)
    print("Error: You have specified a non-Table-Type Object as a Table-Type Object")
  else {
    #Write to rtf
    rtf <- RTF(
      filename,
      width = 8.5,
      height = 11,
      font.size = 10,
      omi = c(1, 1, 1, 1)
    )
    addParagraph(rtf, tableHeading)
    len <- length(table[1, ])
    columnwid <- rep(column.margins, len)
    if (row.names == TRUE)
      columnwid <- c(columnwid, column.margins)
    addTable(
      rtf,
      table,
      font.size = fontsize,
      row.names = row.names,
      NA.string = "-",
      col.widths = columnwid
    )
    done(rtf)
  }
}
