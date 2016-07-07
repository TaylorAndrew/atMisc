#' rep_
#'
#' rep_  is a 'better' implementation of the rep (repeat) base R function
#'
#' @param x Vector
#' @param times Number of times to repeat the vector
#' @param each Number of times to repeat each element in the vector
#'
#' @return A vector
#' @export
#'
#' @examples
#' #rep_(c("A","B","C"),each=c(1,2,3))
#' #lst<-c(letters[1:4])
#' #rep(lst,times=c(1,1,4,4,3,3,2,2),each=2)
#' #rep_(lst,times=2,each=c(1,4,3,2))
#' #x<-letters[1:4]
#' #rep(x,times=3)
#' #rep_(x,times=3)
#' #rep(x,each=3)
#' #rep_(x,each=3)
#' #rep(x, each=c(1,2,3,4))
#' #rep(x, times=c(1,2,3,4))
#' #rep_(x, each=c(1,2,3,4))
#' #rep(x,times=c(1,2,3,4),each=2)
#' #rep_(x,times=2,each=c(1,2,3,4))
rep_ <- function(x,times = 1,each = NULL) {
  if (is.null(each))
    x <- rep(x,times)
  if (!is.null(each)) {
    if (length(each) == 1)
      x <- rep(rep(x,each = each),times)
    if (length(each) > 1) {
      if (length(x) != length(each))
        print("STOP, length x != length each")
      if (length(x) == length(each)) {
        eachDo <- function(i)
          j <- rep(x[i], each[i])
          x <- rep(do.call(c,lapply(1:length(each),eachDo)),times)
      }
    }
  }
  return(x)
}
