#' rbind_
#'
#' rbind_ finds columns that are missing in either data.frame, add them as NA and then row bind the two data.frames together
#'
#' @param data1 a data.frame
#' @param data2 a data.frame
#'
#' @return a row-binded data.frame containing all columns in both data.frames
#' @export
#'
#' @examples
#' #Needs example
rbind_ <- function(data1, data2) {
  nms1 <- names(data1)
  nms2 <- names(data2)
  if(mean(nms1%in%nms2)==1 & mean(nms2%in%nms1)==1) {
    out <- rbind(data1, data2)
  } else {
    data1[nms2[!nms2%in%nms1]] <- NA
    data2[nms1[!nms1%in%nms2]] <- NA
    out <- rbind(data1, data2)
  }
  return(out)
}
