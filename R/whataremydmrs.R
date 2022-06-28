#' Extract DMRs from the regression DMR object
#'
#' @param dmr_object
#'
#' @return Returns a df with DMRs arranged by stouffers value
#' @export
#'
#' @examples
whataremydmrs <-
  function(dmr_object){
    arrange(as.data.frame(extractRanges(dmr_object$DMRs)), Stouffer)
  }
