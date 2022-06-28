#' Extract individual dmCpGs from your regression DMR object
#'
#' @param dmr_object The DMR object from the regression
#' @param dmr_number DMR number x in the table
#'
#' @return A table of the CpGs within the DMR
#' @export
#'
#' @examples
extractDMRcpgs <-
  function(dmr_object, dmr_number){
    RR <-extractRanges(dmr_object$DMRs)
    dab <- sort(RR, by=~Stouffer)
    cgID <-dmr_object$cpg_object@ranges
    nibs <- as.data.frame(subsetByOverlaps(cgID, dab[dmr_number]))
    return(nibs)
  }s
