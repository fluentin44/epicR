#' Get universe og genes without duplicates
#'
#' @param bmiq_doc
#'
#' @return A character string of around 23k entries
#' @export
#'
#' @examples
universe <- function(bmiq_doc){
  bmiq_doc <- bmiq_doc[!(is.na(bmiq_doc$UCSC_RefGene_Name) | bmiq_doc$UCSC_RefGene_Name == ""),]
  bmiq_doc$UCSC_RefGene_Name <- sub(";.*", "", bmiq_doc$UCSC_RefGene_Name)
  gene_names <- unique(bmiq_doc$UCSC_RefGene_Name)
  return(gene_names)
}
