#' Perform missMethyl on array hits with an FDR <0.5
#'
#' @param sig_hits
#'
#' @return A data frame with the top 50 most significant results from all GO ontologies.
#' @export
#'
#' @examples
mm_sig_cpgs <- function(sig_hits) {
  if (length(sig_hits) > 1) {
    missMethyl::gometh(
      sig.cpg = sig_hits,
      all.cpg = Universe,
      collection = "GO",
      array.type = "EPIC"
    ) %>%
      rename("TERM" = "Term") %>%
      limma::topGO(., number = 50) %>%
      arrange(ONTOLOGY) %>%
      readr::write_csv(paste0(data_output, "./missMethyl_results.csv"), col_names = T)
  } else {
    print("n too low for missMethyl on sig cpgs alone")
  }
}
