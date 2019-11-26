#' Bacon
#'
#' @param bmiq_results_file
#'
#' @return A list with lamdas for before and after and tibble with bacon added
#' @export
#'
#' @examples
bacon_short <- function(x){
  col_order <- c("Row.names",
                 "logFC",
                 "AveExpr",
                 "t",
                 "P.Value",
                 "adj.P.Val",
                 "bac_tstat",
                 "bac_pval",
                 "bac_fdr",
                 "B",
                 "chr",
                 "pos",
                 "strand",
                 "Name",
                 "Relation_to_Island",
                 "UCSC_RefGene_Name",
                 "UCSC_RefGene_Accession",
                 "UCSC_RefGene_Group",
                 "Regulatory_Feature_Group"
  )
  x <- arrange(x, t)
  bc<-bacon(teststatistics = x$t)
  bc_pval<-as.data.frame(pval(bc, corrected = T))
  bc_tstat<-tstat(bc, corrected = T)
  bacon_results<-cbind(bc_pval, bc_tstat)
  colnames(bacon_results)<-c("bac_pval", "bac_tstat")
  bacon_results$bac_fdr<-p.adjust(
    bacon_results$bac_pval,
    method = "BH",
    n = length(bacon_results$bac_pval)
  )
  final_results <- cbind(x, bacon_results)
  final_results <- final_results[, col_order]
  bacon_lambdas <- tibble(
    "Lambda" = c((estlambda(final_results$P.Value, method = "median"))$estimate,
                 (estlambda(final_results$bac_pval, method = "median"))$estimate),
    "FDR<0.25" = c(sum(final_results$adj.P.Val < 0.25), sum(final_results$bac_fdr < 0.25)),
    "FDR<0.20" = c(sum(final_results$adj.P.Val < 0.20), sum(final_results$bac_fdr < 0.20)),
    "FDR<0.15" = c(sum(final_results$adj.P.Val < 0.15), sum(final_results$bac_fdr < 0.15)),
    "FDR<0.10" = c(sum(final_results$adj.P.Val < 0.10), sum(final_results$bac_fdr < 0.10)),
    "FDR<0.05" = c(sum(final_results$adj.P.Val < 0.05), sum(final_results$bac_fdr < 0.05))
  )
  bacon_final <- list(final_results, bacon_lambdas)
  names(bacon_final) <- c("table", "lambdas")
  return(bacon_final)
}
