#' Plot results from missMethyl analysis of significant array hits
#'
#' @param x
#'
#' @return A table of missMrthyl results
#' @export
#'
#' @examples
mm_plot_0.25 <- function(x) {
  if (class(x) == "data.frame") {
    x %>%
      DT::datatable(
        filter = 'top',
        rownames = FALSE,
        extensions = 'FixedColumns',
        options = list(
          pageLength = 10,
          dom = 'tpf',
          scrollX = TRUE,
          fixedColumns = TRUE
        )
      ) %>%
      DT::formatStyle(
        columns = c("ONTOLOGY", "N", "DE", "P.DE", "FDR"),
        `text-align` = 'center'
      ) %>%
      DT::formatSignif(columns = c("P.DE", "FDR"), digits = 3)
  }
}
