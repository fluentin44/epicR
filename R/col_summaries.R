#' Summaries for multiple columns in a df
#'
#' @param df
#'
#' @return A data frame with mean, min, max and range for each column of a df
#' @export
#'
#' @examples
col_summaries <- function(df) {
  mean <- vector("double", length(df))
  min <- vector("double", length(df))
  max <- vector("double", length(df))
  range <- vector("double", length(df))
  for (i in seq_along(df)) {
    it <- names(df)
    mean[i] <- mean(df[[i]])
    min[i] <- min(df[[i]])
    max[i] <- max(df[[i]])
    range[i] <- max(df[[i]])-min(df[[i]])

    full_table <- rbind(mean, min, max, range)
    colnames(full_table) <- it
  }
  as.data.frame(full_table)
}
