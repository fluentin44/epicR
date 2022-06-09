#' Show normality graphs for a given variable
#'
#' @param df A data frame
#' @param variable A continuous variable
#'
#' @return Plot with 4 graphs highlighting normality
#' @export
#'
#' @examples
normality_graphs <- function(df, variable){

  df <- df[!is.na(df[[variable]]), ]
  p1 <- ggdensity(df[[variable]], fill = "lightgray") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
  p2 <- ggplot(df, aes(x = df[[variable]])) +
    geom_histogram(color = "darkblue", fill = "lightblue")
  p3 <- ggqqplot(df[[variable]])
  p4 <- ggplot(df, aes(x = "", y = df[[variable]])) +
    geom_boxplot(outlier.colour = "red",
                 outlier.size = 2)
  #print(paste0("Number of values for ", variable, " = ", length(df[[variable]])))
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  #grid.arrange(p2, p4, ncol = 2)
}
