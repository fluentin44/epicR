#' Show normality graphs for a given variable
#'
#' @param df A data frame
#' @param variable A continuous variable
#'
#' @return Plot with 4 graphs highlighting normality
#' @export
#'
#' @examples
bip <- function (df, variable) {
  # df <- df[!is.na(df[[name]]), ]
  #var <- sym(variable)
  p1 <- ggdensity(df, variable,
                  fill = "lightgray",
                  xlab = variable) +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
  p2 <- df %>% ggplot(aes(x = pull(., variable))) + geom_histogram(color = "darkblue", fill = "lightblue") +
    xlab(variable)
  p3 <- ggqqplot(df, variable)
  p4 <- df %>% ggplot(aes(x = "", y = pull(., variable))) +
    geom_boxplot(outlier.colour = "red", outlier.size = 2) +
    ylab(variable)
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}
