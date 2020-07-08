#' List all the colnames in alphabetical order as a tibble
#'
#' @param df
#'
#' @return printed tibble in console of colnames
#' @export
#'
#' @examples
list_colnames <- function(df){
  print(enframe(sort(colnames(df)), name = NULL), n=Inf)
}
