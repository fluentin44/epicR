#' See the corner of an object
#'
#' @param object
#'
#' @return printed corner in console
#' @export
#'
#' @examples
corner <- function(object){
  object[1:5,1:5]
}



#' See corner of big text file
#'
#' @param file_path
#'
#' @return Prints corner of big text file in console
#' @export
#'
#' @examples
see_txt_corner <- function(file_path){
that <- read.table(file_path, sep = "\t", header = T, nrows = 5)
that[1:5,1:5]
}
