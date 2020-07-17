#' Generating a report - takes a regression output file only
#'
#' @param output_file_name
#' @param raw_results_path
#' @param report_template_filename
#' @param covariate_names
#'
#' @return Rmarkdown report in an outs folder in reports folder
#' @export
#'
#' @examples
#'
#' Need to specify the raw results path, the report file *without* extenstion and output file name *without* extension
gen_report <- function(raw_results_path, report_template_filename, output_file_name, covariate_names){
  report_path <- paste0("./reports/", report_template_filename, ".Rmd")
  output_path <- "./reports/outs/"
  rmarkdown::render(
    report_path,
    output_dir = output_path,
    output_file = paste0(output_file_name, ".html"),
    params = list(name = str_replace(output_file_name, ".html", ""),
                  raw_results = raw_results_path,
                  covariates = covariate_names
    )
  )
}
