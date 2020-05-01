#' Generating a report - takes a limm atable file only
#'
#' @param output_file_name
#' @param raw_results_path
#'
#' @return Rmarkdown report in an outs folder in reports folder
#' @export
#'
#' @examples
gen_report <- function(output_file_name, raw_results_path){
  report_path <- "./reports/red_report.Rmd"
  output_path <- "./reports/outs/"
  #output_file <- paste0("etotarea_bone_without_d", ".html")
  rmarkdown::render(
    report_path,
    output_dir = output_path,
    output_file = paste0(output_file_name, ".html"),
    params = list(name = str_replace(output_file_name, ".html", ""),
                  cutoff = "0%",
                  raw_results = raw_results_path
                  # corr_plot = ("pipeline_outs/corr_plot_area.txt"),
                  # pheno_nona = ("objects/p_post-subsetNAs_area.rds"),
                  # pheno_forpcs = ("objects/p_postsubset_forPCs_area.rds"),
                  # combat_data = ("../combat_beta_10_universal.rds")
    )
  )
}
