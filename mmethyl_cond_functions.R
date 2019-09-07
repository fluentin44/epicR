this <- function(sig_0.25) {
  if (sig_0.25 < 3) {
    tit <- 12+20
  }
  else{
    print("that")
  }
  return(tit)
}

this <- function(sig_0.25) {
  if (sig_0.25 < 3) {
    tit <- 12+20
    titll <- tit+20
  }
  return(titll)
}



library(tidyverse)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

raw_data <- "E:/BMIQ MAV/19-08-30-re-runs_full_cohort/results/etotarea_pca1-7_results.txt"
raw_data <- "E:/BMIQ MAV/19-08-30-re-runs_full_cohort/results/etotbmd_pca1-16_results.txt"

cols<-c(
  "Row.names",
  "logFC",
  "AveExpr",
  "t",
  "P.Value",
  "adj.P.Val",
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

var_raw<-readr::read_tsv(paste0(raw_data),
                  col_names = F,
                  skip = 1,
                  na = character()
)
var<-var_raw[,-1]
#var<-var_raw
colnames(var)<-cols
var <- var %>% dplyr::arrange(adj.P.Val) %>% rowid_to_column("Rank")
var$UCSC_RefGene_Name<-sub(";.*", "", var$UCSC_RefGene_Name)
var$UCSC_RefGene_Group<-sub(";.*", "", var$UCSC_RefGene_Group)



Universe<-as.character(var$Row.names)
sig_0.25<-as.character(var$Row.names[var$adj.P.Val<0.25])
top100_named_genes <- var[!(is.na(var$UCSC_RefGene_Name) | var$UCSC_RefGene_Name == ""),]
top_100_cpgs <- top100_named_genes %>% arrange(adj.P.Val) %>%  dplyr::slice(1:100) %>% pull(Row.names)

#FDR<0.25
#Sig_0.25<-as.character(Sig_0.25)
#Universe<-as.character(Universe)
mm_sig_cpgs <- function(x) {
  if (length(x) > 1) {
    missMethyl::gometh(
      sig.cpg = x,
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


class(gst_0.25_GO) == "data.frame"

gst_0.25_GO <- mm_sig_cpgs(sig_0.25)

plot_mm_0.25(gst_0.25_GO)

class(gst_0.25_GO)

exists("var")



this(that)
sig <- this(or)

this(or)

that <- 81
or<-0.11

