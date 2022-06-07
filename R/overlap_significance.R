#' Determines significance of overlap using hypergeometric probability test
#'
#' @param table_a
#' @param table_b
#' @param follow_bacon
#' @param top_100
#' @param fdr
#'
#' @return GeneOverlap result to console
#' @export
#'
#' @examples
overlap_significance <- function(table_a, table_b, follow_bacon=FALSE, top_100=NULL, fdr=NULL){

  if(follow_bacon & is.null(top_100)) {
    print("bacon & is null")
  } else if (follow_bacon & !is.null(top_100)) {
    print("bacon & is not null")
  } else if (!follow_bacon & is.null(top_100)) {
    print("not bacon & is null")
  } else if (!follow_bacon & !is.null(top_100)) {
    print("not bacon & is not null")
  }

  sepd_a <-
    table_a %>%
    separate(., UCSC_RefGene_Name, sep= ";", into="gene_name", extra="drop")

  sepd_b <-
    table_b %>%
    separate(., UCSC_RefGene_Name, sep= ";", into="gene_name", extra="drop")

  if(length(sepd_a) == length(sepd_b)){
    n <- nrow(sepd_a)
  }else{
    stop("Not comparing analyses from same qc")
  }

  if(follow_bacon & !is.null(top_100)) {
    A <-
      sepd_a %>%
      arrange(bacon_FDR) %>%
      dplyr::slice(1:100)
    B <-
      sepd_b %>%
      arrange(bacon_FDR) %>%
      dplyr::slice(1:100)
  } else if (follow_bacon & is.null(top_100)) {
    A <- filter(sepd_a, bacon_FDR < fdr)
    B <- filter(sepd_b, bacon_FDR < fdr)
  } else if (!follow_bacon & !is.null(top_100)) {
    A <-
      sepd_a %>%
      arrange(adj.P.Val) %>%
      slice(1:100)
    B <-
      sepd_b %>%
      arrange(adj.P.Val) %>%
      dplyr::slice(1:100)
  } else if (!follow_bacon & is.null(top_100)) {
    A <- filter(sepd_a, adj.P.Val < fdr)
    B <- filter(sepd_b, adj.P.Val < fdr)
  }

  A <- A %>%
    drop_na(gene_name) %>%
    distinct(gene_name) %>%
    pull(gene_name)

  #message("Length A = ", length(A))

  B <- B %>%
    drop_na(gene_name) %>%
    distinct(gene_name) %>%
    pull(gene_name)

  #message("Length B = ", length(B))

  n <- 26645 # number of unique genes on the array

  # message("my matrix ========")
  # dat <-
  #   as.matrix(data.frame(
  #     row.names = c("NotA", "InA"),
  #     "NotB" = c(n - length(union(A,B)), length(setdiff(A,B))),
  #     "InB" = c(length(setdiff(B,A)), length(intersect(A,B))),
  #     stringsAsFactors = FALSE
  #   ))
  # print(dat)
  #
  # message("my matrix results ========")
  #
  # l <- fisher.test(dat, alternative = "l")$p.value
  # g <- fisher.test(dat, alternative = "g")$p.value
  # t <- fisher.test(dat, alternative = "t")$p.value
  #
  # message("less = ", l)
  # message("greater = ", g)
  # message("two.sided = ", t)
  #
  #message("geneOverlap matrix ========")

  go.obj <- newGeneOverlap(A, B, 26645)
  #print(go.obj)  # not tested yet.
  go.obj <- testGeneOverlap(go.obj)

  #matrix(c(length(intersect(A,B)), length(B)-length(intersect(A,B)), A-length(intersect(A,B)), n-B-A+length(intersect(A,B))), nrow = 2)

  #message("geneOverlap results ========")

  print(go.obj)

  # message("other my matrix ========")
  #
  # overlap <- length(intersect(A,B))
  # A <- length(A)
  # B <- length(B)
  #
  # absy <- matrix(c(overlap, B-overlap, A-overlap, n-B-A+overlap), nrow = 2)
  # print(absy)
  # absy <- matrix(c(overlap, B-overlap, A-overlap, n-((B-overlap)+(A-overlap)+overlap)), nrow = 2)
  # print(absy)
  #
  # reprfact=overlap/((A*B)/n)
  # message("Representation factor = ", reprfact)  # representation factor
  # OR=((n-B-A+overlap)*overlap)/((B-overlap)*(A-overlap))
  # message("Odds ratio = ", OR) # odds ratio
  #
  # message("other my matrix results ========")
  #
  # l <- fisher.test(absy, alternative = "l")$p.value
  # g <- fisher.test(absy, alternative = "g")$p.value
  # t <- fisher.test(absy, alternative = "t")$p.value
  #
  # message("less = ", l)
  # message("greater = ", g)
  # message("two.sided = ", t)
  #
  # message("Hypergeometric probability overlap ========")
  #
  # # q=size of overlap-1
  # # m=number of upregulated genes in experiment #1;
  # # n=(total number of genes on platform-m);
  # # k=number of upregulated genes in experiment #2.
  #
  # peep <- phyper(overlap-1, A, n-A, B, lower.tail = FALSE, log.p = FALSE)
  # print(peep)
  # deep <- sum(dhyper(overlap:B, A, n-A, B))
  # print(deep)
  #
  # # phyper=(overlap-1,list1,PopSize-list1,list2,lower.tail = FALSE, log.p = FALSE)
  #
  # return(list(dat=dat, a=A, b=B, n=n, overlap=overlap, go.obj=go.obj))
}
