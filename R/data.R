#' @name poorprog
#' @title Analysis results for poor prognosis serous ovarian cancer
#' @description Data frames listing a set of genes
#' differentially expressed between long surviving (good prognosis)
#' and short surviving (poor prognosis) cases, their putative targeting
#' miRNAs, and significantly enriched pathways (FDR < 0.1). Each row
#' contains a miRNA-gene/gene-pathway pair, a description, expression
#' correlation (path values), and expression fold-change (node values).
#' @return data frame
#' @docType data
#' @usage ovca
#' @format a \code{list} instance containing 2 data frames.
#' @source The Cancer Genome Atlas
NULL 

#' @name mesenchymal
#' @title Analysis results of Yang et al.'s (2013) master microRNA regulatory network
#' @description Data frames listing a set of genes
#' differentially expressed between mesenchymal and three other serous
#' ovarian cancer subtypes, eight key miRNAs predicted to target them,
#' and significantly enriched pathways (FDR < 0.1). Each row
#' contains a miRNA-gene/gene-pathway pair, a description, expression
#' correlation (path values), and expression fold-change (node values).
#' @return data frame
#' @docType data
#' @usage ovca
#' @format a \code{list} instance containing 2 data frames.
#' @source The Cancer Genome Atlas. Yang et al., 2013.
NULL 