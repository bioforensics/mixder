# -------------------------------------------------------------------------------------------------
# Copyright (c) 2024, DHS.
#
# This file is part of MixDeR and is licensed under the BSD license: see LICENSE.
#
# This software was prepared for the Department of Homeland Security (DHS) by the Battelle National
# Biodefense Institute, LLC (BNBI) as part of contract HSHQDC-15-C-00064 to manage and operate the
# National Biodefense Analysis and Countermeasures Center (NBACC), a Federally Funded Research and
# Development Center.
# -------------------------------------------------------------------------------------------------
#' @title Calculating metrics for a single set of allele 1 and allele 2 probability thresholds
#'
#' @param x Data frame of allele probabilities and genotype calls
#' @param A1_thresh Allele 1 probability threshold
#' @param A2_thresh Allele 2 probability threshold
#' @param ref Data frame of reference genotypes
#' @param minimum_snps Minimum number of SNPs required
#' @param filter_missing TRUE/FALSE to filter SNPs with missing allele 2 values
#'
#' @importFrom rlang .data
#'
#' @return return row of metrics for specified thresholds
#' @export
#'
calc_metrics = function(x, A1_thresh, A2_thresh, ref, minimum_snps, filter_missing) {
  . = NULL
  if (filter_missing) {
    x = subset(x, !(x$A2 == 99 & x$A2_Prob<A2_thresh))
  }
  if (A1_thresh == "Min") {
    x_sort = x %>%
      filter(.data$A1 != 99) %>%
      arrange(desc(.data$A1_Prob)) %>%
      .[c(1:minimum_snps),]
  } else {
    x_sort = x %>%
      filter(.data$A1_Prob >= A1_thresh)
  }
  x_sort$A2_real = ifelse(x_sort$A2_Prob<A2_thresh | x_sort$A2 == 99, x_sort$A1, x_sort$A2)
  x_sort$geno = ifelse(x_sort$A1>x_sort$A2_real, paste0(x_sort$A1, x_sort$A2_real), paste0(x_sort$A2_real, x_sort$A1))
  x_sort$het = ifelse(x_sort$A1==x_sort$A2_real, 0, 1)
  ref$ref_geno = paste0(ref$A1_order,ref$A2_order)
  merged_ref = merge(x_sort, ref, by.x="Locus", by.y="Marker", all.x=T)
  n_snps_total=nrow(x_sort)
  n_noref = sum(is.na(merged_ref$ref_geno))
  matching_ref = subset(merged_ref, !is.na(merged_ref$ref_geno))
  n_snps_tested = nrow(matching_ref)
  n_correct = sum(matching_ref$geno==matching_ref$ref_geno)
  n_het=sum(x_sort$het==1)
  snp_row = data.frame(A1_Cutoff=A1_thresh, A2_Cutoff=A2_thresh, Total_SNPs=n_snps_total, N_noref=n_noref, SNPs_Tested=n_snps_tested, N_Geno_Correct=n_correct, Genotype_Accuracy=n_correct/n_snps_tested, Heterozygosity = n_het/n_snps_total)
  #if (n_snps_total < minimum_snps) {
  #  return(NULL)
  #} else {
  return(snp_row)
  #}
}
