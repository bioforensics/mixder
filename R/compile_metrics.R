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
#' @title Compiling metrics for the range of allele 1 and allele 2 probablity thresholds
#'
#' @param x Data frame of allele probabilities and genotype calls
#' @param ref Data frame of reference genotypes
#' @param minimum_snps Minimum number of SNPs required
#' @param A1min Minimum value of allele 1 probability threshold
#' @param A1max Maximum value of allele 1 probability threshold
#' @param A2min Minimum value of allele 2 probability threshold
#' @param A2max Maximum value of allele 2 probability threshold
#'
#' @return List of data frames compiled using the min/max values of the allele probability thresholds and the minimum number of SNPs
#' @export
#'
#' @importFrom stats setNames
compile_metrics = function(x, ref, minimum_snps, A1min, A1max, A2min, A2max) {
  final_table = setNames(data.frame(matrix(ncol=8, nrow=0)), c("A1_Cutoff", "A2_Cutoff", "Total_SNPs", "N_noref", "SNPs_Tested", "N_Geno_Correct", "Genotype_Accuracy", "Heterozygosity"))
  final_table_min = setNames(data.frame(matrix(ncol=8, nrow=0)), c("A1_Cutoff", "A2_Cutoff", "Total_SNPs", "N_noref", "SNPs_Tested", "N_Geno_Correct", "Genotype_Accuracy", "Heterozygosity"))
  if (A1max < A1min | A2max < A2min) {
    message("Allele ranges not entered correctly. Please re-run!")
    stop()
  }
  for (t in seq(A2min, A2max, 0.01)) {
    message(paste0(t, "<br/>"))
    for (j in seq(A1min, A1max, 0.01)) {
      message(paste0(j, "<br/>"))
      new_row = calc_metrics(x, j, t, ref, minimum_snps)
      if (!is.null(new_row)) {
        final_table = rbind(final_table, new_row)
      }
    }
    new_row_min = calc_metrics(x, "Min", t, ref, minimum_snps)
    final_table_min = rbind(final_table_min, new_row_min)
  }
  sort_final_table = final_table %>%
    arrange(desc(.data$Genotype_Accuracy))
  sort_final_table_min = final_table_min %>%
    arrange(desc(.data$Genotype_Accuracy))
  return(list(sort_final_table, sort_final_table_min))
}
