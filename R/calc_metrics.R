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
#'
#' @importFrom rlang .data
#'
#' @return return row of metrics for specified thresholds
#' @export
#'
calc_metrics = function(x, A1_thresh, A2_thresh, ref, minimum_snps) {
  . = NULL
  n_snps_total = 0
  n_snps_tested = 0
  n_correct = 0
  n_noref = 0
  n_het = 0
  if (A1_thresh == "Min") {
    x_sort = x %>%
      filter(.data$A1 != 99) %>%
      arrange(desc(.data$A1_Prob)) %>%
      .[c(1:minimum_snps),]
  } else {
    x_sort = x
  }
  for (row in 1:nrow(x_sort)) {
    snp = x_sort[row, "Locus"]
    if (A1_thresh == "Min" | (A1_thresh != "Min" & x_sort[row, "A1_Prob"] >= A1_thresh)) { ## check if Allele 1 is above threshold, if not drop from dataset
      A1 = x_sort[row, "A1"]
    } else {
      next
    }
    if (x_sort[row, "A2"] == 99 | x_sort[row, "A2_Prob"] < (A2_thresh)) { ## check if A2 is missing or below threshold
      A2 = x_sort[row, "A1"]
    } else {
      A2 = x_sort[row, "A2"]
    }
    pred_geno = ifelse(A1 > A2, paste0(A1,A2), paste0(A2,A1))
    if (A1 != A2) {
      n_het = n_het + 1
    }
    n_snps_total = n_snps_total + 1
    if (snp %in% ref$Marker) { ## check if SNP is in reference
      true_geno = paste0(ref$A1_order[ref$Marker==snp], ref$A2_order[ref$Marker==snp])
      n_snps_tested = n_snps_tested + 1
      if (pred_geno == true_geno) {
        n_correct = n_correct + 1
      }
    } else {
      n_noref = n_noref + 1
    }
  }
  snp_row = data.frame(A1_Cutoff=A1_thresh, A2_Cutoff=A2_thresh, Total_SNPs=n_snps_total, N_noref=n_noref, SNPs_Tested=n_snps_tested, N_Geno_Correct=n_correct, Genotype_Accuracy=n_correct/n_snps_tested, Heterozygosity = n_het/n_snps_total)
  #if (n_snps_total < minimum_snps) {
  #  return(NULL)
  #} else {
  return(snp_row)
  #}
}
