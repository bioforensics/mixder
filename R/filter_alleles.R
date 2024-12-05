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
#' @title Filters the called genotypes based on the allele 1 and allele 2
#'    probability thresholds
#'
#' @param all_files Data frame containing the called genotypes
#' @param contrib_status Major/Minor
#' @param minimum_snps Minimum number of SNPs required
#' @param A1_threshold Allele 1 probability threshold
#' @param A2_threshold Allele 2 probability threshold
#'
#' @return Data frame with filtered allele calls
#' @export
filter_alleles = function(all_files, contrib_status, minimum_snps, A1_threshold, A2_threshold) {
  . = NULL
  filt_df = all_files %>%
    filter(.data$A1_Prob>=A1_threshold)
  if (nrow(filt_df) < minimum_snps | contrib_status == "minor") {
    message("Does not contain minimum number of SNPs above established thresholds or is the minor component. Will use SNP minimum.<br/>")
    filt_df = all_files %>%
      arrange(desc(.data$A1_Prob)) %>%
      .[c(1:minimum_snps),]
  }
  final_df = assigned_A2(filt_df, A2_threshold)
  return(final_df)
}
