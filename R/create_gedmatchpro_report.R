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
#' @title Create GEDmatch PRO report
#'
#' @param x Data frame of EFM predicted genotypes
#' @param contrib Contributor number from the EFM output file (C1/C2)
#' @param contrib_status Major/Minor
#' @param minimum_snps Minimum number of SNPs required
#' @param A1 Allele 1 probability threshold for report
#' @param A2 Allele 2 probability threshold for report
#' @param A1min Minimum value for allele 1 probability threshold
#' @param A1max Maximum value for allele 1 probability threshold
#' @param A2min Minimum value for allele 2 probability threshold
#' @param A2max Maximum value for allele 2 probability threshold
#'
#' @return Data frame
#' @export
create_gedmatchpro_report = function(x, contrib, contrib_status, minimum_snps, A1, A2, A1min, A1max, A2min, A2max) {
  formatted_df = suppressWarnings(process_efm_files(x, contrib, NULL, minimum_snps, A1min, A1max, A2min, A2max, metrics=FALSE))
  report = filter_alleles(formatted_df, contrib_status, minimum_snps, A1, A2)
  return(report)
}
