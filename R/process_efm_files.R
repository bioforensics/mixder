#' @title Process files created by EFM
#'
#' @description Subsets the EFM files by contributor (C1/C2)
#'
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
#' @param x EFM file
#' @param contrib C1/C2
#' @param ref Reference genotype file
#' @param minimum_snps Minimum number of SNPs required
#' @param A1min Minimum value for allele 1 probability thresholds for calculating metrics
#' @param A1max Maximum value for allele 1 probability thresholds for calculating metrics
#' @param A2min Minimum value for allele 2 probability thresholds for calculating metrics
#' @param A2max Maximum value for allele 2 probability thresholds for calculating metrics
#' @param metrics TRUE/FALSE to calculate validation metrics
#'
#' @return Data frame containing either calculated metrics (if specified) or
#'    data frame containing genotype calls
#'
#' @importFrom rlang .data
#' @importFrom tidyr separate
#' @importFrom stats aggregate
#' @export


process_efm_files = function(x, contrib, ref, minimum_snps, A1min, A1max, A2min, A2max, metrics=TRUE) {
  . = NULL
  C_df=x %>%
    filter(.data$Contr. == contrib)
  df_agg = aggregate(.~ Locus, data=C_df[,c(2:4)], FUN=function(x) paste(x, collapse=","))
  df_sep = df_agg %>%
    separate(., .data$Allele, c("A1","A2")) %>%
    separate(., .data$Probability, c("A1_Prob", "A2_Prob"), sep=",", convert=TRUE) %>%
    filter(.data$A1 != 99)
  df_sep$A2 = ifelse(is.na(df_sep$A2), df_sep$A1, df_sep$A2)
  df_sep$A2_Prob = ifelse(is.na(df_sep$A2_Prob), df_sep$A1_Prob, df_sep$A2_Prob)
  if (metrics) {
    df_sep$Locus = tolower(df_sep$Locus)
    final_table_list = compile_metrics(df_sep, ref, minimum_snps, A1min, A1max, A2min, A2max)
  } else {
    final_table_list = df_sep
  }
  return(final_table_list)
}
