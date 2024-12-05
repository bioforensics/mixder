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

#' Title Check the mixture ratios for potentially single source samples (or samples with very high mixture ratios)
#'
#' @param df dataframe of mixture ratios computed by EFM
#' @param uncond TRUE/FALSE if unconditioned analysis
#'
#' @return message
#' @export
#'
#' @importFrom stats median

check_ratios = function(df, uncond=FALSE) {
  alert = "<B>Sample may be single source or have a very large mixture ratio. Please confirm a 2-person mixture and proceed with caution. Minor contributor inferred genotypes may be unreliable.<br/>"
  if (uncond) {
    range = max(as.numeric(df$C1_Prob), na.rm=TRUE) - min(as.numeric(df$C1_Prob), na.rm=TRUE)
    median_C1 = median(as.numeric(df$C1_Prob), na.rm=TRUE)
    if (median_C1 > 0.99 | range > 0.15) {
      message(alert)
    }
  } else {
    mean_C1 = mean(as.numeric(df$C1_Prob), na.rm=TRUE)
    mean_C2 = mean(as.numeric(df$C2_Prob), na.rm=TRUE)
    if (mean_C1 > 0.99 | mean_C2 > 0.99) {
      message(alert)
    }
  }
}
