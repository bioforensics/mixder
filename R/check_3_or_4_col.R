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
#' @title Adds additional columns if necessary to data frame to ensure same
#'    number of columns between sample and replicate
#'
#' @param larger_df The larger of the two data frames
#' @param smaller_df The smaller of the two data frames
#'
#' @return Data frame
#' @export
#'
#' @importFrom tibble add_column
check_3_or_4_col = function(larger_df, smaller_df) {
  if (ncol(larger_df) == 11) {
    if (ncol(smaller_df) == 9) {
      smaller_df=add_column(smaller_df, "Allele.4" = 0, .after="Allele.3")
      smaller_df=add_column(smaller_df, "Height.4" = 0, .after="Height.3")
    } else {
      smaller_df=add_column(smaller_df, "Allele.3" = 0, .after="Allele.2")
      smaller_df=add_column(smaller_df, "Height.3" = 0, .after="Height.2")
      smaller_df=add_column(smaller_df, "Allele.4" = 0, .after="Allele.3")
      smaller_df=add_column(smaller_df, "Height.4" = 0, .after="Height.3")
    }
  } else if (ncol(larger_df) == 9){
    smaller_df=add_column(smaller_df, "Allele.3" = 0, .after="Allele.2")
    smaller_df=add_column(smaller_df, "Height.3" = 0, .after="Height.2")
  }
  return(smaller_df)
}
