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
#' @title Identifies if one sample has more columns than another;
#'     adds columns as needed.
#'
#' @param df1 Data frame containing sample ID called genotypes
#' @param df2 Data frame containing replicate ID called genotypes
#'
#' @return list of data frames containing equal number of columns
fix_columns = function(df1, df2) {
  if (ncol(df1) > ncol(df2)) {
    df2 = check_3_or_4_col(df1, df2)
  } else if (ncol(df2) > ncol(df1)) {
    df1 = check_3_or_4_col(df2, df1)
  }
  return(list(df1, df2))
}
