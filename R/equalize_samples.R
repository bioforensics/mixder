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
#' @title Equalize samples when using replicates
#'
#' @param df_1 Data table of the specified sample
#' @param df_rep Data table of the replicate sample
equalize_samples = function(df_1, df_rep) {
  if (nrow(df_1) > nrow(df_rep)) {
    df_rep = rbind(df_rep, add_rows(df_1, df_rep))
  } else if (nrow(df_1) < nrow(df_rep)) {
    df_1 = rbind(df_1, add_rows(df_rep, df_1))
  }
  return(list(df_1, df_rep))
}
