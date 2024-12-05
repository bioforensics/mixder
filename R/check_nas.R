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
#' @title Checks if entire column has NA values
#'
#' @param df data frame
#'
#' @return data frame
#' @export
#'
check_nas = function(df) {
  if (sum(!is.na(df$Allele.4))==0) {
    df[,c("Allele.4","Height.4")] = list(NULL)
  }
  if (sum(!is.na(df$Allele.3))==0) {
    df[,c("Allele.3","Height.3")] = list(NULL)
  }
  df[is.na(df)] = 0
  #df %>% replace(is.na(.data), 0)
  return(df)
}
