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
#' Title Read in dataset to run through EFM
#'
#' @param df data table of SNPs
#'
#' @return data frame of SNPs
#' @export
#'
#' @importFrom data.table fread
read_in_table = function(df) {
  evid_df = data.frame(fread(df, header=T, sep="\t"))
  myColClasses = sapply(evid_df, class)
  needed_cols = c("Allele.1", "Allele.2", "Allele.3", "Allele.4")
  myColClasses =
    ifelse(names(myColClasses) %in% needed_cols,
           "character",
           myColClasses)
  evid_df_form = data.frame(fread(df, header=T, sep="\t", colClasses=myColClasses))
  evid_final = convert_table_to_list(evid_df_form)
  return(evid_final)
}
