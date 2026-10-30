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
#' @title Formatting reference file
#'
#' @param refid ID of the reference genotypes of interest
#' @param refs Path of the reference genotypes file
#'
#' @return Data frame containing reference of interest genotypes
#' @export
#'
#' @importFrom rlang .data
#' @importFrom data.table fread
format_ref = function(refid, refs) {
  ref_profile = data.frame(fread(glue("{refs}/EFM_references.csv"))) %>%
    filter(.data$Sample.Name == refid)
  ref_profile$A1_order = ifelse(ref_profile$`Allele1`>ref_profile$`Allele2`, ref_profile$`Allele1`, ref_profile$`Allele2`)
  ref_profile$A2_order = ifelse(ref_profile$`Allele1`>ref_profile$`Allele2`, ref_profile$`Allele2`, ref_profile$`Allele1`)
  return(ref_profile)
}
