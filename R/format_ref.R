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
#' @param refData Data frame of the reference genotypes
#' @param refid Reference order in the reference genotypes file
#' @param refs Path of the reference genotypes file
#'
#' @return Data frame containing reference of interest genotypes
#' @export
#'
#' @importFrom rlang .data
format_ref = function(refData, refid, refs) {
  if (is.numeric(refid)) {
    ref_int = names(refData)[refid]
  } else {
    ref_int = refid
  }
  ref_profile = euroformix::tableReader(glue("{refs}/EFM_references.csv")) %>%
    filter(.data$Sample.Name == ref_int)
  ref_profile$A1_order = ifelse(ref_profile$`Allele1`>ref_profile$`Allele2`, ref_profile$`Allele1`, ref_profile$`Allele2`)
  ref_profile$A2_order = ifelse(ref_profile$`Allele1`>ref_profile$`Allele2`, ref_profile$`Allele2`, ref_profile$`Allele1`)
  return(ref_profile)
}
