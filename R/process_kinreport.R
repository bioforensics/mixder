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
#' @title Function to determine how to create the AT file
#'
#' @description This function determines if there is a replicate sample, it will
#'    calculate the AT for all SNPs using each sample, then for each SNP will
#'    use the higher threshold for the final AT file.
#'
#' @param sample Sample ID
#' @param rep_sample Replicate ID
#' @param kinpath path of the Kintelligence Sample Reports
#' @param dynamicAT Dynamic analytical threshold
#' @param staticAT Static analytical threshold
#'
#' @return Data frame containing the AT for each SNP
#' @export
process_kinreport = function(sample, rep_sample, kinpath, dynamicAT, staticAT) {
  if (rep_sample != "") {
    first_sample = calculate_at(sample, kinpath, dynamicAT, staticAT)
    second_sample = calculate_at(rep_sample, kinpath, dynamicAT, staticAT)
    merged_files = merge(first_sample, second_sample, by="Marker", all=T)
    merged_files$Thresh = ifelse(merged_files$Thresh.x > merged_files$Thresh.y, merged_files$Thresh.x, merged_files$Thresh.y)
    return(merged_files[,c(1,4)])
  } else {
    at = calculate_at(sample, kinpath, dynamicAT, staticAT)
    return(at)
  }
}
