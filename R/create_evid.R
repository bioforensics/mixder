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
#' @title Create evidence files for EFM mixture deconvolution
#'
#' @param sample Sample ID
#' @param rep_sample Replicate ID
#' @param inpath Directory containing the SNP sets
#'
#' @return Evidence file for EFM
#' @export
create_evid = function(sample, rep_sample, inpath) {
  evidfn = glue("{inpath}/{sample}.tsv")
  if (rep_sample == "") {
    if (check_reads(evidfn)) {
      evidData = vector()
    } else {
      evidData = euroformix::sample_tableToList(read_in_table(evidfn))
    }
  } else {
    if (check_reads(evidfn)) {
      dataseta = data.frame()
    } else {
      dataseta = read_in_table(evidfn) %>%
        check_nas()
    }
    evidfnrep = glue("{inpath}/{rep_sample}.tsv")
    if (check_reads(evidfnrep)) {
      repdata = data.frame()
    } else {
      repdata = read_in_table(evidfnrep) %>%
        check_nas()
    }
    if (ncol(dataseta) == ncol(repdata)) {
      final_dfs = equalize_samples(dataseta, repdata)
    } else {
      corrected_tables = fix_columns(dataseta, repdata)
      final_dfs = equalize_samples(corrected_tables[[1]], corrected_tables[[2]])
    }
    merge_table = rbind(final_dfs[[1]], final_dfs[[2]])
    evidData = euroformix::sample_tableToList(merge_table)
  }
  return(evidData)
}
