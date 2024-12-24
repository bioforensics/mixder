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
#' @title Creates replicate df
#'
#' @description This function reads in both sample files, determines which
#'    contains the larger number of SNPs and creates the replicate files,
#'    with ensuring the same SNPs are in each SNP set for both samples.
#'
#' @param inpath Path of SNP sets
#' @param id Sample ID
#' @param replicate_id Replicate ID
#' @param nsets Number of SNP sets
#'
#' @return List of IDs, identifying the replicate sample
#' @export
create_replicatedf = function(inpath, id, replicate_id, nsets) {
  . = NULL
  allsetspath = glue("{inpath}/{id}_snpsetscombined_evidence.tsv")
  rep_allsetspath = glue("{inpath}/{replicate_id}_snpsetscombined_evidence.tsv")
  if (file.exists(allsetspath)) {
    df_allsets = read.table(allsetspath, sep="\t", header=T)
    rep_allsets = read.table(rep_allsetspath, sep="\t", header=T)
  } else {
    df_allsets = setNames(data.frame(matrix(ncol=11, nrow=0)), c("Sample Name", "Marker", "Allele 1", "Allele 2", "Allele 3", "Allele 4", "Height 1", "Height 2", "Height 3", "Height 4", "Total_Reads"))
    rep_allsets = setNames(data.frame(matrix(ncol=11, nrow=0)), c("Sample Name", "Marker", "Allele 1", "Allele 2", "Allele 3", "Allele 4", "Height 1", "Height 2", "Height 3", "Height 4", "Total_Reads"))
    for (i in 1:(nsets)) {
      message(i)
      evidfnrep = glue("{inpath}/{replicate_id}_set{i}.tsv")
      evidfn = glue("{inpath}/{id}_set{i}.tsv")
      dataseta = read_in_table(evidfn) %>%
        check_nas(.)
      if (ncol(dataseta) != ncol(df_allsets)) {
        dataseta = check_3_or_4_col(df_allsets, dataseta)
      }
      repdata = read_in_table(evidfnrep) %>%
        check_nas(.)
      if (ncol(repdata) != ncol(rep_allsets)) {
        repdata = check_3_or_4_col(rep_allsets, repdata)
      }
      df_allsets = rbind(df_allsets, dataseta)
      rep_allsets = rbind(rep_allsets, repdata)
    }
    write.table(df_allsets, allsetspath, row.names=F, quote=F, col.names=T, sep="\t")
    write.table(rep_allsets, rep_allsetspath, row.names=F, quote=F, col.names=T, sep="\t")
  }
  if (nrow(df_allsets) < nrow(rep_allsets)) {
    equalize_bins(inpath, replicate_id, id, df_allsets, nsets)
    return(list(glue("{id}_rep"), replicate_id))
  } else {
    equalize_bins(inpath, id, replicate_id, rep_allsets, nsets)
    return(list(id, glue("{replicate_id}_rep")))
  }
}
