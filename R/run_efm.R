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
#' @title Running mixture deconvolution in EuroForMix
#'
#' @param popFreq Allele frequency file
#' @param refData Reference genotypes file
#' @param id Sample ID
#' @param replicate_id Replicate ID
#' @param inpath Path to SNP sets
#' @param out_path Output path
#' @param attable AT table
#' @param nsets Number of SNP sets
#' @param cond Sample IDs to condition on
#' @param uncond TRUE/FALSE if performing unconditioned analysis
#'
#' @export
run_efm = function(popFreq, refData, id, replicate_id, inpath, out_path, attable, nsets, cond = NULL, uncond=TRUE) {
  if (replicate_id == "") {
    create_evid_all(inpath, id, nsets)
    write_path = glue("{out_path}Single/{id}")
  } else {
    create_evid_all(inpath, id, nsets)
    create_evid_all(inpath, replicate_id, nsets)
    write_path = glue("{out_path}Replicates/{id}")
  }
  snps_input = glue("{inpath}/snp_sets/")
  final_list = c()
  uncond_finaltable_all = data.frame()
  uncond_ratios = data.frame()
  if (replicate_id != "") {
    ids = create_replicatedf(snps_input, id, replicate_id, nsets)
  }
  for (i in 1:(nsets)) {
    message(glue("Processing set #{i}<br/>"))
    if (replicate_id != "") {
      sample = glue("{as.character(ids[[1]])}_set{i}")
      replicate = glue("{as.character(ids[[2]])}_set{i}")
    } else {
      sample = glue("{id}_set{i}")
      replicate = replicate_id
    }
    evidData = create_evid(sample, replicate, snps_input)
    ##create AT vector
    sample_at = create_at(evidData, sample, replicate, attable)
    if (uncond) {
      if (i == 1) {
      }
      dir.create(file.path(write_path, "unconditioned"), showWarnings = FALSE, recursive=TRUE)
      ##unconditioned analysis
      repeat_num = 0
      repeat {
        message("Running unconditioned analysis<br/>")
        uncond_results = euroformix::calcMLE(2, evidData, popFreq, AT=sample_at, BWS=FALSE, FWS=FALSE, DEG=FALSE, steptol=0.001, pC=0.01, lambda=0.05, fst=0.01)
        uncond_finaltable = euroformix::deconvolve(uncond_results)
        if (check_allele_probabilities(data.frame(uncond_finaltable[["table4"]]))) break
        message("Mixture proportion = 0.5 or Allele probability flipping detected- will rerun!<br/>")
        repeat_num = repeat_num + 1
        if (repeat_num == 10) break
      }
      if (repeat_num < 10) {
        ratio_row = data.frame(Set=i, C1_Prop=uncond_results[["fit"]][["thetahat2"]][["Mix-prop. C1"]], C2_Prob=uncond_results[["fit"]][["thetahat2"]][["Mix-prop. C2"]])
        uncond_ratios = rbind(uncond_ratios, ratio_row)
        write.table(uncond_finaltable[["table4"]], glue("{write_path}/unconditioned/{id}_set{i}_uncond.tsv"), quote=F, row.names=F, sep="\t")
        uncond_finaltable_all = rbind(uncond_finaltable_all, uncond_finaltable[["table4"]])
      } else {
        message("Repeated analysis 10 times unsuccessfully. Will skip set!<br/>")
      }
      if (i == nsets) {
        final_list = c(final_list, list(uncond_finaltable_all, uncond_ratios))
      }
    } else {
      if (i == nsets) {
        final_list = c(final_list, list(uncond_finaltable_all, uncond_ratios))
      }
    }
    ## conditioned analysis
    if (!is.null(cond)) {
      total_refs = length(names(refData))
      cond_vector = rep.int(0, total_refs)
      for (cond_on in cond) {
        nam = glue("df_{cond_on}")
        if (i == 1) {
          assign(nam, data.frame())
          assign(glue("ratios_{cond_on}"), data.frame())
        }
        message(glue("Running conditioned analysis on {cond_on}<br/>"))
        list_num = match(cond_on, names(refData))
        dir.create(file.path(write_path, glue("/conditioned/cond_on_{cond_on}")), showWarnings = FALSE, recursive=TRUE)
        condresults = euroformix::calcMLE(2, evidData, popFreq, refData, AT=sample_at, condOrder=replace(cond_vector, list_num, 1), BWS=FALSE, FWS=FALSE, DEG=FALSE, steptol=0.001, pC=0.01, lambda=0.05, fst=0.01)
        final_condresults = euroformix::deconvolve(condresults)
        ratio_row = data.frame(Set=i, C1_Prop=condresults[["fit"]][["thetahat2"]][["Mix-prop. C1"]], C2_Prob=condresults[["fit"]][["thetahat2"]][["Mix-prop. C2"]])
        write.table(final_condresults[["table4"]], glue("{write_path}/conditioned/cond_on_{cond_on}/unknown_cond_on_{cond_on}_set{i}.tsv"), quote=F, row.names=F, sep="\t")
        dfratio = rbind(get(glue("ratios_{cond_on}")), ratio_row)
        assign(glue("ratios_{cond_on}"), dfratio)
        neededdf = data.frame(final_condresults[["table4"]])
        dfall = rbind(get(nam), neededdf)
        assign(nam, dfall)
        if (i == nsets) {
          final_list = c(final_list, list(dfall, dfratio))
        }
      }
    }
  }
  #return(list(uncond_finaltable_all, uncond_ratios, final_condresults_all, final_cond_ratios, final_condresults_2ndref_all, final_cond_2ndref_ratios))
  return(final_list)
}
