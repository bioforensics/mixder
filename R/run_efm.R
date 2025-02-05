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
#' @param date Date and time of MixDeR run
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
#' @param keep_bins To use existing SNP bins or create new bins (and files)
#'
#' @export
#'
#' @import parallel
#'
run_efm = function(date, popFreq, refData, id, replicate_id, inpath, out_path, attable, nsets, ancestry, cond = NULL, uncond=TRUE, keep_bins=TRUE) {
  if (!ancestry) {
    new_path = glue("{out_path}/ancestry_prediction/")
  } else {
    new_path = out_path
  }
  if (replicate_id == "") {
    create_evid_all(inpath, id, nsets, keep_bins)
    write_path = glue("{new_path}Single/{id}")
    log_name = id
  } else {
    create_evid_all(inpath, id, nsets, keep_bins)
    create_evid_all(inpath, replicate_id, nsets, keep_bins)
    write_path = glue("{new_path}Replicates/{id}")
    log_name = glue("{id}_{replicate_id}")
  }
  snps_input = glue("{inpath}/snp_sets/")
  final_list = c()
  if (replicate_id != "") {
    ids = create_replicatedf(snps_input, id, replicate_id, nsets)
  } else {
    ids = NULL
  }
  numCores = ifelse(detectCores()<10, detectCores(), 10)
  message(glue("Running EFM mixture deconvolution using {numCores} cores."))
  cl = makeCluster(numCores, outfile=glue("{out_path}config_log_files/{date}/efm_output_{log_name}_{date}.txt"))
  results = parLapply(cl, 1:(nsets), run_indiv_efm_set, ids=ids, snps_input=snps_input, popFreq=popFreq, refData=refData, id=id, replicate_id=replicate_id, write_path=write_path, attable=attable, cond=cond, uncond=uncond)
  stopCluster(cl)
  #results = list()
  #for (i in 1:nsets) {
  #  message(i)
  #  results[[i]] = run_indiv_efm_set(i, ids, snps_input, popFreq, refData, id, replicate_id, write_path, attable, cond=cond, uncond=uncond)
  #}
  uncond_ratios = data.frame()
  uncond_finaltable_all = data.frame()
  if (uncond) {
    for (i in 1:(nsets)) {
      set_file = glue("{write_path}/unconditioned/{id}_set{i}_uncond.tsv")
      if (file.exists(set_file)) {
        set_results = read.table(set_file, header=T, sep="\t")
        uncond_finaltable_all = rbind(uncond_finaltable_all, set_results)
        uncond_ratios = rbind(uncond_ratios, data.frame(Set=i, C1_Prob=results[[i]][[1]], C2_Prob=results[[i]][[2]]))
      } else {
        message(glue("Set {i} for the unconditioned analysis was skipped. See EFM run log in {out_path}config_log_files/{date}/efm_output_{log_name}_{date}.txt for more information.<br/>"))
      }
    }
    check_ratios(uncond_ratios, uncond=TRUE)
  }
  final_list = list(uncond_finaltable_all, uncond_ratios)
  if (!is.null(cond)) {
    C1_num = 3
    C2_num = 4
    for (cond_on in cond) {
      cond_ratios = data.frame()
      cond_finaltable_all = data.frame()
      for (i in 1:nsets) {
        set_file = glue("{write_path}/conditioned/cond_on_{cond_on}/unknown_cond_on_{cond_on}_set{i}.tsv")
        if (file.exists(set_file)) {
          set_results = read.table(set_file, header=T, sep="\t")
          cond_finaltable_all = rbind(cond_finaltable_all, set_results)
          cond_ratios = rbind(cond_ratios, data.frame(Set=i, C1_Prob=results[[i]][[C1_num]], C2_Prob=results[[i]][[C2_num]]))
        } else {
          message(glue("Set {i} for the conditioned analysis (conditioned on {cond_on}) was skipped. See EFM run log in {out_path}config_log_files/{date}/efm_output_{log_name}_{date}.txt for more information.<br/>"))
        }
      }
      check_ratios(cond_ratios, uncond=FALSE)
      final_list = c(final_list, list(cond_finaltable_all, cond_ratios))
      C1_num = C1_num + 2
      C2_num = C2_num + 2
    }
  }
  return(final_list)
}
