#' Title Running individual EFM SNP sets in parallel
#'
#' @param i Set number
#' @param ids dataframe of replicate data
#' @param snps_input Path of SNP sets
#' @param popFreq Allele Frequency file
#' @param refData Reference(s) file
#' @param id Sample ID
#' @param replicate_id Replicate ID, if provided
#' @param write_path Path to write output to
#' @param attable AT table
#' @param cond Condition on specific references
#' @param uncond TRUE/FALSE if performing unconditioned analyses
#'
#' @return list of mixture ratios for each analysis
#' @export
#'
run_indiv_efm_set = function(i, ids, snps_input, popFreq, refData, id, replicate_id, write_path, attable, cond = NULL, uncond=TRUE) {
  final_list = c()
  if (replicate_id != "") {
    sample = glue("{as.character(ids[[1]])}_set{i}")
    replicate = glue("{as.character(ids[[2]])}_set{i}")
  } else {
    sample = glue("{id}_set{i}")
    replicate = replicate_id
  }
  evidData = create_evid(sample, replicate, snps_input)
  if (isTruthy(evidData)) {
    ##create AT vector
    sample_at = create_at(evidData, sample, replicate, attable)
    ratio_row = list()
    ratio_row[glue("Set{i}_C1_Prob_uncond")] = NA
    ratio_row[glue("Set{i}_C2_Prob_uncond")] = NA
    if (uncond) {
      message("Running unconditioned mixture deconvolution<br/>")
      dir.create(file.path(write_path, "unconditioned"), showWarnings = FALSE, recursive=TRUE)
      ##unconditioned analysis
      repeat_num = 0
      repeat {
        message(glue("Running unconditioned analysis for set {i}, attempt #{repeat_num+1}<br/>"))
        uncond_results = euroformix::calcMLE(2, evidData, popFreq, AT=sample_at, BWS=FALSE, FWS=FALSE, DEG=FALSE, steptol=0.001, pC=0.01, lambda=0.05, fst=0.01)
        uncond_finaltable = euroformix::deconvolve(uncond_results)
        if (check_allele_probabilities(data.frame(uncond_finaltable[["table4"]]), i)) break
        message(glue("Set {i} unconditioned: Mixture proportion = 0.5 or Allele probability flipping detected - will rerun!<br/>"))
        repeat_num = repeat_num + 1
        if (repeat_num == 10) break
      }
      if (repeat_num < 10) {
        ratio_row[glue("Set{i}_C1_Prob_uncond")] = uncond_results[["fit"]][["thetahat2"]][["Mix-prop. C1"]]
        ratio_row[glue("Set{i}_C2_Prob_uncond")] = uncond_results[["fit"]][["thetahat2"]][["Mix-prop. C2"]]
        #ratio_row = list(Set_uncond=i, C1_Prob_uncond=uncond_results[["fit"]][["thetahat2"]][["Mix-prop. C1"]], C2_Prob_uncond=uncond_results[["fit"]][["thetahat2"]][["Mix-prop. C2"]])
        #uncond_ratios = rbind(uncond_ratios, ratio_row)
        write.table(uncond_finaltable[["table4"]], glue("{write_path}/unconditioned/{id}_set{i}_uncond.tsv"), quote=F, row.names=F, sep="\t")
        #uncond_finaltable_all = rbind(uncond_finaltable_all, uncond_finaltable[["table4"]])
        #set_results = uncond_finaltable[["table4"]]
      } else {
        message(glue("Repeated unconditioned analysis 10 times unsuccessfully. Will skip set {i}!<br/>"))
        #set_results = data.frame()
      }
    }
    final_list = c(ratio_row)
    ## conditioned analysis
    if (!is.null(cond)) {
      total_refs = length(names(refData))
      cond_vector = rep.int(0, total_refs)
      for (cond_on in cond) {
        ratio_row = list()
        message(glue("Running mixture deconvolution conditioned on {cond_on}<br/>"))
        #nam = glue("df_{cond_on}")
        list_num = match(cond_on, names(refData))
        dir.create(file.path(write_path, glue("/conditioned/cond_on_{cond_on}")), showWarnings = FALSE, recursive=TRUE)
        message(glue("Running conditioned analysis on {cond_on} for set {i}.<br/>"))
        condresults = euroformix::calcMLE(2, evidData, popFreq, refData, AT=sample_at, condOrder=replace(cond_vector, list_num, 1), BWS=FALSE, FWS=FALSE, DEG=FALSE, steptol=0.001, pC=0.01, lambda=0.05, fst=0.01)
        final_condresults = euroformix::deconvolve(condresults)
        ratio_row[glue("Set{i}_C1_Prob_cond_on_{cond_on}")] = condresults[["fit"]][["thetahat2"]][["Mix-prop. C1"]]
        ratio_row[glue("Set{i}_C2_Prob_cond_on_{cond_on}")] = condresults[["fit"]][["thetahat2"]][["Mix-prop. C2"]]
        write.table(final_condresults[["table4"]], glue("{write_path}/conditioned/cond_on_{cond_on}/unknown_cond_on_{cond_on}_set{i}.tsv"), quote=F, row.names=F, sep="\t")
        final_list = c(final_list, ratio_row)
      }
    }
  } else {
    message(glue("No SNPs called for set {i}. Will skip!<br/>"))
  }
  return(final_list)
}
