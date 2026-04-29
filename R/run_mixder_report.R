# -------------------------------------------------------------------------------------------------
# Copyright (c) 2026, DHS.
#
# This file is part of MixDeR and is licensed under the BSD license: see LICENSE.
#
# This software was prepared for the Department of Homeland Security (DHS) by the Battelle National
# Biodefense Institute, LLC (BNBI) as part of contract HSHQDC-15-C-00064 to manage and operate the
# National Biodefense Analysis and Countermeasures Center (NBACC), a Federally Funded Research and
# Development Center.
# -------------------------------------------------------------------------------------------------
#' run MixDeR using the CLI; create GEDmatch PRO report(s)
#'
#' @param sample_manifest path to sample manifest file (default=NULL)
#' @param sample If running individual sample, specify the sample ID; requires sample_manifest=NULL (default=NULL)
#' @param replicate If running individual sample along with a replicate, specify the replicate ID; requires sample_manifest=NULL (default=NULL)
#' @param sample_reports Path to directory of Sample Reports or CSV file of mixture genotypes (default=current directory)
#' @param output name of output directory, will be outputted into the sample reports directory (default="output")
#' @param assay name of sequencing assay, either `kintelligence` or `custom` (default=kintelligence)
#' @param snp_pos Path to file containing SNP positions of custom assay, required if assay==`custom` (default=NULL)
#' @param twofreqs TRUE if using different allele frequency data for each contributor (default=FALSE)
#' @param freq_both frequency data for both contributors; requires twofreqs=FALSE (default=1000G global)
#' @param freq_major frequency data for major contributor; requires twofreqs=TRUE (default=NULL)
#' @param freq_minor frequency data for minor contributor; requires twofreqs=TRUE (default=NULL)
#' @param refpath Path to directory containing reference genotypes (default=NULL)
#' @param refs list of sample IDs to use as reference for conditioning, can specify more than one (default=NULL, example: list("Ref1", "Ref2"))
#' @param mixdeconv whether to run mixture deconvolution (default=TRUE)
#' @param uncond run (or use) an unconditioned mixture deconvolution (default=TRUE)
#' @param cond run (or use) a conditioned mixture deconvolution (default=FALSE)
#' @param sets number of bins to divide SNPs into (default=10)
#' @param dynamicAT dynamic analytical threshold applied to data (default=0.015)
#' @param staticAT static analytical threshold applied to data (default=10)
#' @param minimum_snps minimum number of SNPs used to generate profile (default=6000)
#' @param A1_threshold Allele 1 probability threshold (default=0.99)
#' @param A2_threshold Allele 2 probability threshold (default=0.60)
#' @param filter_missing Whether to remove SNPs with a missing value for the allele 2 (default=FALSE)
#' @param minor_contrib_threshold Whether to apply the allele 1 probability threshold to the minor contributor, regardless of the minimum number of SNPs (default=FALSE)
#' @param keep_bins Use existing binned SNP data, if exists (default=TRUE)
#'
#' @export
#'
run_mixder_report = function(sample_manifest=NULL, sample=NULL, replicate="", sample_reports = getwd(), output = "output", assay="kintelligence", snp_pos=NULL, twofreqs=FALSE, freq_both="global_1000g", freq_major=NULL, freq_minor=NULL, refpath=NULL, refs=NULL, mixdeconv=TRUE, uncond=TRUE, cond=FALSE, sets=10, dynamicAT=0.015, staticAT=10, minimum_snps=6000, A1_threshold=0.99, A2_threshold=0.6, filter_missing=FALSE, minor_contrib_threshold=FALSE, keep_bins=TRUE) {
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
  out_path = glue("{sample_reports}/snp_sets/{output}/")
  popFreq = load_freq(twofreqs, freq_both, freq_major, freq_minor)
  if (tolower(assay)=="kintelligence") {
    snp_positions = mixder::kintelligence_snp_positions
  } else if (tolower(assay)=="custom") {
    if (file.exists(snp_pos)) {
      snp_positions = euroformix::tableReader(snp_pos)
    } else {
      stop("Custom assay specified but no SNP positions file provided.")
    }
  } else {
    stop("Problem with assay name. Please specify either 'kintelligence' or 'custom'.")
  }
  ## load in references
  if (isTruthy(refpath)) {
    print("loading references")
    if (!file.exists(glue("{refpath}/EFM_references.csv"))) {
      refData = euroformix::sample_tableToList(data.frame(processing_ref_sample_reports(refpath)))
    } else {
      refData = euroformix::sample_tableToList(euroformix::tableReader(glue("{refpath}/EFM_references.csv")))
    }
  } else if (cond) {
    stop("No references provided but selected conditioned analyses. Please re-run!")
  } else {
    refData = NULL
  }
  if (!isTruthy(sample_manifest) & !isTruthy(sample)){
    stop("Please provide sample manifest or sample ID!")
  }
  create_config(date, twofreqs, freq_both, freq_major, freq_minor, refpath, sample_manifest, sample, replicate, output, mixdeconv, uncond, refs, "Create GEDmatch PRO Report", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, filter_missing, TRUE, NA, NA, assay, snp_pos)
  if (isTruthy(sample_manifest)) {
    manifest=suppressWarnings(euroformix::tableReader(sample_manifest))
    for (i in nrow(manifest)) {
      id = manifest[i, 1]
      replicate_id = ifelse(is.na(manifest[i, 2]), "", manifest[i, 2])
      message(glue("Sample ID: {id}"))
      message(glue("Replicate ID: {replicate_id}"))
      run_workflow(date, id, replicate_id, twofreqs, popFreq, refData, refpath, out_path, mixdeconv, uncond, refs, "Create GEDmatch PRO Report", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, minor_contrib_threshold, keep_bins, filter_missing, TRUE, NA, NA, assay, snp_positions)
    }
  } else if (isTruthy(sample)){
    message(glue("Sample ID: {sample}"))
    message(glue("Replicate ID: {replicate}"))
    run_workflow(date, sample, replicate, twofreqs, popFreq, refData, refpath, out_path, mixdeconv, uncond, refs, "Create GEDmatch PRO Report", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, minor_contrib_threshold, keep_bins, filter_missing, TRUE, NA, NA, assay, snp_positions)
  }
}
