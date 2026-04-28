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
#' run MixDeR using the CLI; ancestry prediction
#'
#' @param sample_manifest path to sample manifest file (default=NULL)
#' @param sample If running individual sample, specify the sample ID; requires sample_manifest=NULL (default=NULL)
#' @param replicate If running individual sample along with a replicate, specify the replicate ID; requires sample_manifest=NULL (default=NULL)
#' @param sample_reports Path to directory of Sample Reports or CSV file of mixture genotypes (default=current directory)
#' @param output name of output directory, will be outputted into the sample reports directory (default="output")
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
#' @param snps SNPs to use for ancestry prediction (either ancestry only or all SNPs)
#' @param pcagroups How to color PCA plots (superpopulations and/or subpopulations)
#'
#' @export
#'
run_mixder_ancestry = function(sample_manifest=NULL, sample=NULL, replicate=NULL, sample_reports = getwd(), output = "output", refpath=NULL, refs=NULL, mixdeconv=TRUE, uncond=TRUE, cond=FALSE, sets=10, dynamicAT=0.015, staticAT=10, minimum_snps=6000, A1_threshold=0.99, A2_threshold=0.6, minor_contrib_threshold=FALSE, keep_bins=TRUE, snps, pcagroups) {
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
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
  if (grepl("ancestry", tolower(snps))) {
    snpset = "Ancestry SNPs Only"
  } else if (grepl("all", tolower(snps))) {
    snpset = "All Autosomal SNPs"
  } else {
    stop("Specify either `ancestry` or `all` for SNPs to use for PCA.")
  }
  if (grepl("super", tolower(pcagroups))) {
    pcagroupcat = "Superpopulations (AFR/AMR/EAS/EUR/SAS Only)"
  } else if (grepl("sub", tolower(pcagroups))) {
    pcagroupcat = "Subpopulations"
  } else {
    stop("Please specify either `superpopulations` or `subpopulations` to use for PCA groupings.")
  }
  if (!isTruthy(sample_manifest) & !isTruthy(sample)){
    stop("Please provide sample manifest or sample ID!")
  }
  print("Running ancestry prediction")
  create_config(date, FALSE, "1000G_global", NA, NA, refpath, sample_manifest, sample, replicate, output, mixdeconv, uncond, refs, "", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, FALSE, FALSE, snpset, pcagroupcat)
  if (isTruthy(sample_manifest)) {
  ## sample manifest; loop through each sample
    manifest=suppressWarnings(euroformix::tableReader(sample_manifest))
    for (i in nrow(manifest)) {
      id = manifest[i, 1]
      replicate_id = ifelse(is.na(manifest[i, 2]), "", manifest[i, 2])
      message(glue("Sample ID: {id}"))
      message(glue("Replicate ID: {replicate_id}"))
      run_workflow(date, id, replicate_id, FALSE, "1000G_global", NA, NA, refData, refpath, output, mixdeconv, uncond, refs, "", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, minor_contrib_threshold, keep_bins, FALSE, FALSE, snpset, pcagroupcat)
    }
  } else if (isTruthy(sample)){
    replicate_id = ifelse(isTruthy(replicate), replicate, "")
    message(glue("Sample ID: {sample}"))
    message(glue("Replicate ID: {replicate_id}"))
    run_workflow(date, sample, replicate_id, FALSE, "1000G_global", NA, NA, refData, refpath, output, mixdeconv, uncond, refs, "", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, minor_contrib_threshold, keep_bins, FALSE, FALSE, snpset, pcagroupcat)
  }
}
