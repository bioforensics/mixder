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
#' @param minor_contrib_threshold Whether to apply the allele 1 probability threshold to the minor contributor, regardless of the minimum number of SNPs (default=FALSE)
#' @param keep_bins Use existing binned SNP data, if exists (default=TRUE)
#' @param snps SNPs to use for ancestry prediction (either ancestry only or all SNPs; default="ancestry")
#' @param pcagroups How to color PCA plots (superpopulations and/or subpopulations, default="superpopulations")
#' @param threads threads for EFM (default=0)
#'
#' @export
#'
run_mixder_ancestry = function(sample_manifest=NULL, sample=NULL, replicate="", sample_reports = getwd(), output = "output", refpath=NULL, refs=NULL, mixdeconv=TRUE, uncond=TRUE, cond=FALSE, sets=10, dynamicAT=0.015, staticAT=10, minimum_snps=6000, A1_threshold=0.99, A2_threshold=0.6, minor_contrib_threshold=FALSE, keep_bins=TRUE, snps="ancestry", pcagroups="superpopulations", threads=0) {
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
  popFreq = list(mixder::popFreq_1000G, mixder::popFreq_1000G)
  out_path = glue("{sample_reports}/snp_sets/{output}/")
  ## load in references
  if (isTruthy(refpath)) {
    print("loading references")
    if (file.exists(glue("{refpath}/EFM_references.rda"))) {
      load(glue("{refpath}/EFM_references.rda"))
    } else {
      if (!file.exists(glue("{refpath}/EFM_references.csv"))) {
        refData = convert_table_to_list(data.frame(processing_ref_sample_reports(refpath)))
      } else {
       refsdf = data.frame(fread(glue("{refpath}/EFM_references.csv")))
       refData = convert_table_to_list(refsdf)
      }
      save(refData, file=glue("{refpath}/EFM_references.rda"))
    }
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
  snp_positions = mixder::kintelligence_snp_positions
  print("Running ancestry prediction")
  create_config(date, FALSE, "1000G_global", NA, NA, refpath, sample_manifest, sample, replicate, out_path, mixdeconv, uncond, refs, "", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, FALSE, FALSE, snpset, pcagroupcat, "kintelligence", NULL)
  if (isTruthy(sample_manifest)) {
  ## sample manifest; loop through each sample
    manifest=suppressWarnings(euroformix::tableReader(sample_manifest))
    for (i in nrow(manifest)) {
      id = manifest[i, 1]
      replicate_id = ifelse(is.na(manifest[i, 2]), "", manifest[i, 2])
      message(glue("Sample ID: {id}"))
      message(glue("Replicate ID: {replicate_id}"))
      run_workflow(date, id, replicate_id, FALSE, popFreq, refData, refpath, out_path, mixdeconv, uncond, refs, "", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, minor_contrib_threshold, keep_bins, FALSE, FALSE, snpset, pcagroupcat, "kintelligence", snp_positions, threads)
    }
  } else if (isTruthy(sample)){
    message(glue("Sample ID: {sample}"))
    message(glue("Replicate ID: {replicate}"))
    run_workflow(date, sample, replicate, FALSE, popFreq, refData, refpath, out_path, mixdeconv, uncond, refs, "", sets, sample_reports, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, NA, NA, NA, NA, NA, NA, minor_contrib_threshold, keep_bins, FALSE, FALSE, snpset, pcagroupcat, "kintelligence", snp_positions, threads)
  }
}
