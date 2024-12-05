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
#' @title Run MixDeR workflow
#'
#'@description Runs workflow
#'
#' @param id Sample ID
#' @param replicate_id Sample ID of replicate, if specified
#' @param freq Path of allele frequency data
#' @param refs Path of reference genotype(s) file
#' @param sample_path Path of sample manifest
#' @param output Name of output directory
#' @param run_mixdeconv TRUE if running EFM mixture deconvolution
#' @param unconditioned TRUE if running unconditioned mixture deconvolution
#' @param cond Sample IDs to condition on
#' @param method Calculate Metrics or Create GEDmatch PRO Report
#' @param sets Number of SNP sets
#' @param kinpath Path of Kintelligence Sample Reports
#' @param dynamicAT Dynamic AT
#' @param staticAT Static AT
#' @param minimum_snps Minimum number of SNPs for creating GEDmatch PRO reports
#' @param A1_threshold Allele 1 probability threshold for creating GEDmatch PRO reports
#' @param A2_threshold Allele 2 probability threshold for creating GEDmatch PRO reports
#' @param A1min Minimum value for allele 1 probability thresholds for calculating metrics
#' @param A1max Maximum value for allele 1 probability thresholds for calculating metrics
#' @param A2min Minimum value for allele 2 probability thresholds for calculating metrics
#' @param A2max Maximum value for allele 2 probability thresholds for calculating metrics
#' @param major Major contributor ID
#' @param minor Minor contributor ID
#'
#' @export
#'
#'@import dplyr
#'@import glue
#'@importFrom utils write.table read.table
run_workflow = function(date, id, replicate_id, freq, refs, sample_path, output, run_mixdeconv, unconditioned, cond, method, sets, kinpath, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, major, minor) {
  out_path = glue("{kinpath}/snp_sets/{output}/")
  if (replicate_id == "") {
    logfile = file(glue("{out_path}config_log_files/{date}/run_log_{id}_{date}.txt"), open = "wt")
  } else {
    logfile = file(glue("{out_path}config_log_files/{date}/run_log_{id}_{replicate_id}_{date}.txt"), open = "wt")
  }
  sink(logfile, type = "message", append=T)
  if (method=="Calculate Metrics" & unconditioned & (!isTruthy(major) | !isTruthy(minor))) {
    stop("No major/minor contributor IDs provided but calculating metrics for an unconditioned analysis. Please re-run!")
  }
  message("Loading Frequency Data<br/>")
  if (freq == "General - 1000G") {
    popFreq = mixder::popFreq_1000G
  } else if (freq == "General - gnomAD"){
    popFreq = mixder::popFreq_gnomad
  } else if (freq == "Upload Custom") {
    popFreq = checking_af(freq, out_path)
  }
  if (!isTruthy(sample_path)) {
    stop("No Sample Manifest provided. Please re-run!")
  }
  if (!isTruthy(kinpath)  & method != "Create GEDmatch PRO Report"  & run_mixdeconv) {
    stop("No Kintelligence Sample Reports provided. Please re-run!")
  }
  if (isTruthy(refs)) {
    message("Loading Reference Data<br/>")
    if (!file.exists(glue("{refs}/EFM_references.csv"))) {
      refData = euroformix::sample_tableToList(data.frame(processing_ref_sample_reports(refs)))
    } else {
      refData = euroformix::sample_tableToList(euroformix::tableReader(glue("{refs}/EFM_references.csv")))
    }
  } else if (isTruthy(cond) | method == "Calculate Metrics") {
    stop("No references provided but selected conditioned analyses or calculating metrics. Please re-run!")
  } else {
    refData = ""
  }
  message(glue("Sample: {id}<br/>"))
  message(glue("Replicate Sample: {replicate_id}<br/>"))
    ## run EFM
  if (run_mixdeconv) {
    attable = process_kinreport(id, replicate_id, kinpath, dynamicAT, staticAT)
    efm_results = run_efm(popFreq, refData, id, replicate_id, kinpath, out_path, attable, sets, cond, uncond=unconditioned)
  }
  if (replicate_id == "") {
    type = "Single"
  } else {
    type = "Replicates"
  }
  write_path = paste0(out_path, type)
    ## create GEDmatch PRO report directory
  if (method == "Create GEDmatch PRO Report") {
    dir.create(file.path(write_path, "GEDMatchPROReports"), showWarnings = FALSE, recursive=TRUE)
  }
  if (unconditioned) {
    if (run_mixdeconv) {
      uncond_table = data.frame(efm_results[[1]]) %>%
        filter(.data$Locus != "")
      write.table(uncond_table, glue("{write_path}/{id}/unconditioned/{id}_efm_output_unconditioned.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
      write.table(efm_results[[2]], glue("{write_path}/{id}/unconditioned/{id}_mixture_ratios.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
    } else if (method != "") {
      message("Skipping running EFM. Loading existing unconditioned data!<br/>")
      uncond_filename = glue("{write_path}/{id}/unconditioned/{id}_efm_output_unconditioned.tsv")
      if (file.exists(uncond_filename)) {
        uncond_table = read.table(uncond_filename, header=T, sep="\t")
      } else {
        stop(glue("{uncond_filename} does not exist. You may need to run EFM or check the correct SNP file input folder and Output folder are correct!"))
      }
    }
    if (method == "Create GEDmatch PRO Report") {
      major_report = create_gedmatchpro_report(uncond_table, "C1", "major", minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max)
      write.table(major_report, glue("{write_path}/GEDMatchPROReports/{id}_uncond_major_{type}_GEDmatchPROReport.txt"), col.names=T, sep="\t", row.names=F, quote=F)
      minor_report = create_gedmatchpro_report(uncond_table, "C2", "minor", minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max)
      write.table(minor_report, glue("{write_path}/GEDMatchPROReports/{id}_uncond_minor_{type}_GEDmatchPROReport.txt"), col.names=T, sep="\t", row.names=F, quote=F)
    } else if (method == "Calculate Metrics") {
      major_ref = format_ref(refData, major, refs)
      major_tables = suppressWarnings(process_efm_files(uncond_table, "C1", major_ref, minimum_snps, A1min, A1max, A2min, A2max))
      write_tables(major_tables, glue("{write_path}/{id}/unconditioned/{major}"), minimum_snps)
      minor_ref = format_ref(refData, minor, refs)
      minor_tables = suppressWarnings(process_efm_files(uncond_table, "C2", minor_ref, minimum_snps, A1min, A1max, A2min, A2max))
      write_tables(minor_tables, glue("{write_path}/{id}/unconditioned/{minor}"), minimum_snps)
    }
  }
  if (isTruthy(cond)) {
    i = 1
    for (cond_on in cond) {
      i = i + 2
      run_conditioned_analysis(id, refs, run_mixdeconv, method, efm_results[[i]], refData, efm_results[[i+1]], write_path, cond_on, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, type, major, minor)
    }
  }
  if (method == "") {
    message("Mixture Deconvolution complete!")
  } else if (method == "Calculate Metrics") {
    message("Calculating Metrics Complete!")
  } else {
    message("Creating GEDmatch PRO Reports Complete!")
  }
  shiny::stopApp()
}
