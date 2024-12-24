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
#' @param date date and time of run
#' @param id Sample ID
#' @param replicate_id Sample ID of replicate, if specified
#' @param twofreqs TRUE if using separate AF data for major and minor contributors
#' @param freq_both Path (or name) of allele frequency data if using same data for both
#' @param freq_major Path (or name) of allele frequency data for major contributor
#' @param freq_minor Path (or name) of allele frequency data for minor contributor
#' @param refData Reference data (if available)
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
#' @param minor_threshold If apply the allele 1 probability threshold to the minor contributor
#' @param keep_bins To use existing SNP bins or create new bins (and files)
#' @param filter_missing TRUE/FALSE whether to filter SNPs with either allele missing
#'
#' @export
#'
#'@import dplyr
#'@import ggplot2
#'@import glue
#'@importFrom utils write.table write.csv read.table
#'@importFrom grDevices dev.off png
#'@importFrom methods show
run_workflow = function(date, id, replicate_id, twofreqs, freq_both, freq_major, freq_minor, refData, refs, sample_path, output, run_mixdeconv, unconditioned, cond, method, sets, kinpath, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, major, minor, minor_threshold, keep_bins, filter_missing) {
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
  popFreq = load_freq(out_path, twofreqs, freq_both, freq_major, freq_minor)
  if (!isTruthy(sample_path)) {
    stop("No Sample Manifest provided. Please re-run!")
  }
  if (!isTruthy(kinpath)  & method != "Create GEDmatch PRO Report"  & run_mixdeconv) {
    stop("No Kintelligence Sample Reports provided. Please re-run!")
  }
  if (!isTruthy(refData) & (method == "Calculate Metrics" | isTruthy(cond))) {
    message("No references provided but selected conditioned analyses or calculating metrics. Please re-run!")
  }
  message(glue("Sample: {id}<br/>"))
  message(glue("Replicate Sample: {replicate_id}<br/>"))
    ## run EFM
  if (run_mixdeconv) {
    attable = process_kinreport(id, replicate_id, kinpath, dynamicAT, staticAT)
    if (twofreqs) {
      efm_results_major = run_efm(date, popFreq[[1]], refData, id, replicate_id, kinpath, out_path, attable, sets, cond, uncond=unconditioned, keep_bins)
      efm_results_minor = run_efm(date, popFreq[[2]], refData, id, replicate_id, kinpath, out_path, attable, sets, cond, uncond=unconditioned, keep_bins)
    } else {
      efm_results_major = run_efm(date, popFreq[[1]], refData, id, replicate_id, kinpath, out_path, attable, sets, cond, uncond=unconditioned, keep_bins)
      efm_results_minor = efm_results_major
    }
  }
  if (replicate_id == "") {
    type = "Single"
  } else {
    type = "Replicates"
  }
  write_path = paste0(out_path, type)
    ## create GEDmatch PRO report directory
  if (method == "Create GEDmatch PRO Report") {
    dir.create(file.path(write_path, "GEDMatchPROReports/Metrics"), showWarnings = FALSE, recursive=TRUE)
  }
  if (unconditioned) {
    if (run_mixdeconv) {
      uncond_table_major = data.frame(efm_results_major[[1]]) %>%
        filter(.data$Locus != "")
      uncond_table_minor = data.frame(efm_results_minor[[1]]) %>%
        filter(.data$Locus != "")
      write.table(uncond_table_major, glue("{write_path}/{id}/unconditioned/{id}_efm_output_unconditioned_major.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
      write.table(efm_results_major[[2]], glue("{write_path}/{id}/unconditioned/{id}_major_mixture_ratios.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
      write.table(uncond_table_minor, glue("{write_path}/{id}/unconditioned/{id}_efm_output_unconditioned_minor.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
      write.table(efm_results_minor[[2]], glue("{write_path}/{id}/unconditioned/{id}_minor_mixture_ratios.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
    } else if (method != "") {
      message("Skipping running EFM. Loading existing unconditioned data!<br/>")
      uncond_filename_major = glue("{write_path}/{id}/unconditioned/{id}_efm_output_unconditioned_major.tsv")
      uncond_filename_minor = glue("{write_path}/{id}/unconditioned/{id}_efm_output_unconditioned_minor.tsv")
      if (file.exists(uncond_filename_major)) {
        uncond_table_major = read.table(uncond_filename_major, header=T, sep="\t")
      } else {
        stop(glue("{uncond_filename_major} does not exist. You may need to run EFM or check the correct SNP file input folder and Output folder are correct!"))
      }
      if (file.exists(uncond_filename_minor)) {
        uncond_table_minor = read.table(uncond_filename_minor, header=T, sep="\t")
      } else {
        stop(glue("{uncond_filename_minor} does not exist. You may need to run EFM or check the correct SNP file input folder and Output folder are correct!"))
      }
    }
    if (method == "Create GEDmatch PRO Report") {
      message("Creating GEDmatch PRO report for major contributor in unconditioned analysis.<br/>")
      major_report = create_gedmatchpro_report(write_path, uncond_table_major, "C1", "major", minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, minor_threshold, filter_missing)
      write.table(major_report[[1]], glue("{write_path}/GEDMatchPROReports/{id}_uncond_major_{type}_GEDmatchPROReport.txt"), col.names=T, sep="\t", row.names=F, quote=F)
      write.csv(major_report[[2]], glue("{write_path}/GEDMatchPROReports/Metrics/{id}_uncond_major_{type}_GEDmatchPROReport_Metrics.csv"), row.names=F, quote=F)
      png(glue("{write_path}/GEDMatchPROReports/Metrics/{id}_uncond_major_{type}_GEDmatchPROReport_Allele1_Probabilities_Density_Plot.png"))
      show(major_report[[3]])
      dev.off()
      message("Creating GEDmatch PRO report for minor contributor in unconditioned analysis.<br/>")
      minor_report = create_gedmatchpro_report(write_path, uncond_table_minor, "C2", "minor", minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, minor_threshold, filter_missing)
      write.table(minor_report[[1]], glue("{write_path}/GEDMatchPROReports/{id}_uncond_minor_{type}_GEDmatchPROReport.txt"), col.names=T, sep="\t", row.names=F, quote=F)
      write.csv(minor_report[[2]], glue("{write_path}/GEDMatchPROReports/Metrics/{id}_uncond_minor_{type}_GEDmatchPROReport_metrics.csv"), row.names=F, quote=F)
      png(glue("{write_path}/GEDMatchPROReports/Metrics/{id}_uncond_minor_{type}_GEDmatchPROReport_Allele1_Probabilities_Density_Plot.png"))
      show(minor_report[[3]])
      dev.off()
    } else if (method == "Calculate Metrics") {
      major_ref = format_ref(refData, major, refs)
      major_tables = suppressWarnings(process_efm_files(uncond_table_major, "C1", major_ref, minimum_snps, A1min, A1max, A2min, A2max, metrics=TRUE, filter_missing))
      write_tables(major_tables, glue("{write_path}/{id}/unconditioned/{major}"), minimum_snps)
      minor_ref = format_ref(refData, minor, refs)
      minor_tables = suppressWarnings(process_efm_files(uncond_table_minor, "C2", minor_ref, minimum_snps, A1min, A1max, A2min, A2max, metrics=TRUE, filter_missing))
      write_tables(minor_tables, glue("{write_path}/{id}/unconditioned/{minor}"), minimum_snps)
    }
  }
  if (isTruthy(cond)) {
    i = 3
    for (cond_on in cond) {
      if (run_mixdeconv) {
        mix_ratios = efm_results_major[[i+1]]
        write.table(mix_ratios, glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_mixture_ratios.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
        contrib_status = ifelse(mean(mix_ratios$C1_Prob) > mean(mix_ratios$C2_Prob), "minor", "major")
        if (contrib_status == "major") {
          efm_table_major = data.frame(efm_results_major[[i]])  %>%
            filter(.data$Locus != "")
          write.table(efm_table_major, glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_efm_output_conditioned.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
        } else {
          efm_table_minor = data.frame(efm_results_minor[[i]])  %>%
            filter(.data$Locus != "")
          write.table(efm_table_minor, glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_efm_output_conditioned.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
        }
      } else {
        cond_filename = glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_efm_output_conditioned.tsv")
        mix_ratios = read.table(glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_mixture_ratios.tsv"), header=T, sep="\t")
        contrib_status = ifelse(mean(mix_ratios$C1_Prob) > mean(mix_ratios$C2_Prob), "minor", "major")
        if (file.exists(cond_filename)) {
          if (contrib_status == "major") {
            efm_table_major = read.table(cond_filename, header=T, sep="\t")
          } else {
            efm_table_minor = read.table(cond_filename, header=T, sep="\t")
          }
        } else {
          stop(glue("File {cond_filename} does not exist. You may need to run EFM or check the correct SNP file input folder and Output folder are correct!"))
        }
      }
      if (method == "Create GEDmatch PRO Report") {
        message(glue("Creating GEDmatch PRO report for {contrib_status} contributor conditioned on {cond_on} in conditioned analysis.<br/>"))
        cond_report = create_gedmatchpro_report(write_path, get(glue("efm_table_{contrib_status}")), "C2", contrib_status, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, minor_threshold, filter_missing)
        write.table(cond_report[[1]], glue("{write_path}/GEDMatchPROReports/{id}_{contrib_status}_contrib_conditioned_on_{cond_on}_{type}_GEDmatchPROReport.txt"), col.names=T, sep="\t", row.names=F, quote=F)
        write.csv(cond_report[[2]], glue("{write_path}/GEDMatchPROReports/Metrics/{id}_{contrib_status}_contrib_conditioned_on_{cond_on}_{type}_GEDmatchPROReport_Metrics.csv"), row.names=F, quote=F)
        png(glue("{write_path}/GEDMatchPROReports/Metrics/{id}_{contrib_status}_contrib_{type}_GEDmatchPROReport_Allele1_Probabilities_Density_Plot.png"))
        show(cond_report[[3]])
        dev.off()
      } else if (method == "Calculate Metrics") {
        unk = ifelse(contrib_status == "major", major, minor)
        ref = format_ref(refData, unk, refs)
        geno_correct_tables = suppressWarnings(process_efm_files(get(glue("efm_table_{contrib_status}")), "C2", ref, minimum_snps, A1min, A1max, A2min, A2max, metrics=TRUE, filter_missing))
        write_tables(geno_correct_tables, glue("{write_path}/{id}/conditioned/{unk}"), minimum_snps)
      }
      i = i + 2
    }
  }
  if (method == "") {
    message("Mixture Deconvolution complete!")
  } else if (method == "Calculate Metrics") {
    message("Calculating Metrics Complete!")
  } else {
    message("Creating GEDmatch PRO Reports Complete!")
  }
  #shiny::stopApp()
}
