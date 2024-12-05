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
#' @title Runs the conditioned EFM mixture deconvolution
#'
#' @param id Sample ID
#' @param refs Path to the reference genotypes file
#' @param run_mixdeconv TRUE/FALSE
#' @param method Either Calculate Metrics or Create GEDmatch PRO Report
#' @param efm_table EFM predicted genotypes
#' @param refData Reference data file
#' @param mix_ratios Mixture ratios data frame
#' @param write_path Write path
#' @param cond_on Sample ID which the sample was conditioned on
#' @param minimum_snps Minimum number of SNPs required
#' @param A1_threshold Allele 1 probability threshold for creating GEDmatch PRO reports
#' @param A2_threshold Allele 2 probability threshold for creating GEDmatch PRO reports
#' @param A1min Minimum value for allele 1 probability thresholds for calculating metrics
#' @param A1max Maximum value for allele 1 probability thresholds for calculating metrics
#' @param A2min Minimum value for allele 2 probability thresholds for calculating metrics
#' @param A2max Maximum value for allele 2 probability thresholds for calculating metrics
#' @param type Single/Replicates
#' @param major Major contributor sample ID
#' @param minor Minor contributor sample ID
#'
#' @export
run_conditioned_analysis = function(id, refs, run_mixdeconv, method, efm_table, refData, mix_ratios, write_path, cond_on, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, type, major, minor) {
  if (run_mixdeconv) {
    cond_ref2_table = data.frame(efm_table) %>%
      filter(.data$Locus != "")
    write.table(cond_ref2_table, glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_efm_output_conditioned.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
    write.table(mix_ratios, glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_mixture_ratios.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
  } else if (method != "") {
    message("Skipping running EFM. Loading existing conditioned data!")
    cond_filename = glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_efm_output_conditioned.tsv")
    if (file.exists(cond_filename)) {
      cond_ref2_table = read.table(cond_filename, header=T, sep="\t")
    } else {
      stop(glue("File {cond_filename} does not exist. You may need to run EFM or check the correct SNP file input folder and Output folder are correct!"))
    }
    mix_ratios = read.table(glue("{write_path}/{id}/conditioned/unk_conditioned_on_{cond_on}_mixture_ratios.tsv"), header=T, sep="\t")
  }
  contrib_status = ifelse(mean(mix_ratios$C1_Prop) > mean(mix_ratios$C2_Prob), "minor", "major")
  if (method == "Create GEDmatch PRO Report") {
    cond_report = create_gedmatchpro_report(cond_ref2_table, "C2", contrib_status, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max)
    write.table(cond_report, glue("{write_path}/GEDMatchPROReports/{id}_{contrib_status}_contrib_{type}_GEDmatchPROReport.txt"), col.names=T, sep="\t", row.names=F, quote=F)
  } else if (method == "Calculate Metrics") {
    unk = ifelse(contrib_status == "major", major, minor)
    ref = format_ref(refData, unk, refs)
    geno_correct_tables_ref2 = suppressWarnings(process_efm_files(cond_ref2_table, "C2", ref, minimum_snps, A1min, A1max, A2min, A2max))
    write_tables(geno_correct_tables_ref2, glue("{write_path}/{id}/conditioned/{unk}"), minimum_snps)
  }
}
