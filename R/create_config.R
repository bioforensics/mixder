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

#' Create configuration file containing all settings for a specific run
#'
#' @param date date and time of run
#' @param freq path or frequency dataset used
#' @param refs path to reference folder
#' @param sample_path path to sample manifest
#' @param out_path name of output folder
#' @param run_mixdeconv if running mixture deconvolution
#' @param unconditioned if running an unconditioned deconvolution
#' @param cond if running a conditioned deconvolution, which references to condition on
#' @param method downstream method (calculate metrics/create GEDmatch PRO report)
#' @param sets number of SNP sets
#' @param kinpath path to mixture (evidence) files
#' @param dynamicAT dynamic AT
#' @param staticAT static AT
#' @param minimum_snps minimum number of SNPs
#' @param A1_threshold Allele 1 probability threshold for GEDmatch PRO report creation
#' @param A2_threshold Allele 2 probability threshold for GEDmatch PRO report creation
#' @param A1min minimum allele 1 probability threshold in range for calculating metrics
#' @param A1max maximum allele 1 probability threshold in range for calculating metrics
#' @param A2min minimum allele 2 probability threshold in range for calculating metrics
#' @param A2max maximum allele 2 probability threshold in range for calculating metrics
#' @param major assumed major contributor of mixture
#' @param minor assumed minor contributor of mixture
#'
#' @export
#'
create_config = function(date, freq, refs, sample_path, out_path, run_mixdeconv, unconditioned, cond, method, sets, kinpath, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, major, minor){
  config = setNames(data.frame(matrix(ncol=2, nrow=0)), c("Setting", "Value"))
  config = rbind(config, data.frame(Setting="Path to sample manifest:", Value=sample_path))
  if (isTruthy(refs)){
    config = rbind(config, data.frame(Setting="Path to references:", Value=refs))
  }
  config = rbind(config, data.frame(Setting="Path to mixtures:", Value=kinpath))
  if (isTruthy(run_mixdeconv)) {
    config = rbind(config, data.frame(Setting="Frequency data:", Value=freq))
  }
  config = rbind(config, data.frame(Setting="Output path:", Value=glue("{kinpath}/snp_sets/{out_path}/")))
  config = rbind(config, data.frame(Setting="Number of SNP sets:", Value=sets))
  config = rbind(config, data.frame(Setting="Minimum number of SNPs:", Value=minimum_snps))
  config = rbind(config, data.frame(Setting="Static AT:", Value=staticAT))
  config = rbind(config, data.frame(Setting="Dynamic AT:", Value=dynamicAT))
  config = rbind(config, data.frame(Setting="Running mixture deconvolution:", Value=run_mixdeconv))
  config = rbind(config, data.frame(Setting="Running unconditioned deconvolution:", Value=unconditioned))
  if (isTruthy(cond)) {
    config = rbind(config, data.frame(Setting="Running conditioned deconvolution on references:", Value=cond))
  } else {
    config = rbind(config, data.frame(Setting="Running conditioned deconvolution on references:", Value=FALSE))
  }
  if (method == "") {
    config = rbind(config, data.frame(Setting="Method run post deconvolution:", Value="None"))
  } else {
    config = rbind(config, data.frame(Setting="Method run post deconvolution:", Value=method))
  }
  if (method == "Create GEDmatch PRO Report"){
    config = rbind(config, data.frame(Setting="Allele 1 probability threshold (for GEDmatch PRO reports):", Value=A1_threshold))
    config = rbind(config, data.frame(Setting="Allele 2 probability threshold (for GEDmatch PRO reports:", Value=A2_threshold))
  } else if (method == "Calculate Metrics") {
    config = rbind(config, data.frame(Setting="Allele 1 probability range for validation metric calculations:", Value=glue("{A1min}-{A1max}")))
    config = rbind(config, data.frame(Setting="Allele 2 probability range for validation metric calculations:", Value=glue("{A2min}-{A2max}")))
    config = rbind(config, data.frame(Setting="Assumed major contributor:", Value=major))
    config = rbind(config, data.frame(Setting="Assumed minor contributor:", Value=minor))
  }
  dir.create(file.path(kinpath, "snp_sets", out_path, "config_log_files", date), showWarnings = FALSE, recursive=TRUE)
  write.table(config, glue("{kinpath}/snp_sets/{out_path}/config_log_files/{date}/config_settings_run_{date}.txt"), row.names=F, quote=F, col.names=T, sep="\t")
}
