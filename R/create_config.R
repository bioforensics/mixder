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
#' @param twofreqs TRUE if using separate AF data for major and minor contributors
#' @param freq_all Path (or name) of allele frequency data if using same data for both
#' @param freq_major Path (or name) of allele frequency data for major contributor
#' @param freq_minor Path (or name) of allele frequency data for minor contributor
#' @param refs path to reference folder
#' @param sample_manifest path to sample manifest
#' @param sample sample ID if sample_manifest=NULL
#' @param replicate replicate ID for running a single sample if sample_manifest=NULL
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
#' @param filter_missing TRUE/FALSE whether to filter SNPs if second allele is missing (99)
#' @param skipancestry TRUE/FALSE whether to skip ancestry prediction step
#' @param pcasnps SNPs used for PCA (ancestry prediction)
#' @param pcagroups Groups used for PCA (ancestry prediction), either Superpopulations or Subpopulations
#' @param assay name of sequencing assay, either `kintelligence` or `custom`
#' @param pos Path to file containing SNP positions of custom assay, required if assay==`custom`
#'
#' @export
#'
create_config = function(date, twofreqs, freq_all, freq_major, freq_minor, refs, sample_manifest, sample, replicate, out_path, run_mixdeconv, unconditioned, cond, method, sets, kinpath, dynamicAT, staticAT, minimum_snps, A1_threshold, A2_threshold, A1min, A1max, A2min, A2max, major, minor, filter_missing, skipancestry, pcasnps, pcagroups, assay, pos){
  config = setNames(data.frame(matrix(ncol=2, nrow=0)), c("Setting", "Value"))
  config = rbind(config, data.frame(Setting="MixDeR Version:", Value=getNamespaceVersion("mixder")[["version"]]))
  config = rbind(config, data.frame(Setting="EuroForMix Version:", Value=getNamespaceVersion("euroformix")[["version"]]))
  onfig = rbind(config, data.frame(Setting="Assay:", Value=assay))
  if (assay == "custom") {
    config = rbind(config, data.frame(Setting="SNP hg19 positions file for custom assay:", Value=pos))
  }
  if (isTruthy(sample_manifest)) {
    config = rbind(config, data.frame(Setting="Path to sample manifest:", Value=sample_manifest))
  } else if (isTruthy(sample)) {
    config = rbind(config, data.frame(Setting="Sample:", Value=sample))
    config = rbind(config, data.frame(Setting="Replicate Sample:", Value=ifelse(isTruthy(replicate), replicate, "")))
  }
  if (isTruthy(refs)){
    config = rbind(config, data.frame(Setting="Path to references:", Value=refs))
  }
  config = rbind(config, data.frame(Setting="Path to mixtures:", Value=kinpath))
  config = rbind(config, data.frame(Setting="Run Ancestry Prediction Step:", Value=!skipancestry))
  if (!isTruthy(skipancestry)) {
    config = rbind(config, data.frame(Setting="SNPs Used for PCA (Ancestry Prediction):", Value=pcasnps))
    config = rbind(config, data.frame(Setting="Groups Used for PCA (Ancestry Prediction):", Value=pcagroups))
  }
  if (isTruthy(run_mixdeconv)) {
    if (isTruthy(twofreqs)) {
      config = rbind(config, data.frame(Setting="Frequency data Major Contributor:", Value=freq_major))
      config = rbind(config, data.frame(Setting="Frequency data Minor Contributor:", Value=freq_minor))
    } else {
      config = rbind(config, data.frame(Setting="Frequency data for Major Contributor:", Value=freq_all))
      config = rbind(config, data.frame(Setting="Frequency data for Minor Contributor:", Value=freq_all))
    }
  }
  config = rbind(config, data.frame(Setting="Output path:", Value=glue("{kinpath}/snp_sets/{out_path}/")))
  config = rbind(config, data.frame(Setting="Number of SNP sets:", Value=sets))
  config = rbind(config, data.frame(Setting="Minimum number of SNPs:", Value=minimum_snps))
  config = rbind(config, data.frame(Setting="Static AT:", Value=staticAT))
  config = rbind(config, data.frame(Setting="Dynamic AT:", Value=dynamicAT))
  config = rbind(config, data.frame(Setting="Running mixture deconvolution:", Value=run_mixdeconv))
  config = rbind(config, data.frame(Setting="Running unconditioned deconvolution:", Value=unconditioned))
  if (isTruthy(cond)) {
    for (id in cond) {
      config = rbind(config, data.frame(Setting="Running conditioned deconvolution on reference:", Value=id))
    }
  } else {
    config = rbind(config, data.frame(Setting="Running conditioned deconvolution on reference:", Value=FALSE))
  }
  if (method == "") {
    config = rbind(config, data.frame(Setting="Method run post deconvolution:", Value="None"))
  } else {
    config = rbind(config, data.frame(Setting="Method run post deconvolution:", Value=method))
    config = rbind(config, data.frame(Setting="Filtering SNPs if allele 2 is missing?", Value=filter_missing))
  }
  if (method == "Create GEDmatch PRO Report"){
    config = rbind(config, data.frame(Setting="Allele 1 probability threshold (for GEDmatch PRO reports or ancestry prediction):", Value=A1_threshold))
    config = rbind(config, data.frame(Setting="Allele 2 probability threshold (for GEDmatch PRO reports or ancestry prediction):", Value=A2_threshold))
  } else if (method == "Calculate Metrics") {
    config = rbind(config, data.frame(Setting="Allele 1 probability range for validation metric calculations:", Value=glue("{A1min}-{A1max}")))
    config = rbind(config, data.frame(Setting="Allele 2 probability range for validation metric calculations:", Value=glue("{A2min}-{A2max}")))
    config = rbind(config, data.frame(Setting="Assumed major contributor:", Value=major))
    config = rbind(config, data.frame(Setting="Assumed minor contributor:", Value=minor))
  }
  dir.create(file.path(kinpath, "snp_sets", out_path, "config_log_files", date), showWarnings = FALSE, recursive=TRUE)
  write.table(config, glue("{kinpath}/snp_sets/{out_path}/config_log_files/{date}/config_settings_run_{date}.txt"), row.names=F, quote=F, col.names=T, sep="\t")
}
