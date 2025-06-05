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

#' Title Calculate various metrics on the inferred genotype dataset to assist user in evaluating the final dataset
#'
#' @param report data frame of the inferred single source genotypes
#' @param A1_threshold Allele 1 probability threshold
#' @param A2_threshold Allele 2 probability threshold
#' @param min_num Minimum number of SNPs
#' @param path output path
#'
#' @return data frame of compiled metrics
#' @export
#'
#' @importFrom stats sd
#' @rawNamespace import(ggplot2, except = last_plot)

gedmatch_metrics = function(report, A1_threshold, A2_threshold, min_num, path){
  . = NULL
  report$A2_thresh = ifelse(report$A2_Prob >= A2_threshold & report$A2 != 99, report$A2, report$A1)
  report$het = ifelse(report$A1==report$A2_thresh, 0, 1)
  report_A1applied = subset(report, report$A1_Prob>=A1_threshold)
  report_minsnps = report %>%
    arrange(desc(.data$A1_Prob)) %>%
    .[c(1:min_num),]
  n_snps_total=nrow(report)
  n_snps_A1applied = nrow(report_A1applied)
  mean_all = round(mean(report$A1_Prob), 4)
  mean_A1 = round(mean(report_A1applied$A1_Prob), 4)
  mean_min = round(mean(report_minsnps$A1_Prob), 4)
  median_all = round(median(report$A1_Prob), 4)
  median_A1 = round(median(report_A1applied$A1_Prob), 4)
  median_min = round(median(report_minsnps$A1_Prob), 4)
  sd_all = round(sd(report$A1_Prob), 4)
  sd_A1 = round(sd(report_A1applied$A1_Prob), 4)
  sd_min = round(sd(report_minsnps$A1_Prob), 4)
  het_all = round(sum(report$het==1)/n_snps_total, 4)
  het_A1 = round(sum(report_A1applied$het==1)/n_snps_A1applied, 4)
  het_min = round(sum(report_minsnps$het==1)/min_num, 4)
  compiled_metrics = data.frame(Allele1_Threshold_Applied=c("No", "Yes", "Minimum # of SNPs Used"), Allele2_Threshold_Applied=c("Yes", "Yes", "Yes"), Total_SNPs=c(n_snps_total, n_snps_A1applied, min_num), Mean_A1_Prob=c(mean_all, mean_A1, mean_min), Median_A1_Prob=c(median_all, median_A1, median_min), SD_A1_Prob=c(sd_all, sd_A1, sd_min), Heterozygosity=c(het_all, het_A1, het_min))
  plot = ggplot(data=report, aes(x=report$A1_Prob))+
    geom_density(fill="grey60", color="grey60")+
    xlab("Allele 1 Probabilities for all Inferred Genotypes")
  return(list(compiled_metrics, plot))
}
