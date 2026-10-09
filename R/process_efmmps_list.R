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
#' Format deconvolve_SNP results
#'
#' @param deconvolve_list deconvolved list from EFMmps
#'
#' @returns list of data frames containing allele probabilities and genotype probabilities
#' @export
#'
#' @import dplyr
#' @import tibble
#' @import tidyr

process_efmmps_list = function(deconvolve_list) {
  allele_labels = as.data.frame(deconvolve_list[["alleleLabels"]][,1:2]) %>%
    rownames_to_column(var = "Locus") %>%
    pivot_longer(!Locus, names_to="Num", values_to="Allele")

  allele_prob = as.data.frame(deconvolve_list[["alleleProb"]]) %>%
    rownames_to_column(var = "Contr.") %>%
    pivot_longer(!Contr., names_to="A.SNP", values_to="Probability") %>%
    separate(A.SNP, into=c("Num", "Locus"))


  merged_alleles = full_join(allele_labels, allele_prob, by = c("Locus", "Num")) %>%
    select(-Num) %>%
    select(Contr., Locus, Allele, Probability)


  gt_labels = as.data.frame(deconvolve_list[["genoLabels"]][,1:3]) %>%
    rownames_to_column(var = "Locus") %>%
    pivot_longer(!Locus, names_to="Num", values_to="Genotype")

  gt_prob = as.data.frame(deconvolve_list[["genoProb"]]) %>%
    rownames_to_column(var = "Contr.") %>%
    pivot_longer(!Contr., names_to="G.SNP", values_to="Probability") %>%
    separate(G.SNP, into=c("Num", "Locus"))

  merged_gt = full_join(gt_labels, gt_prob, by = c("Locus", "Num")) %>%
    select(-Num) %>%
    select(Contr., Locus, Genotype, Probability)

  return(list(al=merged_alleles, gt=merged_gt))
}
