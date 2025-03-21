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
#' @title Checks EFM predicted genotypes for errors
#'
#' @describeIn This function checks that the C1 predicted genotypes have higher
#'   allele probabilities than the C2 predicted genotypes.
#'
#' @param data EFM predicted genotypes
#' @param i Set number
#'
#' @return TRUE/FALSE
#' @export
check_allele_probabilities = function(data, i) {
  efm_v = getNamespaceVersion("euroformix")[["version"]]
  correct = FALSE
  if (check_allele_calls(data)) {
    message(glue("Set {i}: alleles all the same!"))
    correct = FALSE
  } else {
    for (i in 1:nrow(data)) {
      if (data[i, "Locus"] == "") next
      snp = data[i, "Locus"]
      data_snp = subset(data, data$Locus==snp)
      if (substr(efm_v, 1,3)!="4.0" & substr(efm_v, 1,2) != "3.") {
        major_snp = subset(data_snp, data_snp$Contr. == "C2")
      } else {
        major_snp = subset(data_snp, data_snp$Contr.=="C1")
      }
      if (length(major_snp) == 0) next
      if (substr(efm_v, 1,3)!="4.0" & substr(efm_v, 1,2) != "3.") {
        minor_snp = subset(data_snp, data_snp$Contr.=="C1")
      } else {
        minor_snp = subset(data_snp, data_snp$Contr.=="C2")
      }
      if (as.numeric(major_snp[1,"Probability"]) == as.numeric(minor_snp[1, "Probability"])) {
        next
      } else if (as.numeric(major_snp[1,"Probability"]) < as.numeric(minor_snp[1, "Probability"])) {
        correct = FALSE
        break
      } else {
        correct = TRUE
        break
      }
    }
  }
  return(correct)
}
