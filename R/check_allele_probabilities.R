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
  correct = FALSE
  if (check_allele_calls(data)) {
    message(glue("Set {i}: alleles all the same!"))
    correct = FALSE
  } else {
    for (i in 1:nrow(data)) {
      if (data[i, "Locus"] == "") next
      snp = data[i, "Locus"]
      data_snp = subset(data, data$Locus==snp)
      C2_snp = subset(data_snp, data_snp$Contr.=="C2")
      if (length(C2_snp) == 0) next
      C1_snp = subset(data_snp, data_snp$Contr.=="C1")
      if (as.numeric(C1_snp[1,"Probability"]) == as.numeric(C2_snp[1, "Probability"])) {
        next
      } else if (as.numeric(C1_snp[1,"Probability"]) < as.numeric(C2_snp[1, "Probability"])) {
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
