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
#' @description This function checks that the C1 allele calls are not the same
#'    as the C2 allele calls.
#'
#' @param x Data frame of EFM predicted genotypes
#'
#' @return TRUE/FALSE
#' @export
check_allele_calls = function(x) {
  same = TRUE
  stop = FALSE
  for (i in 1:nrow(x)) {
    if (x[i, "Locus"] == "") next
    snp = x[i, "Locus"]
    data_snp = subset(x, x$Locus==snp)
    C2_snp = subset(data_snp, data_snp$Contr.=="C2")
    if (length(C2_snp) == 0) next
    C1_snp = subset(data_snp, data_snp$Contr.=="C1")
    if (nrow(C1_snp) != nrow(C2_snp)) {
      same = FALSE
      stop = TRUE
    } else {
      for (j in nrow(C1_snp)) {
        if (C1_snp[j,"Allele"] != C2_snp[j, "Allele"]) {
          stop = TRUE
          same = FALSE
          break
        }
      }
    }
    if (stop) break
  }
  return(same)
}
