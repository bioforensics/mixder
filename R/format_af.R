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
#' Formatting allele frequency files for use in EFM
#'
#' @param af AF file
#'
#' @return data frame with each SNP as own column
#' @export
format_af = function(af) {
  all_snps = data.frame(Allele = c("A", "C", "G", "T"))
  for (row in 1:nrow(af)) {
    als = c()
    for (allele in c("A", "C", "G", "T")) {
      if (allele == af[row, "Ref"]) {
       als = c(als, af[row, "Ref_AF"])
      } else if (allele == af[row, "Alt"]) {
       als = c(als, af[row, "Alt_AF"])
      } else {
       als = c(als, NA)
      }
    }
    all_snps[,af[row, "SNP"]] = data.frame(als)
  }
  all_snps[all_snps == 0] = 0.0001
  all_snps[all_snps == 1] = 0.9999
 return(all_snps)
}
