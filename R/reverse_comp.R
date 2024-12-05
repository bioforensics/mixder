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
#' Identifying SNPs that need to be reverse complemented
#'
#' @param snpdf data frame created directly from UAS Sample Report (version <2.5)
#'
#' @return data.frame
#' @export
#'
reverse_comp = function(snpdf) {
  for (i in 1:nrow(snpdf)) {
    rsid = snpdf[i, "Marker"][[1]]
    if (exists(rsid, where=mixder::snpinfo)) {
      if (mixder::snpinfo[[rsid]][["ReverseCompNeeded"]] == "Yes") {
        allele = snpdf[i, "Allele"]
        snpdf[i, "Allele"] = ifelse(allele == "A", "T", ifelse(allele == "T", "A", ifelse(allele == "G", "C", ifelse(allele == "C", "G", "NA"))))
      }
    }
  }
  return(snpdf)
}
