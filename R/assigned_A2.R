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
#' @title Calls allele 2 based on established threshold
#'
#' @param x Data frame of allele calls
#' @param thresh Allele 2 probability threshold
#'
#' @return data frame
#' @export
assigned_A2 = function(x, thresh) {
  x$Allele2 = ifelse(x$A2_Prob>=thresh, x$A2, x$A1)
  names(x)[names(x) == "A1"] = "Allele1"
  x$Locus = ifelse(x$Locus=="RS201326893_Y152OCH", "rs201326893_Y152OCH", ifelse(x$Locus=="N29INSA", "N29insA", tolower(x$Locus)))
  rsids = merge(mixder::kintelligence_snp_positions, x, by.x="rsid", by.y="Locus")
  final_x = rsids[,c("rsid", "chromosome", "position", "Allele1", "Allele2")]
  final_x_sorted = final_x[order(final_x$chromosome, final_x$position),]
  return(final_x_sorted)
}
