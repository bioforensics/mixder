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
#' @param pos SNP positions data frame
#'
#' @return data frame
#' @export
assigned_A2 = function(x, thresh, pos) {
  x$Allele2 = ifelse(x$A2_Prob>=thresh & x$A2!="99", x$A2, x$A1)
  names(x)[names(x) == "A1"] = "Allele1"
  x$Locus = ifelse(x$Locus=="RS201326893_Y152OCH", "rs201326893_Y152OCH", ifelse(x$Locus=="N29INSA", "N29insA", tolower(x$Locus)))
  colname = colnames(pos)
  lcol = grep("rsid|marker|locus|snp", colname, ignore.case=TRUE, value=TRUE)
  chrcol = grep("chr", colname, ignore.case=TRUE, value=TRUE)
  poscol = grep("pos", colname, ignore.case=TRUE, value=TRUE)
  rsids = merge(pos, x, by.x=lcol, by.y="Locus")
  final_x = rsids[,c(lcol, chrcol, poscol, "Allele1", "Allele2")]
  final_x_sorted = final_x %>%
    arrange(as.numeric(!!sym(chrcol)), as.numeric(!!sym(poscol)))
  return(final_x_sorted)
}
