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
#' @title Add SNPs to ensure same SNPs are present in both samples for a
#'    replicate analysis.
#'
#' @param df1 Data frame of the sample with the higher number of SNPs (rows)
#' @param df2 Data frame of the sample with the smaller number of SNPs (rows)
add_rows = function(df1, df2) {
  locs = unique(df1[,"Marker"])
  new_df = data.frame()
  for (loc in locs) {
    if (!loc %in% df2$Marker) {
      indexnum = which(df1[,"Marker"] == loc)
      A1 = df1[indexnum, "Allele.1"]
      A2 = df1[indexnum, "Allele.2"]
      if (ncol(df1) == 11) {
        A3 = df1[indexnum, "Allele.3"]
        A4 = df1[indexnum, "Allele.4"]
        row = data.frame(Sample.Name=df2[1,1], Marker=loc, Allele.1=A1, Allele.2=A2, Allele.3=A3, Allele.4=A4, Height.1=0, Height.2=0, Height.3=0, Height.4=0, Total_Reads=0)
      } else if (ncol(df1) == 9) {
        A3 = df1[indexnum, "Allele.3"]
        row = data.frame(Sample.Name=df2[1,1], Marker=loc, Allele.1=A1, Allele.2=A2, Allele.3=A3, Height.1=0, Height.2=0, Height.3=0, Total_Reads=0)
      } else {
        row = data.frame(Sample.Name=df2[1,1], Marker=loc, Allele.1=A1, Allele.2=A2, Height.1=0, Height.2=0, Total_Reads=0)
      }
      new_df = rbind(new_df, row)
    }
  }
  return(new_df)
}
