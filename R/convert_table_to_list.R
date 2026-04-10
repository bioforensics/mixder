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
#' Convert table to list
#'
#' @param input table
#'
#' @returns list of table
#' @export
#'
#' @importFrom data.table fread
#' @import rrapply
convert_table_to_list = function(table) {
  #table=data.frame(fread(input, header=T))
  outL=rrapply(table[,c(1:4)], how="unmelt")
  nestedlist = lapply(split(table, table$Sample.Name, drop = TRUE),
                      function(x) c(split(x, x[["Marker"]], drop = TRUE)))
  colname = colnames(table) #colnames in file
  lind = grep("marker",tolower(colname),fixed=TRUE) #locus col-ind
  A_ind = grep("allele",tolower(colname),fixed=TRUE) #allele col-ind
  H_ind = grep("height",tolower(colname),fixed=TRUE) #height col-ind
  if(length(lind)==0) lind = grep("loc",tolower(colname),fixed=TRUE) #try another name
  sind = grep("sample",tolower(colname),fixed=TRUE) #sample col-ind
  if(length(sind)>1)  sind = sind[grep("name",tolower(colname[sind]),fixed=TRUE)] #use only sample name
  samplenames = unique(as.character(table[,sind])) #sample names
  outL = list() #Init outList (insert non-empty characters):
  for(samplename in samplenames) { #for each sample in matrix
    filt_table=subset(table, table[,sind]==samplename)
    locs = unique(toupper(filt_table[,lind])) #locus names: Use uniques and Convert to upper case
    for(loc in locs) { #for each locus
      loc_lower=tolower(loc)
      if (length(A_ind) < 3) {
        outL[[samplename]][[loc]]$adata = c(nestedlist[[samplename]][[loc_lower]][[A_ind[[1]]]], nestedlist[[samplename]][[loc_lower]][[A_ind[[2]]]])
        if(length(H_ind)>0) {
          outL[[samplename]][[loc]]$hdata = c(nestedlist[[samplename]][[loc_lower]][[H_ind[[1]]]], nestedlist[[samplename]][[loc_lower]][[H_ind[[2]]]])
        }
      } else {
        alleles = list()
        heights = list()
        for (i in length(A_ind)) {
          alleles = c(alleles, nestedlist[[samplename]][[loc_lower]][[A_ind[[i]]]])
          heights = c(heights, nestedlist[[samplename]][[loc_lower]][[H_ind[[i]]]])
        }
        outL[[samplename]][[loc]]$adata = alleles
        outL[[samplename]][[loc]]$hdata = heights
      }
    }
  }
    return(outL)
  }
