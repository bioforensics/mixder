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
#' Read in Frequency tables already formatted for EFM
#'
#' @param freq PATH to frequency table
#'
#' @returns list of frequency data in correct EFM list format
#' @export
#'
#' @importFrom data.table fread
#'
read_in_freq = function(freq){
  efmaf=fread(freq, header=T, sep=",")
  tab=data.frame(efmaf)
  Anames = tab[,1] #first column is allele frequeneies
  tab = tab[,-1,drop=FALSE]
  freqlist = vector("list", ncol(tab))
  for(j in 1:ncol(tab)) { #for each locus
    tmp = tab[,j]
    tmp2 = tmp[!is.na(tmp) & as.numeric(tmp)>0] #require that allele is not NA and is>0
    names(tmp2) = Anames[!is.na(tmp)]
    freqlist[[j]] = tmp2
  }
  names(freqlist) = toupper(colnames(tab)) #LOCUS-names are assigned as Upper-case! This is important to do!
  return(freqlist)
}
