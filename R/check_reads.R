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


#' Title Check SNP sets if all SNPs have 0 reads
#'
#' @param sample path of input SNP set
#'
#' @return TRUE/FALSE
#' @export
#'
check_reads = function(sample) {
  input_set = read.table(sample, sep="\t", header=T)
  if (sum(input_set$Total_Reads)==0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
