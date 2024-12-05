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
#' @title Create AT table containing only SNPs contained in the evidence file
#'
#' @param evidData Evidence data frame
#' @param sample Sample ID
#' @param replicate_id Replicate ID if provided
#' @param attable Data frame containing AT for all SNPs
#'
#' @return Data frame
#' @export
create_at = function(evidData, sample, replicate_id, attable) {
  message("Creating AT table<br/>")
  locs = unique(attable[,"Marker"])
  id = ifelse(length(names(evidData[[sample]]) > length(names(evidData[[replicate_id]]))), sample, replicate_id)
  sample_at = c()
  for (loc in locs) {
    if (loc %in% names(evidData[[id]])) {
      rowinds = which(attable[,"Marker"]==loc)
      thresh = attable[rowinds,"Thresh"]
      sample_at[loc] = thresh
    }
  }
  return(sample_at)
}
