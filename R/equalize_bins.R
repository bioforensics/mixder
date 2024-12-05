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
equalize_bins = function(inpath, biggerdf_id, smallerdf_id, smaller_df, nbins) {
  for (i in 1:(nbins)) {
    snp_df = read.table(glue("{inpath}/{biggerdf_id}_set{i}.tsv"), header=T, sep="\t")
    data_filtered=smaller_df[smaller_df$Marker %in% snp_df$Marker,]
    data_filtered$Sample.Name=glue("{smallerdf_id}_set{i}")
    write.table(data_filtered, glue("{inpath}/{smallerdf_id}_rep_set{i}.tsv"), row.names=F, quote=F, col.names=T, sep="\t")
  }
}
