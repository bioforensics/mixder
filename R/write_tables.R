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
#' @title Write metrics tables
#'
#' @param x List containing a compiled metrics table of all allele 1 probability
#'    thresholds and a table containing metrics for the minimum number of SNPs.
#' @param outfile Character string containing the sample ID
#' @param minimum_snps Minimum number of SNPs required.
#'
#' @export
write_tables = function(x, outfile, minimum_snps) {
  if (nrow(x[[1]]) > 0) {
    write.table(x[[1]], glue("{outfile}_metrics_table.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
  }
  if (nrow(x[[2]]) > 0) {
    write.table(x[[2]], glue("{outfile}_metrics_table_minimumsnps_{minimum_snps}.tsv"), col.names=T, sep="\t", row.names=F, quote=F)
  }
}
