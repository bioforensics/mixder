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
#' @title Formatting Kintelligence Sample Reports created with UAS version <2.5
#'
#' @param filename Kintelligence Sample Report
#'
#' @return Data frame containing the reference of interest genotypes
#' @export
load_kin_older = function(filename) {
  all_sheets = data.frame()
  cols=c("Marker", "Genotype", "Allele", "Typed", "Reads")
  for (sheetname in c("Ancestry SNPs", "Phenotype SNPs", "Identity SNPs", "Kinship SNPs")) {
    excel_sheet = suppressMessages(read_excel(filename, sheet = sheetname))
    bottom_row = nrow(excel_sheet)+1
    sheetrange = glue("{sheetname}!A16:E{bottom_row}")
    excel_sheet = suppressMessages(read_excel(filename, range = sheetrange, col_names = cols))
    all_sheets = rbind(all_sheets, excel_sheet)
  }
  all_sheets$Genotype = NULL
  all_sheets$Reads = as.numeric(all_sheets$Reads)
  return(all_sheets)
}
