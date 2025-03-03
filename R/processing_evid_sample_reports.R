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
#' Creating usable data frame from a Kintelligence Sample Report
#'
#' @param inpath input path
#' @param id Sample ID
#'
#' @export
#' @importFrom tidyr pivot_wider
processing_evid_sample_reports = function(inpath, id) {
  for (file in list.files(inpath, pattern = "^[^~]")) {
    if (grepl(id, file, fixed=TRUE) & grepl("Report", file, fixed=TRUE)) {
      filename = paste(inpath, file, sep="/")
      uas_setting = suppressMessages(read_excel(filename, sheet = "Settings"))
      if (grepl("2.5", uas_setting[11,2], fixed=TRUE) | grepl("2.6", uas_setting[11,2], fixed=TRUE)) {
        final_snps = load_kin_uas25(filename)
      } else {
        compiled_snps = load_kin_older(filename)
        final_snps = reverse_comp(compiled_snps)
      }
      final_snps$Typed = NULL
    } else if (grepl(id, file, fixed=TRUE) & grepl(".tsv", file, fixed=TRUE)) {
      filename = paste(inpath, file, sep="/")
      final_snps = read.table(filename, header=T, sep="\t")
    }
  }

  evid_formatted = final_snps %>%
    group_by(.data$Marker) %>%
    rename(Height = "Reads") %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = row, values_from = c("Allele", "Height"), names_sep = ".")
  return(evid_formatted)
}

