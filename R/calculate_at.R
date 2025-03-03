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
#' Title
#'
#' @param sample Sample ID
#' @param kinreports Path of directory containing Kintelligence Sample Reports
#' @param dynamicAT Dynamic analytical threshold (AT)
#' @param staticAT Static analytical threshold (AT)
#'
#' @return data frame containing SNPs and AT values
#' @export
#'
#' @importFrom readxl read_excel
calculate_at = function(sample, kinreports, dynamicAT, staticAT) {
  for (file in list.files(kinreports, pattern = "^[^~]")) {
    sampleid = ifelse(grepl("rep", sample, fixed=TRUE), gsub("_rep","", sample), sample)
    if (grepl(sampleid, file, fixed=TRUE) & grepl("Report", file, fixed=TRUE)) {
      filename = paste(kinreports, file, sep="/")
      uas_setting = suppressMessages(read_excel(filename, sheet = "Settings"))
      if (grepl("2.5", uas_setting[11,2], fixed=TRUE) | grepl("2.6", uas_setting[11,2], fixed=TRUE)) {
        compiled_snps = load_kin_uas25(filename)
      } else {
        compiled_snps = load_kin_older(filename)
      }
      break
    } else if (grepl(sample, file, fixed=TRUE) & grepl(".tsv", file, fixed=TRUE)) {
      filename = paste(kinreports, file, sep="/")
      compiled_snps = read.table(filename, header=T, sep="\t")
      break
    }
  }
  at = compiled_snps %>%
    group_by(.data$Marker) %>%
    summarize(Reads=sum(.data$Reads))
  at$Thresh = ifelse(ceiling(at$Reads*dynamicAT) > staticAT, ceiling(at$Reads*dynamicAT), staticAT)
  at = data.frame(at)
  at$Marker = toupper(at$Marker)
  return(at[,c(1,3)])
}
