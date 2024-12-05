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
#' Get Reference IDs from either CSV file or Sample Reports
#'
#' @param inpath path of folder to reference file(s)
#'
#' @return command to get list of files
#' @export
get_ids = function(inpath) {
  if (file.exists(glue("{inpath}/EFM_references.csv"))) {
    all_samples = utils::read.csv(glue("{inpath}/EFM_references.csv"))
    if ("Sample.Name" %in% colnames(all_samples))  {
      return(unique(all_samples$Sample.Name))
    } else {
      stop("`Sample.Name` column does not exist in reference CSV file. Please fix column names and re-upload folder!")
    }
  } else if (length(list.files(inpath, pattern = "*.xlsx")) > 0) {
    ids = c()
    for (filename in list.files(inpath, pattern = "*.xlsx")) {
      exl = suppressMessages(read_excel(glue("{inpath}/{filename}"), sheet = "Ancestry SNPs"))
      ids = c(ids, exl[[2,2]])
    }
    return(ids)
  }
}
