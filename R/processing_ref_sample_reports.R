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
#' Processing Reference Sample Reports
#'
#' @param inpath input path to folder of Reference Sample Reports
#'
#' @export
processing_ref_sample_reports = function(inpath){
  all_refs = data.frame()
  for (file in intersect(list.files(inpath, pattern = ".xlsx"), list.files(inpath, pattern = "^[^~]"))) {
    filepath = glue("{inpath}/{file}")
    uas_setting = suppressMessages(read_excel(filepath, sheet = "Settings"))
    sampleid = suppressMessages(read_excel(filepath, sheet = "Sample History"))[2,2][[1]]
    if ("2.5" %in% uas_setting[11,2] | "2.6" %in% uas_setting[11,2]) {
      final_snps = load_kin_uas25(filepath)
    } else {
      compiled_snps = load_kin_older(filepath)
      final_snps = reverse_comp(compiled_snps)
    }
    filt_snps = final_snps %>%
      filter(.data$Typed == "Yes") %>%
      group_by(.data$Marker) %>%
      select(-c("Typed", "Reads")) %>%
      mutate(row = row_number()) %>%
      pivot_wider(names_from = row, values_from = "Allele", names_prefix = c("Allele"))
    filt_snps$Allele2 = ifelse(is.na(filt_snps$Allele2), filt_snps$Allele1, filt_snps$Allele2)
    filt_snps = cbind(Sample.Name = sampleid, filt_snps)
    all_refs = rbind(all_refs, filt_snps)
  }
  utils::write.csv(all_refs, glue("{inpath}/EFM_references.csv"), row.names=F)
  return(all_refs)
}
