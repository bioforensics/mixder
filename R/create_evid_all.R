#' Creating SNP sets from evidence samples containing all SNPs
#'
#' @param inpath input path
#' @param id Sample ID
#' @param nsets number of SNP sets to create

#' @export
create_evid_all = function(inpath, id, nsets){
  evid_all = processing_evid_sample_reports(inpath, id)
  evid_all$Total_Reads = rowSums(evid_all[,grepl("Height",colnames(evid_all))], na.rm = TRUE)
  evid_sort = evid_all[order(evid_all$Total_Reads),]
  bin_size = round(10039/nsets)
  dir.create(file.path(inpath, "snp_sets"), showWarnings = FALSE, recursive=TRUE)
  evid_final = cbind(Sample.Name = id, evid_sort)
  write.table(evid_final, glue("{inpath}/snp_sets/{id}_snpsetscombined_evidence.tsv"), row.names=F, quote=F, col.names=T, sep="\t")
  end = 0
  for (snpset in 1:(nsets)) {
    if (!file.exists(glue("{inpath}/snp_sets/",id,"_set",snpset,".tsv"))) {
      start = end + 1
      end = ifelse(snpset != nsets, start + bin_size, nrow(evid_final))
      evid_set=evid_final[start:end,]
      evid_set$Sample.Name = glue("{id}_set{snpset}")
      write.table(evid_set, glue("{inpath}/snp_sets/{id}_set{snpset}.tsv"), row.names=F, quote=F, col.names=T, sep="\t")
    }
  }
}
