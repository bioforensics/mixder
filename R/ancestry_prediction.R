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

#' Title Ancestry prediction using PCA
#'
#' @param report inferred genotypes
#' @param path write path
#' @param id sample ID
#' @param analysis_type mixure deconvolution type (conditioned vs. unconditioned)
#'
#' @import kgp
#' @import ggplot2
#' @import plotly
#'
#' @return NA
#' @export
#'
ancestry_prediction = function(report, path, id, analysis_type, contrib_status) {
  ## using R library to obtain ancestry info for 1000G samples
  #ancestry=kgp::kgp3[,c("id", "reg")]
  #ancestry_filt = subset(ancestry, reg != "SAS")
  geno=mixder::ancestry_1000G_all
  geno_filt=geno[,c(7:10030)]
  snps = data.frame("snp_id"=colnames(geno_filt))
  snps = snps %>%
    separate(snp_id, c("rsid", "ref_allele"), remove=F)
  snps$order = seq(1:nrow(snps))
  merged_alleles = merge(snps, report, by="rsid", all.x=T) %>%
    arrange(order)
  ## count alleles
  merged_alleles$num_alt = ifelse(merged_alleles$Allele1==merged_alleles$ref_allele & merged_alleles$Allele2==merged_alleles$ref_allele, 2, ifelse(merged_alleles$Allele1==merged_alleles$ref_allele | merged_alleles$Allele2==merged_alleles$ref_allele, 1, 0))

  ## re-format to match 1000G samples
  formatted_sample = merged_alleles %>%
    select(snp_id, num_alt) %>%
    pivot_wider(names_from=snp_id, values_from=num_alt)

  ## add unknown to 1000G genotypes
  geno_filt_unk = rbind(geno_filt, formatted_sample)

  ## remove any SNPs with NA values (in unknown sample)
  betaRedNAOmit <- geno_filt_unk %>%
    select_if(~ !any(is.na(.)))

  ##perform PCA
  pcaRed <- prcomp(betaRedNAOmit, center=TRUE, scale=FALSE)

  ## create data table of PCs
  PCs = data.frame(pcaRed$x)

  ## add unknown to ancestry and genotype IDs
  #ancestry_unk = ancestry %>%
    add_row(id = "Unk", reg = "Unk")
  geno_unk = geno %>%
    add_row(IID="Unk")

  ancestry = read.table("/Users/rebecca.mitchell/Desktop/ancestry_colors.txt", header=T, sep="\t") %>%
    add_row(id = "Unk", reg = "Unk", population = "Unk", color="red", superpop_color="red")
  ## merge genotypes with ancestry info; need to preserve order to match to PCA data
  geno_ancestry=merge(geno_unk, ancestry, by.x="IID", by.y="id")

  ## add ancestry info to PC data
  PCs_anc = cbind(ancestry=geno_ancestry[,c(10031:10032)], data.frame(PCs[,c(1:10)]))

  #colScale <- scale_color_manual(name = "Superpopulation",
  #                               values = c("AFR" = "lightblue3",
  #                                          "EAS" = "chartreuse2",
  #                                          "EUR" = "pink2",
  #                                          "AMR" = "gold3",
  #                                          "Unk" = "firebrick3"),
  #                               labels = levels(as.factor(PCs_anc$ancestry)))

  ## PCA plot
  #png(glue("{path}/{id}_{contrib_status}_{analysis_type}_PCA_plot.png"))
  #show(ggplot(PCs_anc, aes(x=PC1,y=PC2))+
  #  geom_point(aes(color=ancestry))+
  #  colScale+
  #  guides(color=guide_legend(title="Superpopulation"))+
  #  ggtitle(glue("{id} {contrib_status} {analysis_type};\n{ncol(betaRedNAOmit)} ancestry SNPs"))+
  #  theme(plot.title = element_text(hjust = 0.5)))
  #dev.off()


    #PCs_anc=merge(ancestry, PCs,by.x="id", by.y="V1")
    ##PCs_anc$Size=ifelse(PCs_anc$reg=="Unk",10,9)

    pal = unique(ancestry$superpop_color)
    pal = setNames(pal, unique(ancestry$reg))


    fig = plot_ly(PCs_anc, x = ~PC1, y = ~PC2, z = ~PC3, color = ~reg, colors=pal, size=10)
    fig = fig %>% add_markers()
    fig = fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                       yaxis = list(title = 'PC2'),
                                       zaxis = list(title = 'PC3')))
    htmlwidgets::saveWidget(as_widget(fig), glue("{path}/{id}_{contrib_status}_{analysis_type}_superpop_3D_PCAPlot.html"))
    fig_sub = plot_ly(PCs_anc, x = ~PC1, y = ~PC2, z = ~PC3, color = ~population, colors=pal, size=10)
    fig_sub = fig_sub %>% add_markers()
    fig_sub = fig_sub %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                       yaxis = list(title = 'PC2'),
                                       zaxis = list(title = 'PC3')))
    htmlwidgets::saveWidget(as_widget(fig), glue("{path}/{id}_{contrib_status}_{analysis_type}_subpopulations_3D_PCAPlot.html"))
}
