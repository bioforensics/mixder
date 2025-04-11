
#' Centroid Calculations and Plotting
#'
#' @param groups Groups used for PCA (Superpopulations and/or Subpopulations)
#' @param pca dataframe of PCs and ancestry
#' @param inpath input path
#' @param ID ID for creating file name(s)
#'
#' @export
#'
centroids = function(groups, pca, inpath, ID) {
  dir.create(file.path(inpath, "Centroids_Plots"), showWarnings = FALSE, recursive=TRUE)

  ancestry_colors = read.table("/Users/rebecca.mitchell/Desktop/ancestry_colors.txt", header=T, sep="\t") %>%
    add_row(id = "Unk", reg = "Unk", population = "Unk", color="red", superpop_color="red") %>%
    add_row(id= "Centroid", reg = "Centroid", population = "Centroid", color = "black", superpop_color="black")


  if ("Superpopulations (AFR/AMR/EAS/EUR/SAS Only)" %in% groups) {
    pca.centroids.pop = aggregate(pca[,5:14], list(Type = pca$reg), mean)

    superpop_dist=data.frame()
    for (pop in unique(pca.centroids.pop$Type)){
      if (pop != "Unk"){
        superpop_dist_calc=dist(rbind(pca.centroids.pop[pca.centroids.pop$Type == "Unk",2:4],pca.centroids.pop[pca.centroids.pop$Type == pop,2:4]), method = "euclidean")
        superpop_dist=rbind(superpop_dist, data.frame(Superpopulation=pop, Distance=superpop_dist_calc))
      }
    }
    superpop_dist_final = superpop_dist %>%
      arrange(Distance)

    write.table(superpop_dist_final, glue("{inpath}/Centroids_Plots/{ID}_Superpopulations_centroids_Calculations.txt"), row.names=F, quote=F, col.names=T, sep="\t")

    plot_df_super = pca.centroids.pop %>%
      filter(Type!="Unk")

    pal = unique(ancestry_colors$superpop_color)
    #pal = append(pal, "black")
    pal = setNames(pal, unique(ancestry_colors$reg))

    fig_super = plot_ly()

    fig_super = fig_super %>% add_markers(data=pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~reg, colors=pal, size=10)
    fig_super = fig_super %>% add_markers(data=plot_df_super, x=~PC1, y=~PC2, z=~PC3, color="Centroid", colors="black", size=10)
    fig_super = fig_super %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                                  yaxis = list(title = 'PC2'),
                                                  zaxis = list(title = 'PC3')))

    htmlwidgets::saveWidget(as_widget(fig_super), glue("{inpath}/Centroids_Plots/{ID}_Superpopulations_centroids.html"))

  }

  if ("Subpopulations" %in% groups) {

  pca.centroids.subpop = aggregate(pca[,5:14], list(Type = pca$population), mean)

  subpop_dist=data.frame()
  for (pop in unique(pca.centroids.subpop$Type)){
    if (pop != "Unk"){
      subpop_dist_calc=dist(rbind(pca.centroids.subpop[pca.centroids.subpop$Type == "Unk",2:4],pca.centroids.subpop[pca.centroids.subpop$Type == pop,2:4]), method = "euclidean")
      subpop_dist=rbind(subpop_dist, data.frame(Subpopulation=pop, Distance=subpop_dist_calc))
    }
  }
  subpop_dist_final = subpop_dist %>%
    arrange(Distance)

  write.table(subpop_dist_final, glue("{inpath}/Centroids_Plots/{ID}_Subpopulations_centroids_Calculations.txt"), row.names=F, quote=F, col.names=T, sep="\t")
  ## subpopulation plot

  plot_df_sub = pca.centroids.subpop %>%
    filter(Type!="Unk")

  pal = unique(ancestry_colors$color)
  #pal = append(pal, "black")
  pal = setNames(pal, unique(ancestry_colors$population))

  fig_sub = plot_ly()

  fig_sub = fig_sub %>% add_markers(data=pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~population, colors=pal, size=10)
  fig_sub = fig_sub %>% add_markers(data=plot_df_sub, x=~PC1, y=~PC2, z=~PC3, color="Centroid", colors="black", size=10)
  fig_sub = fig_sub %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                    yaxis = list(title = 'PC2'),
                                    zaxis = list(title = 'PC3')))

  htmlwidgets::saveWidget(as_widget(fig_sub), glue("{inpath}/Centroids_Plots/{ID}_Subpopulations_centroids.html"))
  }
}
