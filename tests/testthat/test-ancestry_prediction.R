test_that("ancestry prediction", {
  outpath = tempdir()
  #kin_a = test_path("testdata", "Sample01c_uncond_major_Single_GEDmatchPROReport.txt")
  kin_a = read.csv(test_path("testdata", "Sample01c_uncond_major_Single_GEDmatchPROReport.txt"), header=T, sep="\t")
  #file.copy(c(kin_a), outpath)
  expect_true(ancestry_prediction(kin_a, outpath, "Sample01c", "uncond", "major", "AncestrySNPsOnly", "Superpopulations (AFR/AMR/EAS/EUR/SAS Only)"), file.exists(glue("{outpath}/PCA_plots/{id}_{contrib_status}_{analysis_type}_{plotid}_superpop_3D_PCAPlot.html")))
})
