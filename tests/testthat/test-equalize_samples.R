test_that("Equalizing two data frames to contain same SNPs", {
  df1 = data.frame(`Sample Name` = c("A01", "A01", "A01"), Marker = c("rs3843794", "rs5979573", "rs6530168"), Allele.1 = c("A", "A", "A"), Allele.2 = c("C", "C", "C"), Height.1 = c(100, 100, 100), Height.2 = c(100, 100, 100), Total_Reads = c(215, 215, 215))
  df2 = data.frame(`Sample Name` = c("A01", "A01"), Marker = c("rs3843794", "rs5979573"), Allele.1 = c("A", "A"), Allele.2 = c("C", "C"), Height.1 = c(100, 100), Height.2 = c(100, 100), Total_Reads = c(215, 215))
  expect_equal(equalize_samples(df1, df2)[[1]], df1)
  expect_equal(equalize_samples(df1, df2)[[2]], data.frame(`Sample Name` = c("A01", "A01", "A01"), Marker = c("rs3843794", "rs5979573", "rs6530168"), Allele.1 = c("A", "A", "A"), Allele.2 = c("C", "C", "C"), Height.1 = c(100, 100, 0), Height.2 = c(100, 100, 0), Total_Reads = c(215, 215, 0)))
})
