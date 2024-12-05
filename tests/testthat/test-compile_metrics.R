test_that("Compiling metrics into a data frame for each threshold combination", {
  testdf = data.frame(Locus = c("rs28635343", "rs16824588", "rs424079", "rs2055204"), A1 = c("A", "T", "T", "T"), A2 = c("A", "A", "A", "C"), A1_Prob = c(1, 0.99, 0.9, 0.6), A2_Prob = c(1, 0.9, 0.99, 0.5))
  ref = data.frame(Marker = c("rs28635343", "rs16824588", "rs424079", "rs2055204"), A1_order = c("A", "T", "T", "T"), A2_order=c("A", "C", "A", "T"))
  testlist = compile_metrics(testdf, ref, 2, 0.95, 1, 0.9, 1)
  expect_equal(length(testlist), 2)
  expect_equal(nrow(testlist[[1]]), 66)
  expect_equal(nrow(testlist[[2]]), 11)
})
