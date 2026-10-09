test_that("Formatting Reference Genotypes", {
  refs = format_ref("Ref1", test_path("testdata"))
  expect_equal(nrow(subset(refs, A2_order>A1_order)), 0)
  expect_equal(refs[1,1], "Ref1")
})
