test_that("Running MixDeR workflow", {
  outpath = tempdir()
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
  refData = euroformix::sample_tableToList(euroformix::tableReader(test_path("testdata", "EFM_references.csv")))
  ## calculating metrics
  create_config(date, FALSE, "Global - 1000G", "", "", outpath, outpath, "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2", filter_missing=FALSE)
  expect_error(run_workflow(date, "Sample01a", "", FALSE, "Global - 1000G", "","", refData, outpath, test_path("testdata", "samplemanifest.txt"), "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=NULL, "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "", "", filter_missing=FALSE), "No major/minor contributor IDs provided but calculating metrics for an unconditioned analysis. Please re-run!")
  expect_error(run_workflow(date, "Sample01a", "", FALSE, "Global - 1000G", "","", refData, outpath, NULL, "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2", filter_missing=FALSE), "No Sample Manifest provided. Please re-run!")
  expect_true(file.exists(glue("{outpath}/snp_sets/output/config_log_files/{date}/config_settings_run_{date}.txt")))
})

