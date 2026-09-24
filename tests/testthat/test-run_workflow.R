test_that("Running MixDeR workflow", {
  tmpdir = tempdir()
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
  refData = euroformix::sample_tableToList(euroformix::tableReader(test_path("testdata", "EFM_references.csv")))
  snp_pos = mixder::kintelligence_snp_positions
  popFreq = list(mixder::popFreq_1000G, mixder::popFreq_1000G)
  out_path = glue("{tmpdir}/snp_sets/output/")
  ## calculating metrics
  create_config(date, FALSE, "Global - 1000G", NULL, NULL, "refs/", "samplemanifest.csv", NULL, NULL, out_path, run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, tmpdir, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2", filter_missing=FALSE, skipancestry=TRUE, NULL, NULL, "kintelligence", snp_pos)
  expect_error(run_workflow(date, "Sample01a", "", FALSE, popFreq, refData, out_path, out_path, run_mixdeconv=TRUE, unconditioned=TRUE, cond=NULL, "Calculate Metrics", 1, tmpdir, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "", "", filter_missing=FALSE, skipancestry=TRUE, NULL, NULL, "kintelligence", snp_pos), "No major/minor contributor IDs provided but calculating metrics for an unconditioned analysis. Please re-run!")
  expect_true(file.exists(glue("{tmpdir}/snp_sets/output/config_log_files/{date}/config_settings_run_{date}.txt")))
})

