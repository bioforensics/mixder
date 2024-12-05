test_that("Running MixDeR workflow", {
  outpath = tempdir()
  #kin_a = test_path("testdata", "Sample01a_Sample_Report_2021_11_01_16_56_38.xlsx")
  #file.copy(c(input, inputb, alla, allb, kin_a, kin_b), outpath)
  date = glue("{Sys.Date()}_{format(Sys.time(), '%H_%M_%S')}")
  ## calculating metrics
  create_config(date, "General - 1000G", outpath, outpath, "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2")
  expect_error(run_workflow(date, "Sample01a", "", "General - 1000G", outpath, test_path("testdata", "samplemanifest.txt"), "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=NULL, "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "", ""), "No major/minor contributor IDs provided but calculating metrics for an unconditioned analysis. Please re-run!")
  expect_error(run_workflow(date, "Sample01a", "", "General - 1000G", NULL, test_path("testdata", "samplemanifest.txt"), "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2"), "No references provided but selected conditioned analyses or calculating metrics. Please re-run!")
  expect_error(run_workflow(date, "Sample01a", "", "General - gnomAD", NULL, test_path("testdata", "samplemanifest.txt"), "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2"), "No references provided but selected conditioned analyses or calculating metrics. Please re-run!")
  expect_error(run_workflow(date, "Sample01a", "", "General - 1000G", outpath, NULL, "output", run_mixdeconv=TRUE, unconditioned=TRUE, cond=c("Ref1"), "Calculate Metrics", 1, outpath, 0.015, 10, 6000, 0.98, 0.70, 0.98, 0.98, 0.60, 0.60, "Ref1", "Ref2"), "No Sample Manifest provided. Please re-run!")
  expect_true(file.exists(glue("{outpath}/snp_sets/output/config_log_files/{date}/config_settings_run_{date}.txt")))
})

