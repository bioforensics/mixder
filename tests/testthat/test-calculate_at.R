test_that("Calculating the AT for all SNPs", {
  fulltable = calculate_at("Sample01a", test_path("testdata"), 0.015, 10)
  expect_equal(fulltable[fulltable["Marker"]=="RS1000022",][[2]], 31)
  expect_equal(fulltable[fulltable["Marker"]=="RS1000137",][[2]], 10)
  expect_equal(nrow(fulltable), 10039)
})
