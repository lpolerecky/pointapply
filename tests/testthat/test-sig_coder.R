test_that("significance stars can be produced", {
  expect_snapshot(sig_coder(0.01))
})
