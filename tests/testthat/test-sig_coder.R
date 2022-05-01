test_that("significance stars can be produced", {
  expect_snapshot(
    sig_coder(0.01)
  )
  expect_snapshot(
    sig_coder(c(0, 0.001, 0.3, NA))
  )
  expect_snapshot(
    sig_coder(c(0, 0.001, 0.3, 0.02))
  )
  expect_snapshot(
    sig_coder(0, make_lab = FALSE)
  )
})
