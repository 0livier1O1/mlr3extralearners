install_learners("regr.nnet")
load_tests("regr.nnet")

test_that("autotest", {
  learner = lrn("regr.nnet")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
