context("Check that the quantile functions are the inverses of the
        conditional distributions.")

test_that("qcondig21 is the inverse of pcondig21", {
  for (cpar_ in .cpar){
    qu <- qcondig(.u, .v, theta = cpar_[1L], alpha = cpar_[2L])
    u2 <- pcondig(qu, .v, theta = cpar_[1L], alpha = cpar_[2L])
    expect_equal(.u, u2, tolerance = 1e-6)
  }
})


test_that("qcondigl21 is the inverse of pcondigl21", {
  for (alpha_ in .alpha){
    qu <- qcondigl(.u, .v, alpha = alpha_)
    u2 <- pcondigl(qu, .v, alpha = alpha_)
    expect_equal(.u, u2, tolerance = 5e-4)
  }
})

test_that("qcondig12 is the inverse of pcondig12", {
  for (cpar_ in .cpar){
    qu <- qcondig12(.u, .v, theta = cpar_[1], alpha = cpar_[2])
    u2 <- pcondig12(qu, .v, theta = cpar_[1], alpha = cpar_[2])
    expect_equal(.u, u2)
  }
})


test_that("qcondigl12 is the inverse of pcondigl12", {
  for (alpha_ in .alpha){
    qu <- qcondigl12(.u, .v, alpha = alpha_)
    u2 <- pcondigl12(qu, .v, alpha = alpha_)
    expect_equal(.u, u2)
  }
})
