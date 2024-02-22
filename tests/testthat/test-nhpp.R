
test_that("mcddf works", {
  tau <- changepoints(lista_AG)
  theta <- fit_nhpp(lista_AG$segmenter, tau)
  m <- mcdf(theta)
  expect_equal(length(m), length(exceedances(lista_AG)))
})


test_that("BMDL works", {
  y <- test_set(n = 1, seed = 123)
  seg <- segment(y, method = "cpt-pelt")
  expect_s3_class(logLik(seg), "logLik")
  expect_type(BMDL(seg), "double")
})

test_that("parameter fitting works", {
  # Example 1
  y <- test_set(n = 1, seed = 123)
  plot(y)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  plot_mcdf(segment(y, method = "manual", cpts = tau))
  expect_lt(abs(theta$alpha[1] - 1), 0.05)
  
  m <- mcdf(theta)
  
  plot_mcdf(segment(y, method = "manual", cpts = tau))
  
  # Example 2
  y <- test_set(n = 1, seed = 456)
  plot(y)
  tau <- attr(y, "cpt_true")
  z <- split(exceedances(y), cut_inclusive(exceedances(y), pad_tau(tau, length(y))))
  
  theta <- fit_nhpp(y, tau)
  theta
  
  expect_equal(
    fit_nhpp_region(t = z[[1]], tau_left = 0, tau_right = tau)$par,
    theta[1, c("alpha", "beta")] |> unlist() |> unname()
  )
  
  expect_equal(
    fit_nhpp_region(t = z[[2]], tau_left = tau, tau_right = length(y))$par,
    theta[2, c("alpha", "beta")] |> unlist() |> unname()
  )
  
  plot_mcdf(segment(y, method = "manual", cpts = tau))
})
