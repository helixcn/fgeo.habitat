context("test-summary.krig.R")

test_that("outputs correct names", {
  res <- suppressMessages(krig(soil_fake, "c"))
  out <- utils::capture.output(summary(res))
  correct_names <- names(res) %>% 
    map_lgl(~any(suppressWarnings(str_detect(.x, out)))) %>% 
    all()
  expect_true(correct_names)
})

test_that("outputs correct names", {
  res <- suppressMessages(krig_lst(soil_fake, "c"))
  out <- utils::capture.output(summary(res))
  correct_names <- names(res[[1]]) %>% 
    map_lgl(~any(suppressWarnings(str_detect(.x, out)))) %>% 
    all()
  expect_true(correct_names)
})
