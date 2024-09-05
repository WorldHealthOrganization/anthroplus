test_that("general integration test works", {
  input <- readRDS("test_dataset_who2007.rds")
  check_with_survey(input)
})

test_that("sampling weights are considered correctly", {
  input <- readRDS("test_dataset_who2007.rds")
  set.seed(1)
  sw <- runif(nrow(input)) / nrow(input)
  check_with_survey(input, sampling_weights = sw)
})

test_that("clusters are considered correctly", {
  input <- readRDS("test_dataset_who2007.rds")
  set.seed(1)
  cluster <- 1:nrow(input) %% 3
  check_with_survey(input, cluster = cluster)
})

test_that("cluster and strata are considered correctly", {
  input <- readRDS("test_dataset_who2007.rds")
  cluster <- 1:nrow(input) %% 3
  strata <- as.integer(input$sex == 1)
  # implicitly tests nested cluster
  check_with_survey(input, cluster = cluster, strata = strata)
})

test_that("strata are considered correctly", {
  input <- readRDS("test_dataset_who2007.rds")
  strata <- as.integer(input$sex == 1)
  # implicitly tests nested cluster
  check_with_survey(input, strata = strata)
})

test_that("age only between 60 and 229 is considered", {
  input <- readRDS("test_dataset_who2007.rds")
  input$agemons <- input$agemons * 2
  input_filtered <- input[input$agemons >= 60 & input$agemons <= 228, ]
  expect_warning(
    res1 <- anthroplus_prevalence(
      input$sex,
      input$agemons,
      input$oedema,
      input$height,
      input$weight
    ), "excluded"
  )
  res2 <- anthroplus_prevalence(
    input_filtered$sex,
    input_filtered$agemons,
    input_filtered$oedema,
    input_filtered$height,
    input_filtered$weight
  )
  expect_equal(res1, res2)
})

test_that("Female/Male is used in groups, regardles of input coding", {
  res <- anthroplus_prevalence(
    c("1", "2", "f", "m"),
    61:100,
    "n",
    100,
    35
  )
  expect_equal(res$HAZ_pop[res$Group == "Sex: Female"], 20)
  expect_equal(res$HAZ_pop[res$Group == "Sex: Male"], 20)
  expect_true(
    which(res$Group == "Sex: Female") <
      which(res$Group == "Sex: Male")
  )
})

test_that("it fails if sampling weights are negative", {
  expect_error(
    anthroplus_prevalence(
      c("1", "2", "f", "m"),
      61,
      "n",
      100,
      35,
      sw = c(1, 1, -1, 1)
    )
  )
})

test_that("it fails if all values are filtered out", {
  expect_error(
    anthroplus_prevalence(
      1,
      59,
      "n",
      100,
      35,
    ), "age"
  )
})

test_that("NA values in age are filtered out", {
  expect_warning(
    res <- anthroplus_prevalence(
      c("1", "2", "f", "m"),
      c(61, 61, NA_real_, 61),
      "n",
      100,
      35
    ), "excluded"
  )
  expect_equal(res$HAZ_pop[res$Group == "All"], 3)
})

test_that("NA sampling weights are set to 0", {
  expect_warning(
    res <- anthroplus_prevalence(
      c("1", "2", "f", "m"),
      61,
      "n",
      100,
      35,
      sw = c(0.3, 0.3, NA_real_, 0.4)
    ),
    "sampling weights"
  )
  res2 <- anthroplus_prevalence(
    c("1", "2", "f", "m"),
    61,
    "n",
    100,
    35,
    sw = c(0.3, 0.3, 0, 0.4)
  )
  expect_equal(res, res2)
})

test_that("Age groups are in completed years", {
  res <- anthroplus_prevalence(
    1,
    61:228,
    "n",
    seq.int(100, 180, length.out = 168),
    seq.int(35, 70, length.out = 168)
  )
  res <- res[grepl("Age Group 1", res$Group, fixed = TRUE), ]
  expected_unwpop <- table(floor(61:228 / 12))
  expect_equal(
    res$HAZ_unwpop,
    as.numeric(expected_unwpop)
  )
})

test_that("oedema = y implies zscore = -3.1 for weight related indicators", {
  res <- anthroplus_prevalence(
    c("1", "2", "2", "1"),
    61:100,
    "y",
    100,
    35
  )

  # should still be counted in prevalence
  expect_true(all(res$WAZ_unwpop[-(1:3)] >= 0))
  expect_true(all(res$BMIZ_unwpop >= 0))
  expect_true(sum(res$WAZ_unwpop[-(1:3)]) > 0)
  expect_true(sum(res$BMIZ_unwpop) > 0)

  # since all zscores for prevalence are -3.1, it should yield 100% everywhere
  expect_equal(mean(res$WA_3_r, na.rm = TRUE), 100)
  expect_equal(mean(res$WA_2_r, na.rm = TRUE), 100)
  expect_equal(mean(res$WA_1_r, na.rm = TRUE), 100)
  expect_equal(mean(res$BMI_3_r, na.rm = TRUE), 100)
  expect_equal(mean(res$BMI_2_r, na.rm = TRUE), 100)
  expect_equal(mean(res$BMI_1_r, na.rm = TRUE), 100)

  # but the zscore based estimates should still be NA
  expect_true(all(is.na(res$WA_r)))
  expect_true(all(is.na(res$WA_se)))
  expect_true(all(is.na(res$WA_ll)))
  expect_true(all(is.na(res$WA_ul)))
  expect_true(all(is.na(res$BMI_r)))
  expect_true(all(is.na(res$BMI_se)))
  expect_true(all(is.na(res$BMI_ll)))
  expect_true(all(is.na(res$BMI_ul)))
})

test_that("oedema correction is only applied if age <= 120 for wfa", {
  res <- anthroplus_prevalence(
    c(1, 2, 1, 2, 1, 2),
    150,
    "y",
    100,
    60
  )
  expect_true(all(is.na(res[, "WA_3_r"])))
})

test_that("'all' and 'sex w/o age' results are NA for WFA indicator", {
  res <- anthroplus_prevalence(
    c("1", "2", "2", "1"),
    61:100,
    "n",
    100,
    35
  )
  wa_cols <- colnames(res)
  wa_cols <- wa_cols[grepl("^WA", wa_cols)]
  expect_true(
    all(is.na(res[1:3, wa_cols, drop = TRUE]))
  )
})

test_that("age in months = 228 is part of the age group", {
  expect_false(is.na(prev_wider_age_groups(228)))
  expect_false(is.na(prev_age_groups(228)))
})

test_that("age in months = 60 is part of the age group", {
  expect_false(is.na(prev_wider_age_groups(60)))
  expect_false(is.na(prev_age_groups(60)))
})
