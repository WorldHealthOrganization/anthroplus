test_that("zscore references match from previous implementation", {
  data <- readRDS("test_dataset_who2007.rds")
  # unlike the WHO 2007, the age upper limits are inclusive
  data$zhfa[data$agemons > 228] <- NA_real_
  data$fhfa[data$agemons > 228] <- NA_real_
  data$zbfa[data$agemons > 228] <- NA_real_
  data$fbfa[data$agemons > 228] <- NA_real_
  data$zwfa[data$agemons > 120] <- NA_real_
  data$fwfa[data$agemons > 120] <- NA_real_
  result <- anthroplus_zscores(
    sex = data$sex,
    age_in_months = data$agemons,
    oedema = data$oedema,
    height = data$height,
    weight = data$weight
  )
  expect_equal(result$zhfa, data$zhfa)
  expect_equal(result$fhfa, data$fhfa)
  expect_equal(result$zbfa, data$zbfa)
  expect_equal(result$fbfa, data$fbfa)
  expect_equal(result$zwfa, data$zwfa)
  expect_equal(result$fwfa, data$fwfa)
  expect_equal(result$cbmi, data$cbmi)
  expect_equal(result$age_in_months, data$agemons)
})

test_that("different sex encodings work", {
  expect_equal(
    anthroplus_zscores(1, 120, height_in_cm = 60, weight_in_kg = 30),
    anthroplus_zscores("m", 120, height_in_cm = 60, weight_in_kg = 30)
  )
  expect_equal(
    anthroplus_zscores(1, 120, height_in_cm = 60, weight_in_kg = 30),
    anthroplus_zscores("M", 120, height_in_cm = 60, weight_in_kg = 30)
  )
  expect_equal(
    anthroplus_zscores(2, 120, height_in_cm = 60, weight_in_kg = 30),
    anthroplus_zscores("f", 120, height_in_cm = 60, weight_in_kg = 30)
  )
  expect_equal(
    anthroplus_zscores(2, 120, height_in_cm = 60, weight_in_kg = 30),
    anthroplus_zscores("F", 120, height_in_cm = 60, weight_in_kg = 30)
  )
})

test_that("different oedema encodings work", {
  expect_equal(
    anthroplus_zscores(1, 120,
      oedema = "y",
      height_in_cm = 60, weight_in_kg = 30
    ),
    anthroplus_zscores(1, 120,
      oedema = 1,
      height_in_cm = 60, weight_in_kg = 30
    )
  )
  expect_equal(
    anthroplus_zscores(1, 120,
      oedema = 2,
      height_in_cm = 60, weight_in_kg = 30
    ),
    anthroplus_zscores(1, 120,
      oedema = "n",
      height_in_cm = 60, weight_in_kg = 30
    )
  )
  expect_equal(
    anthroplus_zscores(1, 120,
      oedema = NA_character_,
      height_in_cm = 60, weight_in_kg = 30
    ),
    anthroplus_zscores(1, 120,
      oedema = "n",
      height_in_cm = 60, weight_in_kg = 30
    )
  )
})

test_that("input validations work as expected", {
  expect_error(anthroplus_zscores(sex = " F"))
  expect_error(anthroplus_zscores(age_in_months = "1"))
  expect_error(anthroplus_zscores(age_in_months = -1))
  expect_error(anthroplus_zscores(height_in_cm = -1))
  expect_error(anthroplus_zscores(height_in_cm = "1"))
  expect_error(anthroplus_zscores(weight_in_kg = -1))
  expect_error(anthroplus_zscores(weight_in_kg = "1"))
  expect_error(anthroplus_zscores(oedema = "Yes"))
})

test_that("oedema = y implies NA for weight-for-age and bmi-for-age", {
  res1 <- anthroplus_zscores(1, 120,
    oedema = 1, height_in_cm = 60, weight_in_kg = 30
  )
  res2 <- anthroplus_zscores(1, 120,
    oedema = 2, height_in_cm = 60, weight_in_kg = 30
  )
  expect_true(is.na(res1$zwfa))
  expect_true(is.na(res1$zbfa))
  expect_true(is.na(res1$fwfa))
  expect_true(is.na(res1$fbfa))
  expect_false(is.na(res2$zwfa))
  expect_false(is.na(res2$zbfa))
  expect_false(is.na(res2$fwfa))
  expect_false(is.na(res2$fbfa))
})

test_that("Age upper bounds are inclusive", {
  res <- anthroplus_zscores(
    1, c(120, 228, 120.1, 228.1),
    height_in_cm = 60,
    weight_in_kg = 30
  )
  expect_equal(is.na(res$zhfa), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(is.na(res$zwfa), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(is.na(res$zbfa), c(FALSE, FALSE, FALSE, TRUE))
})
