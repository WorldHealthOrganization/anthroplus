test_dataset_who2007 <- read.csv("data-raw/survey_who2007_z.csv")
test_dataset_who2007$dob <- NULL
test_dataset_who2007$dov <- NULL
test_dataset_who2007$region <- NULL
test_dataset_who2007$age.mo <- NULL
saveRDS(
  test_dataset_who2007,
  "tests/testthat/test_dataset_who2007.rds",
  version = 3
)
