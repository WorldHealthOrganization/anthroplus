test_dataset_who2007 <- read.csv("data-raw/survey_who2007_z.csv")
test_dataset_who2007$dob <- NULL
test_dataset_who2007$dov <- NULL
saveRDS(
  test_dataset_who2007,
  "inst/testdata/test_dataset_who2007.rds",
  version = 3
)
