check_df <- function(df) {
  stopifnot(all(colnames(df) == c("sex", "age", "l", "m", "s")))
  stopifnot(all(apply(df, 2, is.numeric)))
  stopifnot(all(df[["sex"]] %in% c(1, 2)))
  stopifnot(all(df[["age"]] >= 60))
}

bfa_growth_standards <- read.csv("data-raw/growthstandards/bfawho2007.txt", sep = "\t")
hfa_growth_standards <- read.csv("data-raw/growthstandards/hfawho2007.txt", sep = "\t")
wfa_growth_standards <- read.csv("data-raw/growthstandards/wfawho2007.txt", sep = "\t")

check_df(bfa_growth_standards)
check_df(hfa_growth_standards)
check_df(wfa_growth_standards)

usethis::use_data(bfa_growth_standards,
  hfa_growth_standards,
  wfa_growth_standards,
  overwrite = TRUE, internal = TRUE, version = 3
)
