#' @importFrom anthro anthro_api_compute_zscore_adjusted
zscore_bmi_for_age <- function(sex, age_in_months, oedema,
                               bmi) {
  bmi[oedema == "y"] <- NA_real_
  zscore_indicator(sex, age_in_months, bmi,
    bfa_growth_standards,
    age_upper_bound = 228,
    zscore_fun = anthro_api_compute_zscore_adjusted
  )
}
