WFA_UPPER_AGE_LIMIT <- 120

#' @importFrom anthro anthro_api_compute_zscore_adjusted
zscore_weight_for_age <- function(sex, age_in_months, oedema,
                                  weight) {
  weight[oedema == "y"] <- NA_real_
  zscore_indicator(sex, age_in_months, weight,
    wfa_growth_standards,
    age_upper_bound = WFA_UPPER_AGE_LIMIT,
    zscore_fun = anthro_api_compute_zscore_adjusted
  )
}
