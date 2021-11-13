#' @importFrom anthro anthro_api_compute_zscore
zscore_height_for_age <- function(sex, age_in_months,
                                  height) {
  zscore_indicator(sex, age_in_months, height,
    hfa_growth_standards,
    age_upper_bound = 228,
    zscore_fun = anthro_api_compute_zscore
  )
}
