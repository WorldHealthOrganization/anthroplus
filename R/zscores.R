#' Compute z-scores for age 5 to 19
#'
#' @param sex A numeric or text variable containing gender information.
#'            If it is numeric, its values must be: 1 for males and 2 for
#'            females. If it is character, it must be "m" or "M" for males
#'            and "f" or "F" for females. No z-scores will be calculated
#'            if sex is missing.
#' @param age_in_months A numeric variable containing age information;
#'            Age-related z-scores will NOT be calculated if age is missing.
#' @param oedema The values of this character variable must be "n", "N" or "2"
#'             for non-oedema, and "y", "Y", "1" for oedema. Although it
#'             is highly recommended that this variable is provided by the
#'             survey, it is possible to run
#'             the analysis without specifying this variable. If unspecified,
#'             the default vector of all "n" with values considered as
#'             non-oedema is used. Missing values will be
#'             treated as non-oedema. For oedema, weight related z-scores
#'             are NOT calculated (set to missing),
#'             BUT they are treated as being < -3 SD in the weight-related
#'             indicator prevalence (\code{\link{anthroplus_prevalence}})
#'             estimation.
#' @param height_in_cm A numeric variable containing standing height
#'               information, which must be in
#'               centimeters. Height-related z-scores will not be
#'               calculated if missing.
#' @param weight_in_kg A numeric variable containing body weight information,
#'               which must be in kilograms. Weight-related z-scores are not
#'               calculated if missing.
#'
#' @details
#' The following age cutoffs are used:
#' \itemize{
#' \item{Height-for-age} age between 60 and 228 months inclusive
#' \item{Weight-for-age} age between 60 and 120 months inclusive
#' \item{BMI-for-age} age between 60 and 228 months inclusive
#' }
#'
#' @return A data.frame with three types of columns. Columns starting with a
#' "c" are cleaned versions of the input arguments. Columns beginning with
#' a "z" are the respective z-scores and columns prefixed by a "f" indicate
#' if these z-scores are flagged (integers).
#' The number of rows is given by the length
#' of the input arguments.
#'
#' The following columns are returned:
#' \itemize{
#' \item{\code{age_in_months}} the input age in months
#' \item{\code{csex}} standardized sex information
#' \item{\code{coedema}} standardized oedema value
#' \item{\code{cbmi}} BMI value based on weight/height
#'
#' \item{\code{zhfa}} Height-for-age z-score
#' \item{\code{fhfa}} 1, if \code{abs(zhfa) > 6}
#'
#' \item{\code{zwfa}} Weight-for-age z-score
#' \item{\code{fwfa}} 1, if \code{zwfa > 5} or \code{zwfa < -6}
#'
#' \item{\code{zbfa}} BMI-for-age z-score
#' \item{\code{fbfa}} 1, if \code{abs(zbfa) > 5}
#'
#' }
#'
#' @examples
#' anthroplus_zscores(
#'   sex = c("f", "m"),
#'   age_in_months = c(100, 110),
#'   height_in_cm = c(100, 90),
#'   weight_in_kg = c(30, 40)
#' )
#' @export
#' @importFrom anthro anthro_api_standardize_oedema_var
#' @importFrom anthro anthro_api_standardize_sex_var
anthroplus_zscores <- function(sex,
                               age_in_months = NA_real_,
                               oedema = NA_character_,
                               height_in_cm = NA_real_,
                               weight_in_kg = NA_real_) {
  stopifnot(all(tolower(sex) %in% c("1", "2", "f", "m", NA_character_)))
  stopifnot(all(tolower(oedema) %in% c("1", "2", "y", "n", NA_character_)))
  stopifnot(all(age_in_months >= 0, na.rm = TRUE))
  stopifnot(all(height_in_cm >= 0, na.rm = TRUE))
  stopifnot(all(weight_in_kg >= 0, na.rm = TRUE))

  input <- data.frame(sex, age_in_months, oedema, height_in_cm, weight_in_kg)

  cbmi <- compute_bmi(input$weight_in_kg, input$height_in_cm)
  coedema <- anthro_api_standardize_oedema_var(input$oedema)
  csex <- anthro_api_standardize_sex_var(input$sex)

  zhfa <- zscore_height_for_age(
    sex = csex, age_in_months = input$age_in_months,
    height = input$height_in_cm
  )
  zwfa <- zscore_weight_for_age(
    sex = csex, age_in_months = input$age_in_months,
    oedema = coedema, weight = input$weight_in_kg
  )
  zbfa <- zscore_bmi_for_age(
    sex = csex, age_in_months = input$age_in_months,
    oedema = coedema, bmi = cbmi
  )

  zhfa <- round(zhfa, digits = 2L)
  zwfa <- round(zwfa, digits = 2L)
  zbfa <- round(zbfa, digits = 2L)

  fhfa <- flag_scores(zhfa, !is.na(zhfa) & abs(zhfa) > 6)
  fwfa <- flag_scores(zwfa, !is.na(zwfa) & (zwfa > 5 | zwfa < -6))
  fbfa <- flag_scores(zbfa, !is.na(zbfa) & abs(zbfa) > 5)

  data.frame(
    age_in_months,
    csex,
    coedema,
    cbmi,
    zhfa,
    zwfa,
    zbfa,
    fhfa,
    fwfa,
    fbfa
  )
}

flag_scores <- function(zscores, condition) {
  stopifnot(length(zscores) == length(condition))
  flags <- rep.int(0L, length(zscores))
  flags[condition] <- 1L
  if (anyNA(zscores)) {
    flags[is.na(zscores)] <- NA_integer_
  }
  flags
}

compute_bmi <- function(weight, height) {
  weight / ((height / 100)^2)
}

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

#' @importFrom anthro anthro_api_compute_zscore
zscore_height_for_age <- function(sex, age_in_months,
                                  height) {
  zscore_indicator(sex, age_in_months, height,
    hfa_growth_standards,
    age_upper_bound = 228,
    zscore_fun = anthro_api_compute_zscore
  )
}

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

zscore_indicator <- function(sex,
                             age_in_months,
                             measure,
                             growth_standards,
                             age_upper_bound,
                             zscore_fun) {
  low_age <- trunc(age_in_months)
  upp_age <- trunc(age_in_months + 1)
  diff_age <- age_in_months - low_age
  data <- data.frame(
    sex,
    low_age,
    upp_age,
    ordering = seq_along(sex)
  )
  match_low_age <- merge(data, growth_standards,
    by.x = c("sex", "low_age"),
    by.y = c("sex", "age"),
    all.x = TRUE, sort = FALSE
  )
  match_upp_age <- merge(data, growth_standards,
    by.x = c("sex", "upp_age"),
    by.y = c("sex", "age"),
    all.x = TRUE, sort = FALSE
  )
  match_low_age <- match_low_age[order(match_low_age$ordering), ]
  match_upp_age <- match_upp_age[order(match_upp_age$ordering), ]

  m <- match_low_age[["m"]]
  l <- match_low_age[["l"]]
  s <- match_low_age[["s"]]

  is_diff_age_pos <- !is.na(diff_age) & diff_age > 0
  if (any(is_diff_age_pos)) {
    adjust_param <- function(x) {
      x_name <- as.character(substitute(x))
      x[is_diff_age_pos] + diff_age[is_diff_age_pos] *
        (match_upp_age[[x_name]][is_diff_age_pos] - x[is_diff_age_pos])
    }
    m[is_diff_age_pos] <- adjust_param(m)
    l[is_diff_age_pos] <- adjust_param(l)
    s[is_diff_age_pos] <- adjust_param(s)
  }
  zscores <- zscore_fun(measure, m, l, s)
  has_invalid_valid_age <- is.na(age_in_months) |
    !(age_in_months >= 60 & age_in_months <= age_upper_bound)
  zscores[has_invalid_valid_age] <- NA_real_
  zscores
}
