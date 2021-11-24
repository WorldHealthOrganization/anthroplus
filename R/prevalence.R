#' Compute prevalence estimates
#'
#' Prevalence estimates according to the WHO recommended standard analysis:
#' includes prevalence estimates with corresponding standard errors
#' and confidence intervals, and z-score summary statistics
#' (mean and standard deviation) with most common cut-offs describing the
#' full index distribution (-3, -2, -1, +1, +2, +3), and at disaggregated
#' levels for all available factors (age and sex).
#'
#' In this function, all available (non-missing and non-flagged) z-score values
#' are used for each indicator-specific prevalence
#' estimation (standard analysis).
#'
#'
#' Note: the function temporarily sets the \code{survey} option
#' \code{survey.lonely.psu} to "adjust" and then restores the original value.
#' It is a wrapper around the \code{survey} package to compute
#' estimates for the different groups (e.g. by age or sex).
#'
#' If not all parameter values have equal length, parameter values will be
#' repeated to match the maximum length.
#'
#' Only cases with age_in_months between 61 (including) and 228 months
#' (including) are used for the analysis. The rest will be ignored.
#'
#' @inheritParams anthroplus_zscores
#'
#' @param sw An optional numeric vector containing the sampling weights.
#' If NULL, no sampling weights are used.
#'
#' @param cluster An optional integer vector representing clusters. If the value
#' is NULL this is treated as a survey without clusters. This is also the case
#' if all values are equal, then it is assumed there are also no clusters.
#'
#' @param strata An optional integer vector representing strata. Pass NULL to
#' indicate that there are no strata.
#'
#' @return Returns a data.frame with prevalence estimates for the various
#' groups.
#'
#' The output data frame includes prevalence estimates with corresponding
#' standard errors and confidence intervals,
#' and z-score summary statistics (mean and standard deviation) with most
#' common cut-offs describing the full index
#' distribution (-3, -2, -1, +1, +2, +3), and at disaggregated levels for
#' all available factors.
#'
#' The resulting columns are coded with a \emph{prefix},
#' \emph{a prevalence indicator} and \emph{a suffix}:
#'
#' \strong{Prefix:}
#' \describe{
#'  \item{HA}{Height-for-age}
#'  \item{WA}{Weight-for-age}
#'  \item{BMI}{Body-mass-index-for-age}
#' }
#'
#' \strong{Prevalence indicator:}
#' \describe{
#'  \item{_3}{Prevalence corresponding to < -3 SD}
#'  \item{_2}{Prevalence corresponding to < -2 SD}
#'  \item{_1}{Prevalence corresponding to < -1 SD}
#'  \item{1}{Prevalence corresponding to > +1 SD}
#'  \item{2}{Prevalence corresponding to > +2 SD}
#'  \item{3}{Prevalence corresponding to > +3 SD}
#' }
#'
#' \strong{Suffix:}
#' \describe{
#'  \item{_pop}{Weighted sample size}
#'  \item{_unwpop}{Unweighted sample size}
#'  \item{_r}{Mean/prevalence}
#'  \item{_ll}{lower 95% confidence interval limit}
#'  \item{_ul}{upper 95% confidence interval limit}
#'  \item{_stdev}{Standard Deviation}
#'  \item{_se}{Standard error}
#' }
#'
#'
#' \strong{For example:}
#' \describe{
#'   \item{HA_r}{Height-for-age z-score mean}
#'   \item{WA_stdev}{Weight-for-age z-score Standard Deviation}
#'   \item{BMI_2_se}{Prevalence of BMI-for-age <-2 SD standard error}
#'   \item{BMI_3_ll}{Prevalence of BMI-for-age <-3 SD lower 95% confidence
#'   interval limit}
#' }
#'
#' Note that weight-for-age results are NA for the groups "All" and the two
#' "Sex" groups, as the indicator is only defined for age in months
#' between 61 and 120.
#'
#' @examples
#' set.seed(1)
#' prev <- anthroplus_prevalence(
#'   sex = c(1, 2),
#'   age_in_months = rpois(100, 100),
#'   height_in_cm = rnorm(100, 100, 10),
#'   weight_in_kg = rnorm(100, 40, 10)
#' )
#' prev[, c(1, 4, 5, 6)]
#' @export
#' @include zscores.R
#' @importFrom anthro anthro_api_compute_prevalence
#' @importFrom stats setNames
anthroplus_prevalence <- function(sex,
                                  age_in_months = NA_real_,
                                  oedema = "n",
                                  height_in_cm = NA_real_,
                                  weight_in_kg = NA_real_,
                                  sw = NULL,
                                  cluster = NULL,
                                  strata = NULL) {
  stopifnot(all(tolower(sex) %in% c("1", "2", "f", "m", NA_character_)))
  stopifnot(all(tolower(oedema) %in% c("1", "2", "y", "n", NA_character_)))
  stopifnot(all(age_in_months >= 0, na.rm = TRUE))
  stopifnot(all(height_in_cm >= 0, na.rm = TRUE))
  stopifnot(all(weight_in_kg >= 0, na.rm = TRUE))
  stopifnot(is.null(cluster) || is.numeric(cluster))
  stopifnot(is.null(strata) || is.numeric(strata))
  stopifnot(is.null(sw) || is.numeric(sw))

  input <- data.frame(sex, age_in_months, oedema, height_in_cm, weight_in_kg)
  if (!is.null(cluster)) {
    input$cluster <- cluster
  }
  if (!is.null(strata)) {
    input$strata <- strata
  }
  if (!is.null(sw)) {
    input$sampling_weights <- sw
  }
  old_rows <- nrow(input)
  input <- input[!is.na(input$age_in_months) &
    input$age_in_months >= 61 &
    input$age_in_months <= 228, , drop = FALSE]
  if (nrow(input) == 0) {
    stop(
      "All age values are either NA or < 61 or > 228, which excludes all",
      " cases from the analysis.",
      call. = FALSE
    )
  } else if (nrow(input) < old_rows) {
    warning(
      old_rows - nrow(input),
      " row(s) with age NA or age < 61 months or > 228 months were excluded",
      " from the computation."
    )
  }
  zscores <- anthroplus_zscores(
    input$sex, input$age_in_months,
    input$oedema, input$height_in_cm, input$weight_in_kg
  )
  # age in months is also part of the z-score output
  zscores$age_in_months <- NULL
  input_zscores <- cbind(input, zscores)

  input_zscores$sex <- as.character(zscores$csex)
  input_zscores$sex[input_zscores$sex == "1"] <- "Male"
  input_zscores$sex[input_zscores$sex == "2"] <- "Female"
  stopifnot(
    all(is.na(input_zscores$sex) | input_zscores$sex %in% c("Male", "Female"))
  )
  input_zscores$sex <- factor(input_zscores$sex, levels = c("Female", "Male"))
  input_zscores$oedema <- zscores$coedema
  input_zscores$all <- "All"

  # Age groups in completed years
  input_zscores$age_groups <- prev_age_groups(input$age_in_months)

  # Wider age group and interaction
  input_zscores$wider_age_group <- prev_wider_age_groups(
    input$age_in_months
  )
  input_zscores$sex_wider_age_group <- interaction(
    input_zscores$sex, input_zscores$wider_age_group
  )

  prev_results <- anthro_api_compute_prevalence(
    data = input_zscores,
    zscores_to_compute = list(
      list(
        name = "HA", column = "hfa",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = FALSE
      ),
      list(
        name = "WA", column = "wfa",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = TRUE,
        auxiliary_zscore_condition = function(dataframe) {
          # for wfa the age limit is 120 and thus below 228.
          # we need to make sure z-scores are only set to -3.1 if the age is
          # in range
          dataframe[["age_in_months"]] <= WFA_UPPER_AGE_LIMIT &
            dataframe[["oedema"]] %in% "y"
        }
      ),
      list(
        name = "BMI", column = "bfa",
        with_cutoffs = TRUE, with_auxiliary_zscore_column = TRUE
      )
    ),
    survey_subsets = setNames(
      list(
        "all", "sex", "age_groups", "wider_age_group",
        "sex_wider_age_group"
      ),
      c("All", "Sex", "Age Group 1", "Age Group 2", "Age + Sex")
    )
  )

  prev_results <- cbind_year_month_columns(prev_results)
  set_wfa_to_na_for_non_age_groups(prev_results)
}

set_wfa_to_na_for_non_age_groups <- function(prev_results) {
  # the groups "All" and "Sex" contain values, though NA, for age in
  # months > 120.
  # We decided it is better to make the respective values NA
  # it assumes the first row is always the total followed by the two sex
  # groups
  cols_to_NA <- colnames(prev_results)
  cols_to_NA <- cols_to_NA[grepl("^WA", cols_to_NA)]
  stopifnot(grepl("All", prev_results[1, "Group", drop = TRUE], fixed = TRUE))
  stopifnot(grepl("Sex", prev_results[2:3, "Group", drop = TRUE], fixed = TRUE))
  prev_results[1:3, cols_to_NA] <- NA_real_
  prev_results
}

cbind_year_month_columns <- function(prev_results) {
  # we always expect the exact same number of rows
  # and in the same ordering.
  stopifnot(nrow(prev_results) == 27)
  wider_labels <- paste0(
    "(",
    gsub(" mo", "", prev_wider_age_group_labels, fixed = TRUE),
    ")"
  )
  age_labels <- data.frame(
    `Years` = c(
      "Total (5-19)", # all
      "Total (5-19)", "Total (5-19)", # sex
      5:19, # age group 1
      "Total (5-9)", "Total (10-14)", "Total (15-19)", # age group 2
      "Total (5-9)", "Total (5-9)", # female/male 1
      "Total (10-14)", "Total (10-14)", # female/male 2
      "Total (15-19)", "Total (15-19)" # female/male 3
    ),
    `Months` = c(
      "(61-228)",
      "(61-228)", "(61-228)",
      paste0("(", gsub(" mo", "", prev_age_group_labels, fixed = TRUE), ")"),
      wider_labels,
      c(wider_labels[1], wider_labels[1]),
      c(wider_labels[2], wider_labels[2]),
      c(wider_labels[3], wider_labels[3])
    ),
    stringsAsFactors = FALSE
  )

  cbind(
    prev_results[, 1, drop = FALSE],
    age_labels,
    prev_results[, -1, drop = FALSE]
  )
}

prev_age_group_labels <- c(
  "61-71 mo",
  "72-83 mo",
  "84-95 mo",
  "96-107 mo",
  "108-119 mo",
  "120-131 mo",
  "132-143 mo",
  "144-155 mo",
  "156-167 mo",
  "168-179 mo",
  "180-191 mo",
  "192-203 mo",
  "204-215 mo",
  "216-227 mo",
  "228-228 mo"
)
prev_age_groups <- function(age_in_months) {
  stopifnot(is.numeric(age_in_months), all(age_in_months <= 228, na.rm = TRUE))
  cut_breaks <- c(
    61, 72, 84, 96, 108, 120, 132,
    144, 156, 168, 180, 192, 204, 216, 228, 229
  )
  cut(age_in_months,
    breaks = cut_breaks,
    labels = prev_age_group_labels,
    right = FALSE
  )
}

prev_wider_age_group_labels <- c(
  "61-119 mo",
  "120-179 mo",
  "180-228 mo"
)
prev_wider_age_groups <- function(age_in_months) {
  stopifnot(is.numeric(age_in_months), all(age_in_months <= 228, na.rm = TRUE))
  cut_breaks <- c(61, 120, 180, 229)
  cut(age_in_months,
    breaks = cut_breaks,
    labels = prev_wider_age_group_labels,
    right = FALSE
  )
}
