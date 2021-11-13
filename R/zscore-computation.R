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
    !(age_in_months >= 61 & age_in_months <= age_upper_bound)
  zscores[has_invalid_valid_age] <- NA_real_
  zscores
}
