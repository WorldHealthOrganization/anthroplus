# reproduces part of the analysis using the survey package
check_with_survey <- function(input,
                              sampling_weights = NULL,
                              cluster = NULL,
                              strata = NULL) {
  expect_warning(
    res <- anthroplus_prevalence(
      input$sex,
      input$agemons,
      input$oedema,
      input$height,
      input$weight,
      sw = sampling_weights,
      cluster = cluster,
      strata = strata
    ), "excluded"
  )
  zscores <- anthroplus_zscores(
    input$sex,
    input$agemons,
    input$oedema,
    input$height,
    input$weight
  )
  zscores <- cbind(zscores, input)
  sw <- if (!is.null(sampling_weights)) {
    zscores$sw <- sampling_weights
    ~sw
  }
  cluster <- if (!is.null(cluster)) {
    zscores$cluster <- cluster
    ~cluster
  } else {
    ~1
  }
  strata <- if (!is.null(strata)) {
    zscores$strata <- strata
    ~strata
  }
  zscores <- zscores[zscores$agemons <= 228, ]
  design <- survey::svydesign(
    id = cluster, data = zscores,
    weights = sw, strata = strata, nest = TRUE
  )
  expected <- survey::svyby(~zhfa, ~sex, design, survey::svymean,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  expect_equal(rev(res$HA_r[2:3]), expected$zhfa)
  expect_equal(rev(res$HA_se[2:3]), expected$se)
}
