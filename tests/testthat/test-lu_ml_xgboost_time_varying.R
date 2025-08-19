test_that("lu_ml_xgboost_time_varying() work for Mian and Sufi Data", {

  data(dt_mian_sufi_2014)
  data(dt_cnty_lu_2010)

  dt_mian_sufi_2014 <- dt_mian_sufi_2014 %>%
    .[, .(GEOID = fips, index = as.Date("2002-01-01"), hp.target = house.net.worth)] %>%
    .[!is.na(hp.target)]

  dt_cnty_lu_2010 <- dt_cnty_lu_2010 %>%
    .[, .(GEOID, unavailable = .SD),
      .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

  res <- lu_ml_xgboost_time_varying(DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010)

  expect_true(is.data.table(res))
  expect_equal(names(res), c("GEOID", "index", "hp.target", "lu_ml_xgboost"))
  ## Round to just 1 decimal place as results depend somewhat on the xgboost version
  expect_equal(round(res[, cor(hp.target, lu_ml_xgboost)], 1), 0.7)

  ## Test for reproducibility
  res_run2 <- lu_ml_xgboost_time_varying(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010
  )

  expect_equal(res$hp.target, res_run2$hp.target)

  dt_cnty_lu_2010_without_total <- dt_cnty_lu_2010 %>%
    .[, .SD, .SDcols = !grepl("total_unavailable", names(.))]

  ## Removing the `total_unavailable` columns should slightly improve results. 
  res_without_total <- lu_ml_xgboost_time_varying(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010_without_total
  )

  ## expect_true(
  ##   res_without_total[, mean((hp.target - lu_ml_xgboost) ^ 2)] <
  ##     res[, mean((hp.target - lu_ml_xgboost) ^ 2)]
  ## )

  res_with_importance <- lu_ml_xgboost_time_varying(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010, importance = TRUE
  )
  
  expect_equal(
    names(res_with_importance), 
    c("DT.oos.pred.panel", "DT.xgb.importance.panel", "DT.xgb.shap.panel",
      "DT.xgb.shap.interactions.panel")
  )
  expect_true(is.data.table(res_with_importance$DT.oos.pred.panel))
  expect_true(is.data.table(res_with_importance$DT.xgb.importance.panel))
  expect_true(is.data.table(res_with_importance$DT.xgb.shap.panel))
  expect_true(is.data.table(res_with_importance$DT.xgb.shap.interactions.panel))
  
  expect_equal(
    names(res_with_importance$DT.oos.pred.panel),
    c("GEOID", "index", "hp.target", "lu_ml_xgboost")
  )
  expect_equal(
    names(res_with_importance$DT.xgb.importance.panel),
    c("index", "Feature", "Gain")
  )
  expect_equal(
    names(res_with_importance$DT.xgb.shap.panel), 
    c("GEOID", "index", "feature", "shap_value")
  )
  expect_equal(
    names(res_with_importance$DT.xgb.shap.interactions.panel), 
    c("GEOID", "index", "feature1", "feature2", "shap_value")
  )

  expect_true(
    cor(
      res_with_importance$DT.oos.pred.panel[, lu_ml_xgboost], res$lu_ml_xgboost
    ) > 0.99,
    all(
      round(res_with_importance$DT.oos.pred.panel[, lu_ml_xgboost], 3)
      ==
      round(res$lu_ml_xgboost, 3)
    )
  )


  ## Make sure that the SHAP values for the main features sum to the prediction
  dt_shap_total_predictions <- res_with_importance$DT.xgb.shap.panel %>%
    .[, .(shap_total = sum(shap_value)), by = .(GEOID, index)] %>%
    merge(
      res_with_importance$DT.oos.pred.panel[, .(GEOID, index, lu_ml_xgboost)],
      by = c("GEOID", "index")
    )

  expect_true(
    dt_shap_total_predictions[, cor(shap_total, lu_ml_xgboost)] > 0.999,
    dt_shap_total_predictions[, all(round(shap_total, 3) == round(lu_ml_xgboost, 3))]
  )

  ## Make sure that the SHAP interaction values sum to the prediction
  dt_shap_interactions_total_predictions <-
    res_with_importance$DT.xgb.shap.interactions.panel %>%
    .[, .(shap_interaction_total = sum(shap_value)), keyby = .(GEOID, index)] %>%
    merge(
      res_with_importance$DT.oos.pred.panel[, .(GEOID, index, lu_ml_xgboost)],
      by = c("GEOID", "index")
    )

  expect_true(
    dt_shap_interactions_total_predictions %>%
      .[, cor(shap_interaction_total, lu_ml_xgboost)] > 0.999,
    dt_shap_interactions_total_predictions %>%
      .[, all(round(shap_interaction_total, 3) == round(lu_ml_xgboost, 3))]
  )

})
