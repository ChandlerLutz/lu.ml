test_that("lu_ml_xgboost_time_varying_spatial_sampled() works for Mian and Sufi Data", {

  data(dt_mian_sufi_2014)
  data(dt_cnty_lu_2010)

  # Setup Data with stfp (State FIPS) included
  dt_mian_sufi_2014 <- dt_mian_sufi_2014 %>%
    .[, .(GEOID = fips, index = as.Date("2002-01-01"), stfp = substr(fips, 1, 2), 
          hp.target = house.net.worth)] %>%
    .[!is.na(hp.target)]

  dt_cnty_lu_2010 <- dt_cnty_lu_2010 %>%
    .[, .(GEOID, unavailable = .SD),
      .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

  ## -- Test where the spatial sampling is GEOID (equivalent to random CV) -- ##
  res_geoid <- lu_ml_xgboost_time_varying_spatial_sampled(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010, spatial_block_col = "GEOID"
  )

  expect_true(is.data.table(res_geoid))
  expect_equal(names(res_geoid), c("GEOID", "index", "stfp", "hp.target",
                                   "lu_ml_xgboost"))
  
  ## Round to just 1 decimal place as results depend somewhat on the xgboost version
  ## Should match the original ~0.7 correlation
  expect_equal(round(res_geoid[, cor(hp.target, lu_ml_xgboost)], 1), 0.7)

  ## -- Test with true spatial sampling using `stfp` (State) -- ##
  res_stfp <- lu_ml_xgboost_time_varying_spatial_sampled(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010, spatial_block_col = "stfp"
  )

  expect_true(is.data.table(res_stfp))
  expect_equal(names(res_stfp), c("GEOID", "index", "stfp", "hp.target",
                                  "lu_ml_xgboost"))
  
  # Ensure valid predictions are generated
  expect_true(res_stfp[, is.numeric(lu_ml_xgboost)])

  ## Test for reproducibility (Ensure predictions are identical on re-run)
  res_stfp_run2 <- lu_ml_xgboost_time_varying_spatial_sampled(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010, spatial_block_col = "stfp"
  )
  expect_equal(res_stfp$lu_ml_xgboost, res_stfp_run2$lu_ml_xgboost)

  ## -- Test importance = TRUE with spatial sampling -- ##
  res_with_importance <- lu_ml_xgboost_time_varying_spatial_sampled(
    DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010, spatial_block_col = "stfp", importance = TRUE
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
    c("GEOID", "index", "stfp", "hp.target", "lu_ml_xgboost")
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

  # Check that importance predictions map exactly to the standard predictions
  expect_true(
    cor(
      res_with_importance$DT.oos.pred.panel[, lu_ml_xgboost], res_stfp$lu_ml_xgboost
    ) > 0.99,
    all(
      round(res_with_importance$DT.oos.pred.panel[, lu_ml_xgboost], 3)
      ==
      round(res_stfp$lu_ml_xgboost, 3)
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
