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

})
