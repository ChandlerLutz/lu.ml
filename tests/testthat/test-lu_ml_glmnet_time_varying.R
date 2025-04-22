test_that("lu_ml_glmnet_time_varying() works for ridge regression with Mian and Sufi Data", {

  data(dt_mian_sufi_2014)
  data(dt_cnty_lu_2010)

  dt_mian_sufi_2014 <- dt_mian_sufi_2014 %>%
    .[, .(GEOID = fips, index = as.Date("2002-01-01"), hp.target = house.net.worth)] %>%
    .[!is.na(hp.target)]

  dt_cnty_lu_2010 <- dt_cnty_lu_2010 %>%
    .[, .(GEOID, unavailable = .SD),
      .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

  unavail_cols <- grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)
  
  dt_cnty_lu_2010 <- dt_cnty_lu_2010

  res <- lu_ml_glmnet_time_varying(DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010,
                                   penalty.type = "ridge") 

  expect_equal(names(res), c("GEOID", "index", "hp.target", "lu_ml_ridge"))
  expect_equal(round(res[, cor(hp.target, lu_ml_ridge)], 2), 0.34)

})

test_that("lu_ml_glmnet_time_varying() works for lasso regression with Mian and Sufi Data", {

  data(dt_mian_sufi_2014)
  data(dt_cnty_lu_2010)

  dt_mian_sufi_2014 <- dt_mian_sufi_2014 %>%
    .[, .(GEOID = fips, index = as.Date("2002-01-01"), hp.target = house.net.worth)] %>%
    .[!is.na(hp.target)]

  dt_cnty_lu_2010 <- dt_cnty_lu_2010 %>%
    .[, .(GEOID, unavailable = .SD),
      .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

  unavail_cols <- grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)
  
  dt_cnty_lu_2010 <- dt_cnty_lu_2010

  res <- lu_ml_glmnet_time_varying(DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010,
                                   penalty.type = "lasso")

  expect_equal(names(res), c("GEOID", "index", "hp.target", "lu_ml_lasso"))
  expect_equal(round(res[, cor(hp.target, lu_ml_lasso)], 2), 0.37)

})


test_that("lu_ml_glmnet_time_varying() works for elastic net regression with Mian and Sufi Data", {

  data(dt_mian_sufi_2014)
  data(dt_cnty_lu_2010)

  dt_mian_sufi_2014 <- dt_mian_sufi_2014 %>%
    .[, .(GEOID = fips, index = as.Date("2002-01-01"), hp.target = house.net.worth)] %>%
    .[!is.na(hp.target)]

  dt_cnty_lu_2010 <- dt_cnty_lu_2010 %>%
    .[, .(GEOID, unavailable = .SD),
      .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

  unavail_cols <- grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)
  
  dt_cnty_lu_2010 <- dt_cnty_lu_2010

  res <- lu_ml_glmnet_time_varying(DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010,
                                   penalty.type = "elasticnet")

  expect_equal(names(res), c("GEOID", "index", "hp.target", "lu_ml_elasticnet"))
  expect_equal(round(res[, cor(hp.target, lu_ml_elasticnet)], 2), 0.37)

})
