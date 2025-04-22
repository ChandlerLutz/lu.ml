test_that("lu_ml_superpc_time_varying() works for Supervised PCA with Mian and Sufi data", {

  data(dt_mian_sufi_2014)
  data(dt_cnty_lu_2010)

  dt_mian_sufi_2014 <- dt_mian_sufi_2014 %>%
    .[, .(GEOID = fips, index = as.Date("2002-01-01"), hp.target = house.net.worth)] %>%
    .[!is.na(hp.target)]

  dt_cnty_lu_2010 <- dt_cnty_lu_2010 %>%
    .[, .(GEOID, unavailable = .SD),
      .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

  result <- lu_ml_superpc_time_varying(
    DT.hp = dt_mian_sufi_2014,
    DT.lu = dt_cnty_lu_2010
  )

  expect_true(is.data.table(result))
  expect_equal(names(result), c("GEOID", "index", "hp.target", "lu_ml_pc1"))
  expect_equal(
    result[, cor(hp.target, lu_ml_pc1)] %>% round(2),
    0.28
  )

})
