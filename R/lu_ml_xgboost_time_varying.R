#' Time-Varying XGBoost Model for Land Use and House Price Prediction
#'
#' This function trains an XGBoost model to predict house prices using time-varying
#' land use data. It performs repeated k-fold cross-validation to assess model
#' performance and can optionally compute predictions based on specific land use
#' components (parts, total, slope, water, wetlands).
#'
#' @param DT.hp A data.table containing house price data with columns 'GEOID',
#'   'index' (Date), and 'hp.target' (house price target variable).
#' @param DT.lu A data.table containing land use data with 'GEOID' as the first
#'   column and other columns representing land use features.
#' @param repeats An integer specifying the number of times to repeat the k-fold
#'   cross-validation. Defaults to 5.
#' @param folds An integer specifying the number of folds for cross-validation.
#'   Defaults to 5.
#' @param compute.lu.ml.parts A logical value indicating whether to compute
#'   predictions for specific land use components. Defaults to FALSE.
#'
#' @return A data.table containing the original house price data (`DT.hp`) merged
#'   with out-of-sample predictions from the XGBoost model. If
#'   `compute.lu.ml.parts` is TRUE, it also includes predictions for land use
#'   components (parts, total, slope, water, wetlands).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Orders the house price data by GEOID and index.
#'   \item Checks for missing values in the house price target.
#'   \item Removes 'saiz.circle' variables from the land use data.
#'   \item Subsets both data.tables to ensure they have the same GEOIDs.
#'   \item Performs various data integrity checks.
#'   \item Sets up repeated k-fold cross-validation.
#'   \item For each time index, trains an XGBoost model on the training folds and
#'     predicts on the test folds.
#'   \item Optionally, computes predictions for specific land use components.
#'   \item Merges the predictions with the original house price data.
#' }
#'
#' The function uses early stopping during XGBoost training to prevent overfitting.
#' It also utilizes parallel processing via the `future` package to speed up
#' computation.
#'
#' @import data.table
#' @import xgboost
#' @import future
#' @import future.apply
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases predict var
#'
#' @examples
#' library(data.table)
#' library(lu.ml)
#'
#' # Load the Mian-Sufi 2014 data
#' data(dt_mian_sufi_2014)
#'
#' # Load the county-level land unavailability data for 2010
#' data(dt_cnty_lu_2010)
#'
#' # Prepare the Mian-Sufi data: select relevant columns, rename, and remove NAs
#' dt_mian_sufi_2014 <- dt_mian_sufi_2014 |>
#'   _[, .(GEOID = fips, index = as.Date("2002-01-01"),
#'         hp.target = house.net.worth)] |>
#'   _[!is.na(hp.target)]
#'
#' # Prepare the land unavailability data: select GEOID and land unavailability columns
#' dt_cnty_lu_2010 <- dt_cnty_lu_2010 |>
#'   _[, .(GEOID, unavailable = .SD),
#'     .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]
#'
#' # Run the lu_ml_xgboost_time_varying function to generate an instrument
#' out <- lu_ml_xgboost_time_varying(DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010)
#'
#' # Calculate and print the correlation between the house price target and the generated instrument
#' print(paste("Correlation:", cor(out[, hp.target], out[, lu_ml])))
#'
#' # Print the resulting data.table
#' print(out)
#'
#' @export
lu_ml_xgboost_time_varying <- function(DT.hp, DT.lu, repeats = 5, folds = 5,
                                       compute.lu.ml.parts = FALSE) {

  ## For R cmd check
  . <- GEOID <- index <- hp.target <- test.geoids <- NULL
  repeat.id <- fold.id <- lu.best.xgboost <- N <- .N <- NULL
  
  DT.hp <- DT.hp %>%
    .[order(GEOID, index)]

  if (DT.hp[, sum(is.na(hp.target))] > 0) {
    stop("Error: In lu_ml_xgboost_time_varying(), there are missing values in hp.target.")
  }

  ## delete the saiz circle vars as they are the same as the 50k
  ## km circle around the central city centroid
  saiz.circle.vars <- names(DT.lu) %>% .[grepl("saiz.circle$", x = .)]
  
  if (length(saiz.circle.vars) > 0)
    DT.lu <- DT.lu[, c(saiz.circle.vars) := NULL]

  DT.hp <- DT.hp[GEOID %chin% c(DT.lu$GEOID)]
  DT.lu <- DT.lu[GEOID %chin% c(DT.hp$GEOID)]

  ## Checks
  f_stopifnot(
    names(DT.hp) == c("GEOID", "index", "hp.target"),
    "DT.hp does not have column names 'GEOID', 'index', 'hp.target'"
  )
  f_stopifnot(DT.hp[, class(index)] == "Date",
              "DT.hp$index must be of type `Date`")
  f_stopifnot(names(DT.lu)[1] == "GEOID")
  f_stopifnot(setdiff(unique(DT.hp$GEOID), DT.lu$GEOID) == character(0))
  num.obs.by.geoid <- DT.hp %>% .[complete.cases(.)] %>%
    .[, .N, by = GEOID] %>% .[, N]
  if (var(num.obs.by.geoid) != 0) {
    print("Warning: In `lu_ml_xgboost_time_varying()`, the `DT.hp` is not a balanced panel.")
  }
  
  geoids <- DT.hp[, unique(GEOID)]

  fold.size <- floor(length(geoids) / folds)
  
  DT.panel.out <- DT.hp[, .(GEOID, index)]

  indices <- DT.hp[, unique(index)]

  DT.est.base <- expand.grid(
    repeat.id = 1:repeats, 
    fold.id = 1:folds
  ) %>% setDT() %>%
    .[order(repeat.id)] %>%
    .[, test.geoids := list()]

  for (i in 1:repeats) {
    geoids.rand <- sample(geoids, length(geoids))
    test.ids.list <- chunk2(geoids.rand, folds)
    for (j in 1:folds) {
      test.ids <- test.ids.list[[j]]
      DT.est.base <- DT.est.base %>%
        .[repeat.id == i & fold.id == j, test.geoids := test.ids]
    }
  }

  f_train_xgboost <- function(DT, test.geoids) {

    ## For early stopping, see
    ## https://codingwiththomas.blogspot.com/2016/03/xgboost-validation-and-early-stopping.html
    ## https://mljar.com/blog/xgboost-early-stopping/#:~:text=Early%20stopping%20is%20a%20technique,loss%20monitoring%20and%20early%20stopping.
    ## https://machinelearningmastery.com/avoid-overfitting-by-early-stopping-with-xgboost-in-python/#:~:text=XGBoost%20supports%20early%20stopping%20after,which%20no%20improvement%20is%20observed.

    data.table::setDTthreads(1)
    xgboost_ntrheads <- 1

    geoids <- DT[, unique(GEOID)]

    train.geoids <- geoids %>% .[!(. %chin% test.geoids)]
    tune.nrounds.geoids <- sample(train.geoids,
                                  size = floor(length(train.geoids) * 0.75))
    validation.nrounds.geoids <- train.geoids %>% .[!(. %chin% tune.nrounds.geoids)]

    f_get_xgboost_dmat <- function(DT) {
      data <- as.matrix(DT[, .SD, .SDcols = patterns("unavailable")])
      label <- DT[, hp.target]
      xgb.DMatrix(data = data, label = label)
    }

    ## -- Tune nrounds -- ##
    
    DT.tune <- DT[GEOID %chin% c(tune.nrounds.geoids)]
    DT.validation <- DT[GEOID %chin% c(validation.nrounds.geoids)]

    dtune <- f_get_xgboost_dmat(DT.tune)
    dvalidation <- f_get_xgboost_dmat(DT.validation)

    watchlist <- list(train=dtune, eval=dvalidation)

    clf <- xgb.train(
      data = dtune, 
      nrounds = 500, 
      watchlist = watchlist,
      maximize = FALSE,
      early_stopping_rounds = 25,
      objective = "reg:squarederror", ## the objective function
      verbose = FALSE,
      nthread = xgboost_ntrheads
    )

    ## -- Train -- ##

    DT.train <- DT[GEOID %chin% c(train.geoids)]
    
    dtrain <- f_get_xgboost_dmat(DT.train)

    xgboost.mod <- xgboost(
      data = dtrain, ## the data   
      nrounds = clf$best_iteration, ## max number of boosting iterations
      objective = "reg:squarederror", ## the objective function
      verbose = FALSE,
      nthread = xgboost_ntrheads
    )

    ## -- Predict -- ##

    DT.test <- DT[GEOID %chin% c(test.geoids)] %>%
      .[order(GEOID, index)]

    dtest <- f_get_xgboost_dmat(DT.test)

    xgboost.pred <- predict(xgboost.mod, dtest)

    DT.oos.pred <- DT.test[, .(GEOID, index)] %>%
      .[, lu.best.xgboost := xgboost.pred]

    return(list(DT.oos.pred = DT.oos.pred))

  }

  f_get_xgboost_lu_predictions <- function(index) {

    print(index)

    index.tmp <- index
    DT.est <- copy(DT.est.base)
    
    DT <- DT.hp[index == index.tmp] %>%
      merge(DT.lu, by = "GEOID")

    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.all <- future.apply::future_Map(f_train_xgboost,
                                              DT = list(DT),
                                              test.geoids = DT.est$test.geoids,
                                              future.seed = TRUE)
    future::plan(future::sequential())

    ## list.pred.all <- Map(f_train_xgboost,
    ##                      DT = list(DT),
    ##                      test.geoids = DT.est$test.geoids)
    
    DT.oos.pred.all <- lapply(list.pred.all, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml = mean(lu.best.xgboost)), by = .(GEOID, index)]

    if (compute.lu.ml.parts == FALSE)
      return(DT.oos.pred.all)

    ##Parts
    parts.cols <- names(DT) %>% .[grepl("^slope|^water|^wetlands", x = .)]
    ## list.pred.parts <- Map(
    ##   f_train_xgboost,
    ##   DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", parts.cols)]),
    ##   test.geoids = DT.est$test.geoids
    ## )
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.parts <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", parts.cols)]),
      test.geoids = DT.est$test.geoids,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.parts <- lapply(list.pred.parts, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_parts = mean(lu.best.xgboost)), by = .(GEOID, index)]


    ## Total 
    total.cols <- names(DT) %>% .[grepl("^total", x = .)]
    ## list.pred.total <- Map(
    ##   f_train_xgboost,
    ##   DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", total.cols)]),
    ##   test.geoids = DT.est$test.geoids
    ## )
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.total <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", total.cols)]),
      test.geoids = DT.est$test.geoids,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.total <- lapply(list.pred.total, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_total = mean(lu.best.xgboost)), by = .(GEOID, index)]
    
    ## Slope 
    slope.cols <- names(DT) %>% .[grepl("^slope", x = .)]
    ## list.pred.slope <- Map(
    ##   f_train_xgboost,
    ##   DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", slope.cols)]),
    ##   test.geoids = DT.est$test.geoids
    ## )
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.slope <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", slope.cols)]),
      test.geoids = DT.est$test.geoids,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.slope <- lapply(list.pred.slope, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_slope = mean(lu.best.xgboost)), by = .(GEOID, index)]

    ## Water 
    water.cols <- names(DT) %>% .[grepl("^water", x = .)]
    ## list.pred.water <- Map(
    ##   f_train_xgboost,
    ##   DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", water.cols)]),
    ##   test.geoids = DT.est$test.geoids
    ## )
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.water <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", water.cols)]),
      test.geoids = DT.est$test.geoids,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.water <- lapply(list.pred.water, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_water = mean(lu.best.xgboost)), by = .(GEOID, index)]

    ## Wetlands 
    wetlands.cols <- names(DT) %>% .[grepl("^wetlands", x = .)]
    ## list.pred.wetlands <- Map(
    ##   f_train_xgboost,
    ##   DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", wetlands.cols)]),
    ##   test.geoids = DT.est$test.geoids
    ## )
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.wetlands <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", wetlands.cols)]),
      test.geoids = DT.est$test.geoids,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.wetlands <- lapply(list.pred.wetlands, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_wetlands = mean(lu.best.xgboost)), by = .(GEOID, index)]

    DT.oos.pred <- merge(
      DT.oos.pred.all, DT.oos.pred.parts, by = c("GEOID", "index")
    ) %>%
      merge(DT.oos.pred.total, by = c("GEOID", "index")) %>%
      merge(DT.oos.pred.slope, by = c("GEOID", "index")) %>%
      merge(DT.oos.pred.water, by = c("GEOID", "index")) %>%
      merge(DT.oos.pred.wetlands, by = c("GEOID", "index"))

    return(DT.oos.pred)

  }

  DT.oos.pred.panel <- lapply(indices, f_get_xgboost_lu_predictions) %>%
    rbindlist

  DT.oos.pred.panel <- merge(DT.hp, DT.oos.pred.panel, by = c("GEOID", "index"))

  return(DT.oos.pred.panel)

}
