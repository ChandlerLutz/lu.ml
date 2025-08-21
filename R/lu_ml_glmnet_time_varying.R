#' Time-Varying GLMNet Model for Land Use and House Price Prediction
#'
#' This function trains a penalized regression model (Ridge, Lasso, or Elastic Net)
#' using `glmnet` to predict house prices from time-varying land use data.
#' It performs repeated k-fold cross-validation to assess model performance
#' and can optionally compute predictions based on specific land use components
#' (parts, total, slope, water, wetlands).
#'
#' @param DT.hp A `data.table` containing house price data with columns `GEOID`,
#'   `index` (Date), and `hp.target` (house price target variable).
#' @param DT.lu A `data.table` containing land use data with `GEOID` as the first
#'   column and other columns representing land use features.
#' @param repeats An integer specifying the number of times to repeat the k-fold
#'   cross-validation. Defaults to 5.
#' @param folds An integer specifying the number of folds for cross-validation.
#'   Defaults to 5.
#' @param compute.lu.ml.parts A logical value indicating whether to compute
#'   predictions for specific land use components. Defaults to FALSE.
#' @param penalty.type A character string indicating which penalized regression model
#'   to use. Options are `"ridge"` (alpha = 0), `"lasso"` (alpha = 1), or `"elasticnet"`.
#'   If `"elasticnet"` is chosen, the function performs a grid search over `alpha`
#'   in `seq(0.1, 0.9, 0.1)` and selects the best alpha based on cross-validated MSE.
#' @param seed An integer seed for reproducibility of random sampling and cross-validation
#'   folds. Defaults to 123.
#'
#' @return A `data.table` containing the original house price data (`DT.hp`) merged
#'   with out-of-sample predictions from the fitted penalized regression model. If
#'   `compute.lu.ml.parts` is TRUE, additional columns are included with predictions
#'   based on specific land use components:
#'   \itemize{
#'     \item `lu_ml_parts` (slope, water, wetlands)
#'     \item `lu_ml_total` (total land unavailability measures)
#'     \item `lu_ml_slope`, `lu_ml_water`, `lu_ml_wetlands` (individual parts)
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Orders house price and land use data by GEOID and index.
#'   \item Ensures the data has consistent GEOIDs and no missing values in the target.
#'   \item Removes `saiz.circle` variables from the land use data.
#'   \item Sets up repeated k-fold cross-validation across GEOIDs.
#'   \item For each time index, trains a GLMNet model on the training folds and
#'     predicts on the test folds. If `penalty.type = "elasticnet"`, alpha is
#'     chosen via cross-validation across a grid.
#'   \item Optionally computes predictions using subsets of land use features.
#'   \item Aggregates out-of-sample predictions and merges them back with `DT.hp`.
#' }
#'
#' All parallel computation is managed via the `future` and `future.apply` packages.
#' Parallelization occurs at the cross-validation level (folds × repeats × time index).
#'
#' @import data.table
#' @import glmnet
#' @import future
#' @import future.apply
#' @importFrom stats predict var model.matrix
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Load house price and land use data
#' data(dt_mian_sufi_2014)
#' data(dt_cnty_lu_2010)
#'
#' dt_mian_sufi_2014 <- dt_mian_sufi_2014[
#'   , .(GEOID = fips, index = as.Date("2002-01-01"),
#'       hp.target = house.net.worth)][!is.na(hp.target)]
#'
#' dt_cnty_lu_2010 <- dt_cnty_lu_2010[
#'   , .(GEOID, unavailable = .SD),
#'   .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]
#'
#' # Run the model using Ridge regression
#' out <- lu_ml_glmnet_time_varying(
#'   DT.hp = dt_mian_sufi_2014,
#'   DT.lu = dt_cnty_lu_2010,
#'   penalty.type = "ridge",
#'   seed = 42
#' )
#'
#' # Correlation between predicted and actual house prices
#' cor(out$hp.target, out$lu_ml_ridge)
#' }
#'
#' @export
lu_ml_glmnet_time_varying <- function(DT.hp, DT.lu,
                                      repeats = 5,
                                      folds = 5,
                                      compute.lu.ml.parts = FALSE,
                                      penalty.type = c("ridge", "lasso", "elasticnet"),
                                      seed = 123) {

  penalty.type <- match.arg(penalty.type)

  . <- GEOID <- index <- hp.target <- test.geoids <- fold_id <- NULL
  repeat.id <- fold.id <- task.seed <- lu.best.glmnet <- N <- .N <- NULL

  DT.hp <- DT.hp[order(GEOID, index)]
  DT.lu <- DT.lu[order(GEOID)]

  if (DT.lu[, .N, by = GEOID][, any(N > 1)]) {
    stop("Error: In lu_ml_xgboost_time_varying(), DT.lu has duplicate GEOIDs.")
  }

  if (DT.hp[, sum(is.na(hp.target))] > 0)
    stop("Error: There are missing values in hp.target.")

  saiz.circle.vars <- grep("saiz.circle$", names(DT.lu), value = TRUE)
  if (length(saiz.circle.vars) > 0) DT.lu[, (saiz.circle.vars) := NULL]

  DT.hp <- DT.hp[GEOID %chin% DT.lu$GEOID]
  DT.lu <- DT.lu[GEOID %chin% DT.hp$GEOID]
  
  indices <- DT.hp[, unique(index)]

  ## <<< REMOVED >>> The global DT.est.base and fold creation block was removed from here.
  ## It is now located inside the f_get_glmnet_lu_predictions function.

  f_train_glmnet <- function(DT, test.geoids, train.seed) {

    train.geoids <- setdiff(unique(DT$GEOID), test.geoids)
    DT.train <- DT[GEOID %chin% train.geoids]
    DT.test  <- DT[GEOID %chin% test.geoids][order(GEOID, index)]

    x.train <- model.matrix(hp.target ~ . - GEOID - index, data = DT.train)
    y.train <- DT.train$hp.target

    if (penalty.type == "elasticnet") {
      alpha.grid <- seq(0.1, 0.9, by = 0.1)
      cv.results <- lapply(alpha.grid, function(a) {
        withr::with_seed(train.seed, {
          cv <- cv.glmnet(x.train, y.train, alpha = a)
          list(cv = cv, alpha = a, mse = min(cv$cvm))
        })
      })
      best <- cv.results[[which.min(sapply(cv.results, `[[`, "mse"))]]
      glmnet.mod <- best$cv
    } else {
      alpha.val <- ifelse(penalty.type == "ridge", 0, 1)
      glmnet.mod <- withr::with_seed(train.seed, {
        cv.glmnet(x.train, y.train, alpha = alpha.val)
      })
    }

    x.test <- model.matrix(hp.target ~ . - GEOID - index, data = DT.test)
    pred <- predict(glmnet.mod, s = "lambda.min", newx = x.test)

    DT.oos.pred <- DT.test[, .(GEOID, index)][, lu.best.glmnet := as.numeric(pred)]
    return(list(DT.oos.pred = DT.oos.pred))
  }

  f_get_glmnet_lu_predictions <- function(index_val) {
    cat("Processing index:", as.character(index_val), "\n")
    
    DT <- DT.hp[index == c(index_val)] %>% merge(DT.lu, by = "GEOID")

    ## <<< MODIFICATION START: Robust fold creation is now here >>>
    geoids.this.index <- DT[, unique(GEOID)]
    seed.offset <- DT.hp[, which(unique(index) == c(index_val))]

    DT.est <- expand.grid(repeat.id = 1:repeats, fold.id = 1:folds) %>%
      setDT() %>%
      .[order(repeat.id)] %>%
      .[, test.geoids := list()] %>%
      .[, task.seed := seed + repeat.id + fold.id + seed.offset]

    for (i in 1:repeats) {
      geoids.rand <- withr::with_seed(seed = seed + i + seed.offset, {
        sample(geoids.this.index, length(geoids.this.index))
      })
      
      dt_folds <- data.table(GEOID = geoids.rand)
      dt_folds[, fold_id := rep(1:folds, length.out = .N)] 
      
      test.ids.list <- dt_folds[, .(test_geoids = list(GEOID)), by = fold_id]$test_geoids
      
      DT.est[repeat.id == i, test.geoids := test.ids.list]
    }
    ## <<< MODIFICATION END >>>

    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.all <- future.apply::future_Map(f_train_glmnet,
                                              DT = list(DT),
                                              test.geoids = DT.est$test.geoids,
                                              train.seed = DT.est$task.seed)
    future::plan(future::sequential())

    DT.oos.pred.all <- rbindlist(lapply(list.pred.all, `[[`, "DT.oos.pred")) %>%
      .[, .(lu_ml = mean(lu.best.glmnet)), by = .(GEOID, index)]

    if (!compute.lu.ml.parts) return(DT.oos.pred.all)

    get_cols_and_predict <- function(pattern, name) {
      cols <- grep(pattern, names(DT), value = TRUE)
      list.pred <- future.apply::future_Map(
        f_train_glmnet,
        DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", cols)]),
        test.geoids = DT.est$test.geoids,
        future.seed = seed
      )
      rbindlist(lapply(list.pred, `[[`, "DT.oos.pred")) %>%
        .[, .(mean_pred = mean(lu.best.glmnet)), by = .(GEOID, index)] %>%
        setnames("mean_pred", name)
    }

    parts    <- get_cols_and_predict("^slope|^water|^wetlands", "lu_ml_parts")
    total    <- get_cols_and_predict("^total", "lu_ml_total")
    slope    <- get_cols_and_predict("^slope", "lu_ml_slope")
    water    <- get_cols_and_predict("^water", "lu_ml_water")
    wetlands <- get_cols_and_predict("^wetlands", "lu_ml_wetlands")

    Reduce(function(...) merge(..., by = c("GEO.ID", "index")),
           list(DT.oos.pred.all, parts, total, slope, water, wetlands))
  }

  DT.oos.pred.panel <- rbindlist(lapply(indices, f_get_glmnet_lu_predictions))
  DT.oos.pred.panel <- merge(DT.hp, DT.oos.pred.panel, by = c("GEOID", "index"))

  DT.oos.pred.panel <- setnames(DT.oos.pred.panel, "lu_ml",
                                paste0("lu_ml_", penalty.type))
  
  return(DT.oos.pred.panel)
}
