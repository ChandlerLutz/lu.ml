#' Perform XGBoost Prediction with Fixed Inference Groups
#'
#' @description
#' This function trains an XGBoost model and generates out-of-sample (OOS)
#' predictions for specified groups of observations. The process is repeated
#' for each unique time point in the dataset. It uses a leave-group-out
#' cross-validation strategy where, for each time point, one group is held out
#' for inference while the remaining groups are used for training.
#'
#' @details
#' The function operates independently for each time period specified in the `index`
#' column of `DT.hp`. For each period, it performs the following steps:
#'
#' 1.  **Grouping**: It uses the `inference.grp` column to define distinct groups of
#'     `GEOID`s.
#' 2.  **Leave-Group-Out CV**: For each group, it holds out all associated `GEOID`s
#'     as the inference set. The data from all other groups becomes the training set.
#' 3.  **Hyperparameter Tuning**: The training set is further split into a sub-training
#'     set (75%) and a validation set (25%) to find the optimal number of boosting
#'     rounds (`nrounds`). This is achieved using XGBoost's early stopping mechanism
#'     (with `early_stopping_rounds = 25`) to prevent overfitting.
#' 4.  **Model Training**: A final XGBoost model is trained on the entire training
#'     set using the optimal `nrounds` determined in the previous step.
#' 5.  **Prediction**: The trained model is used to generate predictions for the
#'     held-out inference set.
#' 6.  **Parallel Execution**: This entire process is parallelized across the
#'     different inference groups for each time period using the `future` framework.
#'
#' The final output is a `data.table` that merges these OOS predictions back with
#' the original input data. Note that the function automatically identifies predictor
#' variables in `DT.lu` by searching for column names containing the pattern
#' `"unavailable"`. It also removes any columns with the suffix `saiz.circle`
#' before training.
#'
#' @param DT.hp A `data.table` containing the target variable and grouping information.
#'   It must include the following columns:
#'   \itemize{
#'     \item `GEOID`: A unique identifier for each location.
#'     \item `index`: A `Date` object representing the time period.
#'     \item `inference.grp`: A character vector defining the cross-validation group
#'           for each `GEOID`.
#'     \item `hp.target`: A numeric vector representing the target variable for the model.
#'   }
#' @param DT.lu A `data.table` containing time-invariant features for each `GEOID`.
#'   It must have a `GEOID` column for merging with `DT.hp`. Feature columns to be
#'   used in the model must contain the pattern `"unavailable"` in their names.
#' @param seed An integer used to set the seed for reproducibility. Defaults to `123`.
#'
#' @return A `data.table` that includes the original columns of `DT.hp` plus a
#'   new column, `lu_ml_xgboost`, which contains the out-of-sample XGBoost predictions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # --- 0. Load sample datasets ---
#' # These would typically be loaded from your data package.
#' data(dt_mian_sufi_2014)
#' data(dt_cnty_lu_2010)
#'
#' # --- 1. Prepare the house price data (DT.hp) ---
#' # Select relevant columns, rename them, and handle missing values.
#' DT.hp <- dt_mian_sufi_2014[, .(
#'   GEOID = fips,
#'   index = as.Date("2002-01-01"),
#'   hp.target = house.net.worth
#' )][!is.na(hp.target)]
#'
#' # Add the required 'inference.grp' column. For this example, we'll use
#' # each GEOID as its own group, effectively performing leave-one-out CV.
#' DT.hp[, inference.grp := as.character(GEOID)]
#' setcolorder(DT.hp, c("GEOID", "index", "inference.grp", "hp.target"))
#'
#'
#' # --- 2. Prepare the land use feature data (DT.lu) ---
#' # The function automatically selects columns with "unavailable" in the name.
#' # We subset the data to keep only GEOID and those feature columns.
#' feature_cols <- grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)
#' DT.lu <- dt_cnty_lu_2010[, .SD, .SDcols = c("GEOID", feature_cols)]
#'
#'
#' # --- 3. Run the function ---
#' predictions <- lu_ml_xgboost_fixed_inference_set(
#'   DT.hp = DT.hp,
#'   DT.lu = DT.lu,
#'   seed = 123
#' )
#'
#' # --- 4. View and verify the results ---
#' print(head(predictions))
#' }
lu_ml_xgboost_fixed_inference_set <- function(DT.hp, DT.lu, seed = 123) {

  ## For R cmd check
  . <- GEOID <- index <- hp.target <- test.geoids <- inference.grp <- NULL
  repeat.id <- fold.id <- task.seed <- lu.best.xgboost <- N <- .N <- NULL
  
  DT.hp <- DT.hp %>%
    .[order(GEOID, index)]

  DT.lu <- DT.lu %>%
    .[order(GEOID)]

  if (DT.lu[, .N, by = GEOID][, any(N > 1)]) {
    stop("Error: In lu_ml_xgboost_time_varying(), DT.lu has duplicate GEOIDs.")
  }

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
    names(DT.hp) == c("GEOID", "index", "inference.grp", "hp.target"),
    "DT.hp does not have column names 'GEOID', 'index', 'inference.grp','hp.target'"
  )
  f_stopifnot(DT.hp[, class(index)] == "Date",
              "DT.hp$index must be of type `Date`")
  f_stopifnot(DT.hp[, class(inference.grp)] == "character",
              "DT.hp$inference.grp must be of type `character`")
  f_stopifnot(names(DT.lu)[1] == "GEOID")
  f_stopifnot(setdiff(unique(DT.hp$GEOID), DT.lu$GEOID) == character(0))
  num.obs.by.geoid <- DT.hp %>% .[complete.cases(.)] %>%
    .[, .N, by = GEOID] %>% .[, N]
  if (var(num.obs.by.geoid) != 0) {
    print("Warning: In `lu_ml_xgboost_time_varying()`, the `DT.hp` is not a balanced panel.")
  }

  DT.hp <- DT.hp %>%
    .[order(GEOID, index)]

  DT.lu <- DT.lu %>%
    .[order(GEOID)]
  
  indices <- DT.hp[, unique(index)]

  ## <<< REMOVED >>> The global DT.est.base creation was removed from here.
  
  f_train_xgboost <- function(DT, inference.geoids, train.seed) {
    data.table::setDTthreads(1)
    xgboost_ntrheads <- 1

    geoids <- DT[, unique(GEOID)]

    train.geoids <- geoids %>% .[!(. %chin% inference.geoids)]
    tune.nrounds.geoids <- withr::with_seed(seed = train.seed, {
      sample(train.geoids, size = floor(length(train.geoids) * 0.75))
    })
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
    withr::with_seed(seed = train.seed, {
      clf <- xgb.train(
        data = dtune, nrounds = 500, watchlist = watchlist,
        maximize = FALSE, early_stopping_rounds = 25,
        objective = "reg:squarederror", verbose = FALSE, nthread = xgboost_ntrheads
      )
    })

    ## -- Train -- ##
    DT.train <- DT[GEOID %chin% c(train.geoids)]
    dtrain <- f_get_xgboost_dmat(DT.train)
    withr::with_seed(seed = train.seed, {
      xgboost.mod <- xgboost(
        data = dtrain, nrounds = clf$best_iteration,
        objective = "reg:squarederror", verbose = FALSE, nthread = xgboost_ntrheads
      )
    })

    ## -- Predict -- ##
    DT.inference <- DT[GEOID %chin% c(inference.geoids)] %>%
      .[order(GEOID, index)]
    dinference <- f_get_xgboost_dmat(DT.inference)
    xgboost.pred <- predict(xgboost.mod, dinference)
    DT.oos.pred <- DT.inference[, .(GEOID, index)] %>%
      .[, lu.best.xgboost := xgboost.pred]

    return(list(DT.oos.pred = DT.oos.pred))
  }

  f_get_xgboost_lu_predictions <- function(index_val) {
    print(index_val)
    index.tmp <- index_val
    
    DT <- DT.hp[index == index.tmp] %>%
      merge(DT.lu, by = "GEOID")

    ## <<< MODIFICATION START: Inference sets are now created here for each time index >>>
    seed.offset <- DT.hp[, which(unique(index) == c(index_val))]
    
    # Create inference sets based on the groups present in THIS time period's data
    DT.est <- DT[, .(inference.geoids = list(unique(GEOID))), keyby = .(inference.grp)] %>%
      .[, task.seed := seed + seq_len(.N) + seed.offset]
    ## <<< MODIFICATION END >>>

    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.all <- future.apply::future_Map(f_train_xgboost,
                                              DT = list(DT),
                                              inference.geoids = DT.est$inference.geoids,
                                              train.seed = DT.est$task.seed,
                                              future.seed = TRUE)
    future::plan(future::sequential())
    
    DT.oos.pred.all <- lapply(list.pred.all, `[[`, "DT.oos.pred") %>% rbindlist %>%
      .[, .(lu_ml_xgboost = mean(lu.best.xgboost)), by = .(GEOID, index)]

    return(DT.oos.pred.all)
  }

  DT.oos.pred.panel <- lapply(indices, f_get_xgboost_lu_predictions) %>%
    rbindlist

  DT.oos.pred.panel <- merge(DT.hp, DT.oos.pred.panel, by = c("GEOID", "index"))

  return(DT.oos.pred.panel)
}
