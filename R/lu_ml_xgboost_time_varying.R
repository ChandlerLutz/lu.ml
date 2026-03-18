#' Time-Varying XGBoost Model for Land Use and House Price Prediction with Spatial Grouping
#'
#' This function trains an XGBoost model to predict house prices using time-varying
#' land use data. It performs repeated k-fold cross-validation—optionally grouped 
#' by a spatial variable—to assess model performance and prevent spatial data leakage.
#'
#' @param DT.hp A data.table containing house price data with columns 'GEOID',
#'   'index' (Date), and 'hp.target' (house price target variable). It must also 
#'   contain the column specified in `spatial.group`.
#' @param DT.lu A data.table containing land use data with 'GEOID' as the first
#'   column and other columns representing land use features (typically containing 
#'   the string "unavailable").
#' @param spatial.group A string specifying the column name in `DT.hp` to use for 
#'   spatial cross-validation. Defaults to `"GEOID"`. If set to a higher-level 
#'   geography (e.g., `"cz20"` for Commuting Zones or `"stfp"` for State), the 
#'   function performs **Grouped K-Fold Cross-Validation**, ensuring that no data 
#'   from a specific group is used to train the model for that group.
#' @param repeats An integer specifying the number of times to repeat the k-fold
#'   cross-validation. Defaults to 5.
#' @param folds An integer specifying the number of folds for cross-validation.
#'   Defaults to 5.
#' @param compute.lu.ml.parts A logical value indicating whether to compute
#'   predictions for specific land use components (slope, water, wetlands, etc.). 
#'   Defaults to FALSE.
#' @param seed An integer seed for reproducibility of random sampling and 
#'   cross-validation folds. Defaults to 123.
#' @param importance A logical value indicating whether to compute and return
#'   feature importance and SHAP values. Defaults to FALSE. If TRUE, the function
#'   returns a list containing out-of-sample predictions, feature importance,
#'   and SHAP values. Note: Cannot be TRUE if `compute.lu.ml.parts` is also TRUE.
#' @param run_in_parallel A logical value indicating whether this function should
#'   internally manage parallel processing via `future`. Defaults to FALSE. 
#'
#' @return If `importance = FALSE`, returns a `data.table` containing the original 
#'   house price data merged with out-of-sample predictions. The columns are 
#'   ordered: `GEOID`, `spatial.group` (if not GEOID), `index`, `hp.target`, 
#'   and `lu_ml_xgboost`.
#'
#'   If `importance = TRUE`, returns a list:
#'   \itemize{
#'     \item \code{DT.oos.pred.panel}: The prediction data.table described above.
#'     \item \code{DT.xgb.importance.panel}: Average feature importance (Gain) per time index.
#'     \item \code{DT.xgb.shap.panel}: Average SHAP values per GEOID, index, and feature.
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Identifies unique spatial units based on the `spatial.group` column.
#'   \item For each time index, it splits the data into K-folds based on those groups.
#'   \item For each fold, it identifies a "tuning" set (75% of training groups) and 
#'     a "validation" set (25% of training groups).
#'   \item Uses `xgb.train` with early stopping (25 rounds) based on the validation 
#'     set to determine the optimal number of iterations.
#'   \item Re-trains the model on the full training set (tuning + validation) and 
#'     predicts on the held-out spatial groups.
#'   \item Aggregates predictions across all repeats and folds.
#' }
#'
#' By using a `spatial.group` broader than `GEOID`, the function ensures the model 
#' learns structural relationships that generalize across regions, mitigating 
#' concerns about spatial autocorrelation and data leakage.
#'
#' @import data.table
#' @import xgboost
#' @import future
#' @import future.apply
#' @importFrom stats predict complete.cases var
#'
#' @examples
#' \dontrun{
#' # Standard GEOID-level CV
#' res <- lu_ml_xgboost_time_varying(DT.hp = dt_hp, DT.lu = dt_lu)
#'
#' # Spatial Grouped CV (using Commuting Zones)
#' res_cz <- lu_ml_xgboost_time_varying(
#'   DT.hp = dt_hp, 
#'   DT.lu = dt_lu, 
#'   spatial.group = "cz20"
#' )
#' }
#' @export
lu_ml_xgboost_time_varying <- function(DT.hp, DT.lu, 
                                       spatial.group = "GEOID",
                                       repeats = 5, folds = 5,
                                       compute.lu.ml.parts = FALSE, seed = 123,
                                       importance = FALSE, run_in_parallel = FALSE) {
  
  ## For R cmd check
  . <- GEOID <- index <- hp.target <- test.groups <- obs_idx <- NULL
  repeat.id <- fold.id <- fold_id <- task.seed <- lu.best.xgboost <- N <- .N <- NULL
  Gain <- Feature <- BIAS <- shap_value <- feature <- feature1 <- feature2 <- NULL
  
  if (compute.lu.ml.parts == TRUE && importance == TRUE) {
    stop("Error: You cannot set both `compute.lu.ml.parts` and `importance` to TRUE.")
  }
  
  if (!(spatial.group %in% names(DT.hp))) {
    stop(paste0("Error: spatial.group column '", spatial.group, "' not found in DT.hp."))
  }

  DT.hp <- copy(DT.hp) %>% setorder(GEOID, index)
  DT.lu <- copy(DT.lu) %>% setorder(GEOID)
  
  required_cols <- unique(c("GEOID", "index", "hp.target", spatial.group))
  DT.hp <- DT.hp[, ..required_cols]
  
  if (DT.lu[, .N, by = GEOID][, any(N > 1)]) stop("Error: DT.lu has duplicate GEOIDs.")
  if (DT.hp[, sum(is.na(hp.target))] > 0) stop("Error: Missing values in hp.target.")
  
  saiz.circle.vars <- names(DT.lu) %>% .[grepl("saiz.circle$", x = .)]
  if (length(saiz.circle.vars) > 0) DT.lu <- DT.lu[, c(saiz.circle.vars) := NULL]
  
  DT.hp <- DT.hp[GEOID %chin% c(DT.lu$GEOID)]
  DT.lu <- DT.lu[GEOID %chin% c(DT.hp$GEOID)]
  
  indices <- DT.hp[, unique(index)]
  
  # --- Worker Function ---
  f_train_xgboost <- function(DT, test.groups, train.seed, spatial_col, importance_flag) {
    data.table::setDTthreads(1)
    xgb_params <- list(objective = "reg:squarederror", nthread = 1)
    
    all_groups <- DT[, unique(get(spatial_col))]
    train.groups <- all_groups[!(all_groups %in% test.groups)]
    
    tune.groups <- withr::with_seed(seed = train.seed, {
      sample(train.groups, size = floor(length(train.groups) * 0.75))
    })
    val.groups <- train.groups[!(train.groups %in% tune.groups)]
    
    f_get_xgboost_dmat <- function(DT_sub) {
      data_mat <- as.matrix(DT_sub[, .SD, .SDcols = patterns("unavailable")])
      label_vec <- DT_sub[, hp.target]
      return(xgboost::xgb.DMatrix(data = data_mat, label = label_vec))
    }
    
    dtune <- f_get_xgboost_dmat(DT[get(spatial_col) %in% tune.groups])
    dvalidation <- f_get_xgboost_dmat(DT[get(spatial_col) %in% val.groups])
    
    withr::with_seed(seed = train.seed, {
      clf <- xgboost::xgb.train(
        params = xgb_params, data = dtune, nrounds = 500, 
        evals = list(train = dtune, eval = dvalidation),
        verbose = 0,
        callbacks = list(xgboost::xgb.cb.early.stop(25, metric_name = "eval_rmse", maximize = FALSE, verbose = FALSE))
      )
    })
    
    best_nrounds <- if (!is.null(clf$best_iteration)) clf$best_iteration else 500
    dtrain <- f_get_xgboost_dmat(DT[get(spatial_col) %in% train.groups])
    
    withr::with_seed(seed = train.seed, {
      xgboost.mod <- xgboost::xgb.train(params = xgb_params, data = dtrain, nrounds = best_nrounds, verbose = 0)
    })
    
    DT.test <- DT[get(spatial_col) %in% test.groups] %>% setorder(GEOID, index)
    dtest <- f_get_xgboost_dmat(DT.test)
    xgboost.pred <- stats::predict(xgboost.mod, dtest)
    
    DT.oos.pred <- DT.test[, .(GEOID, index)] %>% .[, lu.best.xgboost := xgboost.pred]
    
    if (!importance_flag) {
      return(list(DT.oos.pred = DT.oos.pred))
    } else {
      # 1. Feature Importance
      DT.xgb.importance <- xgboost::xgb.importance(feature_names = colnames(dtrain), model = xgboost.mod)
      
      # 2. SHAP Values
      DT.shap <- data.table::as.data.table(stats::predict(xgboost.mod, newdata = dtest, predcontrib = TRUE)) %>%
        cbind(DT.test[, .(GEOID, index)], .) %>%
        data.table::melt(id.vars = c("GEOID", "index"), variable.name = "feature", value.name = "shap_value", variable.factor = FALSE)
      
      # 3. SHAP Interactions
      array.shap.interactions <- stats::predict(xgboost.mod, newdata = dtest, predinteraction = TRUE)
      DT.shap.interactions <- data.table::as.data.table(array.shap.interactions) %>%
        setnames(c("V1", "V2", "V3", "value"), c("obs_idx", "feature1", "feature2", "shap_value")) %>%
        .[, `:=`(GEOID = DT.test$GEOID[obs_idx], index = DT.test$index[obs_idx])] %>%
        .[, obs_idx := NULL]
      
      return(list(DT.oos.pred = DT.oos.pred, DT.xgb.importance = DT.xgb.importance, 
                  DT.shap = DT.shap, DT.shap.interactions = DT.shap.interactions))
    }
  }
  
  # --- Prediction Loop ---
  f_get_xgboost_lu_predictions <- function(index_val) {
    DT <- DT.hp[index == index_val] %>% merge(DT.lu, by = "GEOID")
    unique_groups <- DT[, unique(get(spatial.group))]
    seed.offset <- DT.hp[, which(unique(index) == index_val)]
    
    DT.est <- expand.grid(repeat.id = 1:repeats, fold.id = 1:folds) %>% 
      setDT() %>% .[, test.groups := list()] %>%
      .[, task.seed := seed + repeat.id + fold.id + seed.offset]
    
    for (i in 1:repeats) {
      groups.rand <- withr::with_seed(seed = seed + i + seed.offset, { sample(unique_groups, length(unique_groups)) })
      dt_folds <- data.table(grp = groups.rand) %>% .[, fold_id := rep(1:folds, length.out = .N)]
      DT.est[repeat.id == i, test.groups := dt_folds[, .(list(grp)), by = fold_id]$V1]
    }
    
    if (run_in_parallel) future::plan(future::multisession(workers = future::availableCores()))
    list.pred.all <- future.apply::future_Map(f_train_xgboost, DT = list(DT), 
                                              test.groups = DT.est$test.groups, 
                                              train.seed = DT.est$task.seed, 
                                              spatial_col = spatial.group, 
                                              importance_flag = importance,
                                              future.seed = TRUE, future.packages = c("xgboost", "data.table"))
    if (run_in_parallel) future::plan(future::sequential())
    
    DT.oos.pred.all <- lapply(list.pred.all, \(x) x$DT.oos.pred) %>% rbindlist() %>%
      .[, .(lu_ml_xgboost = mean(lu.best.xgboost)), by = .(GEOID, index)]
    
    if (importance) {
      # Aggregate Importance and set column order
      DT.xgb.importance.all <- lapply(list.pred.all, \(x) x$DT.xgb.importance) %>% rbindlist() %>%
        .[, .(Gain = mean(Gain)), by = Feature] %>% .[, index := index_val] %>%
        setcolorder(c("index", "Feature", "Gain"))
      
      DT.xgb.shap.all <- lapply(list.pred.all, \(x) x$DT.shap) %>% rbindlist() %>%
        .[, .(shap_value = mean(shap_value)), keyby = .(GEOID, index, feature)]
      
      DT.xgb.shap.interactions.all <- lapply(list.pred.all, \(x) x$DT.shap.interactions) %>% rbindlist() %>%
        .[, .(shap_value = mean(shap_value)), by = .(GEOID, index, feature1, feature2)]
      
      return(list(DT.oos.pred = DT.oos.pred.all, 
                  DT.xgb.importance = DT.xgb.importance.all, 
                  DT.xgb.shap = DT.xgb.shap.all,
                  DT.xgb.shap.interactions = DT.xgb.shap.interactions.all))
    }
    return(DT.oos.pred.all)
  }
  
  list.out <- lapply(indices, f_get_xgboost_lu_predictions)
  
  if (!importance) {
    DT.oos.pred.panel <- rbindlist(list.out)
    final_out <- merge(DT.hp, DT.oos.pred.panel, by = c("GEOID", "index"))
    pred_cols <- names(final_out)[grepl("lu_ml_xgboost", names(final_out))]
    setcolorder(final_out, unique(c("GEOID", spatial.group, "index", "hp.target", pred_cols)))
    return(final_out)
  } else {
    DT.oos.pred.panel <- rbindlist(lapply(list.out, \(x) x$DT.oos.pred)) %>% merge(DT.hp, by = c("GEOID", "index"))
    setcolorder(DT.oos.pred.panel, unique(c("GEOID", spatial.group, "index", "hp.target", "lu_ml_xgboost")))
    
    return(list(
      DT.oos.pred.panel = DT.oos.pred.panel,
      DT.xgb.importance.panel = rbindlist(lapply(list.out, \(x) x$DT.xgb.importance)),
      DT.xgb.shap.panel = rbindlist(lapply(list.out, \(x) x$DT.xgb.shap)),
      DT.xgb.shap.interactions.panel = rbindlist(lapply(list.out, \(x) x$DT.xgb.shap.interactions))
    ))
  }
}
