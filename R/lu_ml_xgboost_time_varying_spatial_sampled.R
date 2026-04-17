#' Time-Varying XGBoost Model with Spatial Block Sampling
#'
#' @param DT.hp A data.table containing house price data.
#' @param DT.lu A data.table containing land use data.
#' @param spatial_block_col A character string specifying the column in `DT.hp` 
#'   to use for spatial block holdouts (e.g., "state").
#' @param repeats An integer specifying the number of times to repeat CV.
#' @param folds An integer specifying the number of folds.
#' @param compute.lu.ml.parts Logical, whether to compute parts predictions.
#' @param seed An integer seed for reproducibility.
#' @param importance Logical, whether to compute feature importance.
#' 
#' @export
lu_ml_xgboost_time_varying_spatial_sampled <- function(DT.hp, DT.lu, spatial_block_col,
                                                       repeats = 5, folds = 5,
                                                       compute.lu.ml.parts = FALSE, 
                                                       seed = 123, importance = FALSE) {

  ## For R cmd check
  . <- GEOID <- index <- hp.target <- test.geoids <- obs_idx <- NULL
  repeat.id <- fold.id <- fold_id <- task.seed <- lu.best.xgboost <- N <- .N <- NULL
  Gain <- Feature <- BIAS <- shap_value <- feature <- feature1 <- feature2 <- NULL

  if (compute.lu.ml.parts == TRUE && importance == TRUE) {
    stop("Error: You cannot set both `compute.lu.ml.parts` and `importance` to TRUE.")
  }
  
  if (!spatial_block_col %in% names(DT.hp)) {
    stop(sprintf("Error: Spatial block column '%s' not found in DT.hp.", spatial_block_col))
  }
  
  # Ensure there are enough spatial blocks to create the desired number of folds
  if (length(unique(DT.hp[[spatial_block_col]])) < folds) {
    stop(sprintf("Error: Number of unique spatial blocks in '%s' is less than the number of folds.", spatial_block_col))
  }

  DT.hp <- DT.hp %>% .[order(GEOID, index)]
  DT.lu <- DT.lu %>% .[order(GEOID)]

  if (DT.lu[, .N, by = GEOID][, any(N > 1)]) {
    stop("Error: DT.lu has duplicate GEOIDs.")
  }

  if (DT.hp[, sum(is.na(hp.target))] > 0) {
    stop("Error: There are missing values in hp.target.")
  }

  ## delete the saiz circle vars
  saiz.circle.vars <- names(DT.lu) %>% .[grepl("saiz.circle$", x = .)]
  if (length(saiz.circle.vars) > 0)
    DT.lu <- DT.lu[, c(saiz.circle.vars) := NULL]

  DT.hp <- DT.hp[GEOID %chin% c(DT.lu$GEOID)]
  DT.lu <- DT.lu[GEOID %chin% c(DT.hp$GEOID)]

  ## Checks
  f_stopifnot(
    all(c("GEOID", "index", "hp.target", spatial_block_col) %in% names(DT.hp)),
    "DT.hp is missing required columns."
  )
  f_stopifnot(DT.hp[, class(index)] == "Date",
              "DT.hp$index must be of type `Date`")
  f_stopifnot(names(DT.lu)[1] == "GEOID")
  f_stopifnot(setdiff(unique(DT.hp$GEOID), DT.lu$GEOID) == character(0))
  
  num.obs.by.geoid <- DT.hp %>% .[complete.cases(.)] %>%
    .[, .N, by = GEOID] %>% .[, N]
  if (var(num.obs.by.geoid) != 0) {
    print("Warning: `DT.hp` is not a balanced panel.")
  }

  DT.hp <- DT.hp %>% .[order(GEOID, index)]
  DT.lu <- DT.lu %>% .[order(GEOID)]
  
  geoids <- DT.hp[, unique(GEOID)]
  DT.panel.out <- DT.hp[, .(GEOID, index)]
  indices <- DT.hp[, unique(index)]

  
  f_train_xgboost <- function(DT, test.geoids, train.seed) {
    data.table::setDTthreads(1)
    xgboost_ntrheads <- 1

    geoids <- DT[, unique(GEOID)]
    train.geoids <- geoids %>% .[!(. %chin% test.geoids)]
    
    ## -- Spatial Split for Internal Validation (Early Stopping) -- ##
    ## FIX: Wrapped columns in unique() to prevent duplicate names if spatial_block_col == "GEOID"
    dt_train_blocks <- unique(DT[GEOID %chin% train.geoids, unique(c("GEOID", spatial_block_col)), with = FALSE])
    train.blocks <- unique(dt_train_blocks[[spatial_block_col]])

    tune.nrounds.blocks <- withr::with_seed(seed = train.seed, {
      sample(train.blocks, size = floor(length(train.blocks) * 0.75))
    })
    
    tune.nrounds.geoids <- dt_train_blocks[get(spatial_block_col) %in% tune.nrounds.blocks, GEOID]
    validation.nrounds.geoids <- train.geoids %>% .[!(. %chin% tune.nrounds.geoids)]

    f_get_xgboost_dmat <- function(DT_subset) {
      data <- as.matrix(DT_subset[, .SD, .SDcols = patterns("unavailable")])
      label <- DT_subset[, hp.target]
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
        data = dtune, 
        nrounds = 500, 
        watchlist = watchlist,
        maximize = FALSE,
        early_stopping_rounds = 25,
        objective = "reg:squarederror",
        verbose = FALSE,
        nthread = xgboost_ntrheads
      )
    })

    ## -- Train -- ##
    DT.train <- DT[GEOID %chin% c(train.geoids)]
    dtrain <- f_get_xgboost_dmat(DT.train)

    withr::with_seed(seed = train.seed, {
      xgboost.mod <- xgboost(
        data = dtrain,
        nrounds = clf$best_iteration,
        objective = "reg:squarederror",
        verbose = FALSE,
        nthread = xgboost_ntrheads
      )
    })

    ## -- Predict -- ##
    DT.test <- DT[GEOID %chin% c(test.geoids)] %>%
      .[order(GEOID, index)]

    stopifnot(
      nrow(DT.test) == length(test.geoids),
      nrow(DT.test) == length(unique(DT.test$GEOID))
    )

    dtest <- f_get_xgboost_dmat(DT.test)
    xgboost.pred <- predict(xgboost.mod, dtest)

    DT.oos.pred <- DT.test[, .(GEOID, index)] %>%
      .[, lu.best.xgboost := xgboost.pred]

    if (importance ==  FALSE) {
      return(list(DT.oos.pred = DT.oos.pred))
    } else {
      DT.xgb.importance <- xgb.importance(
        feature_names = colnames(dtrain),
        model = xgboost.mod
      )

      DT.shap <- predict(xgboost.mod, newdata = dtest, predcontrib = TRUE) %>%
        as.data.table() %>%
        cbind(DT.test[, .(GEOID, index)], .) %>%
        melt(id.vars = c("GEOID", "index"),
             variable.name = "feature", value.name = "shap_value",
             variable.factor = FALSE)

      array.shap.interactions <- predict(xgboost.mod, newdata = dtest,
                                         predinteraction = TRUE)
      
      stopifnot(length(DT.test$GEOID) == dim(array.shap.interactions)[1])

      DT.test <- DT.test[, obs_idx := .I]

      DT.shap.interactions <- as.data.table(array.shap.interactions) %>%
        setnames(c("V1", "V2", "V3", "value"),
                 c("obs_idx", "feature1", "feature2", "shap_value")) %>%
        merge(DT.test[, .(obs_idx, GEOID, index)], by = "obs_idx") %>%
        .[, obs_idx := NULL] %>%
        setcolorder(c("GEOID", "index", "feature1", "feature2", "shap_value"))
            
      return(list(DT.oos.pred = DT.oos.pred, 
                  DT.xgb.importance = DT.xgb.importance, 
                  DT.shap = DT.shap,
                  DT.shap.interactions = DT.shap.interactions
                  ))
    }
  }

  f_get_xgboost_lu_predictions <- function(index_val) {
    print(index_val)

    index.tmp <- index_val
    DT <- DT.hp[index == index.tmp] %>%
      merge(DT.lu, by = "GEOID")

    seed.offset <- DT.hp[, which(unique(index) == c(index_val))]

    DT.est <- expand.grid(
      repeat.id = 1:repeats, 
      fold.id = 1:folds
    ) %>% setDT() %>%
      .[order(repeat.id)] %>%
      .[, test.geoids := list()] %>%
      .[, task.seed := seed + repeat.id + fold.id + seed.offset]

    ## -- Create Spatial Folds -- ##
    ## FIX: Wrapped columns in unique() to prevent duplicate names if spatial_block_col == "GEOID"
    dt_geoid_block <- unique(DT[, unique(c("GEOID", spatial_block_col)), with = FALSE])
    blocks.this.index <- unique(dt_geoid_block[[spatial_block_col]])

    for (i in 1:repeats) {
      blocks.rand <- withr::with_seed(seed = seed + i + seed.offset, {
        sample(blocks.this.index, length(blocks.this.index))
      })

      # Assign blocks to folds
      dt_block_folds <- data.table(block_col = blocks.rand) %>%
        setnames("block_col", spatial_block_col) %>%
        .[, fold_id := rep(1:folds, length.out = .N)]

      # Merge back to map GEOIDs to their block's fold
      dt_folds <- merge(dt_geoid_block, dt_block_folds, by = spatial_block_col)

      # Extract GEOIDs per fold, ensuring order 1:folds
      test.ids.list <- lapply(1:folds, function(f) {
        dt_folds[fold_id == f, GEOID]
      })

      DT.est <- DT.est %>%
        .[repeat.id == i, test.geoids := test.ids.list]
    }

    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.all <- future.apply::future_Map(f_train_xgboost,
                                              DT = list(DT),
                                              test.geoids = DT.est$test.geoids,
                                              train.seed = DT.est$task.seed,
                                              future.seed = TRUE)
    future::plan(future::sequential())
    
    DT.oos.pred.all <- lapply(list.pred.all, \(x) x$DT.oos.pred) %>% rbindlist() %>%
      .[, .(lu_ml_xgboost = mean(lu.best.xgboost)), by = .(GEOID, index)]

    if (importance == TRUE) {
      DT.xgb.shap.all <- lapply(list.pred.all, \(x) x$DT.shap) %>% 
        rbindlist() %>%
        .[, .(shap_value = mean(shap_value, na.rm = TRUE)),
          keyby = .(GEOID, index, feature)]
            
      DT.xgb.shap.interactions.all <- lapply(
        list.pred.all, \(x) x$DT.shap.interactions
      ) %>%
        rbindlist() %>%
        .[, lapply(.SD, mean, na.rm = TRUE), by = .(GEOID, index, feature1, feature2)]

      DT.xgb.importance.all <- lapply(list.pred.all, \(x) x$DT.xgb.importance) %>%
        rbindlist() %>%
        .[, .(Gain = mean(Gain, na.rm = TRUE)), by = .(Feature)] %>%
        .[, Gain := Gain / sum(Gain)] %>%
        .[order(-Gain)] %>%
        .[, index := c(index_val)] %>%
        setcolorder(c("index", "Feature", "Gain"))
    }
    
    if (compute.lu.ml.parts == FALSE && importance == FALSE)  {
      return(DT.oos.pred.all)
    } else if (compute.lu.ml.parts == FALSE && importance == TRUE) {
      return(list(DT.oos.pred = DT.oos.pred.all, 
                  DT.xgb.importance = DT.xgb.importance.all, 
                  DT.xgb.shap = DT.xgb.shap.all,
                  DT.xgb.shap.interactions = DT.xgb.shap.interactions.all))
    }
    
    ##Parts
    parts.cols <- names(DT) %>% .[grepl("^slope|^water|^wetlands", x = .)]
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.parts <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", parts.cols)]),
      test.geoids = DT.est$test.geoids,
      train.seed = DT.est$task.seed,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.parts <- lapply(list.pred.parts, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_xgboost_parts = mean(lu.best.xgboost)), by = .(GEOID, index)]


    ## Total 
    total.cols <- names(DT) %>% .[grepl("^total", x = .)]
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.total <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", total.cols)]),
      test.geoids = DT.est$test.geoids,
      train.seed = DT.est$task.seed,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.total <- lapply(list.pred.total, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_xgboost_total = mean(lu.best.xgboost)), by = .(GEOID, index)]
    
    ## Slope 
    slope.cols <- names(DT) %>% .[grepl("^slope", x = .)]
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.slope <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", slope.cols)]),
      test.geoids = DT.est$test.geoids,
      train.seed = DT.est$task.seed,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.slope <- lapply(list.pred.slope, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_xgboost_slope = mean(lu.best.xgboost)), by = .(GEOID, index)]

    ## Water 
    water.cols <- names(DT) %>% .[grepl("^water", x = .)]
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.water <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", water.cols)]),
      test.geoids = DT.est$test.geoids,
      train.seed = DT.est$task.seed,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.water <- lapply(list.pred.water, \(x) x$DT.oos.pred) %>% rbindlist %>%
      .[, .(lu_ml_xgboost_water = mean(lu.best.xgboost)), by = .(GEOID, index)]

    ## Wetlands 
    wetlands.cols <- names(DT) %>% .[grepl("^wetlands", x = .)]
    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.wetlands <- future.apply::future_Map(
      f_train_xgboost,
      DT = list(DT[, .SD, .SDcols = c("GEOID", "index", "hp.target", wetlands.cols)]),
      test.geoids = DT.est$test.geoids,
      train.seed = DT.est$task.seed,
      future.seed = TRUE
    )
    future::plan(future::sequential())
    
    DT.oos.pred.wetlands <- lapply(list.pred.wetlands, \(x) x$DT.oos.pred) %>%
      rbindlist %>%
      .[, .(lu_ml_xgboost_wetlands = mean(lu.best.xgboost)), by = .(GEOID, index)]

    DT.oos.pred <- merge(
      DT.oos.pred.all, DT.oos.pred.parts, by = c("GEOID", "index")
    ) %>%
      merge(DT.oos.pred.total, by = c("GEOID", "index")) %>%
      merge(DT.oos.pred.slope, by = c("GEOID", "index")) %>%
      merge(DT.oos.pred.water, by = c("GEOID", "index")) %>%
      merge(DT.oos.pred.wetlands, by = c("GEOID", "index"))

    return(DT.oos.pred)

  }

  list.out <- lapply(indices, f_get_xgboost_lu_predictions)

  if (importance == FALSE) {
    DT.oos.pred.panel <- rbindlist(list.out)
    DT.oos.pred.panel <- merge(DT.hp, DT.oos.pred.panel, by = c("GEOID", "index"))
    
    # FIX: Robustly order by putting all original DT.hp columns first, prediction at the end
    setcolorder(DT.oos.pred.panel, unique(c(names(DT.hp), "lu_ml_xgboost")))
    
    return(DT.oos.pred.panel)
  } else {
    
    DT.oos.pred.panel <- rbindlist(lapply(list.out, \(x) x$DT.oos.pred)) %>%
      merge(DT.hp, by = c("GEOID", "index")) %>%
      # FIX: Apply the same robust ordering here
      setcolorder(unique(c(names(DT.hp), "lu_ml_xgboost")))
      
    DT.xgb.importance.panel <- rbindlist(lapply(list.out, \(x) x$DT.xgb.importance))
    DT.xgb.shap.panel <- rbindlist(lapply(list.out, \(x) x$DT.xgb.shap))
    DT.xgb.shap.interactions.panel <- lapply(
      list.out, \(x) x$DT.xgb.shap.interactions
    ) %>%
      rbindlist()

    return(list(
      DT.oos.pred.panel = DT.oos.pred.panel,
      DT.xgb.importance.panel = DT.xgb.importance.panel,
      DT.xgb.shap.panel = DT.xgb.shap.panel,
      DT.xgb.shap.interactions.panel = DT.xgb.shap.interactions.panel
    ))

  }
}
