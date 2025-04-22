#' Time-Varying Supervised PCA Model for Land Use and House Price Prediction
#'
#' This function trains a supervised principal component regression model using the
#' `superpc` package to predict house prices from time-varying land use data. It performs
#' repeated k-fold cross-validation to tune the feature selection threshold and computes
#' first supervised principal component for east test dataset out-of-sample.
#'
#' @param DT.hp A `data.table` containing house price data with columns `GEOID`,
#'   `index` (Date), and `hp.target` (house price target variable).
#' @param DT.lu A `data.table` containing land use data with `GEOID` as the first
#'   column and other columns representing land use features.
#' @param repeats An integer specifying the number of times to repeat the k-fold
#'   cross-validation. Defaults to 5.
#' @param folds An integer specifying the number of folds for cross-validation.
#'   Defaults to 5.
#' @param seed An integer seed for reproducibility of random sampling and cross-validation
#'   folds. Defaults to 1234.
#'
#' @return A `data.table` containing the original house price data
#'   (`DT.hp`) merged with out-of-sample results from the supervised
#'   PCA model. The column `lu_ml_pc1` contains the first supervised
#'   principal component score (i.e., the projected value) computed
#'   for each test observation. These scores are computed out-of-sample:
#'   the principal component factor loadings (i.e., the weights on the
#'   selected features) are estimated from the training folds, and the
#'   scores are then calculated by applying those loadings to the
#'   held-out test data.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Orders house price and land use data by GEOID and time index.
#'   \item Merges land use and house price data and filters out any GEOIDs not present in both.
#'   \item Constructs repeated k-fold cross-validation splits across GEOIDs for each time index.
#'   \item For each time index:
#'     \itemize{
#'       \item Trains a supervised PCA model using `superpc.train` on the training folds.
#'       \item Uses `superpc.cv` to select the optimal threshold \eqn{\theta}.
#'       \item Computes both out-of-sample predictions (`"continuous"`) and the first component
#'         scores (`"components"`) using `superpc.predict`.
#'     }
#'   \item Aggregates the predictions and component scores across all folds and repeats.
#' }
#'
#' The function uses `future` and `future.apply` to parallelize model estimation across folds and repeats.
#'
#' @import data.table
#' @import superpc
#' @import future
#' @import future.apply
#' @importFrom stats predict
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
#' # Run the model using Supervised PCA
#' out <- lu_ml_superpc_time_varying(
#'   DT.hp = dt_mian_sufi_2014,
#'   DT.lu = dt_cnty_lu_2010,
#'   seed = 42
#' )
#'
#' # Correlation between predicted and actual house prices
#' cor(out$hp.target, out$lu_ml)
#'
#' # Correlation between PC1 and house prices
#' cor(out$hp.target, out$lu_pc1)
#' }
#'
#' @export
lu_ml_superpc_time_varying <- function(DT.hp, DT.lu, repeats = 5, folds = 5,
                                       seed = 1234) {
  
  . <- GEOID <- index <- hp.target <- test.geoids <- NULL
  repeat.id <- fold.id <- lu_ml_pc1 <- NULL

  DT.hp <- DT.hp[order(GEOID, index)]
  DT.lu <- DT.lu[order(GEOID)]

  if (DT.hp[, sum(is.na(hp.target))] > 0)
    stop("Error: There are missing values in hp.target.")

  DT.hp <- DT.hp[GEOID %chin% DT.lu$GEOID]
  DT.lu <- DT.lu[GEOID %chin% DT.hp$GEOID]

  geoids <- DT.hp[, unique(GEOID)]
  indices <- DT.hp[, unique(index)]

  DT.est.base <- expand.grid(repeat.id = 1:repeats, fold.id = 1:folds) %>%
    data.table::setDT() %>%
    .[, test.geoids := list(vector("list", .N))]

  for (i in 1:repeats) {
    geoids.rand <- withr::with_seed(seed, sample(geoids))
    test.ids.list <- chunk2(geoids.rand, folds)
    for (j in 1:folds)
      DT.est.base[repeat.id == i & fold.id == j,
                  test.geoids := list(test.ids.list[[j]])]
  }

  f_train_superpc <- function(DT, test.geoids) {
    train.geoids <- setdiff(unique(DT$GEOID), test.geoids)
    DT.train <- DT[GEOID %chin% train.geoids]
    DT.test  <- DT[GEOID %chin% test.geoids][order(GEOID, index)]

    x.train <- t(as.matrix(DT.train[, !c("GEOID", "index", "hp.target"), with = FALSE]))
    y.train <- DT.train$hp.target
    featurenames <- rownames(x.train)

    train.data <- list(x = x.train, y = y.train, featurenames = featurenames)
    trained.model <- superpc::superpc.train(train.data, type = "regression")

    withr::with_seed(seed, {
      cv.results <- superpc::superpc.cv(trained.model, train.data, n.components = 1)
    })
    
    best.threshold <- cv.results$threshold[which.min(cv.results$scor)]

    x.test <- t(as.matrix(DT.test[, !c("GEOID", "index", "hp.target"), with = FALSE]))
    test.data <- list(x = x.test, y = rep(0, ncol(x.test)), featurenames = featurenames)

    pred_vals <- superpc::superpc.predict(
      object = trained.model,
      data = train.data,
      newdata = test.data,
      threshold = best.threshold,
      n.components = 1,
      prediction.type = "continuous"
    )
  
    DT.oos.pred <- DT.test[, .(GEOID, index)][
      , `:=`(lu_ml_pc1 = as.numeric(pred_vals$v.pred))]
    return(list(DT.oos.pred = DT.oos.pred))
  }

  f_get_superpc_lu_predictions <- function(index_val) {
    cat("Processing index:", as.character(index_val), "\n")
    DT.est <- copy(DT.est.base)
    DT <- DT.hp[index == c(index_val)] %>% merge(DT.lu, by = "GEOID")

    future::plan(future::multisession(workers = future::availableCores()))
    list.pred.all <- future.apply::future_Map(
      f_train_superpc,
      DT = list(DT),
      test.geoids = DT.est$test.geoids,
      future.seed = seed
    )
    future::plan(future::sequential())

    rbindlist(lapply(list.pred.all, \(x) x$DT.oos.pred)) %>%
      .[, .(lu_ml_pc1 = mean(lu_ml_pc1)), by = .(GEOID, index)]
  }

  DT.oos.pred.panel <- rbindlist(lapply(indices, f_get_superpc_lu_predictions))
  DT.oos.pred.panel <- merge(DT.hp, DT.oos.pred.panel, by = c("GEOID", "index"))
  return(DT.oos.pred.panel)
}
