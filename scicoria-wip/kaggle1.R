library(data.table)
library(lightgbm) # lightgbm)


#this was taken from existin Kaggle users submission

set.seed(0)

h <- 28
max_lags <- 366
tr_last <- 1913
fday <- as.IDate("2016-04-25")

#---------------------------
cat("Creating auxiliary functions...\n")

free <- function() invisible(gc())

create_dt <- function(is_train = TRUE, nrows = Inf, sample_size = 1000) {

  prices <- fread("./data/sell_prices.csv")
  cal <- fread("./data/calendar.csv")
  cal[, `:=`(date = as.IDate(date, format="%Y-%m-%d"),
             is_weekend = as.integer(weekday %chin% c("Saturday", "Sunday")))]

  if (is_train) {
    dt <- fread("./data/sales_train_validation.csv", nrows = nrows)
    #dt <- df[sample(nrow(df), sample_size), ]
  } else {
    dt <- fread("./data/sales_train_validation.csv", nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags)))
    dt[, paste0("d_", (tr_last+1):(tr_last+2*h)) := NA_real_]
  }

  dt <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "sales")

  dt <- dt[cal, `:=`(date = i.date,
                     is_weekend = i.is_weekend,
                     wm_yr_wk = i.wm_yr_wk,
                     event_name_1 = i.event_name_1,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]

  dt[prices, sell_price := i.sell_price, on = c("store_id", "item_id", "wm_yr_wk")]
}

create_fea <- function(dt) {

  lag <- c(7, 28, 29)
  dt[, (paste0("lag_", lag)) := shift(.SD, lag), .SDcols = "sales", by = "id"]

  win <- c(7, 30, 90, 180)
  dt[, (paste0("roll_mean_28_", win)) := frollmean(lag_28, win, na.rm = TRUE), by = "id"]

  win <- c(28)
  dt[, (paste0("roll_max_28_", win)) := frollapply(lag_28, win, max), by = "id"]
  dt[, (paste0("roll_var_28_", win)) := frollapply(lag_28, win, var), by = "id"]

  dt[, price_change_1 := sell_price / shift(sell_price) - 1, by = "id"]
  dt[, price_change_365 := sell_price / frollapply(shift(sell_price), 365, max) - 1, by = "id"]

  cols <- c("item_id", "state_id", "dept_id", "cat_id", "event_name_1")
  dt[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]

  dt[, `:=`(wday = wday(date),
            mday = mday(date),
            week = week(date),
            month = month(date),
            quarter = quarter(date),
            year = year(date),
            store_id = NULL,
            d = NULL,
            wm_yr_wk = NULL)]
}

#---------------------------
cat("Processing datasets...\n")
cat("Processing datasets dt...\n")

tr <- create_dt() #nrows = 1000) # was Inf
free()

cat("Processing datasets fea...\n")
create_fea(tr)
free()

cat("Processing datasets na.omit...\n")
tr <- na.omit(tr)
y <- tr$sales


cat("Processing datasets idx...\n")
idx <- tr[date <= max(date)-h, which = TRUE]


cat("Processing datasets. null..\n")
tr[, c("id", "sales", "date") := NULL]
free()

cat("Processing datasets data matrix...\n")
tr <- data.matrix(tr)
free()

cat("Processing datasets cats...\n")
cats <- c("item_id", "state_id", "dept_id", "cat_id",
          "wday", "mday", "week", "month", "quarter", "year", "is_weekend",
          "snap_CA", "snap_TX", "snap_WI")

cat("Processing datasets - lgb 1...\n")
xtr <- lgb.Dataset(tr[idx, ], label = y[idx], categorical_feature = cats)

cat("Processing datasets - lgb 2...\n")
xval <- lgb.Dataset(tr[-idx, ], label = y[-idx], categorical_feature = cats)

rm(tr, y, idx)
free()

#---------------------------
cat("Training model...\n")
cat("Training model poisson...\n")

p <- list(objective = "poisson",
          metric ="rmse",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 10, # was 1
          lambda_l2 = 0.1,
          nthread = 8)  # of cores

cat("Training model lgb train...\n")
m_lgb <- lgb.train(params = p,
                   data = xtr,
                   nrounds = 2000, # 2000,
                   valids = list(valid = xval),
                   early_stopping_rounds = 400,
                   eval_freq = 200) #200)

cat("Best score:", m_lgb$best_score, "at", m_lgb$best_iter, "iteration")

pdf(file = 'importand.pdf')

lgb.plot.importance(lgb.importance(m_lgb), 20)

dev.off()

rm(xtr, xval, p)
free()

#---------------------------
cat("Forecasting...\n")

te <- create_dt(FALSE)

for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
  cat("for day setting up feature")
  cat(as.character(day), " ")
  tst <- te[date >= day - max_lags & date <= day]
  cat("calling create_fea")
  create_fea(tst)
  tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
  cat("calling predict")
  te[date == day, sales := predict(m_lgb)]
}

te[date >= fday
   ][date >= fday+h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "sales")
         ][, fwrite(.SD, "sub_dt_lgb.csv")]
