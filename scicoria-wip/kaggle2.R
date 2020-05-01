

library(data.table)
library(lightgbm)
library(ggplot2)

set.seed(0)

cores = 8


h <- 28 # forecast horizon
max_lags <- 420 # number of observations to shift by
tr_last <- 1913 # last training day
fday <- as.IDate("2016-04-25") # first day to forecast
nrows <- Inf

free <- function() invisible(gc()) 


create_dt <- function(is_train = TRUE, nrows = Inf) {
  
  if (is_train) { # create train set
    dt <- fread("./data/sales_train_validation.csv", nrows = nrows)
    cols <- dt[, names(.SD), .SDcols = patterns("^d_")]
    dt[, (cols) := transpose(lapply(transpose(.SD),
                                    function(x) {
                                      i <- min(which(x > 0))
                                      x[1:i-1] <- NA
                                      x})), .SDcols = cols]
    free()
  } else { # create test set
    dt <- fread("./data/sales_train_validation.csv", nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags))) # keep only max_lags days from the train set
    dt[, paste0("d_", (tr_last+1):(tr_last+2*h)) := 0] # add empty columns for forecasting
  }
  
  dt <- na.omit(melt(dt,
                     measure.vars = patterns("^d_"),
                     variable.name = "d",
                     value.name = "sales"))
  
  cal <- fread("./data/calendar.csv")
  dt <- dt[cal, `:=`(date = as.IDate(i.date, format="%Y-%m-%d"), # merge tables by reference
                     wm_yr_wk = i.wm_yr_wk,
                     event_name_1 = i.event_name_1,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]
  
  prices <- fread("./data/sell_prices.csv")
  dt[prices, sell_price := i.sell_price, on = c("store_id", "item_id", "wm_yr_wk")] # merge again
}

create_fea <- function(dt) {
  dt[, `:=`(d = NULL, # remove useless columns
            wm_yr_wk = NULL)]
  
  cols <- c("item_id", "store_id", "state_id", "dept_id", "cat_id", "event_name_1") 
  dt[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols] # convert character columns to integer
  free()
  
  lag <- c(7, 28) 
  lag_cols <- paste0("lag_", lag) # lag columns names
  dt[, (lag_cols) := shift(.SD, lag), by = id, .SDcols = "sales"] # add lag vectors
  
  win <- c(7, 28) # rolling window size
  roll_cols <- paste0("rmean_", t(outer(lag, win, paste, sep="_"))) # rolling features columns names
  dt[, (roll_cols) := frollmean(.SD, win, na.rm = TRUE), by = id, .SDcols = lag_cols] # rolling features on lag_cols
  
  dt[, `:=`(wday = wday(date), # time features
            mday = mday(date),
            week = week(date),
            month = month(date),
            year = year(date))]
}


tr <- create_dt()
free()

pdf('plot2.pdf')
tr[, .(sales = unlist(lapply(.SD, sum))), by = "date", .SDcols = "sales"
   ][, ggplot(.SD, aes(x = date, y = sales)) +
       geom_line(size = 0.3, color = "steelblue", alpha = 0.8) + 
       geom_smooth(method='lm', formula= y~x, se = FALSE, linetype = 2, size = 0.5, color = "gray20") + 
       labs(x = "", y = "total sales") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
       scale_x_date(labels=scales::date_format ("%b %y"), breaks=scales::date_breaks("3 months"))]
free()

dev.off()


create_fea(tr)
free()

tr <- na.omit(tr) # remove rows with NA to save memory
free()

idx <- tr[date <= max(date)-h, which = TRUE] # indices for training
y <- tr$sales
tr[, c("id", "sales", "date") := NULL]
free()

tr <- data.matrix(tr)
free()

cats <- c("item_id", "store_id", "state_id", "dept_id", "cat_id", 
          "wday", "mday", "week", "month", "year",
          "snap_CA", "snap_TX", "snap_WI") # list of categorical features
xtr <- lgb.Dataset(tr[idx, ], label = y[idx], categorical_feature = cats) # construct lgb dataset
xval <- lgb.Dataset(tr[-idx, ], label = y[-idx], categorical_feature = cats)

rm(tr, y, cats, idx)
free()


p <- list(objective = "poisson",
          metric ="rmse",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = cores)

m_lgb <- lgb.train(params = p,
                   data = xtr,
                   nrounds = 4000,
                   valids = list(val = xval),
                   early_stopping_rounds = 400,
                   eval_freq = 400)

cat("Best score:", m_lgb$best_score, "at", m_lgb$best_iter, "iteration")   

imp <- lgb.importance(m_lgb)

rm(xtr, xval, p)
free()

pdf('plot3.pdf')

imp[order(-Gain)
    ][1:15, ggplot(.SD, aes(reorder(Feature, Gain), Gain)) +
        geom_col(fill = "steelblue") +
        xlab("Feature") +
        coord_flip() +
        theme_minimal()]


dev.off()

te <- create_dt(FALSE, nrows)

for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
  cat(as.character(day), " ")
  tst <- te[date >= day - max_lags & date <= day]
  create_fea(tst)
  tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
  te[date == day, sales := 1.03*predict(m_lgb, tst, n_jobs = cores)]
}


te[, .(sales = unlist(lapply(.SD, sum))), by = "date", .SDcols = "sales"
   ][, ggplot(.SD, aes(x = date, y = sales, colour = (date < fday))) +
       geom_line() + 
       geom_smooth(method='lm', formula= y~x, se = FALSE, linetype = 2, size = 0.3, color = "gray20") + 
       labs(x = "", y = "total sales") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
       scale_x_date(labels=scales::date_format ("%b %d"), breaks=scales::date_breaks("14 day"))]


te[date >= fday
   ][date >= fday+h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "sales")
         ][, fwrite(.SD, "sub_dt_lgbV2.csv")]
         

         
