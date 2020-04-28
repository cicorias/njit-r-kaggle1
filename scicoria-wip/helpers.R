


get_m5_files <- function() {

  data_dir = './data'
  current_dir = getwd()
  dir.create(file.path(current_dir, data_dir), showWarnings = FALSE)


  files = c('https://scicorianjit.blob.core.windows.net/njit/m5/calendar.csv',
            'https://scicorianjit.blob.core.windows.net/njit/m5/sell_prices.csv',
            'https://scicorianjit.blob.core.windows.net/njit/m5/sales_train_validation.csv',
            'https://scicorianjit.blob.core.windows.net/njit/m5/sample_submission.csv')


  for (f in files) {
    target <- basename(f)
    target <- file.path('./data', target)
    # using mode 'wb' to ensure line endings are LF and not CRLF x-plat
    download.file(f, target) #, mode = 'wb')
  }
}


# creattes a data.table.

create_dt <- function(is_train = TRUE, nrows = Inf) {
  library(data.table)

  prices <- fread("./data/sell_prices.csv")
  cal <- fread("./data/calendar.csv")
  cal[, `:=`(date = as.IDate(date, format="%Y-%m-%d"),
             is_weekend = as.integer(weekday %chin% c("Saturday", "Sunday")))]

  if (is_train) {
    dt <- fread("./data/sales_train_validation.csv", nrows = nrows)
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


