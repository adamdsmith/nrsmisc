repair_monthly <- function(dat, start, end) {
  n_days <- as.numeric(difftime(lubridate::ymd(end),
                                lubridate::ymd(start), units = "days")) + 1
  repair <- (n_days * 240 == nrow(dat)) && all(lubridate::hour(dat$t) == 0)
  if (repair) {
    tmp <- expand.grid(m = seq(0, 54, 6), h = 0:23, s = 0)
    # Create string for HMS conversion
    tmp <- apply(tmp, 1, function(cols) paste(cols[2], cols[1], cols[3]))
    dat$t <- lubridate::ymd_hms(paste(dat$t, tmp))
    message("Missing time information on sub-hourly data; attempted repair.\n")
  }
  dat
}
