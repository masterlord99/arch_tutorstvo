source("code/lib.R")

data <- readRDS("~/Desktop/backtest/00_BIG_RULE/data/spx_min.RDS")

# head(data)
# tail(data)


# ensure datetime is POSIXct (adjust tz if you need a specific timezone)
# data <- data %>%
#   mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

# floor to 15-min period and aggregate
oh15 <- data %>%
  mutate(period = floor_date(datetime, "5 minutes")) %>%
  group_by(period) %>%
  summarise(
    open  = open[which.min(datetime)],   # open at earliest minute in the period
    high  = max(high, na.rm = TRUE),
    low   = min(low,  na.rm = TRUE),
    close = close[which.max(datetime)],  # close at latest minute in the period
    .groups = "drop"
  ) %>%
  arrange(period)

system.time({
  
  for(i in 1:nrow(oh15)){
    ## do some strat
    2+2
  }
  
})


data = oh15 %>% as.data.frame()

class(data)

data$SMA_20 = data$close/SMA(data$close,20)
data$SMA_50 = data$close/SMA(data$close,50)
data$EMA_8 = data$close/EMA(data$close,8)

data = na.omit(data)

data$wday = wday(label = T,x = data$period)
data$hour = hour(data$period)
data$minute = minute(data$period)

data$EC = 0
data$EC[data$wday=="Fri" & data$hour==15 & data$minute>=50] = 1

saveRDS(data,"data/prepared_ta.RDS")


