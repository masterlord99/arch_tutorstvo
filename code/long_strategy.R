source("code/lib.R")

data = readRDS("data/prepared_ta.RDS")

head(data)
tail(data)

# data$EC = 0

## SMA EMA crossover

strat <- data %>%
  mutate(
    # detect SMA_50 crossing from below 1 to above 1
    sma50_cross_up = (lag(SMA_50) < 1 & SMA_50 >= 1),
    
    # condition: SMA_20 > 1 and EMA_8 > 1
    cond = (SMA_20 > 1 & EMA_8 > 1),
    
    # final signal
    signal = ifelse(sma50_cross_up & cond, 1, 0)
  )
strat = na.omit(strat)

## vtopsva v trejd ko je 1
## assumption vstopiva na close
## trail 3% sl


library(dplyr)

backtest <- function(df, stop_pct = 0.02, fee = 0.0001,sl=1) {
  trades <- list()
  
  i <- 1
  while (i <= nrow(df)) {
    if (df$signal[i] == 1) {
      entry_time  <- df$period[i]
      raw_entry_price <- df$close[i]
      
      # fee-adjusted entry
      entry_price <- raw_entry_price * (1 + fee)
      
      max_close <- raw_entry_price
      exit_time <- NA
      raw_exit_price <- NA
      exit_price <- NA
      exit_trail_stop <- NA
      
      hard_stop_price = entry_price*(1-sl)
      # loop forward after entry
      for (j in (i+1):nrow(df)) {
        price <- df$close[j]
        low = df$low[j]
        
        # update trailing max
        max_close <- max(max_close, price)
        trail_stop <- max_close * (1 - stop_pct)
        
        # calculate current trade PnL (before fee on exit)
        current_pnl <- price - entry_price / (1 + fee)  # optional adjustment
        
        
        if (low <= hard_stop_price) {
          print("Triggering!")
          exit_time <- df$period[j]
          raw_exit_price <- hard_stop_price
          exit_price <- raw_exit_price * (1 - fee)
          exit_hard_stop <- hard_stop_price
          exit_trail_stop <- trail_stop
          i <- j
          break
        }
        
        # exit conditions
        if (price <= trail_stop) {
          exit_time <- df$period[j]
          raw_exit_price <- price
          exit_price <- raw_exit_price * (1 - fee)
          exit_trail_stop <- trail_stop
          i <- j
          break
        }
        
        if (df$EC[j] == 1 && price < entry_price) {  # EC exit if PnL < 0
          exit_time <- df$period[j]
          raw_exit_price <- price
          exit_price <- raw_exit_price * (1 - fee)
          exit_trail_stop <- trail_stop
          i <- j
          break
        }
      }
      
      # if never exited, close at last bar
      if (is.na(exit_time)) {
        exit_time <- df$period[nrow(df)]
        raw_exit_price <- df$close[nrow(df)]
        exit_price <- raw_exit_price * (1 - fee)
        exit_trail_stop <- max_close * (1 - stop_pct)
        i <- nrow(df)
      }
      
      trades[[length(trades)+1]] <- data.frame(
        entry_time,
        entry_price,
        exit_time,
        exit_price,
        trailing_max = max_close,
        trailing_stop = exit_trail_stop,
        return = (exit_price / entry_price) - 1
      )
    }
    i <- i + 1
  }
  
  trades <- do.call(rbind, trades)
  return(trades)
}
# Run backtest
results <- backtest(strat, stop_pct = 0.02,sl=0.01)
tail(results)


# prepare equity curve with compounding
equity <- results %>%
  arrange(entry_time) %>%
  mutate(cum_equity = cumprod(1 + return) - 1)

# compute stats
winrate <- mean(results$return > 0)

sharpe <- mean(results$return) / sd(results$return) * sqrt(nrow(results))
sortino <- mean(results$return) / sd(pmin(results$return, 0)) * sqrt(nrow(results))

title_text <- sprintf(
  "Cumulative Return | Sharpe: %.2f | Sortino: %.2f | Winrate: %.1f%%",
  sharpe, sortino, winrate * 100
)

# plot
ggplot(equity, aes(x = entry_time, y = cum_equity)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = title_text,
    x = "Time",
    y = "Cumulative Return"
  )

