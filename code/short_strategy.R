source("code/lib.R")

data = readRDS("data/prepared_ta.RDS")

head(data)
tail(data)


## SMA EMA crossover

strat <- data %>%
  mutate(
    # detect SMA_50 crossing up or down relative to 1
    sma50_cross_down = (lag(SMA_50) > 1  & SMA_50 <= 1),
    
    # conditions
    short_cond = (SMA_20 < 1 & EMA_8 < 1),
    
    # final signal
    signal = case_when(
      sma50_cross_down & short_cond ~ -1,
      TRUE ~ 0
    )
  )



backtest_short <- function(df, stop_pct = 0.02, fee = 0.001) {
  trades <- list()
  
  i <- 1
  while (i <= nrow(df)) {
    if (df$signal[i] != 0) {
      side <- ifelse(df$signal[i] == 1, "long", "short")
      entry_time  <- df$period[i]
      raw_entry_price <- df$close[i]
      
      # fee-adjusted entry
      entry_price <- if (side == "long") {
        raw_entry_price * (1 + fee)
      } else {
        raw_entry_price * (1 - fee)
      }
      
      exit_time <- NA
      raw_exit_price <- NA
      exit_price <- NA
      exit_trail_stop <- NA
      
      # set initial trailing reference
      if (side == "long") {
        trail_ref <- raw_entry_price
      } else {
        trail_ref <- raw_entry_price
      }
      
      # loop forward to check trailing stop
      for (j in (i+1):nrow(df)) {
        price <- df$close[j]
        
        if (side == "long") {
          trail_ref <- max(trail_ref, price)
          trail_stop <- trail_ref * (1 - stop_pct)
          if (price <= trail_stop) {
            exit_time <- df$period[j]
            raw_exit_price <- price
            exit_price <- raw_exit_price * (1 - fee)
            exit_trail_stop <- trail_stop
            i <- j
            break
          }
        } else {
          trail_ref <- min(trail_ref, price)
          trail_stop <- trail_ref * (1 + stop_pct)
          if (price >= trail_stop) {
            exit_time <- df$period[j]
            raw_exit_price <- price
            exit_price <- raw_exit_price * (1 + fee)
            exit_trail_stop <- trail_stop
            i <- j
            break
          }
        }
      }
      
      # if no stop hit, exit at last bar
      if (is.na(exit_time)) {
        exit_time <- df$period[nrow(df)]
        raw_exit_price <- df$close[nrow(df)]
        exit_price <- if (side == "long") {
          raw_exit_price * (1 - fee)
        } else {
          raw_exit_price * (1 + fee)
        }
        exit_trail_stop <- if (side == "long") {
          trail_ref * (1 - stop_pct)
        } else {
          trail_ref * (1 + stop_pct)
        }
        i <- nrow(df)
      }
      
      trades[[length(trades)+1]] <- data.frame(
        entry_time,
        side,
        entry_price,
        exit_time,
        exit_price,
        trailing_ref = trail_ref,
        trailing_stop = exit_trail_stop,
        return = if (side == "long") {
          (exit_price / entry_price) - 1
        } else {
          (entry_price / exit_price) - 1
        }
      )
    }
    i <- i + 1
  }
  
  trades <- do.call(rbind, trades)
  return(trades)
}

# Example usage

results <- backtest_short(strat, stop_pct = 0.02, fee = 0.0001)
print(head(results))


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

