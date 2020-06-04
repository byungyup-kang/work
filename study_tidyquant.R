rm(list = ls())

library(tidyverse)
library(tidyquant)

Ra <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get = "stock.prices", from = "2010-01-01", to = "2015-12-31") %>%
    group_by(symbol) %>% 
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly", col_rename = "Ra")

Rb <- "XLK" %>%
    tq_get(get = "stock.prices", from = "2010-01-01", to = "2015-12-31") %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly", col_rename = "Rb")

RaRb <- left_join(Ra, Rb, by = c("date" = "date"))

RaRb_capm <- RaRb %>% tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
RaRb_capm
RaRb_capm %>% select(symbol, Alpha, Beta)

#####   #####   #####   #####   #####   #####   #####   #####   #####   #####

data(FANG)

monthly_returns_stocks <- FANG %>% group_by(symbol) %>% tq_transmute(adjusted, periodReturn, period = "monthly")

# Method 1: Use tq_portfolio with numeric vector of weights
weights <- c(0.50, 0.25, 0.25, 0)
tq_portfolio(data = monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights,
             col_rename = NULL,
             wealth.index = FALSE)

# Method 2: Use tq_portfolio with two column tibble and map weights
weights_df <- tibble(symbol = c("FB", "AMZN", "NFLX"), weights = c(0.50, 0.25, 0.25))
tq_portfolio(data = monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_df,
             col_rename = NULL,
             wealth.index = T)

# Method 3: Working with multiple portfolios
### 3A: Duplicate monthly_returns_stocks multiple times
mult_monthly_returns_stocks <- tq_repeat_df(monthly_returns_stocks, n = 4)
### 3B: Create weights table grouped by portfolio id
weights <- c(0.50, 0.25, 0.25, 0.00,
             0.00, 0.50, 0.25, 0.25,
             0.25, 0.00, 0.50, 0.25,
             0.25, 0.25, 0.00, 0.50)
stocks <- c("FB", "AMZN", "NFLX", "GOOG")
weights_table <- tibble(stocks) %>%
    tq_repeat_df(n = 4) %>%
    bind_cols(tibble(weights)) %>%
    group_by(portfolio)
### 3C: Scale to multiple portfolios
tq_portfolio(data = mult_monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_table,
             col_rename = NULL,
             wealth.index = FALSE)

