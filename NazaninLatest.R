library(flexdashboard)
library(tidyquant) 
library(plotly) 
library(timetk)
library(shiny)
library(tidyr)
library(knitr)
library(ggplot2)
library(forcats)


tick <- c('ACITX','PTTRX','VTINX','VTXVX','VTWNX','VTTVX','VTHRX','VTTHX','VFORX','VTIVX','VFIFX','VFFVX','VTTSX','VLXVX','VEIPX','SVSPX','TRBCX','SSMLX',
          'VSMAX','NSVAX','PRDSX','SSAIX','VWIGX','EW')

names = c('American Century Inflation Adjusted Bond','PIMCO Total Return Fund Institutional','Vanguard Target Retirement Income Fund',
          'Vanguard Target Retirement 2015 Fund','Vanguard Target Retirement 2020 Fund','Vanguard Target Retirement 2025 Fund',
          'Vanguard Target Retirement 2030 Fund','Vanguard Target Retirement 2035 Fund','Vanguard Target Retirement 2040 Fund',
          'Vanguard Target Retirement 2045 Fund','Vanguard Target Retirement 2050 Fund','Vanguard Target Retirement 2055 Fund',
          'Vanguard Target Retirement 2060 Fund','Vanguard Target Retirement 2065 Fund','Vanguard Equity-Income Fund Investor',
          'State Street S&P 500 Index Fund','T. Rowe Price Blue Chip Growth Fund','State Street Small/Mid Cap Equity Index',
          'Vanguard Small-Cap Index Fund','Columbia Small Cap Value Fund II Institutional','T. Rowe Price QM U.S. Small-Cap Growth',
          'State Street International Stock Fund','Vanguard International Growth Fund','Edwards Lifesciences Corp')

# This is where you specify weights in the same order as above ticks (here as a default I put all the weights on EW) Note: weights must add up to one
wts = c(0.3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.7)


# Data (price) Retrieval for the past 10 years #######################################
price_data <- tq_get(tick,
                     from = '2010-01-01',
                     to = '2020-10-05',
                     get = 'stock.prices')

# Calculating Returns ################################################################
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

log_ret_xts[is.na(log_ret_xts)] = 0

# Return, Risk and Sharpe Ratio of your chosen portfolio (based on the wts that you created)
mean_ret <- colMeans(log_ret_xts)
cov_mat <- cov(log_ret_xts) * 252
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
sharpe_ratio <- port_returns/port_risk

print(paste("---Annualized Return: ",port_returns,"---Annualized Risk(std): ",port_risk,"---Sharpe Ratio: ",sharpe_ratio))


################################################### Here we do 10000 simulations to find the optimal portfolio weights ##########3
num_port = 10000

all_wts = matrix(nrow  = num_port,ncol = length(tick))
port_returns_vec = vector('numeric',length = num_port)
port_risk_vec = vector('numeric',length = num_port)
port_SR_vec = vector('numeric',length = num_port)

for (i in seq_along(port_returns_vec)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns_vec[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk_vec[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  port_SR_vec[i] <- sr
  
}

portfolio_values <- tibble(Return = port_returns_vec,
                           Risk = port_risk_vec,
                           SharpeRatio = port_SR_vec)

all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

############################### Finding Minimum Variance Portfolio and Tangency Portfolio (The one with highest Sharpe Ratio) ##############3
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

############################### Ploting the weights of Minimum Variance Portfolio ##########################################################
p <- min_var %>%
  gather(tick, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

################################ Ploting the weights of tangency portfolio #####################################3
q <- max_sr %>%
  gather(tick, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(q)

##################################### Plotting all the random portfolios and visualize the efficient frontier #####
eff <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.15, y = 0.17, label = "Tangency Portfolio") +
  annotate('text', x = 0.15, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.14, xend = 0.105,  y = 0.01, 
           yend = 0.08, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.15, xend = 0.12,  y = 0.17, 
           yend = 0.10, color = 'red', arrow = arrow(type = "open"))

ggplotly(eff)


dict = data.frame('Fund'=names,'Ticker'=tick)
dict










