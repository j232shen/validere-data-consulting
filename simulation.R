#' Run portfolio backtest with monthly rebalancing.
#'
#' Uses a rolling window to optimize weights and simulates portfolio returns
#' based on the rebalancing schedule and given specification.
#'
#' @param returns xts object of asset returns
#' @param pspec Portfolio specification object (from PortfolioAnalytics)
#' @return A list with optimized weights (xts) and backtested returns (xts)
optimize_portfolio <- function(returns, pspec) {
  opt.rebal <- optimize.portfolio.rebalancing(returns, pspec,
                                              optimize_method="CVXR",
                                              rebalance_on="months",
                                              # training_period=3 * 252, # 3 years is the default
                                              rolling_window=3 * 252)
  
  # extract optimal weights from regime-switching optimization  
  weights <- extractWeights(opt.rebal)  
  weights <- weights[complete.cases(weights),]  
  
  # compute optimized portfolio returns  
  returns <- Return.rebalancing(clean_returns, weights)  # rebalancing covered by weights object
  
  # get annualized performance metrics  
  summary_results <- summary(opt.rebal)  
  annualized_return <- summary_results$annualized_returns 
  
  # # plot backtest results
  # backtest.plot(returns)
  
  return(list(weights = weights, returns = returns))
}


#' Simulate DCA portfolio growth using optimal weights.
#'
#' Applies monthly cash inflows and fully rebalances according to specified weights,
#' computing the resulting portfolio value over time.
#'
#' @param prices_xts xts object of asset prices
#' @param weights_xts xts object of portfolio weights (aligned to rebalance dates)
#' @param inflow_amount Monthly cash contribution (default = 1000)
#' @param initial_cash Starting cash balance (default = 0)
#' @return xts object of simulated portfolio values
simulate_dca <- function(prices_xts, weights_xts, inflow_amount = 1000, initial_cash = 0) {
  dates <- index(weights_xts)
  n_assets <- ncol(weights_xts)
  holdings <- rep(0, n_assets)
  cash <- initial_cash
  portfolio_value <- numeric(length(dates))
  
  # align prices to weights dates
  prices_xts <- prices_xts[dates, ]
  
  for (i in seq_along(dates)) {
    # add new cash inflow
    cash <- cash + inflow_amount
    
    # get prices for this date
    prices <- as.numeric(prices_xts[i, ])
    
    # FULL REBALANCING: allocate total portfolio value according to weights
    w <- as.numeric(weights_xts[i, ])
    current_holdings_value <- sum(holdings * prices)
    total_value <- current_holdings_value + cash
    target_value <- total_value * w
    holdings <- target_value / prices
    holdings[is.na(holdings)] <- 0  # handle any NA prices
    cash <- 0  # all cash invested
    
    # calculate portfolio value for this period
    portfolio_value[i] <- sum(holdings * prices)
  }
  
  # return as xts
  portfolio_value_xts <- xts(portfolio_value, order.by = dates)
  colnames(portfolio_value_xts) <- "Portfolio.Value"
  return(portfolio_value_xts)
}