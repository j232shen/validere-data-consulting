#' Calculate IRR from DCA portfolio values.
#'
#' Computes the internal rate of return (IRR) assuming regular monthly contributions
#' and using the final portfolio value as the terminal cash flow.
#'
#' @param dca_portfolio xts or numeric vector of portfolio values
#' @param monthly_contribution Amount contributed each month (default = 1000)
#' @return A list with monthly and annual IRR
calculate_irr <- function(dca_portfolio, monthly_contribution = 1000) {
  n_periods <- length(dca_portfolio)
  
  # construct cash flows: -1000 each month, final portfolio value as last cash flow
  cash_flows <- rep(-monthly_contribution, n_periods)
  cash_flows[n_periods] <- as.numeric(dca_portfolio[n_periods])  # replace last inflow with final portfolio value
  
  # IRR using uniroot
  monthly_irr <- uniroot(
    function(r) sum(cash_flows * (1 + r)^-(0:(n_periods - 1))),
    interval = c(-0.99, 1),  # allow for negative returns
    tol = 1e-8
  )$root
  
  annual_irr <- (1 + monthly_irr)^12 - 1
  
  return(list(monthly = monthly_irr, annual = annual_irr))
}


#' Compute drawdowns at each time point.
#'
#' Measures percentage drop from running peak in portfolio value.
#'
#' @param dca_portfolio xts or numeric vector of portfolio values
#' @return Numeric vector of drawdown values
calculate_drawdowns <- function(dca_portfolio) {
  cummax_values <- cummax(dca_portfolio)
  drawdowns <- (dca_portfolio / cummax_values) - 1
  
  return(drawdowns)
}


#' Compute maximum drawdown.
#'
#' Returns the largest peak-to-trough decline in portfolio value.
#'
#' @param portfolio_values xts or numeric vector of portfolio values
#' @return Maximum drawdown as a negative proportion
calculate_max_drawdown <- function(portfolio_values) {
  cummax_values <- cummax(portfolio_values)
  drawdowns <- (portfolio_values / cummax_values) - 1
  max_dd <- min(drawdowns, na.rm = TRUE)
  return(max_dd)
}


#' Calculate annualized portfolio volatility.
#'
#' Computes the standard deviation of log returns, scaled by frequency.
#'
#' @param portfolio_values xts or numeric vector of portfolio values
#' @param frequency Number of periods per year (default = 12 for monthly)
#' @return Annualized volatility
calculate_portfolio_volatility <- function(portfolio_values, frequency = 12) {
  # calculate log returns from portfolio values
  log_returns <- diff(log(portfolio_values))
  # annualized volatility
  volatility <- sd(log_returns, na.rm = TRUE) * sqrt(frequency)
  return(volatility)
}


#' Calculate modified Sharpe ratio.
#'
#' Computes the Sharpe ratio using IRR as the return and volatility from portfolio values.
#'
#' @param irr Annualized return
#' @param portfolio_values xts or numeric vector of portfolio values
#' @param risk_free_rate Annual risk-free rate (default = 0.03)
#' @return Modified Sharpe ratio
calculate_modified_sharpe <- function(irr, portfolio_values, risk_free_rate = 0.03) {
  excess_return <- irr - risk_free_rate
  volatility <- calculate_portfolio_volatility(portfolio_values)
  sharpe <- excess_return / volatility
  return(sharpe)
}


#' Calculate Sortino ratio.
#'
#' Computes risk-adjusted return based on downside deviation only.
#'
#' @param irr Annualized return
#' @param portfolio_values xts or numeric vector of portfolio values
#' @param risk_free_rate Annual risk-free rate (default = 0.03)
#' @return Sortino ratio
calculate_sortino_ratio <- function(irr, portfolio_values, risk_free_rate = 0.03) {
  # calculate monthly returns from portfolio values
  returns <- diff(log(portfolio_values))
  # downside deviation (only negative returns)
  negative_returns <- returns[returns < 0]
  if(length(negative_returns) == 0) {
    downside_deviation <- 0
  } else {
    downside_deviation <- sd(negative_returns, na.rm = TRUE) * sqrt(12)  # annualized
  }
  
  excess_return <- irr - risk_free_rate
  if(downside_deviation == 0) {
    sortino <- Inf  # perfect - no downside
  } else {
    sortino <- excess_return / downside_deviation
  }
  return(sortino)
}


#' Calculate time spent in drawdown.
#'
#' Returns the proportion of periods where the portfolio is below its peak.
#'
#' @param portfolio_values xts or numeric vector of portfolio values
#' @return Proportion of time in drawdown
calculate_time_in_drawdown <- function(portfolio_values) {
  cummax_values <- cummax(portfolio_values)
  drawdowns <- (portfolio_values / cummax_values) - 1
  time_in_dd <- mean(drawdowns < -0.001, na.rm = TRUE)  # allow for small rounding errors
  return(time_in_dd)
}


#' Calculate average drawdown.
#'
#' Computes the average drop from peak during drawdown periods.
#'
#' @param portfolio_values xts or numeric vector of portfolio values
#' @return Average drawdown during drawdown periods
calculate_average_drawdown <- function(portfolio_values) {
  cummax_values <- cummax(portfolio_values)
  drawdowns <- (portfolio_values / cummax_values) - 1
  drawdown_periods <- drawdowns[drawdowns < -0.001]  # only periods in drawdown
  
  if(length(drawdown_periods) == 0) {
    avg_dd <- 0
  } else {
    avg_dd <- mean(drawdown_periods, na.rm = TRUE)
  }
  return(avg_dd)
}


#' Calculate Calmar ratio.
#'
#' Returns risk-adjusted return using IRR divided by maximum drawdown.
#'
#' @param irr Annualized return
#' @param portfolio_values xts or numeric vector of portfolio values
#' @return Calmar ratio
calculate_calmar_ratio <- function(irr, portfolio_values) {
  max_dd <- abs(calculate_max_drawdown(portfolio_values))
  if(max_dd == 0) {
    calmar <- Inf
  } else {
    calmar <- irr / max_dd
  }
  return(calmar)
}


#' Perform full risk analysis for a DCA strategy.
#'
#' Computes IRR, drawdowns, volatility, Sharpe, Sortino, and Calmar ratios.
#'
#' @param portfolio_values xts or numeric vector of portfolio values
#' @param irr Annualized return
#' @param risk_free_rate Annual risk-free rate (default = 0.03)
#' @param strategy_name Name of the strategy for labeling
#' @return A named list of risk metrics
analyze_dca_risk <- function(portfolio_values, irr, risk_free_rate = 0.03, strategy_name = "Portfolio") {
  
  risk_metrics <- list(
    strategy = strategy_name,
    irr = irr,
    max_drawdown = calculate_max_drawdown(portfolio_values),
    volatility = calculate_portfolio_volatility(portfolio_values),
    modified_sharpe = calculate_modified_sharpe(irr, portfolio_values, risk_free_rate),
    sortino_ratio = calculate_sortino_ratio(irr, portfolio_values, risk_free_rate),
    time_in_drawdown = calculate_time_in_drawdown(portfolio_values),
    avg_drawdown = calculate_average_drawdown(portfolio_values),
    calmar_ratio = calculate_calmar_ratio(irr, portfolio_values)
  )
  
  return(risk_metrics)
}


#' Compare risk metrics across multiple DCA strategies.
#'
#' Builds a summary table comparing IRR, drawdowns, Sharpe/Sortino, and other risk stats.
#'
#' @param portfolio_list List of xts or numeric vectors of portfolio values
#' @param irr_list List of annual IRRs for each strategy
#' @param strategy_names Character vector of strategy labels
#' @param risk_free_rate Annual risk-free rate (default = 0.03)
#' @return A data frame of comparative risk metrics
create_risk_comparison <- function(portfolio_list, irr_list, strategy_names, risk_free_rate = 0.03) {
  
  risk_results <- list()
  
  for(i in seq_along(portfolio_list)) {
    risk_results[[i]] <- analyze_dca_risk(
      portfolio_list[[i]], 
      irr_list[[i]], 
      risk_free_rate, 
      strategy_names[i]
    )
  }
  
  # convert to data frame
  risk_df <- data.frame(
    Strategy = sapply(risk_results, function(x) x$strategy),
    IRR = round(sapply(risk_results, function(x) x$irr) * 100, 2),
    Max_Drawdown_pct = round(sapply(risk_results, function(x) x$max_drawdown) * 100, 2),
    Volatility_pct = round(sapply(risk_results, function(x) x$volatility) * 100, 2),
    Modified_Sharpe = round(sapply(risk_results, function(x) x$modified_sharpe), 3),
    Sortino_Ratio = round(sapply(risk_results, function(x) x$sortino_ratio), 3),
    Time_in_DD_pct = round(sapply(risk_results, function(x) x$time_in_drawdown) * 100, 1),
    Avg_DD_pct = round(sapply(risk_results, function(x) x$avg_drawdown) * 100, 2),
    Calmar_Ratio = round(sapply(risk_results, function(x) x$calmar_ratio), 3)
  )
  
  return(risk_df)
}