This R project analyzes the performance and risk of an equal-weighted portfolio composed of three major tech stocks: Apple (AAPL), Microsoft (MSFT), and Google (GOOG) over the period from January 1, 2023 to January 1, 2024.

🔧 What the script does:
Retrieves historical adjusted closing prices using quantmod

Calculates daily log returns for each stock

Constructs an equal-weight portfolio (1/3 weight per stock)

Computes annualized standard deviation (volatility) and Sharpe Ratio

Merges individual and portfolio returns for comparison

Plots daily return trajectories for all assets

📈 Key Metrics:
Annualized Volatility of each stock and the portfolio

Annualized Sharpe Ratio (risk-adjusted return, assuming risk-free rate = 0)

