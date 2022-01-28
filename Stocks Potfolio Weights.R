#----------------------------------------------------------------#
# This code does not tell you where to invest automatically. 
# It is your responsibility to use it wisely and with criticism, depending on your needs and goals.  
#----------------------------------------------------------------# 

# Purpose of the Code: Download Stock Prices from Yahoo Finance and create a Portfolio (Only Stocks).

# 1ST STEP:  
##--- Download the needed packages. ---## 
install.packages("TTR")                       # 1st Step: Install TTR package.
install.packages("quantmod")                  # 2nd Step: Install quandom package.
library(quantmod)                             # 3rd Step: Load quandom package.
install.packages("tidyquant")                 # 4th Step: Install tidyquant package.
library(tidyquant)                            # 5th Step: Load tidyquant.
installed.packages("PerformanceAnalytics")    # 6th Step: Install PerformanceAnalytics package.
library(PerformanceAnalytics)                 # 7th Step: Load PerformanceAnalytics package.
library(xts)                                  # 8th Step: Load xts package.
install.packages("dygraphs")                  # 9th Step: Install dygraphs package. 
library(dygraphs)                             # 10th Step: Load dygraphs package. 
install.packages("moments")                   # 11th Step: Install moments package. Need for Skewness and Kurtosis. 
library(moments)                              # 12th Step: Load moments package.
library(tseries)                              # 13th Step: Load tseries package
install.packages("PortfolioAnalytics")        # 14th Step: Install PortfolioAnalytics. 
library(PortfolioAnalytics)                   # 15th Step: Load PortfolioAnalytics. 
install.packages("corrplot")                  # 16th Step: Install corrplot package.
library(corrplot)                             # 17th Step: Load corrplot package.
install.packages("ROI.plugin.quadprog")       # 18th Step: Install ROI.plugin.quadprog package.
library(ROI.plugin.quadprog)                  # 19th Step: Load ROI.plugin.quadprog package. 
options(max.print = 1000000)                  # 20th Step: (Optional) Maximize the layout of your data.  

# 2ND STEP: 
##--- Download Financial Data (Stocks). ---## 
# Note: 1) You can assign them to a variable or not. 
#       2) You can add as many symbols as you want. Make sure you use the right ones. 
#       3) Specify the period YYYY-MM-DD. Sys.Date() provides you the current date. 
# 1st Step: Download the stocks that you want. Use getSymbols().
Stocks <- getSymbols(c("AAPL","UPS", "IBM","NFLX", "MA", "PYPL","ABBV"), scr="yahoo", from = "2017-01-01", to = Sys.Date()) 

# 3RD STEP (optional): 
##--- Observe your data & Graphical representation. ---## 
# 1st Step: Observe your data. -> Just type the symbol or View(symbol).
# Note: You can do it for all your data. 
View(AAPL) 
# 2nd Step: Get & Assign specific data to a variable.
# Note: If you need a specific variable you should use one the functions : Op() , Hi(), Lo(), Cl(), Vo(), Ad(). 
# Note: If you need multiple variables you should use one of the functions: HLC(), OHLCV()
# Note: You should assign to a specific variable. Example op_variable <- Op(Symbol or Variable name). 
# Note: You should do it for all your data.
Cl_UPS <- Cl(UPS)
# 3rd Step: Graphical Representation of your data.
# 3a. For ONE time series.
#Note: dygraph is interactive. 
dygraph(Cl_AAPL, main = "Closing Price APPLE", ylab = "Price", xlab = "Time") 
# 3b. For MORE THAN ONE time series.
# 1st Step: Combine data and assign them to a variable. 
closing_prices <-cbind(Cl_AAPL,Cl_ABBV,Cl_IBM,Cl_MA,Cl_NFLX,Cl_PYPL,Cl_UPS)
# 2nd Step: Plot the data. 
dygraph(closing_prices, main = "Closing Price") %>%
  dySeries("AAPL.Close", label = "AAPL") %>%                                   
  dySeries("ABBV.Close", label = "ABBV") %>% 
  dySeries("IBM.Close", label = "IBM") %>% 
  dySeries("MA.Close", label = "MA") %>%
  dySeries("NFLX.Close", label = "NFLX") %>%
  dySeries("PYPL.Close", label = "PYPL") %>%
  dySeries("UPS.Close", label = "UPS") %>%
  dyAxis("y", label = "Stock Price (Dollars)") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyLegend(width = 400) %>%
  dyRangeSelector() 

# 4TH STEP (optional): 
##--- Download Splits and Dividends.---## 
# Note: Apply this step only if you want to get the splits and dividends separately. 
# Note: You can apply it for each stock. 
# 1st Step: Download the Stock's split data.
aapl_splits <-getSplits("AAPL")
# 2nd Step: Download the Stock's dividend data. 
aapl_divid <- getDividends("AAPL")
# 3rd Step: Download AAPL dividend data that is not split-adjusted.
aapl_raw_divid <- getDividends("AAPL", split.adjust = FALSE)

#-------------------------------------------------------------------------------#
# Note: The importance of using adjust for stock splits and dividends: 
# a) Stock splits can create large historical price changes even though they do not change the value of the company. So, you must adjust all pre-split prices in order to calculate historical returns correctly.
# b) Similarly, you must adjust all pre-dividend prices. Dividends do reduce the company's value by the amount of the dividend payment, but the investor's return isn't affected because they receive the offsetting dividend payment.
# Hence, you should use the adjusted prices provided by Yahoo or create them on your own. 
#-------------------------------------------------------------------------------# 

# 5TH STEP: 
##--- Get the Adjusted closing price and their Graphical Representation. ---## 
# 1st Step: Assign adjusted price to a variable. 
Ad_AAPL <- Ad(AAPL)
Ad_ABBV <- Ad(ABBV)
Ad_IBM <- Ad(IBM)
Ad_MA <- Ad(MA)
Ad_NFLX <- Ad(NFLX)
Ad_PYPL <- Ad(PYPL)
Ad_UPS <- Ad(UPS)
# 2nd Step: Merge all the adjusted prices to one variable. 
adjusted_stocks <- cbind(Ad_AAPL,Ad_ABBV,Ad_IBM,Ad_MA,Ad_NFLX,Ad_PYPL,Ad_UPS)
# 3rd Step: Plot the data. 
dygraph(adjusted_stocks , main = "Adjusted Closing Price") %>%
  dySeries("AAPL.Adjusted", label = "AAPL") %>%                                   
  dySeries("ABBV.Adjusted", label = "ABBV") %>% 
  dySeries("IBM.Adjusted", label = "IBM") %>% 
  dySeries("MA.Adjusted", label = "MA") %>%
  dySeries("NFLX.Adjusted", label = "NFLX") %>%
  dySeries("PYPL.Adjusted", label = "PYPL") %>%
  dySeries("UPS.Adjusted", label = "UPS") %>%
  dyAxis("y", label = "Stock Price (Dollars)") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyLegend(width = 400) %>%
  dyRangeSelector()

# 6TH STEP: 
##--- Convert the daily adjusted closing price into returns. ---## 
# 1st Step: Convert the stocks' data into returns.
r.AAPL <- Return.calculate(Ad_AAPL)
r.ABBV <- Return.calculate(Ad_ABBV)
r.ΙΒΜ <- Return.calculate(Ad_IBM)
r.MA <- Return.calculate(Ad_MA)
r.NFLX <- Return.calculate(Ad_NFLX)
r.PYPL <- Return.calculate(Ad_PYPL)
r.UPS <- Return.calculate(Ad_UPS)
# 2nd Step: Since the first row has a NA value, you have to remove the first row. 
r.AAPL <- r.AAPL[-1, ]
r.ABBV <- r.ABBV[-1, ]
r.MA <- r.MA[-1, ]
r.NFLX <- r.NFLX[-1, ]
r.PYPL <- r.PYPL[-1, ]
r.UPS <- r.UPS[-1, ]
r.ΙΒΜ <- r.ΙΒΜ[-1, ]
# 3rd Step: Rename the column. 
names(r.AAPL)[1]<- 'APPL'
names(r.ABBV)[1]<- 'ABBV'
names(r.MA)[1]<- 'MA'
names(r.NFLX)[1]<- 'NFLX'
names(r.PYPL)[1]<- 'PYPL'
names(r.UPS)[1]<- 'UPS'
names(r.ΙΒΜ)[1]<- 'IBM'
# 4th Step: Graphical Representation of the returns.
# Note: Repeat the same for the other variables.
dygraph(r.AAPL, main = "APPLE Daily Returns", ylab = "Returns", xlab = "Time") 

# 7TH STEP (Optional): 
##--- Sub-period performance analysis. ---## 
#Note: Sometimes you may be interested in the performance of a specific sub-period.
#Note: You can use it for every stock and period you may want. 
# 1st Step: Create a sub-period for the returns of 2019.
r.appl_1719 <- window(r.AAPL, start = "2017-01-01", end ="2019-12-31")
# 2nd Step: Create a sub-period for the returns of 2020.
r.appl_2021 <- window(r.AAPL, start = "2020-01-01", end ="2021-12-31")
# 3rd Step: Plot the returns 
par( mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
names(r.appl_1719) <- "AAPL returns for 2019"
names(r.appl_2021) <- "AAPL returns for 2020"
# 4th Step: Create the histograms for the years. 
chart.Histogram(r.appl_1719, methods = c("add.density", "add.normal"))
chart.Histogram(r.appl_2021, methods = c("add.density", "add.normal")) 

# 8TH STEP: 
##--- Calculate specific measures for evaluating a stock price. ---## 
# It is important to transform your data into numeric in order to calculate the specific measures. 
#Note: Repeat the same steps for the other returns. 
# 1st Step: Calculate Arithmetic mean. 
num.AAPL <- as.numeric(r.AAPL)
mean_r.AAPL<- Mean.arithmetic(num.AAPL) 
# 2nd Step: Calculate Geometric mean.
geom.r.AAPL <- mean.geometric(num.AAPL)
# 3rd Step: Calculate Standard Deviation. 
std.r.AAPL<- StdDev(num.AAPL)   # Volatility  

# 9TH STEP: 
##--- Skewness & Kurtosis. ---## 
#Repeat the same steps for the other returns.
# 1st Step: Calculate Skewness. 
#There are three types of Skewness: 
# a) Zero Skewness (Distribution is symmetric)
# b) Negative Skewness (Large Negative returns occur more often than large positive returns.)
# c) Positive Skewness (Large Positive returns occur more often than large negative returns.) 
skewness(r.AAPL)
# 2nd Step: Calculate Kurtosis.
# A distribution with fat tails is a distribution in which extremely large positive or negative returns occur more often than a normal distribution would predict.
# The larger the excess kurtosis, the fatter the tails are compared to the tails of a normal distribution.
kurtosis(r.AAPL) 

# -----------------------------------------------------------------------------# 
#Note: Standard deviation gives equal weight to positive and negative returns in calculating the return variability. 
#Note: when the return distribution is asymmetric (skewed), it is usually used as an additional risk measures that focus on describing the potential losses. 
#These measures are: 
# a) The Semi-Deviation: The calculation of the variability of returns below the mean return. 
# b) VaR (Value-at-Risk): A statistic that quantifies the extent of possible financial losses over a specific time frame. 
# c) Expected Shortfall: A risk measure sensitive to the shape of the tail of the distribution of returns on a portfolio
# ! Note: Expected shortfall at % level, is the expected return on the portfolio in the worst of cases. 
#------------------------------------------------------------------------------# 

# 10TH STEP: 
##--- Other Measures. ---##
# 1st Step:Calculate Semi-Deviation. 
SemiDeviation(r.AAPL)                                                           #Semi-deviation is a method of measuring the below-mean fluctuations in the returns on investment.
# 2nd Step:Calculate Value at Risk for 2.5% and 5%.                             #Value at risk (VaR) is a statistic that quantifies the extent of possible financial losses. 
VaR(r.AAPL ,p = 0.05)                                                           #Calculate the value at risk (5%).
VaR(r.AAPL ,p = 0.025)                                                          #Calculate the value at risk (2.5%).
# How to interpret VaR: What is the largest loss I could potentially take within the next quarter such that I only have 5 / 2.5% probability of observing an even larger loss. 
# 3rd Step: Calculate the expected shortfall for 2.5% and 5%
ES(r.AAPL, p = 0.05)                                                            #Evaluates the credit risk of a portfolio at risk 5%. 
ES(r.AAPL, p = 0.025)                                                           #Evaluates the credit risk of a portfolio at risk 2.5%.
# 4th Step:See the drawdowns of Portfolio. 
table.Drawdowns(r.AAPL)                                                         #Reports the five largest drawdown episodes over a sample period. 
chart.Drawdown(r.AAPL)                                                          #Visualize the evolution of the drawdowns from each peak over time. 

# 11TH STEP: 
##--- Correlation between stocks.  ---##
# 11a. Correlation for RAIR of stocks' returns.
# 1st Step: Calculate the correlation. 
cor(r.AAPL,r.NFLX)
# 2nd Step:Scatter Plot for pair returns.
chart.Scatter(r.AAPL,r.NFLX, xlab = "AAPL returns", ylab = "NFLX returns", main = "AAPL-NFLX returns")
# 3rd Step: Visualize the rolling correlation estimates by using chart.RollingCorrelation()
chart.RollingCorrelation(r.AAPL, r.NFLX, width = 30)                            # Width defines the rolling number. 
# 11b. Correlation for MORE than two stocks. 
# 1st Step: Merge the returns. 
returns <- merge(r.AAPL,r.ABBV,r.MA,r.NFLX,r.PYPL, r.UPS,r.ΙΒΜ)
# 2nd Step: Calculate the correlation & table.
corrplot::corrplot(cor(returns), method = 'number')

# 12TH STEP:
##--- Covariance between stocks. ---##
# 1st Step: Calculate the covariance matrix of returns.
cov_matrix <- cov(returns) 

#------------------------------------------------------------------------------#
#After selecting the assets - stocks that you wanted. Continue with the following steps.
# Depending on what strategy and goal you want to accomplish choose one of the following strategies.
#------------------------------------------------------------------------------#

# 13TH STEP: 
# MANDATORY Step: Specify the returns of the stock that you will use. 
returns <- merge(r.AAPL,r.ABBV,r.MA,r.NFLX,r.PYPL,r.UPS,r.ΙΒΜ) 

# 14TH STEP: 
##--- Create a Benchmark. ---##
# Note: The Benchmark can be very useful in order to compare how weights are distributed after the optimization. 
# 1st Step: Create a vector of equal weights. 
equal_weights <- rep(1 / ncol(returns), ncol(returns)) 
# 2nd Step: Compute the benchmark returns.
r_benchmark <- Return.portfolio(returns, weights = equal_weights, rebalance_on = "weeks") # Note: You should change the rebalance_on based on what you need to calculate. 
colnames(r_benchmark) <- "Benchmark" 
# 3rd Step:  Plot the benchmark returns
plot(r_benchmark, main = "Returns of Benchmark")
# 4th Step: Create a graphical representation
names(equal_weights) <- names(returns)
barplot(equal_weights, main = "Benchmark Portfolio's Weights",
        xlab = "Stocks",
        ylab = "Weights")

# 15TH STEP: (STRATEGY 1)
## --- Find the mean-variance efficient portfolio (MVP). ---##
# 1st Step: Create a mean-variance efficient portfolio of returns.  
opt_mvp<- portfolio.optim(returns)
# 2nd Step: Create a vector of weights from your optimized portfolio.
weights_mvp<- opt_mvp$pw 
# 3rd Step: Assign asset names. 
names(weights_mvp) <- colnames(returns)
# 4th Step: Create a graphical representation - Bar Plot. 
barplot(weights_mvp, main = "MV Portfolio's Weights",
        xlab = "Stocks",
        ylab = "Weights") 
# 5th Step: See the expected portfolio's return and volatility. 
opt_mvp$pm                                                                          
opt_mvp$ps
# 6th Step: Compute the mean-variance portfolio's returns.
r_mvp <- Return.portfolio(returns, weights = weights_mvp, rebalance_on = "weeks")  # Note: You should change the rebalance_on based on what you need to calculate. 
colnames(r_mvp) <- "MVP" 
# 7th Step: Plot the mean-variance portfolio's returns. 
plot(r_mvp, main = "The mean-variance portfolio's returns")

# 16TH STEP:  (STRATEGY 2)
## --- Specify the weights of the portfolio based on the return target. ---## 
# 1st Step: Create a portfolio with target return of average returns. 
pf.mean <- portfolio.optim(returns, pm = mean(returns))
# 2nd Step: Create a portfolio with target return 10% grater than average returns. 
pf.mean.10plus <- portfolio.optim(returns, pm = 1.1*mean(returns))
# 3rd Step: See the volatility of both portfolios (standard deviation).
pf.mean$ps
pf.mean.10plus$ps
# 4th Step: Calculate the proportion increase in standard deviation. 
(pf.mean.10plus$ps - pf.mean$ps)/ (pf.mean$ps) 
# 5th Step: See the portfolio's return. 
pf.mean$pm 
pf.mean.10plus$pm
# 6th Step: Create a vector of weights from your optimized portfolio.
weights.mean <- pf.mean$pw
weights.mean.10plus <- pf.mean.10plus$pw
# 7th Step: Assign asset names. 
names(weights.mean) <- colnames(returns)
names(weights.mean.10plus) <- colnames(returns)
# 8th Step: Create a graphical representation - Bar Plot.
barplot(weights.mean, main = "Portfolio's Weights with Return Target equal to the Average Returns",
        xlab = "Stocks",
        ylab = "Weights")
barplot(weights.mean.10plus, main = "Portfolio's Weights with Return Target 10% greater than the Average Returns",
        xlab = "Stocks",
        ylab = "Weights")
# 9th Step: Compute the returns for the portfolios. 
r_pf_mean <- Return.portfolio(returns, weights = weights.mean, rebalance_on = "weeks")
colnames(r_pf_mean) <- "Average Returns" 
r_pf_mean_10 <- Return.portfolio(returns, weights = weights.mean.10plus, rebalance_on = "weeks")
colnames(r_pf_mean_10n) <- "+10% Average Returns" 
# 10th Step: Plot the returns. 
plot(r_pf_mean, main = "Potfolio's Mean Returns (Av. Target Returns)")
plot(r_pf_mean_10, main = "Portfolio's Mean Returns (10% Greater than the Av. Target Returns)")

# 17TH STEP: (STRATEGY 3)
## --- Simple Optimization. --- ##
# 1st Step: Create the portfolio specification. 
port_spec_simpl <- portfolio.spec(colnames(returns))
# 2nd Step: Add a full investment constraint such that the weights sum to 1.
port_spec_simpl <- add.constraint(portfolio = port_spec_simpl, type = "full_investment")
# 3rd Step: Add a long only constraint such that the weight of an asset is between 0 and 1.
port_spec_simpl <- add.constraint(portfolio = port_spec_simpl, type = "long_only")
# 4st Step: Add a return objective to maximize mean return. 
# Note: Return -->  Seeks to maximize the objective.
port_spec_simpl<- add.objective(portfolio = port_spec_simpl, type = "return", name = "mean")
# 5th Step: Add a risk objective to minimize portfolio standard deviation.
# Note: Risk --> Seeks to minimize the objective.
port_spec_simpl <- add.objective(portfolio = port_spec_simpl, type = "risk", name = "StdDev")
# 6th Step: Run the optimization.
opt_simpl <- optimize.portfolio(returns, portfolio = port_spec_simpl,
                          optimize_method = "random",
                          trace=TRUE)
# 7th Step: Create the risk reward chart. (Umbrella of Markovich)
chart.RiskReward(opt_simpl, risk.col = "StdDev", return.col = "mean",
                 chart.assets = TRUE)
# 8th Step: See the results 
opt_simpl
# 9th Step : Extract the optimal weights.
optim.weights.simpl<- extractWeights(opt_simpl)
# 10th Step:Create a graphical representation (Bar graph) for weights. 
barplot(optim.weights.simpl, main = "Portfolio's Weights",
        xlab = "Stocks",
        ylab = "Weights")
# 11th Step: Compute the returns for portfolio. .
r_opt_simpl <- Return.portfolio(returns, weights = optim.weights.simpl, rebalance_on = "weeks")
colnames(r_opt_simpl) <- "Simple Optimization"
# 12th Step: Plot the mean-variance portfolio's returns. 
plot(r_opt_simpl, main = "Portofolio's Returns (Simple Optimization)")

#------------------------------------------------------------------------------#
# Optimization Work flow. 
#------------------------------------------------------------------------------#
# (STRATEGY 4)
# 18TH STEP: 
##--- Create a portfolio specification. ---## 
# 1st Step: Create a portfolio specification. 
port_spec <- portfolio.spec(assets = colnames(returns)) 
# 2nd Step: Check the class of the porfolio specification object.
class(port_spec)
# 3rd Step: Print the portfolio specification object. 
print(port_spec)

# 19TH STEP: 
##--- Add Constraints.--- ##
# Note: It is important to add each constraint separately and store it in the constraints slot of the portfolio object.
# 1st Step: Add full investment constraint on the sum of the weights.
port_spec <- add.constraint(portfolio = port_spec, type = "weight_sum", min_sum = 1, max_sum = 1) 
# Choose between 2nd and 3rd Step. 
# 2nd Step: Add box constraint for the individual asset weights. 
# Note: By adding a box constraint this will apply in every object. 
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0.05, max =0.35)
# 3rd Step: Add a box constrain with specific weights for each asset. 
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = c(0.1, 0.2, 0.15, 0.25, 0.15, 0.08, 0.07), max = 0.4)

# 20TH STEP: 
## --- Add objectives. --- ## 
# Note: It is important to add each objective separately and stored in the objectives slot in the portfolio specification object. 
# 1st Step: Add a return objective to maximize mean return. 
# Note: Return -->  Seeks to maximize the objective.
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
# 2nd Step: Add a risk objective to minimize portfolio standard deviation.
# Note: Risk --> Seeks to minimize the objective.
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
# 3rd Step: Add a risk budget objective. 
# Note: Risk budget --> seeks to minimize risk concentration or penalize contribution to risk that exceeds the minimum or maximum allowable percentage contribution to risk.
# Note: Standard Deviation risk budget: Minimum risk = 5% & Maximum risk = 10%.   
port_spec <- add.objective(portfolio = port_spec, type = "risk_budget", name = "StdDev", min_prisk = 0.05, max_prisk = 0.1) 

#------------------------------------------------------------------------------#
# The optimization method  you choose should be based on the type of problem you want to solve. 
# The optimization methods are: 
# 1) Deoptim : Differential evolution. --> https://cran.r-project.org/web/packages/DEoptim/DEoptim.pdf 
# 2) random : Random portfolios. --> 
# 3) GenSa : Generalized Simulated Annealing. --> https://cran.r-project.org/web/packages/GenSA/GenSA.pdf 
# 4) pso : Particle swarm optimization. --> https://cran.r-project.org/web/packages/pso/pso.pdf
# 5) ROI : R Optimization Infrastructure for linear and quadratric programming solvers. --> https://cran.r-project.org/web/packages/ROI/ROI.pdf 
#------------------------------------------------------------------------------#

# 21TH STEP: 
## --- Single-Period optimization. --- ##
# 1st Step: Run a single period optimization using random portfolios as the optimization method.
opt_sp <- optimize.portfolio(R = returns, portfolio = port_spec, optimize_method = "random",trace = TRUE)
# 2nd Step: Print the output of the single-period optimization.
print(opt_sp)
# 3rd Step: Extract the objective measures for the single period optimization.
extractObjectiveMeasures(opt_sp) 
# 4th Step: Extract and assign the optimal weights for the single period optimization.
optim.w.ot.sp<- extractWeights(opt_sp)
# 5th Step: Create a line and a bar chart for the weights for the single period optimization. 
chart.Weights(opt_sp) 
barplot(optim.w.ot.sp, main = "Portfolio's Weights",
        xlab = "Stocks",
        ylab = "Weights") 
# 6th Step: Compute the returns for portfolio.
r_sin_per <- Return.portfolio(returns, weights = optim.w.ot.sp, rebalance_on = "weeks")
colnames(r_sin_per) <- "Single Optimization"
# 7th Step: Plot the mean-variance portfolio's returns. 
plot(r_sin_per , main = "Portofolio's Returns (Single Period)")

# 22TH STEP: 
## --- Optimize with periodic re-balancing. ---##
# Note: By using a periodic re-balancing (backtesting) and analyzing sample results you can have a better understanding and potentially refine the constraints and objectives. 
# ---------------------------------------------------#
# INFO:
# Search Size: How many portfolios to rest. By default is 20000.  
# Re-balance on: character string of period to re-balance on (days, weeks, months, quarters, years)
# Training Period: an integer of the number of periods to use as a training data in the front of the returns data. 
# Rolling Window: The width of the rolling window.
# ---------------------------------------------------#
# 1st Step: Create a portfolio specification. 
port_per_reb <- portfolio.spec(assets = colnames(returns)) 
# 2nd Step: Check the class of the portfolio specification object.
class(port_per_reb )
# 3rd Step: Print the portfolio specification object. 
print(port_per_reb)
# 4th Step: Add a full investment constraint such that the weights sum to 1.
port_per_reb <- add.constraint(portfolio = port_per_reb, type = "full_investment")
# 5th Step: Add a long only constraint such that the weight of an asset is between 0 and 1.
port_per_reb <- add.constraint(portfolio = port_per_reb, type = "long_only")
# 6th Step: Add a risk objective to minimize portfolio standard deviation.
port_per_reb <- add.objective(portfolio = port_per_reb, type = "risk", name = "StdDev")
# 7th Step: Run the optimization backtest weekly. 
# Note: it will take some time in order to make the calculations. 
# Note: You can define the optimize method, search size, re-balance on, training period and rolling window. 
opt_rebal <- optimize.portfolio.rebalancing(R = returns, portfolio = port_per_reb, optimize_method = "random", trace = TRUE, search_size = 15000, rebalance_on = "weeks", training_period = 30, rolling_window = 7)
# 8th Step: Print the output of the optimization backtest.
print(opt_rebal)
# 9th Step: Extract the objective measures for the optimization backtest. 
extractObjectiveMeasures(opt_rebal)
# 10th Step: Extract the optimal weights for the optimization backtest.
optim.per.re. <- extractWeights(opt_rebal)
# 11th Step: Chart the weights for the optimization backtest.
chart.Weights(opt_rebal)
# 12th Step: Compute the portfolio returns.
r_optim.per.re <- Return.portfolio(R = returns, weights = extractWeights(opt_rebal))
colnames(r_optim.per.re) <- "Optimization with periodic rebalancing"
# 13th Step: Plot portfolio's returns. 
plot(r_optim.per.re, main = "Portofolio's Returns (Periodic Rebalance)")

# 23TH STEP: 
## --- Refine constraints and objectives and optimize with periodic re-balancing. ---## 
# 1st Step: Create a portfolio specification. 
port_re_per_reb <- portfolio.spec(assets = colnames(returns)) 
# 2nd Step: Check the class of the portfolio specification object.
class(port_per_reb )
# 3rd Step: Print the portfolio specification object. 
print(port_per_reb)
# 4th Step: Add a full investment constraint such that the weights sum to 1.
port_re_per_reb <- add.constraint(portfolio = port_re_per_reb, type = "full_investment")
# 5th Step: Add a long only constraint such that the weight of an asset is between 0 and 1.
port_re_per_reb <- add.constraint(portfolio = port_re_per_reb, type = "long_only")
# 6th Step: Add a risk objective to minimize portfolio standard deviation.
port_re_per_reb<- add.objective(portfolio = port_re_per_reb, type = "risk", name = "StdDev")
# 7th Step: Add a risk budget Objective,  Minimum risk = 5% & Maximum risk = 10%.   
port_re_per_reb<- add.objective(portfolio = port_re_per_reb, type = "risk_budget", name = "StdDev", min_prisk = 0.05, max_prisk = 0.1)
# 8TH STEP: Calculate the optimization.
opt_re_per_reb <- optimize.portfolio.rebalancing(R = returns, portfolio = port_re_per_reb, optimize_method = "random", trace = TRUE, rebalance_on = "weeks", training_period = 60, rolling_window = 60)
# 9TH STEP: Chart the weights for the optimization.
chart.Weights(opt_re_per_reb)
# 10TH STEP: Chart the percentage contribution to risk.
chart.RiskBudget(opt_re_per_reb, match.col = "StdDev", risk.type = "percentage")
# 11TH STEP: Compute the portfolio returns. 
r_opt_re_per_reb <- Return.portfolio(R = returns, weights = extractWeights(opt_re_per_reb))
colnames(r_opt_re_per_reb) <- "Refined Optimization with periodic rebalancing"
# 12TH STEP: Plot portfolio's returns. 
plot(r_opt_re_per_reb, main = "Portofolio's Returns (Refine Periodic Rebalance)")

# 24TH STEP: 
## --- Analyze results and compare the Results. --- ##
# 1st Step: Assign all the returns to a variable. 
total_returns <- cbind(r_benchmark, r_mvp, r_pf_mean, r_pf_mean_10,r_opt_simpl, r_sin_per, r_optim.per.re, r_opt_re_per_reb)
# 2nd Step: Compute Annualized Returns. 
table.AnnualizedReturns(R = total_returns) 
# 3rd Step: Create a chart for the performance summary.
charts.PerformanceSummary(R = total_returns)

# --- END OF CODE ---#