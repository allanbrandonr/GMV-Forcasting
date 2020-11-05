# GMV-Forcasting
Look for an optimum additive and non linear component model to forecast GMV and explore the influence of marketing performance KPIs

# Summary
The objective of this business case is to develop an accurate model for revenue forecasting 
while taking into account any valuable data patterns such as changes in trend, seasonality patterns, 
holiday effects and explanatory variables.

This will boost digital marketing planning and performance tracking.

With available daily data up until Jul 2020, I made 3 forecasts for future daily revenue.
Forecast 1: Only based on past behavior
Forecast 2: Including influence of ads spend
Forecast 3: Including influence of ads spend and conversion rate

As there is no future information for independent variables, I did 2 additional daily forecasts
Auxiliar forecast 1: Ads spend based only on past behavior
Auxiliar forecast 2: Conversion rate based only on past behavior

Each forecast was made with the best possible model. 
The best model was selected while looking for the least percentage error in a 2020 out of sample.

Summaries are presented at a midyear level to facilitate interpretation

Check the summary deck included for more detail

# The codes
The codes are ordered by their usage in the whole process. The codes goe from reviewing data, to estimating different 
model parametrizations and optimizing on hyperparameters
