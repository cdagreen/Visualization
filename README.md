# Visualization

## Drillown

Functions to aggregate data according to user-selected set of hierarchy variables, and produce the corresponding drilldown chart using [highcharter](https://jkunst.com/highcharter/). 

### drilldown_functions.R 

Functions for aggregating data and producing the drilldown chart. **make_drilldown()** is the main function, and takes the others as dependencies. 

## drilldown.Rmd

[Flexdashboard](https://pkgs.rstudio.com/flexdashboard/) file demonstrating the drilldown functionality on sales data aggregated from [Adidas Sales Dataset](https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset). The user can select variables to define an aggregation hierarchy and view the corresponding drilldown chart. 
