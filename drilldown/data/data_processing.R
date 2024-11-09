library(tidyverse)

path_base <- Sys.getenv('path_base')
path_demand <- 'data/store-sales-time-series-forecasting/train.csv'
tbl_demand <- readr::read_csv(paste0(path_base, path_demand))

path_stores<- 'data/store-sales-time-series-forecasting/stores.csv'
tbl_stores <- readr::read_csv(paste0(path_base, path_stores))

str(tbl_stores)

tbl_stores %>% 
  group_by(cluster) %>% 
  summarize(n_city = n_distinct(city), 
            n_state = n_distinct(state))


tbl_stores %>% 
  group_by(city) %>% 
  summarize(n_cluster = n_distinct(cluster))


###### Adidas data ###### 
# downloaded from Kaggle: https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset

path_adidas <- 'code/drilldown/data/Adidas US Sales Datasets.csv'
tbl_adidas_load <- readr::read_csv(paste0(path_base, path_adidas), 
                skip = 4)

# convert strings formatted with dollar sign and commas to numeric,
# ex. str "$500,010.40" ->  num 500,010.40
dollar_to_numeric <- function(x){
  stringr::str_remove(x, '[$]') %>% 
    stringr::str_remove(',') %>% 
    as.numeric()
}

tbl_adidas_load <- tbl_adidas_load %>% 
  mutate(`Invoice Date` = as.Date(`Invoice Date`, format = '%m/%d/%y'), 
         `Total Sales` = dollar_to_numeric(`Total Sales`))

tbl_adidas_load <- tbl_adidas_load %>% 
  mutate(Sex = case_when(stringr::str_detect(Product, "Men's") ~ "Men's", 
                         TRUE ~ "Women's"), 
         Type = case_when(stringr::str_detect(Product, 'Apparel') ~ 'Apparel', 
                          TRUE ~ 'Footwear'), 
         Subtype = case_when(stringr::str_detect(Product, 'Apparel') ~ 'Apparel', 
                             stringr::str_detect(Product, 'Street') ~ 'Street', 
                             TRUE ~ 'Athletic')) %>% 
  mutate(Year = year(`Invoice Date`))

tbl_adidas_2021 <- tbl_adidas_load %>% 
  filter(Year == 2021) %>% 
  group_by(Retailer, Region, State, City, Sex, Type, Subtype) %>% 
  summarize(`Total Sales` = sum(`Total Sales`)) %>% 
  ungroup()

readr::write_csv(tbl_adidas_2021, 
                 paste0(path_base, 'code/drilldown/data/Adidas_US_2021_aggregated.csv'))

names(tbl_adidas_2021)

tbl_adidas_2021 %>% 
  select(-`Total Sales`) %>% 
  distinct()

js_click_line_id <- htmlwidgets::JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.drilldown]);}")

hier_vars <- c('Region', 'State', 'City', 'Retailer')
drilldown_ <- make_drilldown(df=tbl_adidas_2021,hier= hier_vars ,yname="Total Sales", js_click_line_id=js_click_line_id)

drilldown_