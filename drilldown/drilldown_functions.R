# function to construct data for drilldown plot. used in make_drilldown()
  # df: data frame; lvl = integer level of hierarchy for which to construct data; 
  #hier_names: vector of names of variables defining the hierarchy. In descending order of hierarchy,
  # eg. c("State","City");
  # yname: name in df of variable to display 
aggregate_hier <- function(df, lvl, hier_names2, yname, js_click_line_id2=NULL) {
  hier_names <- c("level1",hier_names2)  
  hier_names_num <- c("level1",paste0(hier_names2,"_num"))
  
  df %>% 
    mutate(id1 = paste(!!! syms(hier_names[seq_len(lvl)]),sep="."), name1 = !! sym(hier_names[lvl]),
           id2 = paste(!!! syms(hier_names[seq_len(lvl+1)]),sep="."), name2 = !! sym(hier_names[lvl+1])) %>% 
    group_by(id1,id2,name1,name2) %>%
    summarize(y=sum(y)) %>%
    group_by(id2,id1,name1) %>%
    #mutate(color = case_when(y>=0~'blue', y<0~'red')) %>% 
    do(data = list(name = .$name2, y = .$y, drilldown= .$id2
                   #, color =.$color
                   ))  %>%
    ungroup() %>%
    group_by(id1,name1) %>%
    do(data = .$data) %>%
    mutate(events = list(list(click = js_click_line_id2))) %>% 
    rename(id=id1,name=name1) %>% 
    arrange(name) %>% 
    highcharter::list_parse()
} 


# make some modifications to the dataframe 
standardize_df <- function(df,hier,yname) {
  # names for numerical hierarchical variables 
  names_num <-paste0(hier,"_num")

  #create variable level1 (a constant). Allows the initial plot to be created using same method used for drilldown plots 
  df$level1 <- "level1"

  # for each hierarchical variable "var", create a numeric variable "var_num". 
  #In aggregate_hier function, this helps avoid creation of very long strings
  df <- df %>% 
    mutate(y = !! sym(yname)) %>% 
   mutate_at(hier,df,.funs = list(num =~as.integer(factor(.)))) %>% 
    mutate(id_tbl = paste(!!! syms(c("level1",hier)),sep="."))
  
  df
} 


# function to construct the data using aggregate_hier and standardize_df 

structure_hc <- function(df,hier,yname,js_click_line_id2){
  df <- standardize_df(df,hier,yname) 
  map(seq_along(hier),~aggregate_hier(df,lvl=.,hier,yname,js_click_line_id2))
} 

# function to produce plot 
  # df: dataframe
  #hier: vector of names of variables defining the hierarchy. In descending order of hierarchy,
  # eg. c("State","City")
  # yname: name in df of numerical variable display
  # js_click_line_id: function to get id of category selected when a bar is clicked 

make_drilldown <- function(df, hier, yname, js_click_line_id2, hc_data2 =NULL){
  if (is.null(hc_data2)) hc_data <- structure_hc(df,hier,yname,js_click_line_id2)
  else hc_data <- hc_data2 
  data_lower <- do.call(c,hc_data[-1])
  hc <- highchart() %>%  
    hc_chart(type="column") %>%  
    hc_xAxis(type="category",style=list(fontSize='14px'),labels = list(style=list(fontSize="14px"))) %>%  
    hc_add_series(
      #name= "Level 1",
      name = hier[1],
      data =hc_data[[1]][[1]]$data, 
      events = list(click = js_click_line_id2))  %>%  
    hc_drilldown(series = data_lower)  
  hc
}


