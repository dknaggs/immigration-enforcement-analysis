### Functions and constants used across analyses
std_age_bins <- c(0,1,5,10,14,17,21,seq(30,80,10)-1,84,Inf)
options(scipen = 999)


clean_strings <- function(df) {
  # basic string cleaning
  df |> 
    mutate(across(where(is.character), \(x) str_to_upper(str_trim(x))))
}


percent_missing <- function(df,select_cols){
  # prints the percent of missing values in select_cols
  df |>
    summarise(across({{select_cols}},~ round(mean(is.na(.x))*100,2))) |>
    t()
}

most_common_vals <- function(df,x,n=5){
  # get the top n values (and frequency) of x
  vals <- df[[x]] |> 
    table() |> 
    sort(decreasing=TRUE) |>
    head(n)
  length(vals) <- n
  
  paste(names(vals),vals,sep="-")
}


datenum_summary <- function(df,select_cols){
  # min, mean, median, max, and percent missingness of select_cols
  df |>
    summarise(
      across({{select_cols}},
             list(
               min=~min(.x,na.rm=TRUE),
               mean=~mean(.x,na.rm=TRUE),
               med=~median(.x,na.rm=TRUE),
               max=~max(.x,na.rm=TRUE),
               p_miss=~round(mean(is.na(.x))*100,2)
             )
      )) |>
    t()
}

logical_summary <- function(df){
  # percent true and missing of all logical values
  df |>
    summarise(
      across(is.logical,
             list(
               pct_true=~(round(mean(.x,na.rm=TRUE)*100,2)),
               pct_miss=~(round(mean(is.na(.x))*100,2))
             )
      )) 
  
}


column_percents <- function(df) {
  # convenience function to show the percent each observation makes of all
  # columns in df
  df |> 
    mutate(across(where(is.numeric),~round(100*.x/sum(.x,na.rm=TRUE),1)))
}

make_age_bins <- function(df,col,age_bins=std_age_bins){
  # converts a column of ages into age bins (default defined at top of this 
  # script) 
  
  age_labs <- sapply(2:(length(age_bins)-1),\(x) 
                     paste(age_bins[x]+1,age_bins[x+1],sep="-")
  )
  age_labs <- c("0-1",age_labs)
  age_labs[length(age_labs)] <- paste0(age_bins[length(age_bins)-1]+1,"+")
  df |> 
    mutate(age_bin=cut(
      {{col}},
      breaks=age_bins,
      labels=age_labs,
      right=TRUE,
      include.lowest=TRUE,
      dig.lab=0,
      ordered_result=TRUE
    )
    )
}

comp_table <- function(df,var,by=post_0125_f,sort_by_var=FALSE){
  # makes a table showing the counts of `var` pivoted by `by` 
  # if sort_by_var=TRUE, the table will be sorted by the values in var. 
  # Otherwise, it will sort by overall frequency
  tbl <- df |>
    count({{by}},{{var}}) |> 
    arrange(desc(n)) |>
    pivot_wider(names_from={{by}},values_from=n,values_fill = 0,
                names_sort = TRUE)
  
  if(sort_by_var){
    tbl |>
      arrange({{var}})
  } else{
    tbl
  }
}

make_output_cols <- function(df){
  # convenience function to make column titles nicer
  rename_with(df,.fn=~ str_to_title(str_replace_all(.x,"_"," ")))
}

add_before_after_change <- function(df){
  # adds a percent change column to tables comparing a variable before vs 
  # after Jan 2020
  df |> mutate(
    `Percent Change`=round(100*(
      (`After Jan 2025`-`Before Jan 2025`)/`Before Jan 2025`),
      1)
  )
}
