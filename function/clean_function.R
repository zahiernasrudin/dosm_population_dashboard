## Create a function
clean_function <- function(population_data, remark, factor_level) {
  
  population_data |> 
    filter(variable != "total",
           state != "Malaysia",
           remark == !!remark) |> 
    group_by(state, variable) |> 
    summarise(value = sum(value)) |>
    mutate(variable = str_to_title(variable),
           variable = factor(variable, levels = factor_level)) |> 
    ## Calculate share
    group_by(state) %>% 
    mutate(total = sum(value)) %>% 
    ungroup() %>% 
    mutate(share = value / total)
  
}

desired_level_function <- function(population_data, variable) {
  
  population_data |> 
    filter(variable == !!variable) %>% 
    arrange(share) %>% 
    pull(state) %>% 
    unique()
}



## Calculate ratio function
ratio_function <- function(population_data, remark, factor_level) {
  
  population_data |> 
    filter(variable != "total",
           state != "Malaysia",
           remark == !!remark) |> 
    group_by(state, variable) |> 
    summarise(
      female_value = sum(value[gender == "female"]),
      male_value = sum(value[gender == "male"]),
      gender_ratio = female_value / male_value
    ) |> 
    ungroup() |> 
    mutate(variable = str_to_title(variable),
           variable = factor(variable, levels = factor_level))
  
}

