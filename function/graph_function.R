## Colour palette

color_palette <- c("#313030", "#62adf0", 
                   "#ca4944", "#22737c", 
                   "#eec168", "#245699",
                   "#8338ec", "#06d6a0",
                   "#4a4e69", "#b08968",
                   "#efd3d7", "#f72585",
                   "#95c623", "#fff152",
                   "#907f8c", "green")



## 1st graph function

population_graph <- function(population_data, level,
                             breaks = c(1000000, 2000000, 3000000, 4000000, 5000000, 6000000), 
                             labels = c('1M', '2M', '3M', '4M', '5M', '6M'), 
                             variable){
  
  population_data %>% 
    mutate(state = factor(state, levels = level)) |> 
    ggplot(aes(x = state, y = value, fill = variable, group = variable)) +
    geom_col_interactive(width = 0.75,
                         aes(tooltip = sprintf("%s: %s", variable, comma(value)))) +
    scale_fill_manual(values = color_palette,
                      breaks = rev(levels(population_data$variable))) +
    guides(fill=guide_legend(ncol = 4)) +
    coord_flip() +
    scale_y_continuous(breaks= breaks, 
                       labels= labels) +
    labs(x = "",
         y = "",
         title = paste(variable, "Composition in 2020")) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.title = element_blank())
  
  
}

## 2nd graph function

ratio_graph <- function(population_data, variable) {
  
  population_data |> 
    mutate(variable = fct_rev(variable)) |> 
    filter(variable != "Total",
           state != "Malaysia") |> 
    mutate(state = reorder_within(state, gender_ratio, variable)) |> 
    ggplot(aes(x = state, y = gender_ratio, fill = variable)) +
    geom_col_interactive(show.legend = F,
                         width = 0.75,
                         aes(tooltip = round(gender_ratio,3))) +
    scale_fill_manual(values = color_palette,
                      breaks = rev(levels(population_data$variable))) +
    coord_flip() +
    scale_x_reordered() +
    facet_wrap(~ variable, scales = "free_y",
               ncol = 2) +
    labs(x = "",
         y = "",
         title = paste( "Female to Male Ratio by", variable, "Composition in 2020")) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5))  
  
}


## 3rd graph function
distribution_graph <- function(population_data, level, variable) {
  
  population_data |> 
    mutate(state = factor(state, levels = level)) |> 
    ggplot(aes(x = state, y = share, fill = variable, group = variable)) +
    geom_bar_interactive(stat = "identity",
                         aes(tooltip = percent(share))) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = color_palette,
                      breaks = rev(levels(population_data$variable))) +
    coord_flip() +
    labs(x = "",
         y = "",
         title = paste( variable, "Population Distribution")) +
    guides(fill=guide_legend(ncol = 4)) +
    theme(# remove grid
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))
  
}


