make_sankey_diagram <- \(data, trt, ae, cycle_limit = 10) {
  
  data <- data |> 
    filter(ncycle <= !!cycle_limit)
  
  data <- data |> 
    filter(trt == !!trt & ae == !!ae)
  
  total_patients <- data |> 
    filter(ncycle == 1) |> 
    pull(patientid) |> 
    unique() |> 
    length()
  
  data <- data |> 
    mutate(
      ncycle = ncycle * 6,
      ncycle = glue::glue('{ncycle} M')
    )
  
  data <- data |> 
    pivot_wider(names_from = ncycle, values_from = ae_grade,
                values_fill = 'Off Treatment')
  
  plot_data <- data |> make_long(4:ncol(data))
  
  cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  
  grade_colors <- c(
    "0-1" = "#999999",
    "2" = "#E69F00",
    "3" = "#56B4E9",
    "4" = "#009E73",
    "5" = "#D55E00",
    "Off Treatment" = "#000000"
  )
  
  plot <- ggplot(plot_data,
                 aes(
                   x = x,
                   next_x = next_x,
                   node = node,
                   next_node = next_node,
                   fill = factor(node)
                 )) +
    geom_sankey(flow.alpha = .3) +
    theme_sankey(base_size = 20) +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          axis.text.x = element_text(face = 'bold'),
          axis.title = element_blank()) +
    scale_color_manual(
      values = grade_colors
    ) + 
    scale_fill_manual(
      values = grade_colors
    ) + 
    labs(title = ae, subtitle = glue::glue('{trt}, n = {total_patients}'))
  
  plot
  
}