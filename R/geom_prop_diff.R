compute_layer_diff_prop_segment <- function(data, ...){
  
  data |> 
    summarise(prop = mean(x |> as.factor() |> as.numeric() -1),
              .by = PANEL) |> 
    select(prop, PANEL) |>
    pivot_wider(values_from = prop, names_from = PANEL, names_prefix = "V") |>
    rename(x = V1, xend = V2) |> 
    mutate(y = 0, yend = 0) |> 
    crossing(data.frame(PANEL = 1:2))
  
}


compute_layer_diff_prop_segment_label <- function(data, ...){
  
  data |> 
    summarise(prop = mean(x |> as.factor() |> as.numeric() -1),
              .by = PANEL) |> 
    select(prop, PANEL) |>
    pivot_wider(values_from = prop, names_from = PANEL, names_prefix = "V") |>
    rename(x = V1, xend = V2) |> 
    mutate(y = 0, yend = 0) |> 
    crossing(data.frame(PANEL = 1:2)) |> 
    mutate(difference = c((x - xend)) |> round(2)) |>
    mutate(label = paste0("Difference: \n", difference)) |> 
    mutate(x = I(.2), y = I(.8)) |> 
    mutate(alpha = c(0, 1))
  
}

geom_prop_diff <- function(...){

  qlayer(stat = compute_layer_diff_prop_segment |> qstat_layer(), 
         geom = GeomSegment |> 
           qproto_update(aes(color = from_theme(accent),
                   linewidth = from_theme(linewidth*3))), 
         ...)
  
}

geom_prop_diff_label <- function(...){
  
    qlayer(
      geom = GeomLabel |> qproto_update(aes(color = from_theme(accent))),
      stat = compute_layer_diff_prop_segment_label |> qstat_layer(),
      ...
      ) 
  
}
