compute_layer_diff_prop_segment <- function(data, ...){
  
  data |> 
    dplyr::summarise(prop = mean(x |> as.factor() |> as.numeric() -1),
              .by = .data$PANEL) |> 
    dplyr::select(.data$prop, .data$PANEL) |>
    tidyr::pivot_wider(values_from = .data$prop, 
                       names_from = .data$PANEL, 
                       names_prefix = "V") |>
    dplyr::rename(x = V1, xend = V2) |> 
    dplyr::mutate(y = 0, yend = 0) |> 
    tidyr::crossing(data.frame(PANEL = 1:2))
  
}


compute_layer_diff_prop_segment_label <- function(data, ...){
  
  data |> 
    dplyr::summarise(prop = mean(x |> as.factor() |> as.numeric() -1),
              .by = PANEL) |> 
    dplyr::select(.data$prop, .data$PANEL) |>
    tidyr::pivot_wider(values_from = prop, names_from = PANEL, names_prefix = "V") |>
    dplyr::rename(x = .data$V1, xend = .data$V2) |> 
    dplyr::mutate(y = 0, yend = 0) |> 
    tidyr::crossing(data.frame(PANEL = 1:2)) |> 
    dplyr::mutate(difference = c((x - xend)) |> round(2)) |>
    dplyr::mutate(label = paste0("Difference: \n", .data$difference)) |> 
    dplyr::mutate(x = I(c(.2, -5)), y = I(.8)) # think about another way
  
}

geom_prop_diff <- function(...){

  qlayer(stat = compute_layer_diff_prop_segment |> qstat_layer(), 
         geom = ggplot2::GeomSegment |> 
           qproto_update(ggplot2::aes(color = from_theme(accent),
                   linewidth = from_theme(linewidth*3))), 
         ...)
  
}

geom_prop_diff_label <- function(...){
  
    qlayer(
      geom = ggplot2::GeomLabel |> 
        qproto_update(aes(color = ggplot2::from_theme(colour %||% accent),
                          fill  = ggplot2::from_theme(colour %||% paper))),
      stat = compute_layer_diff_prop_segment_label |> qstat_layer(),
      ...
      ) 
  
}
