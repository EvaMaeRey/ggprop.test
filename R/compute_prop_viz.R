# 4. layer add balancing point 
compute_xmean_at_y0 <- function(data, scales){
  
  data |> 
    dplyr::summarise(x = mean(x),
              y = 0, 
              label = "^") 
  
}


# 5. layer add balancing point value label
compute_xmean_at_y0_label <- function(data, scales){
  
  data |> 
    dplyr::summarise(x = mean(x),
              y = 0, 
              label = after_stat(round(x, 2))) 
  
}


# 6. Add 'point' for asserted balancing point (null)
compute_panel_prop_asserted <- function(data, scales, value = .5){
  
  # stamp type layer - so ignore input data
  data.frame(y = 0, 
             x = value,
             label = "^"
             )
  
}

# 6. Add label for asserted balancing point (null)
compute_panel_prop_asserted_label <- function(data, scales, value = .5){
  
  # stamp type layer - so ignor input data
  data.frame(y = 0, 
             x = value,
             label = round(value, 2)
             )
  
}



#' @export
geom_prop <- function(...){
  
  list(
  .layers = qlayer(geom = qproto_update(ggplot2::GeomText, 
                              ggplot2::aes(size = 6, vjust = 1,
                                           color = ggplot2::from_theme(colour %||% accent))),
         stat = qstat_panel(compute_xmean_at_y0),
         ...),
  .scales = scale_x_prop()
  )
  
  }

#' @export
geom_prop_label <- function(...){ 
  qlayer(geom = qproto_update(ggplot2::GeomLabel, 
                              ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), 
                                           color = ggplot2::from_theme(colour %||% accent),
                                           label.size = NA, vjust = 0)),
         stat = qstat_panel(compute_xmean_at_y0_label), 
         ...) 
  }

#' @export
stamp_prop <- function(value = .5, ...){ 
  
  # qlayer(geom = qproto_update(ggplot2::GeomText, 
  #                             ggplot2::aes(size = 6, 
  #                                          vjust = 1, 
  #                                          color = ggplot2::from_theme(colour %||% ink))),
  #        stat = qstat_panel(compute_panel_prop_asserted), 
  #        data = data.frame(x = 1), 
  #        inherit.aes = FALSE,
  #        ...
  #        )
  
  annotate(geom = qproto_update(ggplot2::GeomText, 
                              ggplot2::aes(size = 6, 
                                           vjust = 1, 
                                           color = ggplot2::from_theme(colour %||% ink))),
           x = value, y = 0, label = "^")
  
  
  }
  
#' @export  
stamp_prop_label <- function(value = .5, ...){  
  # qlayer(geom = qproto_update(ggplot2::GeomLabel, 
  #                             ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), 
  #                                 label.size = NA, vjust = 0, 
  #                                 color = ggplot2::from_theme(colour %||% ink))),
  #        stat = qstat_panel(compute_panel_prop_asserted_label), 
  #        data = data.frame(x = 1), 
  #        inherit.aes = FALSE,
  #        ...
  #        )
  
  GeomLabelExtra <- qproto_update(ggplot2::GeomLabel, 
                               ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), 
                                   label.size = NA, vjust = 0, 
                                   color = ggplot2::from_theme(colour %||% ink)))
  
  annotate(geom = GeomLabelExtra,
           x = value, 
           y = 0, 
           label = value)
  
  }
