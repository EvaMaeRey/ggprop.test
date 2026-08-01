scale_x_prop <- function(...){

  scale_x_discrete(palette = scales::pal_manual(0:1), drop = FALSE, ...)

}


# 1. layer stack of bricks
compute_group_bricks <- function(data, scales, width = .2){
  
  data |> 
    dplyr::mutate(row = row_number()) |> 
    dplyr::mutate(y = row - .5) |> 
    dplyr::mutate(width = width)
  
}


# 2. layer label stack with count
compute_group_count <- function(data, scales){
  
  data |> 
    dplyr::count(x) |> 
    dplyr::mutate(y = n,
           label = n)
  
}


# 3. layer add x span
compute_balance <- function(data, scales){
  
  tibble(x = 0,
         xend = 1,
         y = 0,
         yend = 0)
  
}



#' @export
geom_stack <- function(...){
  
  list(
    
  qlayer(geom = qproto_update(ggplot2::GeomTile, ggplot2::aes(color = "white")), 
         stat = qstat(compute_group_bricks), 
         ...),
  scale_x_prop()

  ) 
  
  } 

#' @export
geom_stack_label <- function(...){
  
    list(
      qlayer(geom = qproto_update(ggplot2::GeomText, ggplot2::aes(vjust = 0)), 
         stat = qstat(compute_group_count), 
         ...),
      scale_x_prop()
      )
  
  } 

#' @export
geom_support <- function(...){
  list(
  qlayer(geom = ggplot2::GeomSegment, 
         stat = qstat_panel(compute_balance), 
         ...)
  )
  }
