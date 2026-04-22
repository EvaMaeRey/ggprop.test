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
  
  data |> 
    dplyr::summarise(min_x = min(x),
              xend = max(x),
              y = 0,
              yend = 0) |> 
    dplyr::rename(x = min_x)
  
}



#' @export
geom_stack <- function(...){
  qlayer(geom = qproto_update(ggplot2::GeomTile, ggplot2::aes(color = "white")), 
         stat = qstat(compute_group_bricks), 
         ...)
  } 

#' @export
geom_stack_label <- function(...){
  qlayer(geom = qproto_update(ggplot2::GeomText, ggplot2::aes(vjust = 0)), 
         stat = qstat(compute_group_count), 
         ...)
  } 

#' @export
geom_support <- function(...){
  qlayer(geom = ggplot2::GeomSegment, 
         stat = qstat_panel(compute_balance), 
         ...)
  }
