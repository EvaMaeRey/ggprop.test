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
         stat = qstat_panel(compute_scale), 
         ...)
  }

#' @export
geom_prop <- function(...){
  qlayer(geom = qproto_update(ggplot2::GeomText, ggplot2::aes(size = 6, vjust = 1)),
         stat = qstat_panel(compute_xmean_at_y0),
         ...)
  }

#' @export
geom_prop_label <- function(...){ 
  qlayer(geom = qproto_update(ggplot2::GeomLabel, 
                              ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), label.size = NA, vjust = 0)),
         stat = qstat_panel(compute_xmean_at_y0_label), 
         ...) 
  }

#' @export
stamp_prop <- function(...){ 
  qlayer(geom = qproto_update(ggplot2::GeomText, 
                              ggplot2::aes(size = 6, 
                                           vjust = 1, 
                                           color = ggplot2::from_theme(colour %||% accent))),
         stat = qstat_panel(compute_panel_prop_asserted), 
         ...)
  }
  
#' @export  
stamp_prop_label <- function(...){  
  qlayer(geom = qproto_update(ggplot2::GeomLabel, 
                              ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), 
                                  label.size = NA, vjust = 0, 
                                  color = ggplot2::from_theme(colour %||% accent))),
         stat = qstat_panel(compute_panel_prop_asserted_label), 
         ...)
  }

#' @export
geom_normal_prop_null <- function(...){
  qlayer(geom = qproto_update(ggplot2::GeomArea, ggplot2::aes(alpha = .2)),
         stat = qstat_panel(compute_dnorm_prop), 
         ...)
  } 

#' @export
geom_normal_prop_null_sds <- function(...){
   qlayer(geom = qproto_update(ggplot2::GeomSegment, ggplot2::aes(linetype = "dotted")),
          stat = qstat_panel(compute_dnorm_prop_sds), 
          ...)
  }




