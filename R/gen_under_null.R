#' @export 
to_synthetic <- function(x, prob = .5){
  
  levels(x) |>  # take two 
    sample(size = length(x), 
           replace = T, 
           prob = c(1-prob, prob)) |> 
    # restore category ordering
    factor(levels = levels(x))
  
}


#' @export
data_add_synth <- function(data, var, prob = .5){
  
  x <- data |> 
    pull({{var}})
  
  generated <- to_synthetic(x, prob = prob)
  
  data |> 
    mutate(synthetic = generated)
  
}


# 
# #' @export
# x_from_null <- function(data = NULL, prob = .5) {
# 
#   structure(
#     list(prob = prob), 
#     class = "x_from_null"
#     )
# 
# }
# 
# 
# #' @import ggplot2
# #' @importFrom ggplot2 ggplot_add
# #' @export
# ggplot_add.x_from_null <- function(object, plot, object_name) {
#   
#   xname <- plot@mapping |> as.character() |> str_remove("~")
#   
#   xname
#   
#   var <- plot$data |> pull(xname)
#   
#   plot$data[xname] <-  
#      sample(levels(var), 
#             size = length(var), 
#             replace = T, prob = c(1-object$prob, object$prob)
#             ) |> 
#      # restore category ordering
#     factor(levels = levels(var))
#   
#   plot + labs(x = "plausible from null") + 
#   stamp_prop(value = mean(var |> as.numeric()) -1 ) + 
#   stamp_prop_label(value = mean(var |> as.numeric()) - 1) 
# 
# }

