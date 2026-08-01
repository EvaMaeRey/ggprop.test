#' @export
data_shuffle_var <- function(data, var){
  
  data |> 
    dplyr::mutate(shuffled = sample({{var}}, replace = F))
  
}
