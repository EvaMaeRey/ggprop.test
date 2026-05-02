facet_align <- function(var){
  
  facet_wrap(vars({{var}}), ncol = 1)
  
}
