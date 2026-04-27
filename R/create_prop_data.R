#' @export
create_prop_data <- function(failure = "failure (0)", 
                             success = "success (1)", 
                             num_failure = 5, 
                             num_success = 5, 
                             var_name = "outcome"){
  
   outcome <-  c(failure, success) |> rep(c(num_failure, num_success)) |> sample()

   tibble(outcome)
  
}
