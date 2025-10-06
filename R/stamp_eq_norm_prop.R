#' @export
stamp_eq_norm_prop <- function(x = I(.125),
    y = I(.8), size = 3.5){
  
  annotate(
    "text",
    x = x,
    y = y,
    label = latex2exp::TeX("sd = \\sqrt{\\frac{p*(1-p)}{n}}", output = "character"),
    parse = TRUE,
    size = size
  )

}


