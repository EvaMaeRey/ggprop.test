GeomTextBig <- ggproto("GeomTextBig", GeomText,
                       default_aes = modifyList(GeomText$default_aes,
                                                aes(size = from_theme(fontsize))
                                                ))


#' @export
stamp_eq_norm_prop <- function(x = I(.125),
    y = I(.8), ...){
  
  annotate(
    "text",
    x = x,
    y = y,
    label = latex2exp::TeX("sd = \\sqrt{\\frac{p*(1-p)}{n}}", output = "character"),
    parse = TRUE, ...
  )

}


