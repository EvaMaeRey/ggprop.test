# 7. normal distribution based on null and n
compute_dnorm_prop <- function(data, scales, prob = .5, dist_sds = seq(-3.5, 3.5, by = .1)
){
  
  n <- data |> nrow()
  n_max <- data |> dplyr::count(.by = x) |> dplyr::pull() |> max()

  
  sd = sqrt(prob * (1 - prob)/n) # sd of the null distribution
  
  q <- dist_sds * sd + prob
  
  data.frame(x = q) |>
    dplyr::mutate(height = dnorm(q, sd = sd, mean = prob)) |>
    dplyr::mutate(height_max = dnorm(0, sd = sd, mean = 0)) |>
    dplyr::mutate(y = .5*n*height/height_max) |>  # This is a bit fragile...
    dplyr::mutate(xend = x,
           yend = 0) |> 
    # @teunbrand ggplot2::GeomArea$setup_data() requires a group column. Your panel computation does not preserve groups, but it should.
    dplyr::mutate(group = 1) 
  
}  


# 8. normal distribution mean and sds based on null and n
compute_dnorm_prop_sds <- function(data, scales, prob = .5, dist_sds = -4:4){
  
  n <- data |> nrow()
  
  n_max <- data |> dplyr::count(.by = x) |> dplyr::pull() |> max()
  
  sd = sqrt(prob * (1 - prob)/n) # sd of the null distribution
  
  q <- dist_sds * sd + prob
  
  data.frame(x = q) |>
    dplyr::mutate(height = dnorm(q, sd = sd, mean = prob)) |>
    dplyr::mutate(height_max = dnorm(0, sd = sd, mean = 0)) |>
    dplyr::mutate(y = .5*n*height/height_max) |> # This is a bit fragile...
    dplyr::mutate(xend = x, yend = 0)

}  



# Compute from ma206 data
#' @export
tidy_dbinom <- function(single_trial_prob = .5, num_trials = 10){

  num_successes <- 0:num_trials
  probability <- stats::dbinom(x = num_successes, size = num_trials, prob = single_trial_prob)

  tibble::tibble(num_successes, probability, single_trial_prob, num_trials)

}


compute_dbinom <- function(data, scales, prob = .5){
  
  num_trials <- nrow(data)
  
  tidy_dbinom(single_trial_prob = prob, 
              num_trials = num_trials) |> 
    mutate(x = num_successes/num_trials,
           y = num_trials/2*probability/max(probability),
           yend = 0,
           xend = x)
  
}

#' @export
geom_binomial_null <- function(prob = .5, ...){
  
  qlayer(geom = GeomSegment,
         stat = qstat_panel(compute_dbinom), prob = prob, ...)

}

#' @export
geom_normal_prop_null <- function(..., prob = .5){
  qlayer(geom = qproto_update(ggplot2::GeomArea, 
                              ggplot2::aes(alpha = .2)),
         stat = qstat_panel(compute_dnorm_prop), 
         prob = prob, 
         ...)
  } 

#' @export
geom_normal_prop_null_sds <- function(..., prob = .5){
   qlayer(geom = qproto_update(ggplot2::GeomSegment, 
                               ggplot2::aes(linetype = "dotted")),
          stat = qstat_panel(compute_dnorm_prop_sds), 
          prob = prob,
          ...)
  }

GeomTextBig <- ggplot2::ggproto("GeomTextBig", ggplot2::GeomText,
                       default_aes = 
                         modifyList(ggplot2::GeomText$default_aes,
                                    ggplot2::aes(size = ggplot2::from_theme(fontsize))))


#' @export
stamp_eq_norm_prop <- function(x = I(.125),
    y = I(.8), ...){
  
  ggplot2::annotate(
    "text",
    x = x,
    y = y,
    label = latex2exp::TeX("sd = \\sqrt{\\frac{p*(1-p)}{n}}", output = "character"),
    parse = TRUE, ...
  )

}
