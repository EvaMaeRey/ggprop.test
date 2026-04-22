# 7. normal distribution based on null and n
compute_dnorm_prop <- function(data, scales, null = .5, dist_sds = seq(-3.5, 3.5, by = .1)
){
  
  n <- data |> nrow()
  n_max <- data |> dplyr::count(.by = x) |> dplyr::pull() |> max()

  
  sd = sqrt(null * (1 - null)/n) # sd of the null distribution
  
  q <- dist_sds * sd + null
  
  data.frame(x = q) |>
    dplyr::mutate(height = dnorm(q, sd = sd, mean = null)) |>
    dplyr::mutate(height_max = dnorm(0, sd = sd, mean = 0)) |>
    dplyr::mutate(y = .55*n_max*height/height_max) |>  # This is a bit fragile...
    dplyr::mutate(xend = x,
           yend = 0) |> 
    # @teunbrand ggplot2::GeomArea$setup_data() requires a group column. Your panel computation does not preserve groups, but it should.
    dplyr::mutate(group = 1) 
  
}  


# 8. normal distribution mean and sds based on null and n
compute_dnorm_prop_sds <- function(data, scales, null = .5,
  dist_sds = -4:4){
  
  n <- data |> nrow()
  
  n_max <- data |> dplyr::count(.by = x) |> dplyr::pull() |> max()
  
  sd = sqrt(null * (1 - null)/n) # sd of the null distribution
  
  q <- dist_sds * sd + null
  
  data.frame(x = q) |>
    dplyr::mutate(height = dnorm(q, sd = sd, mean = null)) |>
    dplyr::mutate(height_max = dnorm(0, sd = sd, mean = 0)) |>
    dplyr::mutate(y = .55*n_max*height/height_max) |> # This is a bit fragile...
    dplyr::mutate(xend = x,
           yend = 0)

}  



# Compute from ma206 data
tidy_dbinom <- function(single_trial_prob = .5, num_trials = 10){

  num_successes <- 0:num_trials
  probability <- stats::dbinom(x = num_successes, size = num_trials, prob = single_trial_prob)

  tibble::tibble(num_successes, probability, single_trial_prob, num_trials)

}


compute_dbinom <- function(data, scales, prob = .5){
  
  num_trials <- nrow(data)
  
  tidy_dbinom(single_trial_prob = .5, 
              num_trials = num_trials) |> 
    mutate(x = num_successes/max(num_successes),
           y = num_trials/2*probability/max(probability),
           yend = 0,
           xend = x) 
  
}

#' @export
geom_binomial_null <- function(...){
  
  qlayer(geom = GeomSegment,
         stat = qstat(compute_dbinom))
  
  
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

GeomTextBig <- ggproto("GeomTextBig", GeomText,
                       default_aes = 
                         modifyList(GeomText$default_aes,
                                    aes(size = from_theme(fontsize))))


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
