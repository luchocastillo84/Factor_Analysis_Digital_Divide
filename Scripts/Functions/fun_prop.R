fun_prop <- function(x, mean, sd, proportion){
  proportion * dnorm(x = x, mean = mean, sd = sd)
}