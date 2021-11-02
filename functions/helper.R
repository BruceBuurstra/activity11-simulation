generate_n_samples <- function(n){
  norm <- rnorm(n, mean = 5, sd = 2)
  exp <- rexp(n, rate = 0.05)
  chisq <- rchisq(n, df = 8)
  unif <- runif(n, min = 0, max = 25)
  data <- data.frame(cbind(norm, exp, chisq, unif))
  return(data)
}


distribution_exploration <- function(n, rep, mean, sd, rate, df, min, max){
  
  .samps <- map(1:rep, generate_n_samples)
  
  .samp_means <- map_df(.samps, sample_mean)
  
  .restructured_means <- .samp_means %>% 
    pivot_longer(
      cols = everything(),
      names_to = "distribution",
      values_to = "means"
    )
  
  .plot <- .restructured_means %>% 
    ggplot(aes(x = means)) +
    geom_histogram(aes(y = ..count.. / sum(..count..))) +
    facet_wrap(~ distribution, ncol = 4, scales = "free") +
    labs(y = "proportion")
  
  .tab <- .restructured_means %>% 
    group_by(distribution) %>% 
    summarise(mean = mean(means),
              sd = sd(means))
  
  list(.plot, .tab)
  
}
