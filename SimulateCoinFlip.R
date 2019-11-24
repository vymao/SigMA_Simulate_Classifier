SimulateCoinFlip <- function(N_CoinA, n_flips, A_prob, B_prob, N_A, N_B, N_Tot, bins){
  set.seed(100) 
  A_flips <- rbinom(N_CoinA, n_flips, A_prob)
  B_flips <- rbinom(N_Tot - N_CoinA, n_flips, B_prob)
  
  A_likelihoods <- c()
  B_likelihoods <- c()
  
  for (val in A_flips) {
    A_vector <- c(rep(1, val), rep(0, n_flips - val))
    
    LL <- function(prob){
      R = dbern(A_vector, prob)
      -sum(log(R))
    }
    
    output <- suppressWarnings(mle(LL, start = list(prob = 0.9)))
    prob <- slot(output, 'coef')
    
    A_likelihoods <- c(A_likelihoods, prob[[1]])
  }
  
  
  for (val in B_flips) {
    B_vector <- c(rep(1, val), rep(0, n_flips - val))
    
    LL <- function(prob){
      R = dbern(B_vector, prob)
      -sum(log(R))
    }
    
    output <- suppressWarnings(mle(LL, start = list(prob = 0.9)))
    prob <- slot(output, 'coef')
    
    B_likelihoods <- c(B_likelihoods, prob[[1]])
  }
  
  
  A_binned <- round(A_likelihoods, digits = 1)
  B_binned <- round(B_likelihoods, digits = 1)
  
  A_unique <- unique(A_binned)
  B_unique <- unique(B_binned)
  
  A_counts <- rep(0, length(A_unique))
  B_counts <- rep(0, length(B_unique))
  for (i in 1:length(A_unique)) {
    for (j in A_binned) {
      if (j == A_unique[i]) {
        A_counts[i] <- A_counts[i] + 1
      }
    }
  }
  for (i in 1:length(B_unique)) {
    for (j in B_binned) {
      if (j == B_unique[i]) {
        B_counts[i] <- B_counts[i] + 1
      }
    }
  }

  return(c(A_unique, A_counts, B_unique, B_counts))
}