TrainModel <- function(A_prob, B_prob, N_A, N_B, N_sample, N_Test, n_flips_sample, A_Draw_Prob) {
  set.seed(100)
  binom <- rbinom(1, N_Sample, A_Draw_prob)
  binom 
  
  train <- SimulateCoinFlip(binom, n_flips_sample, A_prob, B_prob, N_Sample)
  A_unique_train <- train[1][[1]]
  A_counts_train <- train[2][[1]]
  B_unique_train <- train[3][[1]]
  B_counts_train <- train[4][[1]]
  
  
  #Get the corresponding SigMA score
  master_set <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  SigMA_scores <- rep(0, 11)
  
  for (index in 1:length(master_set)) {
    num <- master_set[index]
    if (num %in% A_unique_train) {
      A_index <- match(num, A_unique_train)
      if (num %in% B_unique_train) {
        B_index <- match(num, B_unique_train)
        SigMA_scores[index] <- (A_counts_train[A_index] / (A_counts_train[A_index] + B_counts_train[B_index]))
        #print(SigMA_scores)
      } else{
        SigMA_scores[index] <- 1
      }
    } else if ((num %in% B_unique_train)) {
      SigMA_scores[index] <- 1
    } else {
      SigMA_scores[index] <- 0.5
    }
  }
  
  return(SigMA_scores)
}
