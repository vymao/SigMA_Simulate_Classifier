PrepareTest <- function(A_unique_test, A_counts_test, B_unique_test, B_counts_test, SigMA_scores) {
  samples <- c();
  prob <- c();
  for (i in 1:length(A_counts_test)) {
    current_prob_index <- match(A_unique_test[i], master_set)
    Sig_score <- SigMA_scores[current_prob_index]
    A_counts_test[i]
    for (j in 1:A_counts_test[i]) {
      samples <- c(samples, 1)
      prob <- c(prob, Sig_score)
    }
  }
  
  for (i in 1:length(B_counts_test)) {
    current_prob_index <- match(B_unique_test[i], master_set)
    Sig_score <- SigMA_scores[current_prob_index]
    for (j in 1:B_counts_test[i]) {
      samples <- c(samples, 0)
      prob <- c(prob, Sig_score)
    }
  }
  
  return(list(samples, prob))
}