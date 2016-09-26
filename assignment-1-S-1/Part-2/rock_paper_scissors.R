# monty hall problem
rm(list = ls())

rps <- function(k) {
  
  A <- k
  B <- k
  
  move <- c('R', 'S', 'P')
  
  total_trials <- 0
  
  while(A > 0 & B > 0) {
    
    A_move <- sample(move, 1)
    B_move <- sample(move, 1)
    
    total_trials = total_trials + 1
    
    if(A_move == B_move) {
      # tie
    } 
    else if (A_move == "R") {
      if(B_move == "S") {
        B = B - 1
      }
      else {
        A = A - 1
      }
    }
    else if (A_move == "S") {
      if(B_move == "P") {
        B = B - 1
      }
      else {
        A = A - 1
      }
    }
    else if (A_move == "P") {
      if(B_move == "R") {
        B = B - 1
      }
      else {
        A = A - 1
      }
    }
  }
  
  return(total_trials)
}

n = 10^5

k3 = 3
k5 = 5

Y3 <- replicate(n, rps(k3))
Y5 <- replicate(n, rps(k5))

mean_Y3 <- ceiling(mean(Y3))

sum_Y3 =  mean(Y3>=8)
sum_Y3_1 = sum(Y3 >= 8) / n
sum_Y5 = sum(Y5 >= 8) / n

plot(sum_Y3, Y3, type = 's')
