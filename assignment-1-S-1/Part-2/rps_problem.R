# RopePaperScissors problem
rm(list = ls())

rps <- function(k) {
  
  # define number of lifes to the contestants
  A <- k
  B <- k
  
  # create a sequence of trys 
  move <- c('R', 'S', 'P')
  
  # game doesn't started yet ...
  total_trials <- 0
  
  # starting the game here
  while(A > 0 & B > 0) {
    
    # increase number of trials
    total_trials = total_trials + 1
    
    # sample the move for each player
    A_move <- sample(move, 1)
    B_move <- sample(move, 1)
    
    #
    # decision making algorithm here ...
    # tie? do nothing, otherwise check moves
    #
    if(A_move == B_move) {
      # tie
      # do nothing here
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


#
# *** START HERE: ***
# 

# predefined variables
n = 10^5
k3 = 3
k5 = 5

# 
# perform 10^5 simulation
# for 3 lives and 5 lives
#
Y3 <- replicate(n, rps(k3))
Y5 <- replicate(n, rps(k5))

#
# b) calculate average number of times
# needed for winning when number of lives is 3 and 5
#
mean_Y3 <- mean(Y3)
mean_Y5 <- mean(Y5)

#
# c) estimate the probability of
# 8 or more trials for both cases
#
mean_Y3_8 = mean(Y3 >= 8)
mean_Y5_8 = mean(Y5 >= 8)

#
# store all numbers that are
# >= 8 in a separate subset
#
mean_Y3_8_subset = Y3[Y3 >= 8]
mean_Y5_8_subset = Y5[Y5 >= 8]

#
# plot both results by means of
# histogram, prob: T; breaks: sequence (5, 50, 1)
#
hist(mean_Y3_8_subset, prob=T, density = 12, breaks = seq(k3, 50, 1))
hist(mean_Y5_8_subset, prob=T, density = 12, breaks = seq(k5, 50, 1))


