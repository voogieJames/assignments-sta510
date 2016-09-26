# monty hall problem
rm(list = ls())

monty_hall_problem <- function(switcher=FALSE) {
  
  n=10^3
  doors_number <- 3
  win <-0
  doors <- seq(1, doors_number, 1)
  
  prizes <- floor(runif(n, 1, 4))
  attempts <- floor(runif(n, 1, 4))
  
  for (i in 1:n) {
    car <- prizes[i]
    door_guessed <- attempts[i]
    
    goat_door <- get_other_door(doors, car, door_guessed)
    
    if(switcher == TRUE) {
      door_guessed = get_other_door(doors, goat_door, door_guessed)
    }
    
    if (car == door_guessed) {
      win = win + 1
    }
  }
  
  print(win/n)
}

get_other_door <- function(doors, car, door_guessed) {
  
  door <- 0
  
  for (i in 1:length(doors)) {
    
    temp_door <- doors[i]
    if(temp_door != car & temp_door != door_guessed) {
      door = temp_door
    }
  }
  
  return(door)
}

monty_hall_problem()



