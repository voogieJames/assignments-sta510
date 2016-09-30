# monty hall problem
rm(list = ls())

monty_hall_problem <- function(switcher=FALSE) {
  
  # predefined variables
  n=10^3
  doors_number <- 3
  win <-0
  
  # create sequence of doors
  doors <- seq(1, doors_number, 1)
  
  #
  # simulate number of doors with cars
  # and number attempts to choose the correct door
  #
  prizes <- floor(runif(n, 1, 4))
  attempts <- floor(runif(n, 1, 4))
  
  for (i in 1:n) {
    
    # get the door with the prize and the door guessed by contestant
    car <- prizes[i]
    door_guessed <- attempts[i]
    
    #
    # next, custom 'get_other_door' func is called,
    # the purpose of this func is to return 3rd door
    # from the array of doors. 
    # here we need it to open the door with goat
    #
    goat_door <- get_other_door(doors, car, door_guessed)
    
    #
    # check whether contestant is going to switch
    # (default: switcher = FALSE)
    #
    if(switcher == TRUE) {
      door_guessed = get_other_door(doors, goat_door, door_guessed)
    }
    
    if (car == door_guessed) {
      win = win + 1
    }
  }
  
  print(win/n)
}

#
# custom func to return door from the array
# that is not metioned as an input param
#
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

#
# start here
# running this function without input argument
# will set 'switcher' variable to FALSE, which
# means that contestant will not switch the door
# otherwise you should call the func with TRUE param
#
monty_hall_problem()



