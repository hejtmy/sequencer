#' @param units: how many different units are there
#' @param iterations: how many times shoulld each pair follow
#' @param max_same: how many same units can follow
#' @param sample_replace: defines sampling replacement parameter
#' @param exclude_pairs: list of vector ints defining which pairs to exclude (say list(c(1,3),c(2,4)))
#' 
#' Creates sequence of numbers in a way that two numbers occur after each other exactly defined number of times
create_sequence = function (units, iterations, max_same = 0, sample_replace = FALSE, exclude_pairs = c()){
  
  #creates matrix with possible iterations
  matrix = matrix(data = iterations, nrow = units, ncol = units)
  
  #creates empty matrix to be compared with the matix state
  #If matrix == fullMatrix the script has nowhere to go and finnishes
  fullMatrix = matrix(data = 0, nrow = units, ncol = units)
  
  #creates third empty matrix that fill us when possible units dissapear from matrix
  
  discardedMatrix = matrix(data = 0, nrow = units, ncol = units)
  discardedMatrix2 = matrix(data = 0, nrow = units, ncol = units)
  
  # fills in the matrix at n, n with 0
  # therefore pair the same numbers can't follow one another (11,22 .. etc. aren't allowed)
  if (max_same == 0){
    for (n in 1:units){
      matrix[n, n] = 0
    }
  }
  if (length(exclude_pairs)!= 0){
    for(pair in exclude_pairs){
      if(length(pair) != 2){
        print("Each pair needs to have exacly two elements")
        return(NULL)
      }
      x <- pair[1]
      y <- pair[2]
      if((x > units) || (y > units)){
        print("Pairs cannot contain higher number than number of units")
        return(NULL)
      }
      matrix[x, y] <- 0
    }
  }
  
  #creates the first unit in the sequence
  last = sample.int(units, 1, replace = sample_replace)
  
  #starts writing the sequence
  sequence = last
  n_repetitions = 0
  
  #now function starts creating the sequence untill it has nowhere to go
  while (!all(matrix == fullMatrix)){
    
    assigned = FALSE
    assignTry = 0
    # this loop randomly chooses next unit and if the itteration is possible it assigns it and skips forwards
    while (!assigned){
      possib = sample.int(units, 1, replace = sample_replace)
      if (matrix[last, possib] != 0){
        if (possib == last){n_repetitions = n_repetitions + 1} else {n_repetitions = 0}
        #checks if we didn't already have it many times in a row
        if (n_repetitions <= max_same){
          # lowers the number of iterations
          matrix[last, possib] = matrix[last, possib] - 1
          assigned = TRUE
        }
      }
      # if the loop can't find possible unit to iterate to it means it got 
      # stuck. Return an apology and quit the function
      assignTry = assignTry + 1
      if (assignTry > 50){
        print("Sorry, couldn't randomise the sequence. Let me try that again.")
        return(NULL)
      }
    }
    
    # if a row becomes empty we can't return to it anymore, as it wont be able to iterate to other unit
    # therefore we delete the possibility to return to it from any other unit
    if (all(matrix[last, ] == fullMatrix[last, ])){
      for (i in 1:units){
        # but make note of discarded iterations to discardedMatrix
        discardedMatrix[i, last] = matrix[i, last]
        matrix[i, last] = 0
      }
    }
    last = possib
    #adds to the sequence
    sequence = c(sequence, last)
  }
  
  # This part creates a backup to come back to if the randomising of the next 
  # section doesn't work out the first time
  discardedMatrix2 = discardedMatrix
  last2 = last
  sequence_save = sequence
  n_repetitions = 0
  # Do this until the discardedMatrix is empty
  while(!all(discardedMatrix == fullMatrix)){
    assigned = FALSE
    
    # Looks at the row for the last randomised unit. If it's empty it can't iterate to 
    # another unit. But as the whole matrix isn't empty yet, there are other units to be
    # assigned. So there must have been some mistake. We load the backup and start again
    if (all(discardedMatrix[last, ] == fullMatrix[last, ])){
      discardedMatrix = discardedMatrix2
      last = last2
      sequence = sequence_save
    }
    
    # DO this until you find a possible unit
    while(!assigned){
      # RAndomises a number
      possib = sample.int(units, 1, replace = sample_replace)
      if (possib == last){n_repetitions = n_repetitions + 1} else {n_repetitions = 0}
      # If the last unit can iterate ot this number, it does
      if(discardedMatrix[last, possib] != 0){
        #checks if we didn't already have it many times in a row
        if (n_repetitions > max_same) return(NULL)
        # Decrease number of possible iterations to that unit
        discardedMatrix[last, possib] = discardedMatrix[last, possib] - 1
        last = possib
        #Adds to the sequence
        sequence = c(sequence, last)
        assigned = TRUE
      } 
    }
  }
  
  check_sequence(sequence, units)
  #paste(c(sequence, toString(last)), sep = "", collapse = "")
  return(sequence)
}
