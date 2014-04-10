createSequence <- function (units,iterations,bool=FALSE){
 
  #creates matrix with possible iterations
  matrix = matrix(data=iterations,nrow=units,ncol=units)
  
  #creates empty matrix to be compared with the matix state
  #If matrix == fullMatrix the script has nowhere to go and finnishes
  fullMatrix = matrix(data=0,nrow=units,ncol=units)
  
  #creates third empty matrix that fill us when possible units dissapear from matrix
  
  discardedMatrix = matrix(data=0,nrow=units,ncol=units)
  discardedMatrix2 = matrix(data=0,nrow=units,ncol=units)
 
  #fills in the matrices# 
  for (n in 1:units){
    matrix[n,n]=0
  }
  
  #creates the first unit in the sequence
  last = sample.int(units,1,replace=bool)
  
  #starts writing the sequence
  sequence = toString(last)
    
  #now function starts creating the sequence untill it has nowhere to go
  while (!all(matrix==fullMatrix)){
    assigned = FALSE
    
    # this loop randomly chooses next unit and if the itteration is possible it assigns it and skips forwards
    while (!assigned){
      possib = sample.int(units,1,replace=bool)
      if (matrix[last,possib]!=0){
        # lowers the number of iterations
        matrix[last,possib]=matrix[last,possib]-1
        assigned = TRUE
      }
    }
    
    # if a row becomes empty we can't return to it anymore, as it wont be able to iterate to other unit
    # therefore we delete the possibility to return to it from any other unit
    if (all(matrix[last,]==fullMatrix[last,])){
      for (i in 1:units){
        # but make note of discarded iterations to discardedMatrix
        discardedMatrix[i,last]=matrix[i,last]
        matrix[i,last]=0
      }
    }
    last = possib
    #adds to the sequence
    sequence=paste(c(sequence,toString(last)),sep="",collapse="")
  }
  
  discardedMatrix2 = discardedMatrix
  last2 = last
  sequence2 = sequence
  
  while(!all(discardedMatrix==fullMatrix)){
    assigned=FALSE
    if (all(discardedMatrix[last,]==fullMatrix[last,])){
      discardedMatrix = discardedMatrix2
      last = last2
      sequence=sequence2
    }
    while(!assigned){
      possib = sample.int(units,1,replace=bool)
      if(discardedMatrix[last,possib]!=0){
        discardedMatrix[last,possib]=discardedMatrix[last,possib]-1
        last = possib
        sequence=paste(c(sequence,toString(last)),sep="",collapse="")
        assigned=TRUE
      } 
    }
  }
  return(sequence)
}


checkSequence <- function (sequence,num){
  mat<-matrix(nrow=num,ncol=num,byrow=TRUE)
  for (n in 1:num){
    for (m in 1:num){
      mat[m,n]=0
    }
  }
  for (i in 1:nchar(sequence)){
    a<-as.integer(substr(sequence,i,i))
    b<-as.integer(substr(sequence,i+1,i+1))
    mat[a,b] <- mat [a,b]+1
  }
  return(mat)
}

