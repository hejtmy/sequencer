check_sequence = function (sequence, num){
  mat = matrix(nrow = num, ncol = num, byrow = TRUE)
  for (n in 1:num){
    for (m in 1:num){
      mat[m, n] = 0
    }
  }
  for (i in 1:nchar(sequence)){
    a = as.integer(substr(sequence, i, i))
    b = as.integer(substr(sequence, i+1, i+1))
    mat[a, b] = mat [a, b]+1
  }
  print(mat)
  return(mat)
}

