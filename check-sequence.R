#' @param sequence: int/char array of the sequence produces
#' @param num: how many different elements shoudl there be in a sequence

check_sequence = function (sequence, num){
  mat = matrix(nrow = num, ncol = num, byrow = TRUE)
  for (n in 1:num){
    for (m in 1:num){
      mat[m, n] = 0
    }
  }
  for (i in 1:length(sequence)){
    a = sequence[i]
    b = sequence[i + 1]
    mat[a, b] = mat [a, b] + 1
  }
  print(mat)
  return(mat)
}
