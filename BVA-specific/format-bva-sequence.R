format_bva_sequence = function(ls){
  allo_ego = rep("nothing", length(ls$allo_ego))
  for (i in 1:length(ls$allo_ego)){
    allo_ego[i] = ifelse(ls$allo_ego[i] == 1, "Ego", "Allo")
  }
  allo_ego = paste(allo_ego, collapse = '",\n "')
  ls$starts <- sapply(ls$starts, minus_one_or_zero)
  ls$marks <- sapply(ls$marks, minus_one_or_zero)
  marks = paste(ls$marks, collapse = ",\n")
  starts = paste(ls$starts, collapse = ",\n")
  cat(allo_ego)
  print("marks")
  cat(marks)
  print("starts")
  cat(starts)
}

minus_one_or_zero <- function(x){
  ret = ifelse(((x-1)<0), 0, x-1)
  return(ret)
}