#' Function fits the point on the circle of points 
fit_circle = function(number, n_points){
  number = (number + n_points) %% n_points
  if(length(number) > 1){
    print('what the hell')
  } 
  if (number == 0) number = n_points #we lose that in the division
  return(number)
}