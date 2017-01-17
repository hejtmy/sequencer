#' @param n_trials: number or trials in the experiment - should be half nad half
#' @param n_points: hwo many possible positions of marks and starts are at the 
create_bva_sequence = function(n_trials, n_points, max_same, ego_relation, allo_relation, min_start_distance = 2, 
                               max_start_distance = 3, min_mark_distance = 4){
  starts = c()
  marks = c()
  
  for(i in 1:500){ #Just tries to randomize the sequence. If it can't, it quits
    allo_ego = create_sequence(2, n_trials, max_same) 
    if (!is.null(allo_ego)) break
  }
  if (is.null(allo_ego)){
    print("Couln't do sequence, quitting")
    return(null)
  }
  starts = sample.int(n_points, 1)
  
  for (i_trial in 1:length(allo_ego)){
    start = starts[i_trial]
    #1 is ego, 2 is allo
    if (allo_ego[i_trial] == 1){
      goal = starts[i_trial] + ego_relation
      mark = 0
    } else {
      # places mark in an ark in front of player +- points away
      reverse_min = n_points/2 - min_mark_distance
      mark = sample(c(-reverse_min:min_mark_distance), 1) + start + n_points/2
      mark = fit_circle(mark, n_points)
      goal = mark + allo_relation
      goal = fit_circle(goal, n_points)
    }
    goal = fit_circle(goal, n_points)
    marks = c(marks, mark)
    next_start = sample(c(-max_start_distance:-min_start_distance, 
                          min_start_distance:max_start_distance), 1) + goal
    next_start = fit_circle(next_start, n_points)
    starts = c(starts, next_start)
  }
  starts = starts[-length(starts)] #removes redundant last start
  return(list(allo_ego = allo_ego, starts = starts, marks = marks))
}

#' Function fits the point on the circle of points 
fit_circle = function(number, n_points){
  number = (number + n_points) %% n_points
  if (number == 0) number = n_points #we lose that in the division
  return(number)
}