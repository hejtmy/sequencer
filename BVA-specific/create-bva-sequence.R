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
      possible_points = possible_far_marks(start, min_mark_distance, n_points)
      mark = sample(possible_points, 1)
      goal = fit_circle(mark + allo_relation, n_points)
    }
    goal = fit_circle(goal, n_points)
    marks = c(marks, mark)
    possible_starts = possible_marks(goal, n_points, min_dist = 1, max_dist = max_start_distance)
    next_start = sample(possible_starts, 1)
    starts = c(starts, next_start)
  }
  starts = starts[-length(starts)] #removes redundant last start
  starts = starts - 1 #reorders them to 0 beginning
  marks = marks
  return(list(allo_ego = allo_ego, starts = starts, marks = marks))
}
