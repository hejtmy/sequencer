possible_marks <- function(point, n_points, min_dist = 1, max_dist = 100){
  possible_points_far <- possible_far_marks(point, min_dist, n_points)
  possible_points_close <- possible_close_marks(point, max_dist, n_points)
  possible_points <- intersect(possible_points_far, possible_points_close)
}

possible_far_marks <- function(point, min_dist, n_points){
  min_point <- point - min_dist
  max_point <- point + min_dist
  i_impossible <- c(min_point:max_point)
  impossible_points <- sapply(i_impossible, fit_circle, n_points = n_points)
  possible_points <- setdiff(c(1:n_points), impossible_points)
}

possible_close_marks <- function(point, max_dist, n_points){
  min_point <- point - max_dist
  max_point <- point + max_dist
  i_impossible <- c(min_point:max_point)
  possible_points <- sapply(i_impossible, fit_circle, n_points = n_points)
}