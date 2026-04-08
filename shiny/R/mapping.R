map_suggestions <- function(upload_cols, required_cols, min_score = config$mapping_min_score) {
  upload_cols <- trimws(upload_cols)
  required_cols <- trimws(required_cols)
  if (length(upload_cols) == 0 || length(required_cols) == 0) {
    return(data.frame(required_column = character(0), candidate_column = character(0), score = numeric(0)))
  }
  grid <- expand.grid(required_column = required_cols, candidate_column = upload_cols, stringsAsFactors = FALSE)
  grid$score <- stringdist::stringsim(grid$required_column, grid$candidate_column, method = "jw")
  grid$score <- round(grid$score * 100, 1)
  grid <- grid[order(grid$required_column, -grid$score), ]
  grid <- grid[grid$score >= min_score, ]
  grid
}

pick_best_mapping <- function(suggestions) {
  if (nrow(suggestions) == 0) return(list())
  best <- suggestions |> dplyr::group_by(required_column) |> dplyr::slice_max(score, n = 1, with_ties = FALSE)
  setNames(best$candidate_column, best$required_column)
}
