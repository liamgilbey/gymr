#' Create a new gym environment
#'
#' @param ... Members to add to the gym environment. This must contain at least a 'game', 'action_space' and 'observation_space' member
#' @export
gym_new <- function(...){
  members <- list(...)
  member_names <- names(members)
  if (length(members) != sum(nzchar(member_names))) {
    stop("All members of a gym env object must be named.")
  }
  required_members <- c("game", "action_space", "observation_space")
  missing_members <- required_members[!required_members %in% member_names]
  if( length(missing_members) >0){
    stop(paste0("The gym environment is missing the following required members: ", paste(missing_members, collapse = ", ")), call. = F)
  }
  members$game <- gym_validate_game(members$game, members$action_space, members$observation_space)
  class(members) <- c("gym_env", "list")
  members
}
