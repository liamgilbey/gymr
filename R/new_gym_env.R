#' Build a new gym environment
#'
#' Take a function intended for training with reinforcement learning and create a valid gym environment object.
#'
#' @param ... Members to add to the new gym environment. This must contain at least a 'func', 'action_space' and 'observation_space' member
#' @section Required members:
#' A new gym environment requires the following key members:
#' \describe{
#'   \item{func}{The func function you want to learn parameters for}
#'   \item{action_space}{A vector of possible actions for the func}
#'   \item{observation_space}{A vector of possible observations from the func function}
#' }
#'
#' @section Optional members:
#' There are also the following optional members:
#' \describe{
#'    \item{random}{A number between 0 and 1 dictating the level of randomness applied when running the func function. A lower number means it will
#'    return the intended result less often (exploring the total observation space more often)}
#' }
#' @export
#' @examples
#' \dontrun{
#' build_gym_env(func = nchain_function,
#'   action_space = c(1, 2),
#'   observation_space = 1:5),
#'   random = 0.5
#'  }
build_gym_env <- function(...){
  members <- list(...)
  member_names <- names(members)
  if (length(members) != sum(nzchar(member_names))) {
    stop("All members of a gym env object must be named.")
  }
  required_members <- c("func", "action_space", "observation_space")
  missing_members <- required_members[!required_members %in% member_names]
  if( length(missing_members) >0){
    stop(paste0("The gym environment is missing the following required members: ", paste(missing_members, collapse = ", ")), call. = F)
  }
  members$func <- gym_validate_func(members$func, members$action_space, members$observation_space)
  if(!"random" %in% member_names){
    members$random <- 0.8
  }
  class(members) <- c("gym_env", "list")
  members
}
