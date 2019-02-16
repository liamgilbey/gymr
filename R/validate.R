
# Validate the gym func function
gym_validate_func <- function(func, action_space, observation_space){
  # check it is a function
  if(!is.function(func))stop("The `func` object needs to be a function...", call. = F)

  # check the arguments are there correctly
  func_args <- formalArgs(func)
  required_args <- c("state", "action")
  missing_args <- required_args[!required_args %in% func_args]
  if(length(missing_args) >0){
    stop(paste0("The supplied func function has the following missing arguments: ", paste(missing_args, collapse = ", ")), call. = F)
  }

  # check that we get an output that we want to get
  required_results <- c("next_state", "reward")
  test_result <- func(observation_space[1], action_space[1])
  missing_results <- required_results[!required_results %in% names(test_result)]
  if(length(missing_results) >0){
    stop(paste0("The supplied func function does not return the following missing arguments: ", paste(missing_results, collapse = ", ")), call. = F)
  }

  func
}

# Validate the gym action
gym_validate_action <- function(private, a){
  if(!a %in% private$action_space){
    stop(paste0(a, " is not a valid action for this environment"), call. = F)
  }
  a
}

# Validate the gym state
gym_validate_state <- function(private, s){
  if(!s %in% private$observation_space){
    stop(paste0(s, " is not a valid state for this environment"), call. = F)
  }
  s
}

# Validate the gym environment
gym_validate_env <- function(env){
  if(!"gym_env" %in% class(env)){
    stop("The supplied env argument is not a `gym_env` object", call. = F)
  }
  env
}
