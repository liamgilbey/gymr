
# Validate the gym game function
gym_validate_game <- function(game, action_space, observation_space){
  # check it is a function
  if(!is.function(game))stop("The `game` object needs to be a function...", call. = F)

  # check the arguments are there correctly
  game_args <- formalArgs(game)
  required_args <- c("state", "action")
  missing_args <- required_args[!required_args %in% game_args]
  if(length(missing_args) >0){
    stop(paste0("The supplied game function has the following missing arguments: ", paste(missing_args, collapse = ", ")), call. = F)
  }

  # check that we get an output that we want to get
  required_results <- c("next_state", "reward")
  test_result <- game(observation_space[1], action_space[1])
  missing_results <- required_results[!required_results %in% names(test_result)]
  if(length(missing_results) >0){
    stop(paste0("The supplied game function does not return the following missing arguments: ", paste(missing_results, collapse = ", ")), call. = F)
  }

  game
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

gym_validate_env <- function(env){
  if(!"gym_env" %in% class(env)){
    stop("The supplied env argument is not a `gym_env` object", call. = F)
  }
  env
}
