#' Nchain func
#'
#' A simple function used as an example for Reinforcement learning.
#'
#' It consists of five states, and two actions. Moving right from state 1 through
#' to state 5 offers no reward, and moving back along the chain at any point leads you back to the beginning with a small reward. There is a large
#' reward by trying to exceed state 5. The purpose is try and teach an algorithm to learn to wait for longer term benefits.
#' This is intended to be used in conjunction with \code{build_gym_env}
#'
#' @param state An integer (1:5) representing the current state you are in the chain.
#' @param action An integer (1:2) representing what action to take. 1 equates to moving backwards along the chain, and 2 moves further down the chain.
#'
#' @export
#' @examples
#' # no reward
#' nchain_function(1, 2)
#'
#' # small reward
#' nchain_function(2, 1)
#'
#' # big reward
#' nchain_function(5, 2)
#'
#' # incorporating into a new environment
#' nchain <- build_gym_env(func = nchain_function,
#'   action_space = c(1, 2),
#'   observation_space = 1:5)
nchain_function <- function(state, action){
  if(state < 5 && action == 2){
    next_state <- state + 1
    reward <- 0
  }
  if(action == 1){
    next_state <- 1
    reward <- 2
  }
  if(state >=5 && action == 2){
    next_state <- 5
    reward <- 10
  }
  list(next_state = next_state,
       reward = reward)
}



