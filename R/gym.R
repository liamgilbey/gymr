
#' Build a gym environment
#'
#' A gym environment is designed to work with functions to optimise decisions through reinforcement learning. It is designed to be able to plug in
#' functions to tackle new reinforcement learning tasks, and to take care of the running of these functions using R6 classes.
#'
#' @section Creating the gym:
#' A gym environment is an R6 object, that can be created with \code{gym_env$new()}. It has the following arguments:
#' \describe{
#'   \item{env}{The environment we want to perform reinforcement learning on. This is a function object that determines rewards and new states
#'   based on input states and actions.}
#'   \item{length}{The number of actions within the environment until it is considered to be done. By default this is 1,000 actions.}
#' }
#'
#' @section Using the gym:
#' There are three key functions to use with a gym class.
#' \describe{
#'   \item{\code{gym_env$step(a)}}{This function allows us to step through the environment we have defined above. We supply an action, and gym then
#'   runs this action through the environment}
#'   \item{\code{gym_env$reset()}}{When we don't want to worry about recreating a gym object, we can just reset the object back to the state of
#'   creation. This clears all cumulative rewards etc.}
#'   \item{\code{gym_env$get()}}{This function allows us to extract the private values within the R6 class.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # simple gym environment with nchain
#' mygame <- gym_env$new(nchain)
#'
#' # run through one step of the environment
#' mygame$step(1)
#'
#' # get the results
#' mygame$get()
#' }
#'
#' @name gym_env
NULL

gym_env <- R6::R6Class(
  "gym",

  public = list(
    initialize = function(env, length = 1000){
      gym_init(self, private, env, length)
    },
    print = function(){
      gym_print(private)
    },
    step = function(a, r = 0.8, ...){
      gym_step(self, private, a, r, ...)
    },
    reset = function(){
      gym_reset(self, private)
    },
    get = function(){
      gym_get(private)
    }
  ),

  private = list(
    state = 1,
    total_reward = 0,
    current_reward = 0,
    num_steps = 0,
    done = F,
    game = NA,
    length = NA,
    action_space = NA,
    observation_space = NA
  )
)

# Initialize a gym environment object
gym_init <- function(self, private, env = NULL, length){
  env <- gym_validate_env(env)
  private$game <- env$game
  private$length <- length
  private$action_space <- env$action_space
  private$observation_space <- env$observation_space
  self
}

# Complete an action in the specified game
gym_step <- function(self, private, a, r, ...){
  a <- gym_validate_action(private, a)
  a <- gym_randomize(self, private, a, r)
  game_results <- private$game(state = private$state, action = a, ...)
  s <- gym_validate_state(private, game_results$next_state)
  private$state <- game_results$next_state
  private$current_reward <- game_results$reward
  private$total_reward <- private$total_reward  + game_results$reward
  private$num_steps <- private$num_steps + 1
  gym_check_steps(self, private)
  self
}

# Randomize the action to some degree
gym_randomize <- function(self, private, a, r){
  if(runif(1, 0, 1) >= r){
    as <- private$action_space[!private$action_space %in% a]
    if(length(as) > 1){
      a <- sample(as, 1)
    } else {
      a <- as
    }
  }
  a
}

# Reset the gym environment
gym_reset <- function(self, private){
  private$state <- 1
  private$total_reward <- 0
  private$current_reward <- 0
  private$done <- F
  private$num_steps <- 0
  self
}
