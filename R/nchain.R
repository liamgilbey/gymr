

nchain_function <- function(state, action, ...){
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
