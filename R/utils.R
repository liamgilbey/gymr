
# Check how many steps have been completed
gym_check_steps <- function(self, private){
  if(private$num_steps >= private$length){
    private$done <- T
  }
  self
}


# Print the gym env
gym_print <- function(private){
  cat(paste0("<Gym Object - Step ", private$num_steps, "/", private$length, ">\n"))
}

# Get the private data from the gym object
gym_get <- function(private){
  r_list <- as.list(private)
  class(r_list) <- c("gym_env", "list")
  return(r_list)
}

# Print a gym env
#' @export
print.gym_env <- function(x){
  title <- "<Gym Environment>\n"
  names <- paste0("  ", names(x), ":")
  values <- paste0(unlist(lapply(x, gym_formatter), recursive = F), "\n")
  cat(paste0(title, paste(names, values, collapse = "")))
}

# Format the gym objects
gym_formatter <- function(x){
  this_class <- class(x)
  if(length(x) > 1){
    y <- paste0(this_class, "(", length(x), ")")
  } else if(this_class == "function"){
    y <- this_class
  } else{
    y <- x
  }
  y
}
