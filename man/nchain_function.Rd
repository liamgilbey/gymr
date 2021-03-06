% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nchain.R
\name{nchain_function}
\alias{nchain_function}
\title{Nchain func}
\usage{
nchain_function(state, action)
}
\arguments{
\item{state}{An integer (1:5) representing the current state you are in the chain.}

\item{action}{An integer (1:2) representing what action to take. 1 equates to moving backwards along the chain, and 2 moves further down the chain.}
}
\description{
A simple function used as an example for Reinforcement learning.
}
\details{
It consists of five states, and two actions. Moving right from state 1 through
to state 5 offers no reward, and moving back along the chain at any point leads you back to the beginning with a small reward. There is a large
reward by trying to exceed state 5. The purpose is try and teach an algorithm to learn to wait for longer term benefits.
This is intended to be used in conjunction with \code{build_gym_env}
}
\examples{
# no reward
nchain_function(1, 2)

# small reward
nchain_function(2, 1)

# big reward
nchain_function(5, 2)

# incorporating into a new environment
nchain <- build_gym_env(func = nchain_function,
  action_space = c(1, 2),
  observation_space = 1:5)
}
