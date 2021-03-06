
<!-- README.md is generated from README.Rmd. Please edit that file -->
gymr
====

[![Travis Build Status](https://travis-ci.org/liamgilbey/gymr.svg?branch=master)](https://travis-ci.org/liamgilbey/gymr)

Building reinforcement learning environments inside of R.

Description
-----------

`gymr` is a package inspired by [Open AI's gym](https://gym.openai.com/). It is designed to be a platform to build environments to perform reinforcement learning, but not to actually carry out any learning itself.

It may never be any more than a way for me to understand R6 classes, but it has been designed to be able to handle a variety of functions to train, all from entirely inside of R.

Installation
------------

Currently only available through github:

``` r
# install.packages("devtools")
devtools::install_github("liamgilbey/gymr")
```

Usage
-----

`gymr` comes with a simple reinforcement learning function for illustration purposes - `nchain_function`.

With a predefined function, building a new gym environment is easy.

``` r
library(gymr)
nchain <- build_gym_env(func = nchain_function,
 action_space = c(1, 2),
 observation_space = 1:5)
```

We then need to load this environment into a new object.

``` r
myenv <- load_gym_env$new(nchain)
```

From here we can make steps through the environment to receive the corresponding state and reward.

``` r
myenv$step(1)
#> <Gym Object - Step 1/1000>
```

If at any point we want to review the state of the environment, we can with `get`.

``` r
myenv$get()
#> <Gym Environment>
#>   random: 0.8
#>   observation_space: integer(5)
#>   action_space: numeric(2)
#>   length: 1000
#>   func: function
#>   done: FALSE
#>   num_steps: 1
#>   current_reward: 2
#>   total_reward: 2
#>   state: 1
```
