# Monte Carlo Optimization Example: Finding the best "coin flip" strategy

# Problem: We have a "biased" coin. We don't know the true probability of heads (p).
# We want to find the best strategy for predicting heads or tails to maximize our "score."

# Score System:
# - Correct prediction: +1 point
# - Incorrect prediction: -1 point
library(dplyr)

# True probability of heads (unknown to us in a real-world scenario)
true_p <- 0.6 # Example: The coin is slightly biased towards heads

# Function to simulate a coin flip
flip_coin <- function(p) {
    if (runif(1) < p) { # Generate a uniform random number between 0 and 1
        return("H") # Heads
    } else {
        return("T") # Tails
    }
}

# Function to calculate the score for a given prediction strategy
calculate_score <- function(strategy, num_trials = 100) {
    score <- 0
    for (i in 1:num_trials) {
        outcome <- flip_coin(true_p)
        prediction <- strategy(outcome) # strategy function decides the prediction
        if (outcome == prediction) {
            score <- score + 1
        } else {
            score <- score - 1
        }
    }
    return(score)
}

# Define some prediction strategies

# 1. Always predict heads
always_heads <- function(outcome) {
    return("H")
}

# 2. Always predict tails
always_tails <- function(outcome) {
    return("T")
}

# 3. Random prediction (50/50 chance)
random_prediction <- function(outcome) {
    if (runif(1) < 0.5) {
        return("H")
    } else {
        return("T")
    }
}

# 4. A simple "learning" strategy (very basic example)
learning_strategy <- function(previous_outcome) {
    # If the last outcome was heads, predict heads, otherwise predict tails.
    # This has a slight tendency to adapt to the bias (but it's very simple)
    if (length(previous_outcome) == 0) { # For the first flip, predict randomly
        if (runif(1) < 0.5) {
            return("H")
        } else {
            return("T")
        }
    } else if (last(previous_outcome) == "H") {
        return("H")
    } else {
        return("T")
    }
}

# Monte Carlo Simulation: Evaluate each strategy many times

num_simulations <- 1000 # Number of times to run each strategy
results <- data.frame(strategy = character(), avg_score = numeric())

strategies <- list(always_heads, always_tails, random_prediction, learning_strategy)
strategy_names <- c("Always Heads", "Always Tails", "Random", "Learning")

for (i in 1:length(strategies)) {
    scores <- replicate(num_simulations, calculate_score(strategies[[i]])) # Run the simulations
    avg_score <- mean(scores)
    results <- rbind(results, data.frame(strategy = strategy_names[i], avg_score = avg_score))
}

print(results)

# Analyze results: The strategy with the highest average score is the "best" (in this simulated context)

# Note: In a real optimization problem, you might have many more parameters
# and a more complex "objective function" to optimize.  This example is very simplified.
