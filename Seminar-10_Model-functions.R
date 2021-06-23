# Seminar 10: Model functions 


# Load required package ----
library(decisionSupport)


# Learning to use the vv() function ----
valvar <- vv(var_mean = 100,  # Mean of variable to be varied 
             var_CV = 5,      # Coefficient of variation in % 
             n = 40)          # Number of values to produce (integer) 
plot(valvar)


# Add 'absolute_trend' argument ----
valvar <- vv(var_mean = 50, 
             var_CV = 10, 
             n = 30, 
             absolute_trend = 10) # Absolute increment in 'var_mean' in each time step
plot(valvar)


# Add 'relative_trend' argument ----
valvar <- vv(var_mean = 100, 
             var_CV = 5, 
             n = 40, 
             relative_trend = 5) # Relative trend (in %) in 'var_mean' in each time step 
plot(valvar)


# Simulate occurrence of random events ----
chancevar <- chance_event(chance = 0.25,  # Probability of risky event occurring 
                          value_if = 3,   # Output value in case of event happening 
                          n = 20)
plot(chancevar)

# Additional arguments 
chancevar <- chance_event(chance = 0.1,
                          value_if = 5,
                          value_if_not = 20,  # Output value in case of event not happening 
                          n = 100,
                          CV_if = 10)  # Coefficient of variation for introducing randomness into 'value_if' data set 
plot(chancevar)


# Gompertz function: Yield prediction for perennials ----

# Create vector: Maximum harvest = 500, which is achieved in 10 years (i.e. 100% by the second yield estimate)
gomp_yield <- gompertz_yield(max_harvest = 500,
                             time_to_first_yield_estimate = 5,
                             first_yield_estimate_percent = 10,
                             time_to_second_yield_estimate = 10,
                             second_yield_estimate_percent = 100,
                             n_years = 30)
plot(gomp_yield)
