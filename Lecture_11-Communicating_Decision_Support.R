# Lecture 10: Communicating Decision Support 


# Load required packages ----
library(decisionSupport)
library(bayesplot)
library(ggplot2)
library(tidyverse)
library(ggridges)


# Model from seminar 06 ----
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", 
                                              "Management costs in a normal season"))

model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  overall_costs <- Labor_cost + Management_cost
  
  # Estimate the final results from the model
  final_result <- income - overall_costs
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}


# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")

example_mc_simulation


# Communicating the results of Decision Analysis models ----
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",  # Include boxplot
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


# Plot many results together ---- 

# Create the estimate object:
cost_benefit_table <- data.frame(label = c("Revenue", "Costs"),
                                 variable = c("revenue", "costs"),
                                 distribution = c("norm", "norm"),
                                 lower = c(100,  500),
                                 median = c(NA, NA),
                                 upper = c(10000, 5000))

# (a) Define the model function without name for the return value:
profit1 <- function() {
  Decision <- revenue - costs
  cashflow <- rnorm(rep(revenue, 20))
  return(list(Revenues = revenue,
              Costs = costs, 
              cashflow = cashflow, 
              Decision = Decision))
}

compound_figure(model = profit1, 
                input_table = cost_benefit_table, 
                decision_var_name = "Decision",
                cashflow_var_name = "cashflow",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')


# Other visualisation options ---- 

# Create data set of yield distributions of 3 different farming practices 

# Function 'mcmc_intervals()'
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

color_scheme_set("red")

mcmc_intervals(test,prob = 0.5,prob_outer = 0.9,point_est = "median")

# Function 'mcmc_areas()'
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

color_scheme_set("blue")

mcmc_areas(test,prob = 0.9,point_est = "median")


# Comparative density curves ---- 
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

stacked_test <- stack(test)

ggplot(stacked_test, 
       aes(x=values,group=ind,fill=ind )) +
  geom_density(colour=NA,alpha=.5) +
  ylab("Probability density") +
  xlab("Yield")


# Comparative histogram ---- 
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

stacked_test <- stack(test)

ggplot(stacked_test,aes(x=values))+ 
  geom_histogram(data=subset(stacked_test,ind =='practice.1'),
                 aes(fill = ind), alpha = 0.5, bins = 150) + 
  geom_histogram(data=subset(stacked_test,ind == 'practice.2'),
                 aes(fill = ind), alpha = 0.5, bins = 150) +
  geom_histogram(data=subset(stacked_test,ind == 'practice.3'),
                 aes(fill = ind), alpha = 0.5, bins = 150) 


# Plot cashflow (decisionSupport) ---- 

# Create the estimate object (for multiple options):
variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 10, 8000,  500)
upper = c(100000, 50000, 10, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

# Define the model function without name for the return value:
profit1 <- function(x) {
  
  cashflow_option1 <- vv(revenue_option1 - costs_option1, n = n_years, var_CV = 100)
  cashflow_option2 <- vv(revenue_option2 - costs_option2, n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

# Perform the Monte Carlo simulation:
predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")

# Plot the cashflow distribution over time
plot_cashflow(mcSimulation_object = predictionProfit1, cashflow_var_name = "Cashflow_option_one",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "lightskyblue3", color_5_95 = "lightskyblue1",  # Change colours 
              color_median = "red")


# Comparing cashflow distributions over time for multiple decision options ----

# Create the estimate object (for multiple options):
variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 10, 8000,  500)
upper = c(100000, 50000, 10, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

# Define the model function without name for the return value:
profit1 <- function(x) {
  
  cashflow_option1 <- vv(revenue_option1 - costs_option1, n = n_years, var_CV = 100)
  cashflow_option2 <- vv(revenue_option2 - costs_option2, n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

# Perform the Monte Carlo simulation:
predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")

plot_cashflow(mcSimulation_object = predictionProfit1, 
              cashflow_var_name = c("Cashflow_option_one", "Cashflow_option_two"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "lightskyblue3", color_5_95 = "lightskyblue1",
              color_median = "red", 
              facet_labels = c("Option 1", "Option 2"))


# Violin and box plot overlays ----
ggplot(OrchardSprays, aes(y = decrease, x = treatment, fill = treatment))+
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none")


# Ridge line plot 
ggplot(OrchardSprays, 
       aes(x=decrease,y = treatment,fill = treatment)) +
  geom_density_ridges_gradient(scale=2) + 
  theme_ridges() +
  theme(legend.position = "none")
