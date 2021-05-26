# Seminar 06: Introdution to decisionSupport package 

# Load required package
library(decisionSupport)
library(DiagrammeR)

# Import example data ---- 
example_decision_inputs <- read.csv("example_decision_inputs.csv")


# Modelling functions ----
example_decision_model <- function(x, varnames)
{
  profit <- benefits-costs
  
  final_profits <- profit + additional_benefits   # add 'additional_benefits' variable
  
  return(final_profits)
  
}


# Monte Carlo simulation 
mcSimulation(estimate = as.estimate(example_decision_inputs),
             model_function = example_decision_model,
             numberOfModelRuns = 700,
             functionSyntax = "plainNames")


# mermaid function 

## Flow chart from left to right 
mermaid("graph LR 
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")

# Flow chart from top to bottom + thicker arrows + custom node colour 
mermaid("graph TB
        Y(Yield)-->I(Income); style I fill: green
        linkStyle 0 stroke:green, stroke-width:2.5px, 
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2.5px
        CL(Labor cost)-->F; style CL fill: red
        linkStyle 3 stroke: red, stroke-width:2.5px
        CM(Management cost)-->F; style CM fill: red
        linkStyle 4 stroke: red, stroke-width:2.5px")


# Building the model ----

# Adding 'management cost' variable and corresponding values to the data frame 
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", 
                                        "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", 
                                              "Management costs in a normal season"))
# Print new data frame
input_estimates

# Build model function 
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Summarise labour costs and management costs as overall costs
  overall_costs <- Labor_cost + Management_cost
  
  # Estimate the final results from the model
  final_result <- income - overall_costs
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation


# Plot results as histogram 
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",   # Specify "hist_simple_overlay" according to help 
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


# Use "make_variables" function 
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

# Applying "make_variables()" and "as_estimate()" to the "input_estimates" data frame  
make_variables(as.estimate(input_estimates))

Labor_cost + Management_cost



