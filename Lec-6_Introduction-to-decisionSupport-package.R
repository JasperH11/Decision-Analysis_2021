# Load required package
library(decisionSupport)

# Import example data
example_decision_inputs <- read.csv("example_decision_inputs.csv")

# Modeling function 
example_decision_model <- function(x, varnames)
{
  profit <- benefits-costs
  
  final_profits <- profit + additional_benefits
  
  return(final_profits)
  
}



mcSimulation(estimate = as.estimate(example_decision_inputs),
             model_function = example_decision_model,
             numberOfModelRuns = 700,
             functionSyntax = "plainNames")
