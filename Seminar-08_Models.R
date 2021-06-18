# Seminar 8: Models  


# Load packages
library(decisionSupport)
library(readr)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)

# Generate model as a function ----
example_decision_function <- function(x, varnames){
  
  # calculate ex-ante risks: impact the implementation of interventions ####
  intervention_NonPopInvolvEvent <- chance_event(intervention_NonPopInvolv, 1, 0, n = 1)
  
  # pre-calculate common random draws for all intervention model runs ####
  
  # profits from Tropical Livestock Units (TLU)
  TLU <- vv(TLU_no_intervention, var_CV, n_years)
  TLU_profit <- vv(profit_per_TLU, var_CV, n_years)
  
  # benefits of fruit
  precalc_intervention_fruit_benefits <-
    vv(intervention_fruit_area_ha, var_CV, n_years) *
    vv(intervention_fruit_yield_t_ha, var_CV, n_years) *
    vv(intervention_fruit_profit_USD_t, var_CV, n_years)
  
  # benefits of vegetables
  precalc_intervention_vegetable_benefits <-
    vv(intervention_vegetable_area_ha, var_CV, n_years) *
    vv(intervention_vegetable_yield_t_ha, var_CV, n_years) *
    vv(intervention_vegetable_profit_USD_t, var_CV, n_years)
  
  # benefits of rain-fed crops
  precalc_intervention_rainfed_crop_benefits <-
    vv(intervention_rainfed_crop_area_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_yield_t_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_profit_USD_t, var_CV, n_years)
  
  #  Intervention ####
  
  for (decision_intervention_strips in c(FALSE,TRUE))
  {
    
    if (decision_intervention_strips)
    {
      intervention_strips <- TRUE
      intervention_strips_PlanningCost <- TRUE
      intervention_strips_cost <- TRUE
    } else
    {
      intervention_strips <- FALSE
      intervention_strips_PlanningCost <- FALSE
      intervention_strips_cost <- FALSE
    }
    
    if (intervention_NonPopInvolvEvent) {
      intervention_strips <- FALSE
      intervention_strips_cost <- FALSE
    }
    
    # Costs ####
    if (intervention_strips_cost) {
      cost_intervention_strips <-
        intervention_adaptation_cost + 
        intervention_tech_devices_cost + 
        intervention_nursery_cost +
        intervention_wells_cost +
        intervention_training_cost + 
        intervention_mngmt_oprt_cost + 
        intervention_mngmt_follow_cost +
        intervention_mngmt_audit_cost
    } else
      cost_intervention_strips <- 0
    
    if (intervention_strips_PlanningCost) {
      plan_cost_intervention_strips <-
        intervention_communication_cost + intervention_zoning_cost
    } else
      plan_cost_intervention_strips <- 0
    
    maintenance_cost <- rep(0, n_years)
    
    if (intervention_strips)
      maintenance_cost <-
      maintenance_cost + vv(maintenance_intervention_strips, 
                            var_CV, n_years)
    
    intervention_cost <- maintenance_cost
    intervention_cost[1] <-
      intervention_cost[1] + 
      cost_intervention_strips + 
      plan_cost_intervention_strips
    
    
    # Benefits from  cultivation in the intervention strips ####
    
    intervention_fruit_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_fruit_benefits
    intervention_vegetable_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_vegetable_benefits
    intervention_rainfed_crop_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_rainfed_crop_benefits
    
    # Total benefits from crop production (agricultural development and riparian zone) ####
    crop_production <-
      intervention_fruit_benefits +
      intervention_vegetable_benefits +
      intervention_rainfed_crop_benefits
    
    # Benefits from livestock ####
    # The following allows considering that intervention strips may
    # restrict access to the reservoir for livestock.
    
    if (intervention_strips)
      TLU_intervention <-
      TLU * (1 + change_TLU_intervention_perc / 100)
    else
      TLU_intervention <- TLU
    
    if (decision_intervention_strips){
      livestock_benefits <- TLU_intervention * TLU_profit
      total_benefits <- crop_production + livestock_benefits
      net_benefits <- total_benefits - intervention_cost
      result_interv <- net_benefits}
    
    
    if (!decision_intervention_strips){
      livestock_benefits <- TLU_no_intervention * TLU_profit
      total_benefits <- livestock_benefits
      net_benefits <- total_benefits - intervention_cost
      result_n_interv <- net_benefits}
    
  } #close intervention loop bracket
  
  NPV_interv <-
    discount(result_interv, discount_rate, calculate_NPV = TRUE)
  
  NPV_n_interv <-
    discount(result_n_interv, discount_rate, calculate_NPV = TRUE)
  
  # Beware, if you do not name your outputs 
  # (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  
  return(list(Interv_NPV = NPV_interv,
              NO_Interv_NPV = NPV_n_interv,
              NPV_decision_do = NPV_interv - NPV_n_interv,
              Cashflow_decision_do = result_interv - result_n_interv))
}



# Importing the input table ----
example_data <- read.csv("Seminar-08_example-input-table.csv")

# Check imported data
str(example_data)
head(example_data)


# Perform Monte Carlo simulation ---- 
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("Seminar-08_example-input-table.csv"),
  model_function = example_decision_function,
  numberOfModelRuns = 10000,         # Generally, 10,000 runs is a reasonable choice 
  functionSyntax = "plainNames"
)


# Plotting Net Present Value (NPV) distributions ----

# As overlay distribution
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV", "NO_Interv_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

# As boxplots 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV", "NO_Interv_NPV"),
                                    method = 'boxplot')

# As boxplot + density 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV",
                                             "NO_Interv_NPV"),
                                    method = 'boxplot_density')


# Cashflow analysis ---- 
plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")


# Projection to Latent Structures (PLS) analysis ---- 
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)


# Value of Information (VoI) analysis ----

# VoI is calculated using Expected Value of Perfect Information (EVPI; opportunity loss in case of 
# imperfect information about a decisive variable)

# data.frame() function converts x and y outputs of mcSimulation() for EVPI calculation 
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3]) # Select correct variables

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision_do")  # Processing will take a while!

# Plot results from multi_EPVI() function 
plot_evpi(evpi, decision_vars = "NPV_decision_do")

# Use compound_figure() to provide single figure for quick assessment 
compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = example_data, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", 
                base_size = 7)

