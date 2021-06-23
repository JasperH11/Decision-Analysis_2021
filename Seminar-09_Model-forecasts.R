# Seminar 9: Model forecasts


# Load packages 
library(decisionSupport)
library(igraph)

# Plot graphical impact pathway of investment into hail nets ----
hail_path <- graph.formula(HailNet -+ Yield, 
                           HailNet -+ Cost, 
                           HailEvent -+ Yield,
                           Yield -+ MarketPrice, 
                           MarketPrice -+ NPV,
                           Cost -+ NPV,
                           Discount -+ NPV)  # Factor 'Discount' impacting NPV added 

plot(hail_path)


# Building model ----

# Create input table 
hail_estimates <- data.frame(variable = c("yield", 
                                          "var_CV", 
                                          "initial_investment", 
                                          "price", 
                                          "p_hail"),
                             lower = c(6000, 20, 500, 5, 0.02),
                             median = NA,
                             upper = c(14000, 20, 1000, 80, 0.2),
                             distribution = c("posnorm", 
                                              "const", 
                                              "posnorm", 
                                              "posnorm",
                                              "posnorm"),
                             label = c("Yield (kg/ha)", 
                                       "Coefficient of variation", 
                                       "Investment cost (USD)", 
                                       "Market price (EUR/kg)", 
                                       "% chance hail"),
                             Description = c("Yield under normal conditions",
                                             "Coefficient of variation (measure of relative variability)",
                                             "Investment cost", 
                                             "Market price achieved for yields (EUR/kg)", 
                                             "Probability of a hail storm"))

hail_estimates


# Create function called 'hail_function()' using inputs from above to calculate NPV for investment in hail nets 
hail_function <- function(){
  
  # Use vv() to add variability to the random draws of yield and of  price over a 20 year simulation 
  yields <- vv(var_mean = yield, 
               var_CV = var_CV, 
               n = 20)
  
  prices <- vv(var_mean = price, 
               var_CV = var_CV, 
               n = 20)
  
  # Use rep() to simulate the initial_investment only in the first year (assuming the net lasts 20 years)
  invest_costs <- c(initial_investment, rep(0, 19))
  
  # Use p_hail in the chance_event() to adjust yield for probability of hail assuming no yield at all in 
  # the event of hail
  hail_adjusted_yield <- chance_event(chance = p_hail, 
                                      value_if = 0,
                                      value_if_not = yield,
                                      n = 20)
  
  # Calculate profit without net
  profit_no_net <- hail_adjusted_yield*prices
  
  # Calculate profit with the net
  profit_with_net <- (yields*prices)-invest_costs
  
  # Use 'discount' to calculate net present value discount_rate' is expressed in percent
  NPV_no_net <- discount(profit_no_net, discount_rate = 5, calculate_NPV = TRUE)
  NPV_net <- discount(profit_with_net, discount_rate = 5, calculate_NPV = TRUE)
  
  # Calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_net-NPV_no_net
  
  return(list(NPV_no_net =  NPV_no_net,
              NPV_net =  NPV_net, 
              NPV_decision = NPV_decision))
}


# Run Monte Carlo simulation using model function ----
hail_mc_simulation <- mcSimulation(estimate = as.estimate(hail_estimates),
                                   model_function = hail_function,
                                   numberOfModelRuns = 10000,
                                   functionSyntax = "plainNames")

hail_mc_simulation

# Compare profits with and without hail nets 
plot_distributions(mcSimulation_object = hail_mc_simulation, 
                   vars = c("NPV_no_net", "NPV_net"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)


# Value of Information (VoI) analysis ----

# Subset outputs from  mcSimulation function (y) to run multi_EVPI only on variables that we want 
# (i.e. the NPV_decision)
mcSimulation_table_hail <- data.frame(hail_mc_simulation$x, 
                                      hail_mc_simulation$y[3])

evpi_hail <- multi_EVPI(mc = mcSimulation_table_hail, 
                        first_out_var = "NPV_decision")

# Plot result
plot_evpi(evpi_hail, decision_vars = "NPV_decision")

