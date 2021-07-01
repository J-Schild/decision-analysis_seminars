# Building a decision model as an R function ####

# 1. generate a model as a function -- use the decisionSupport functions
## vv() to produce time series with variation from a pre-defined mean and coefficient of variation 
## chance_event() to simulate whether events occur 
## discount () discount values along a time seriues

example_decision_function <- function(x, varnames){
  
  # calculate ex-ante risks: impact the implementation of interventions ####
  intervention_NonPopInvolvEvent <-
    chance_event(intervention_NonPopInvolv, 1, 0, n = 1)
  
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

# 2. Input table ####

# descirbes the uncertainty in our variables -- exxample_input_table

# load the data using the read.csv function to have a look

example_data<-read.csv("example_input_table.csv")

example_data

##Notice that the data has seven columns. Looking at the file it should be clear
## from the "description" column what each variable is about and in which unti it
## is in. Ideally this is also mostly clear from the name of the variable

names(example_data)

# 3. Perform a Monte Carlo simulation ####

## using the model above, we can perfomr a mcSimulation function from the decisionSupport
## this function generates distributions of all variables in the input table + the specificed model 
## outputs (see return() function above) by claculating random drwas in our 
## defined example_decision_funcion (). 
# Make sure all the variable in the input table are included in the model 

mcSimulation_results<-decisionSupport::mcSimulation(
  estimate=decisionSupport::estimate_read_csv("example_input_table.csv"),
  model_function=example_decision_function,
  numberOfModelRuns=200,
  functionSyntax="plainNames"
)

# 4.Model assessment ####

# For the assessment of the results -- decision support, tidyverse --ggplot2, plyr,dplyr

## Plot Net Present Value (NPV) distributions ####

# plot_distriutions function -- produce one of the several plotting options for 
# distribution outputs.
# Shows us an overlay of the full result of the MC Model of the decision options
# i.e. the expected NPV if we choose to do the intervention Interv_NPV
# or not do the intervention NO_Interv_NPV

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars=c("Interv_NPV","NO_Interv_NPV"),
                                    method='smooth_simple_overlay',
                                    base_size=7)

# can also be shown in a boxplot -- can be useful when comparing multipe outputs
# by illustrating the spread of the data resulting from the decision model 
# boxplots -- show
# the median (central line)
# the 25th and 75th percentile (side of boxes)
# any outliers (light circles outside of boxes)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars=c("Interv_NPV","NO_Interv_NPV"),
                                    method='boxplot')

# look at the outcome distributon of the decison itself (difference in NPV between do and do not do)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do",
                                    method = 'boxplot_density')

## Cashflow analysis #### 

# Plot the distribution of annaul cashflow over the entire simulated period for 
# the intervention (n_years). 
# For this, use plot_cashflow() function -- uses the specified cashflow outputs
# from the mcSimulation ( ) function (in our case cashflow_decision_do) to show 
# cashflow over time 

plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)

plot_pls(pls_result,example_input_table =example_input_table, threshold = 0)
                                    
# PLS
# EVPI