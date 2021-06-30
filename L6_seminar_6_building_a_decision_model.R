# Lecture: creating a model ####

## mc simulation required three inputs 
### 1. estimate 
# default distributions in example_decision_inputs
# file in 'data' folder

# note that the input table can be 
# written of read from a .csv file 
# calculated with the estimate_read_csv function
# converted to the correct format with the as.estimate function

### 2. model_function
# predicts outcomes + model output (Net Present Value)

library (decisionSupport)

example_decision_inputs<-read.csv("example_decision_inputs.csv")

# here we define a simple model function that we call example_decision_model.
# It calculates profits as benefits minus costs and arbitraril adds 500 to the
# result to arive at fimal_profits. 
# This simple example shows us how to use function.

example_decision_model <-function(x, varnames)
{
  profit <- benefits-costs
  
  final_profits <- profit + additional_benefits
  
  return(final_profits)
  
}

# The most basic way to apply the library is to use the mcSimulation finctions 
# to run our example_decision_model. Here we run it 100 times.

mcSimulation(estimate = as.estimate(example_decision_inputs),
             model_function = example_decision_model,
             numberOfModelRuns = 100,
             functionSyntax = "plainNames")

# now we have simulation of possible outcomes.

# 3.numberOfModelRuns

# in the Monte Carlo Simulation all delays are multiplied by the numberOfModelRuns 
# (e.g. 10,000). This can sometimes add up to substantial time losses. 
# Monte Carlo Simulations can take a while when dealing with complex decisions

# the objective of the procedures used in the decision support package is to make
# it easier for analysts to produce decision-relevant information that adequately
# reflects the imperfect nature of the information we usually have. Adding probabilistic elements to 
# a simulation adds substantial value to an analysis. 

#Seminar ####
# Plotting the impact pathway ####

# using mermaid function from DimagrammeR to create graphical impact pathway

library(DiagrammeR)

mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")

# add an additional cost to the graph called Management cost

mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Management cost)-->F; linkStyle 4 stroke: green, stroke-width:1.5px")

# Building a model ####

# Here we generate the input table to feed the model function. 

input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost"),
                              lower = c(6000, 3, 500),
                              median = NA,
                              upper = c(14000, 8, 1000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season"))
input_estimates

# Update this with your ned managememtn cost variable. Call your new variable 
# Managemement_cost, lower bound 100, upper bound 2000, distribution posnorm, 
# label Management cost (USD/ha) and description Management costs in a normal season

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

input_estimates

# Now, use the mcSimulation function from the decisionSupport package to implement a model
# --the model function that describes the graphical impact pathway.

model_function<-function(){
  
    #Estiamte the income in a normal season
    income<-Yield*Market_price
    
    #Estiamte the final results from the model
    final_result<- income-Labor_cost
    
    #Generate the list of outputs from the Monte Carlo simulation
    return(list(final_result=final_result))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation<-mcSimulation(estimate=as.estimate(input_estimates),
                                    model_function=model_function,
                                    numberOfModelRuns=800,
                                    functionSyntax="plainNames")

example_mc_simulation

#Add a new line of code that summarizes the Labor_cost and Management_cost into 
# overall_costs, then subtract these from the income to calculate final_result.

model_function<-function(){
  
  #Summarize Labor and Mangement cost into overall costs
  overall_costs<-Labor_cost+Management_cost
  
  #Estiamte the income in a normal season
  income<-Yield*Market_price

  #Estiamte the final results from the model
  final_result<-income- overall_costs
  
  #Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result=final_result))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation<-mcSimulation(estimate=as.estimate(input_estimates),
                                    model_function=model_function,
                                    numberOfModelRuns=800,
                                    functionSyntax="plainNames")

example_mc_simulation

#Now, we show the results of a Monte Carlo simulation (800model runs) for 
# estimating the profits in sweet cherry orchards.

plot_distributions(mcSimulation_object=example_mc_simulation,
                   vars = "final_result",
                   method="boxplot_density",
                   old_names="final_result",
                   new_names="outcome distribution for profits")

# Change the plot to a histogram by using the method argument in the 
# plot_distributions function

plot_distributions(mcSimulation_object=example_mc_simulation,
                   vars = "final_result",
                   method="hist_simple_overlay",
                   old_names="final_result",
                   new_names="outcome distribution for profits")

# Testing with make_variables ####

# defining sample values for all variable, if you want to test pieces of the
# function code during the development process. Manually or ..
# this function is not included in the decisionSupport package.

make_variables<-function(est,n=1)
{ x<-random(rho=est,n=n)
      for (i in colnames(x)) assign (i,as.numeric(x[1,i]),envir=.GlobalEnv)
}

# Applying make_variable and as.estimate to the data table (with default setting n=1) generates one random number for each variable
# which then allows you to easily test the code you are developing. Try running this finction on your code as you build the decision function.
# This allowd for testing the values within a model rather than running the full model

# Run the make_variable and as.estimate on the input_estimates input table that we created and then calculate the result of Labor_cost+ Management_cost given a single random value for these variables.
# Note that each time you run this code it generates a new random draw and produces
# a different number from within the range for the variables in the input table.

make_variables(as.estimate(input_estimates))

Labor_cost+Management_cost

# paper Applying the mcSimulation finction in decisionSupport ####

# will show how to use tools to compare decision outcomes

## taken all together, all these outputs allow an evaluation of the plausible 
## range of net benefits that can be expected to arise from the decision. They 
## provide a recommendation on which decision option should be preferred, and an
## appraisal of which input variable are responsible for most of the variation in
## the output distribution
