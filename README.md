# Inferring epidemiological parameters under infectious phylogeography model with visitor dynamics 

Here, we introduce a new and novel phylogeographic model with visitor dynamics to infer epidemiological parameters, host movement-related parameters, and disease transmission history from a serially sampled phylogenetic tree within an Suscetible-Infectious-Recovered (SIR) framework. We call our model, **Visitor SIR**.

As a simulation model, we can simulate transmission trees under the full model and the approximation model (see our manuscript), implemented in [MASTER](https://tgvaughan.github.io/MASTER/). As an inference model, due to the complexity of the full model, it is not computationally tractable to do inference under the full model. Therefore, we use an approximation to the full model, which has been shown to be accurate and still retains all the main properties of the full model (see our manuscript). We implement the inference machinery in a Bayesian framework, implemented in [RevBayes](https://revbayes.github.io/). 

In this repository, you can find all the scripts used to do simulations, inferences, and result visualizations under our model and two other models. These two models are what we call as **Migration** models (see this [paper](https://academic.oup.com/mbe/article/33/8/2102/2578541)). 

## Approximation vs. full model 

In [approx_validation](https://github.com/mlandis/visitor_sse/tree/main/approx_validation) directory, you will find the simulation scripts and a plotting script used to compare results under the full Visitor SIR model and its approximation. 

## Simulation 

In [scripts](https://github.com/mlandis/visitor_sse/tree/main/scripts) directory, you will find scripts to simulate under the approximation model for generating dataset used for the coverage experiment. Also, in [plot](https://github.com/mlandis/visitor_sse/tree/main/scripts/plot) sub-directory, you will find R scripts used to visualize the results used in the manuscript (see [final_plot](https://github.com/mlandis/visitor_sse/tree/main/scripts/plot/final_plot)). 

## RevBayes analysis 

In [code](https://github.com/mlandis/visitor_sse/tree/main/code) directory, you will find the RevBayes scripts used to run inference analysis under the approximation model and both migration models using simulated and empirical data. 

## Tree data

In [data](https://github.com/mlandis/visitor_sse/tree/main/data) directory, you will find the simulated dataset under different setups for coverage experiments on the manuscript as well as [empirical](https://github.com/mlandis/visitor_sse/tree/main/data/emp) data, which contain empirical transmission history from this [publication](https://www.pnas.org/doi/abs/10.1073/pnas.2012008118). 

## Travel data

Also in [data](https://github.com/mlandis/visitor_sse/tree/main/data) directory, you will find [depart_rates.csv](https://github.com/mlandis/visitor_sse/blob/main/data/depart_rates.csv) file, which contains empirical estimate of per-capita depart rate between these locations used in our study: Hubei, France, Germany, Italy, and other European countries. The [return_rates.csv](https://github.com/mlandis/visitor_sse/blob/main/data/return_rates.csv) contains empirical estimate of per-capita return rate from an away location to a home country from the country list. The raw data are obtained from [eurostat](https://ec.europa.eu/eurostat/web/tourism/database). 
