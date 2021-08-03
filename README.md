# EconAnalysis
Economic analysis of the project

## Landing model (paper draft: econ_landings_paper.pdf)
In this paper we analayze how probability of precense of squid and sardine affects landings of these two species, taking into consideration closures and no-fishing decision. 
Interralation between species come from their probability of precense. 

+ Work to do...
  + What is the best way to incorporate ACL? Nonlinear function with a cap? Directly in the linear model? Censored data?
  + Try different specifications for Market Squid estimations.
  + Check prediction power of the models... Read [Making prediction from stan models in R](https://medium.com/@alex.pavlakis/making-predictions-from-stan-models-in-r-3e349dfac1ed)
  + Check if other events can simulate a closure of squid
	+ Check logbooks
  	+ Check coastwide CPS survey
	+ Number of vessel: how change after the time

## Discrete Choice Model (paper draft: econ_disc_choice_paper.pdf)
Estimation of a mixed logit for location/species decision using PacFIN vessel data and Google Fishing Watch spatial location. 

+ Work to do...
  + Read more theory
  + I will selecting the choice set based on Hicks's paper.
  + Identify capital (gear) and distances. Region of analysis (sampling) and species choice set might differ
	+ CLuster analysis could help to do this.
  + How to connect decision choice model with ammount of landings?
  + Does congestion costs have an effect of this fishery?
  + Consider to include currents in the analysis (to pull out the net)


