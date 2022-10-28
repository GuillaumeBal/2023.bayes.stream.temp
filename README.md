# Bayesian modelling of stream water temperature

## Requirements

JAGS, R and possibly Rstudio (user friendly GUI) can be dowloaded browsing the following links:
https://mcmc-jags.sourceforge.io/
https://www.r-project.org/
https://www.rstudio.com/
 
Please place your data in the "0.data/" folder in lieu of the 'scorff.data.txt" file. New data must be formatted as the file provided as an example. Do not put any other file in the folder.
By the way flow is optional.

## Running models

You can specify your model run details within the "0.define.your.run.r" R script. If needeed, you can modify the MCMC chains length and thinning by editing the 'mcmc.chains.config'.

Once done with this, please run the "0.define.your.run.r" file.

Full runs may take from a few hours up to about 2 days depending on both the length of your times series (your hardware configuration may slow it down). The first model run only aims a shifting your time so that a year correspond to a full waater temperature cycle. Doing so allows for 6 month values obtained to be more directly comparable. All this is done internally.
 

## Models outputs

They will be stored within a folder called run outputs. In particular, the following files will be available:
- 

