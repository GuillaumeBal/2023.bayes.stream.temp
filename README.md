# Bayesian modelling of stream water temperature

This repo contains all the script relate to the following open access article :
Bal Guillaume & De Eyto Elvira, 2002, Simple bayesian reconstruction and forecasting of stream water temperature for ecologists
*to be completed*

In a nutshell, it allows one to complete, hindcast and forecast stream water temperatures based on air temperature and optionally flow in a temperate context. The model is based on a time series decomposition approach giving a fair appraisal of uncertainties. The overall fit requires only a very limited working knowledge of R.

## Software requirements

JAGS, R and possibly Rstudio (user friendly GUI) can be dowloaded browsing the following links:
https://mcmc-jags.sourceforge.io/
https://www.r-project.org/
https://www.rstudio.com/

## Data formatting
 
Please place your data in the "0.data/" folder in lieu of the 'scorff.data.txt" file. New data must be formatted as the file provided as an example. Do not put any other file in the folder.
By the way flow data are optional.

## Running models

You can specify your model run options within the "0.define.your.run.r" R script by changing a few indices acoording to the explanation given as comments. 

Once done with this, please run the "0.define.your.run.r" file.

Full runs may take from a few hours up to about 2 days depending on both the length of your times series (your hardware configuration may slow it down). 

As you will seen an additional first model is run. It only aims a shifting your time so that a year correspond to a full water temperature cycle, starting at the time your water temperature is usually the lowest temperature. Generally, this correspond to about mid January within the Northern Hemisphere. Doing so allows for 6 month values obtained through the fit to be more directly comparable. All this is done internally.

Running the script leads to the processing of the MCMC chain obtained, more details are given below in the section below

If needeed, you can modify the MCMC chains length and thinning by editing the 'mcmc.chains.config'. This could for instance allow for even quicker checks of the model bein able to run. You could also specify longer runs to increase estimates stability. The longer values given are normally enough to reach this last goals, changes resulting in ecologically negligibles deviations.  

## Models outputs

They will be stored within a folder called run outputs. this folder will have a subloder for the first model shifting the times series and one for the water temperature modelling based on air temperature and flow if given. In particular, the following files will be available:
- "0.param.mcmc.diag.pdf", this file, available within each folder, allows to visually assess the convergence of the MCMC chains for the main parameters.
- "0.param.mcmc.summary.txt", this file, available within each folder, contains summary statistics for model parameters.
- "visual.checks.pdf" for the water temperature model, within the "run.outputs/2.model.wt.hier/" folder. It shows a few figures summarising tests described in the article while providing brief interpretation guidelines in the title. It also allows to check some model outputs. Please note that some figures can be seen within Rstudio as well. In case you have not paid attention to them, you can reprocess the MCMC chains previously saved while running the model. There is an option to modify within the "0.define.your.run.r" file for that. 
- "1.dic.table.txt", within the "run.outputs/2.model.wt.hier/" folder. It contains some fit statistics in case you want to perform some model comparisons.
- "wt.complete.txt", within your main folder, containing you initial water temperature times series completed by estimates derived from the model fitted.

