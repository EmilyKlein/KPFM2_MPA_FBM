# KPFM2_MPA_FBM ~ READ ME and ANALYSIS GUIDE: Feedback management and MPAs with KPFM2
Run KPFM2 to assess outcomes with marine protected areas (MPAs) or feedback management (FBM)

NB! Please refer to the READ ME for basic use of the Krill-Predator-Fishery Model (EmilyKlein/KPFM2) before using this guide!

This file contains information on the (1) data input files, (2) code for analysis, and (3) steps in analysis used to obtain various results presented in several papers (see below), wherein KPFM2 was used to assess feedback management and marine protected areas. The analysis uses functions coded in [R] programming language, which is available for free download at https://www.r-project.org/ (this research used RStudio, https://www.rstudio.com/, to access the R software). For further background and reasoning regarding the model and its use, the reader is referred to:

Watters GM et al. (2013) Decision-making for ecosystem-based management: evaluating options for a krill fishery with an ecosystem dynamics model. Ecol Appl 23 (4):710-725. doi:10.1890/12-137. 

Additional README files are available at EmilyKlein/KPFM2_Climate_change to run the analyses and provide the figures found in Klein et al. (2018): Klein et al. (2018) “Climate-change impacts on prey increase risks for predators in the Scotia Sea”.


(1) DATA INPUT FILES

[mlt, mst, nlt, nst]: The four main .txt input for the KPFM2 ecosystem model, one for each parameterization1 across krill movement (no movement, n, and full movement as passive drifters, m) and predator sensitivity to krill availability (hyperstable, s, and linear, l). The parameterizations are run individually in KPFM2, and they have been updated with the most recent foraging information. 

[mlt_D1, mst_D1, nlt_D1, nst_D1] and [mlt_US10, mst_US10, nlt_US10, nst_US10]: The four main .txt input for the Domain 1 MPA and the US10 MPA scenarios, respectively. These files are identical to the ones above, except for slight changes in the foraging matrices of predators resulting from assessments of foraging inside and outside of the MPA boundaries (the updated foraging information is available in the Klein and Watters MPA Supporting Information). 

areas.D1, areas.D1.nompa, areas.US10, areas.US10.nompa: Matrix of values related to the number of pixels per original area (small scale management units, SSMUs) and if there is fishing there or not. Needed to pixelate original input into MPA pixels (see main text and Supporting Information for more on pixelation process). Note that the “.nompa” files are pixelated but allow fishing across all pixels (i.e. there is no MPA).  

pixel.D1, pixel.D1.nompa, pixel.US10, pixel.US10.nompa: Matrix of values to better define the pixels per original area. Needed to pixelate original input into MPA pixels. Note that the “.nompa” files are pixelated but allow fishing across all pixels (i.e. there is no MPA).

foraging.D1, foraging.US10: A list of values, that define how foraging is pixelated into the MPA from original spatial units (SSMUs). Needed to pixelate original input into MPA pixels.

[GPRCP85CHL100]: Input files for driving krill gross growth potential (GGP) under a climate change scenario, in this case a IPCC Representative Control Pathway (RCPs) projection 8.5 (RCP 8.5). 
-	NB: The GGP input is for 93 years, therefore it needs to be either subset to a smaller number of years or the model needs to be run for the same number of years (93). 

(2) CODE FOR ANALYSIS

The analysis utilizes several functions, some of which call the ecosystem model (the Krill-Predator-Fishery Model, KPFM2) as described in Watters et al. (2013). Detail on KPFM2 is available in the documentation for that manuscript.
  
2.1 The base KPFM2 code
load.funcs.r: Function to load and source all functions in a folder (use to easily get all necessary functions into the R workspace) 
import.all.parameters.r: Function to load the KPFM2 input files (e.g., mlt, mst, nlt, nst). 
ssmu.ss.r: The KPFM2 model, run in stochastic mode. Default plots are by SSMU and predator group for abundance and recruitment. This calls the additional functions: 
allocate.catch.r: Generalized function to allocate catch among areas 
bt.abund,var.r: Generates the variable bathtub krill abundances 
m2f.root.r: Estimates root of catch equation 	
plot.ss.C.r: Plots changes in catch when fishing is implemented.
plot.ss.N.r: Plots changes in abundance of krill and predators)
plot.ss.R.r: Plots changes in recruitment of krill and predators)
ssmu.mc.r: Function to run Monte Carlo simulations of KPFM2. Default plots are by SSMU and predator group for abundance and recruitment, and this calls the following additional functions:
plot.mc.C.r: Plots changes in catch when fishing is implemented.
plot.mc.N.r: Plots changes in abundance of krill and predators)
plot.mc.R.r: Plots changes in recruitment of krill and predators)
compare.output.r: Aggregates species output across Monte Carlo simulations and for plotting. Runs the loop.pixel script for MPA output (see below). 
compare.catch.r: Aggregates catch output across Monte Carlo simulations and for plotting. Runs the loop.pixel script for MPA output (see below). 



2.2 Code for Monte Carlo trials and running across scenarios
main.r: Function to run either or both Monte Carlo simulations across multiple scenarios and risk assessment. If designated by the user, this can call the following functions: 
run.mcsims.r (if runmcsims=TRUE)
	assess.risk.predators.r (if runassessment=TRUE)
	assess.krill.fishery.r (if runassessment=TRUE)
	assess.regional.krill.r (if runassessment=TRUE)

run.mcsims.r: Runs Monte Carlo simulations over multiple fishing scenarios and parameterizations. Calls the following function: 
get.gamma.fractions.r
	ssmu.ss.r 
ssmu.ss.mc.r 
assess.risk.predators.r: Risk assessment for the predator groups across multiple fishing options and parameterizations; averages output across parameterizations. Calls the following functions:
calc.risk.predators.r: Calculates depletion risks for predator groups. This code calls the script 
get.gamma.fractions.r 
merge.mc.r: Aggregates output across Monte Carlo trials into a single output
relative.mc.r: Makes one Monte Carlo trial output object relative to others. 

assess.krill.fishery.r: Performance assessment of the fishery in terms of krill catch over multiple fishing scenarios and parameterizations; averages output across parameterizations. Calls the functions: 
calc.pms.krill.fishery.r: Calculates performance measures for krill and the fishery.
get.gamma.fractions.r 
merge.mc.r

assess.regional.krill.r: : Computes regional krill performance measures; averages output across parameterizations. Calls the following functions:
calc.pms.regional.krill.r 
calc.regional.krill.mc.r
get.gamma.fractions.r
merge.regional.krill.mc.r
relative.regional.krill.mc.r


2.3. Code for decomposing original KPFM2 for marine protected area scenarios
main.pixel(): Function to run simulations of the KPFM2 ecosystem model (described in Watters et al. 2013) across Monte Carlo trials and yield multipliers for risk assessment. This calls the runmcsims(), which runs the KPFM2 simulations across the number of Monte Carlo trials and yield multipliers specified by the analyst. 

The risk assessment process here requires the analyst to update several things in the main.pixel() code so that all scenarios are run, including those needed for the pixelation adjustment (manuscript Supporting Information). The primary input for the code is  

main.pixel<-function(
input.objects=c(“mst”, “mlt”, “nst”, “nlt”) The input data files for parameterizations described above
fishing.options=c(0,1)  One simulation without fishing (0) and one with fishing according to current distributions (1)
runassessment=TRUE Whether or not to run assessments on the output data
runmcsims=TRUE Whether or not to run Monte Carlo trials
areas=[input data] One of the areas.[] input files described above, depending on MPA scenario 
pixels=[input data] One of the pixel.[] input files described above, depending on MPA scenario 
foraging.list=[input data] One of the foraging.[] input files described above, depending on MPA scenario 
parameter.path=””  Parameter path for location of input.objects above
output.file.path=”” 	 Parameter path for output objects

This script calls the following additional functions: 
run.mcsims.pixel(): If called, the code for running Monte Carlo trials of KPFM2. 
assess.risk.predators.pix(): Assessment of risk for predator groups
assess.krill.fishery.pix(): Assessment of fishery and for krill populations

The run.mcsims.pixel() script calls the following additional functions: 
get.gamma.fractions() 
pixel.foosa(): Pixelates the original input data into MPA (closed to fishing) and non-MPA (closed to fishing) pixels. More on pixelation in the main text and Supporting Information. 
ssmu.ss() OR ssmu.ss.pixel(): Runs the KPFM2 ecosystem model
ssmu.ss.mc() OR ssmu.mc.pixel(): Runs Monte Carlo simulations of KPFM2 

The KPFM2 model (ssmu.ss) calls the additional functions: 
m2f.root.r(): Estimates root of catch equation 	
bt.abund,var(): Generates the variable bathtub krill abundances 
allocate.catch(): Generalized function to allocate catch among areas 

compare.output() script pulls the krill and predator output from KPFM2, aggregates across Monte Carlo simulations and parameterization, and saves overall statistics. 

compare.catch() script pulls the catch or allocation output from KPFM2, aggregates across Monte Carlo simulations and parameterization, and saves overall statistics. This script can call the following additional function if get.tv!=NULL: 
thresholds.out(): Pulls the threshold violations for the set year or end of model run.

loop.pixel: Aggregates output decomposed into an MPA back to SSMU level. This is important for running the risk analysis code and comparing with other ouput that has not been decomposed and is still by SSSMU.

2.4. Convention for naming output
For output from the initial Monte Carlo trials:[parameterization].[fishing option].g[yield multiplier].mc. For example: 
 mst.0.0.mc: Parameterization = movement + stable, fishing option = 0, yield multiplier = 0.
 nlt.1.g0.1.mc: Parameterization = no movement + linear, fishing option = 1, yield multiplier = 0.1.
 mlt.1.g0.1.mc: Parameterization = movement + linear, fishing option = 1, yield multiplier = 1.

For output from that denotes a base case scenario with the input decomposed (“pixelated”) into the MPA areas but with all open to fishing:[parameterization].[fishing option].3g[yield multiplier].mc. For example: 
nlt.1.3g1.mc: Parameterization = no movement + linear, fishing option = 1, base case no MPA, yield multiplier = 1.


(3) STEPS FOR ANALYSIS

3.1	Basic set-up of model imput: 
1.	Make sure all code for analysis denoted above is sourced, and have GPRCP85CHL100 available in the Global Environment, and folders are available to call data from and for saving output. 
2.	Develop input data file and save in “data” folder: 
a.	Data is stored and uploaded into the R code for KPFM2 via a text file (“.txt”)
i.	This can be easily updated in Excel. A guide to this input is available here. 
b.	KPFM2 uses four parameterizations – these are separate files usually saved in a “data” folder: mlt, mst, nlt, nst for movement-linear, movement-stable, no movement-linear, no movement-stable.
3.	Import each of the parameterizations using “import.all.parameters()” – these files must be named “mlt”, “mst”, “nlt”, and “nst” as appropriate or later aspects of the code cannot find them: 
a.	Make the file string reflects location of the data (.txt file), e.g.: 
> mlt <- import.all.parameters(file.string ="C:/Users/Desktop/KPFM2/data/mlt.txt")
b.	Response reports the SSMUs, bathtuns, number of years and seasons – check that these are correct, e.g.: 
[1] "The setup says you have 15 SSMU(s), 3 BATHTUB(s), 38 YEAR(s) of time series data, and 2 SEASON(s) specified"

4.	Save each input parameterization as an .rdata file in the “data” folder via save():
save(mst,file="C:[your file path]/data/mst.rdata")

3.2 Using KPFM2 to assess feedback management (FBM): 
You will need to run two scenarios for comparison as in the Klein and Watters paper, one with and one without FBM. 
1.	Initial setup for all scenarios: 
a.	Open the script “main()” ensure following are called: 

input.objects=c(“mst”, “mlt”, “nst”, “nlt”) 
fishing.options=c(0,3,7)  
runassessment=FALSE 
runmcsims=TRUE 
ggp.in = GGPRCP85CHL100 
parameter.path=  [Location of input.objects from “DATA INPUT” section]
output.file.path=  [Location for output to be saved]
b.	Also check the following are set for calling run.mcsims(), starting line 28: 
ntrials <- 1001 	[number of MC trials to run] 
nyears <- 93    	[number of years for the model to run; NB this must be 93 to use GPRCP26CHL100, or that file must be subset]
start.fishing.yr <- 1  	  [when to start fishing]
stop.fishing.yr <- NULL	  [when to stop fishing]
  
c.	Klein and Watters only ran the model for gamma = 1.0; open the get.gamma.fractions.r script and make sure it is set for this only, i.e.: 
# gamma.fractions <- c(0.02, 0.04, 0.06, 0.08, 0.1, 0.11…
gamma.fractions <- 1.0

2.	Run the “No FBM” scenario: 
a.	in main(), make sure the code is set to turn off reassessment, line 34: 
reassessment.interval.yr <- NULL
b.	Save, source, and run the model via main(runassessments=FALSE)
i.	Output will be saved in the location designated by “output.file.path”.
c.	Rename the output folder to denote it as output from the No FBM base case. 
3.	Run the “FBM” scenario
a.	Create a new folder for output of the FBM scenario model run and update output.file.path in the main() script.
b.	Turn on FBM by including an assessment interval at line 34, i.e. 
reassessment.interval.yr <- 5 [for assessments every 5ys]
c.	Save, source, and run the model via main(runassessments=FALSE)
d.	Rename the output folder to denote it as output from the No FBM base case. 


3.2	Using KPFM2 to assess marine protected areas (MPAs):
You will need to run many scenarios for comparison as in the Klein and Watters paper, a reference or base case scenario that is decomposed to the MPA areas but has no areas closed to fishing, and scenarios where the areas inside the MPA are closed to fishing. Of these scenarios, there is the D1MPA and the US10 scenarios, each across three redistribution of displaced fishing alternatives (see paper for details on this). 
Due to the current status of the input data and code, running marine protected areas requires the user to update the code depending on which MPA scenario they are running. PLEASE READ THE FOLLOWING CAREFULLY. 

Running the D1MPA scenario (Klein and Watters MPA paper)	
1.	Make sure to have the D1 set of parameterization .rdata files (mlt_D1, mst_D1, nlt_D1, nst_D1) available in your source input folder – but rename them as “mlt”, “mst”, “nlt” and “nst” once they are there, as the code must have input so named at this time. 
2.	Open R Studio and ensure all scripts noted above are loaded and sourced. Open the following D1MPA files: areas.D1, areas.D1.nompa, pixel.D1, pixel.D1.nompa, foraging.D1
3.	Open the script pixel.foosa() and update the highlighted sections of lines 140-158 (as per directions there): 
a.	Update line 142 to read “for(i in 1:16){“ 
b.	Comment in lines 146-148 and 151
c.	Comment out line 153
d.	Update line 155 to read “for(c in 1:16){“
e.	Update line 160 to read “for(cc in 17:24){“
f.	Save and source pixel.foosa()
4.	Run the reference .nompa scenario: 
a.	Make sure main.pixel() has the following at lines 1-11 reflect the following:
input.objects=c(“mst”, “mlt”, “nst”, “nlt”) 
fishing.options=c(0,1)  
runassessment=FALSE 
runmcsims=TRUE 
areas=areas.D1.nompa
pixels=pixels.D1.nompa
foraging.list=foraging.D1
catch.redist = 2
scenario = "base”
parameter.path=  [Location of input.objects from “DATA INPUT” section]
output.file.path=  [Location for output to be saved]
b.	Also ensure the following, staring line 41:
ntrials <- 1001 	[number of MC trials to run] 
nyears <- 93    	[number of years for the model to run; NB this must be 93 to use GPRCP26CHL100, or that file must be subset]
start.fishing.yr <- 1  	  [when to start fishing]
stop.fishing.yr <- NULL	  [when to stop fishing]
c.	
d.	Save and source main.pixel()
e.	Run main.pixel() 
5.	Once model runs finish, rename the folder used to house ouput (convention “risk_[MPA scenario]_nompa, e.g. “risk_D1_nompa”). Create a new output folder. 
6.	Now run the MPA scenarios across alternative distributions of displaced fishing: 
a.	Update  main.pixel() so lines 1-11 reflect the following (changes highlighted):
input.objects=c(“mst”, “mlt”, “nst”, “nlt”) 
fishing.options=c(0,1)  
runmcsims=TRUE 
runassessment=FALSE 
ggp.in=GGPRCP85CHL100,
areas=areas.D1
pixels=pixels.D1
foraging.list=foraging.D1
catch.redist = 2
scenario = "MPA”
parameter.path=  [Location of input.objects]
output.file.path=  [Location of output to be saved]
b.	Also ensure the following, staring line 41:
ntrials <- 1001 	[number of MC trials to run] 
nyears <- 30    	[number of years for the model to run]
start.fishing.yr <- 1  	  [when to start fishing]
stop.fishing.yr <- NULL	  [when to stop fishing]
c.	Save, source, and then run main.pixel()
d.	Once model runs finish, rename the output folder and make a new one. 
e.	Repeat from (a) but only update main.pixel() for each redistribution alternative, running it for catch.redist = 3 and catch.redist = 4. Save, source and run main.pixel() and update output folder each time. 
Running the US10 scenario (Klein and Watters MPA paper) 
NB that these are the same steps as above, only for the US10 data and output. They are reiterated here for clarity. 

1.	Open the script pixel.foosa() and update the highlighted sections of lines 140-158 (as per directions there): 
a.	Update line 142 to read “for(i in 1:15){“ 
b.	Comment out lines 146-149 and 151
c.	Comment in line 153
d.	Update line 155 to read “for(c in 1:15){“
e.	Update line 160 to read “for(cc in 16:20){“
f.	Source and save pixel.foosa()
2.	Run the reference .nompa scenario: 
a.	Make sure main.pixel() has the following at lines 1-11 reflect the following:
input.objects=c(“mst”, “mlt”, “nst”, “nlt”) 
fishing.options=c(0,1)  
runassessment=FALSE 
runmcsims=TRUE 
ggp.in=GGPRCP85CHL100,
areas=areas.US10.nompa
pixels=pixels.US10.nompa
foraging.list=foraging.US10
catch.redist = 2
scenario = "base”
parameter.path=  [Location of input.objects from “DATA INPUT” section]
output.file.path=  [Location for output to be saved]
f.	Also ensure the following, staring line 41:
ntrials <- 1001 	[number of MC trials to run] 
nyears <- 30    	[number of years for the model to run]
start.fishing.yr <- 1  	  [when to start fishing]
stop.fishing.yr <- NULL	  [when to stop fishing]
b.	Save and source main.pixel()
c.	Run main.pixel() 
3.	Once model runs finish, rename the folder used to house ouput (convention “risk_[MPA scenario]_nompa, e.g. “risk_US10_nompa”). Create a new output folder. 
4.	Now run the MPA scenarios across alternative distributions of displaced fishing: 
a.	Update  main.pixel() so lines 1-11 reflect the following (changes highlighted):
input.objects=c(“mst”, “mlt”, “nst”, “nlt”) 
fishing.options=c(0,1)  
runmcsims=TRUE 
runassessment=FALSE 
ggp.in=GGPRCP85CHL100,
areas=areas.US10
pixels=pixels.US10
foraging.list=foraging.US10
catch.redist = 2
scenario = "MPA”
parameter.path=  [Location of input.objects]
output.file.path=  [Location of output to be saved]
b.	Save, source, and then run main.pixel()
c.	Once model runs finish, rename the output folder and make a new one. 
d.	Repeat from (a) but only update main.pixel() for each redistribution alternative, running it for catch.redist = 3 and catch.redist = 4. Save, source and run main.pixel() and update output folder each time. 
Running MPA analysis with GGP (Klein and Watters FBM + MPA paper)
1.	To run an MPA with climate change impacts on krill (i.e. with GGP inputs), run the same steps for the above two sections on D1MPA and US10, but with ggp.in=GGPRCP85CHL100 and run to model for 93 years (nyears <- 93 in main.pixel line 42).

3.4 Accessing output for figures – MPAs and FBM
1.	Open and update the compare.output() script: 
a.	Update parameter.path1 and parameter.path1 to indicate locations of KPFM2 output for the base case (reference) scenario and the scenario with an action (MPA or FBM), respectively (lines 20 and 22).
2.	Run the compare.output() script…
a.	…for comparing an MPA as (should be current defaults): 
compare.output <- function(spp.string = c("krill", "seals", "pengs", "whales", "fish"), input.objects = c("mst", "mlt", "nst", "nlt"), scalar="MPA", gamma=1, season="summer")
b.	…same as above but with scalar="FBM" to assess a FBM scenario. 
3.	Script output will be: 
[scalar]_stats_spp = the overall total statistics comparing the actioned scenario (MPA or FBM) with the base case scenario
[species]_[scalar]_base = output by SSMU for that species, base case scenario
[species]_[scalar] = SSMU output for that species, scenario with action (MPA or FBM)

4.	This output can be copied and pasted in to Excel to develop the figures from Klein and Watters for both the MPA and FBM papers. Both of those manuscripts include this output in their Supporting Information. 

