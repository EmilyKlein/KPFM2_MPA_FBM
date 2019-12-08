main <- function(
		input.objects = c("mlt", "mst", "nlt", "nst"),
		# input.objects = c("mlt"),
		fishing.options=c(0,3,7),  
		runassessments=TRUE, 
		runmcsims=TRUE,
		ggp.in=GGPRCP85CHL100,
		parameter.path="C:/Users/esklein/Desktop/SWFSC/FOOSA/data/",
		output.file.path="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk/"
) {

#################################################
#######              SETUP                #######
#

memory.limit(75e3) #try about 20Gb	
	
########################### GLOBAL VARIABLES	#########################

OVERWRITE <<- TRUE
#whether or not to overwrite existing files within runmcsims()
#NB not yet coded for runassessments()
	
############################### OTHER SETTINGS ##################################
	
#YOU MIGHT ALSO NEED TO SET GAMMA.FRACTIONS - SEE GET.GAMMA.FRACTIONS.R
	
ntrials <- 500
nyears <- 93
start.fishing.yr <- 1
stop.fishing.yr <- NULL
actual.gamma <-  5607900 / 60300000 #precautionary catch limit / B0
sd.krill.Rdev <- 0.7
reassessment.interval.yr <- NULL


# used in runmcsims()	
risk.reference.levels <- c(0.25,0.5,0.75) 
# set to NULL if do not want to run threshold densities, but also used in assess.krill.fishery
dens.thresholds <- 15
#used in assess.krill.fishery(), #for assessing whether krill density fell below certain thresholds
###########	

################# END OF SETUP ##################################

if (runmcsims==TRUE) {
	run.mcsims(fishing.options,
			input.objects, 
			ntrials=ntrials,
			nyears=nyears,
			GGP.in<- ggp.in,
			start.fishing.yr=start.fishing.yr,
			stop.fishing.yr=stop.fishing.yr,
			actual.gamma=actual.gamma,
			sd.krill.Rdev=sd.krill.Rdev,
			parameter.path=parameter.path, 
			output.file.path=output.file.path,
			reassessment.interval.yr = reassessment.interval.yr,
			dens.thresholds = dens.thresholds
			)
}

if (runassessments==TRUE) {

	assess.risk.predators(fishing.options=fishing.options, input.objects=input.objects, risk.reference.levels=risk.reference.levels, parameter.path=parameter.path, output.file.path=output.file.path)
	assess.krill.fishery(fishing.options=fishing.options, input.objects=input.objects, dens.thresholds = dens.thresholds, parameter.path=parameter.path, output.file.path=output.file.path)
	assess.regional.krill(fishing.options=fishing.options, input.objects=input.objects, parameter.path=parameter.path, output.file.path=output.file.path)
}

}



