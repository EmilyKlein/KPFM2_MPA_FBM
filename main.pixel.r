main.pixel <- function(
		input.objects=c("mlt", "mst", "nlt", "nst"),
    fishing.options=c(1),
		runmcsims=TRUE,
		runassessments=FALSE,
		ggp.in=NULL,
		areas=areas.US10.nompa,
		pixels=pixel.US10.nompa,
		foraging.list=foraging.US10,    # Need to include foraging when using it otherwise comment out
		catch.redist=4,    # how to redistribute fishing when pixelated - this does not matter if all areas are open to fishing 
		scenario = "base",   # "base", "MPA", or "FBM" (default is "MPA" - nust run as an MPA for the pixelated but no MPA scenario)
		parameter.path="C:/Users/esklein/Desktop/SWFSC/FOOSA/data/",
		output.file.path="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk/"
) {

  # George Watters last edited 26 March 2009
  # ESK updates to allow run on pixel foosa for risk assessment of MPAs - 8/2017
  #	
  # fishing.options is by default a vector containing:	
  # 0 : NO FISHING
  # 1 : historical catch distribution
  # 2 : predator demand
  # 3 : krill standing stock
  # (4,5,6) : not used this time
  # 7 : CM 51-02 [new option to represent current krill management up to the trigger level]	-- THIS WAS REMOVED BY ESK 11/2018
  
  
  #################################################
  #######              SETUP                #######
  #
  
  
  memory.limit(75e3) # Can adjust here as needed	
  
  ########################### GLOBAL VARIABLES	#########################
  
  OVERWRITE <<- TRUE
  #whether or not to overwrite existing files within runmcsims()
  
  ############################### OTHER SETTINGS ##################################
  ntrials <- 1001
  nyears <- 30
  start.fishing.yr <- 1
  stop.fishing.yr <- NULL
  actual.gamma <-  5607900 / 60300000 #precautionary catch limit / B0
  sd.krill.Rdev <- 0.7
  
  # used in runmcsims()	
  risk.reference.levels <- c(0.25,0.5,0.75) 
  #used in assess.risk.predators()
  dens.thresholds <- c(15) 
  #used in assess.krill.fishery(), #for assessing whether krill density fell below certain thresholds
  ###########	
  
  ################# END OF SETUP ##################################
  
  if (runmcsims==TRUE) {
    run.mcsims.pixel(fishing.options,
  	input.objects, 
  	ntrials=ntrials,
  	nyears=nyears,
  	GGP.in=ggp.in,
  	start.fishing.yr=start.fishing.yr,
  	stop.fishing.yr=stop.fishing.yr,
  	actual.gamma=actual.gamma,
  	sd.krill.Rdev=sd.krill.Rdev,
  	parameter.path=parameter.path, 
  	output.file.path=output.file.path,
  	areas.list=areas,
  	pixel.list=pixels,
  	FORAG = foraging.list,
  	redist = catch.redist, 
  	SCENARIO = scenario)
  }
  
  if (runassessments==TRUE) {
  
    assess.risk.predators.pix(fishing.options=fishing.options, input.objects=input.objects, risk.reference.levels=risk.reference.levels, parameter.path=parameter.path, output.file.path=output.file.path)
    # 
    assess.krill.fishery.pix(fishing.options=fishing.options, input.objects=input.objects, dens.thresholds = dens.thresholds, parameter.path=parameter.path, output.file.path=output.file.path)
    # 
   }
}



