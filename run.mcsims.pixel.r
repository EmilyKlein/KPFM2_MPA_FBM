# assumes a few variables have been set in the global environment by main()
###############################################################################################################
run.mcsims.pixel <- function(fishing.options, input.objects, GGP.in, ntrials, nyears, start.fishing.yr, stop.fishing.yr, 
		actual.gamma, sd.krill.Rdev, parameter.path, output.file.path, areas.list, pixel.list, FORAG, 
		pixel=TRUE,calendar=TRUE, redist, SCENARIO) {
    #
    #  Code updated for pixelated input - ESK 08/2017
    #  Code updated to remove fishing opn 7 insertion (JH 12/12), calendar and pixelation as options - ESK 11/208
  gc()
	for (fishing.option in fishing.options) {	
		
		print("===========================================")
		print(paste("FISHING OPTION: ", fishing.option))
		
		gamma.fractions <- get.gamma.fractions(fishing.option)
		
		if(!is.null(GGP.in)){
		  print("We have climate change! For krill anyway")
		  if(pixel==TRUE){GGP.in<-pixel.ggp(GGP.in,areas.list)}
		}
		
		for(b in seq(input.objects)){ # loop over parameterizations
		  
		  ## read in parameter set
			#load(path.to.saved.parameters[b])
			eval(parse(text=
				paste("load(file='",parameter.path, input.objects[b],".rdata')",sep="")))
		  #
			p1 <- eval(parse(text=input.objects[b]))
			if(!is.null(p1$new.params)) p1 <- p1$new.params
			#
      # Additon to update the threshold densities - ESK 14 Feb 2018
			p1$THRESHOLD.DENSITY <- rep(15, 15)
			#
			if(calendar==TRUE){
			  print("Conditioning on calendar, 1970-2007")
			  if(pixel==TRUE){
			    print("RUNNING A PIXELATED SCENARIO")
			    p1 <- pixel.foosa(p1,areas.list, pixel.list, foraging = FORAG, effort.dist = redist)
			    #
			    ## CALENDAR: generate simulation that is used for the saved state
			    ## nyears=38 here given timeline of calendar (1970-2007)
			    tt.ss <- ssmu.ss.pixel(p1,nyears=38,random.Rkrill=FALSE,single.sim=FALSE)
			  }else{
			    tt.ss <- ssmu.ss(p1,nyears=38,random.Rkrill=FALSE,single.sim=FALSE)
			  }
			}else{
			  if(pixel==TRUE){
			    print("RUNNING A PIXELATED SCENARIO.")
			    p1 <- pixel.foosa(p1,areas.list, pixel.list, foraging = FORAG, effort.dist = redist)
			  }
			}
			##
			## generate the parameter set for the projections
			## assume that the last environmental index will apply for the projection period
			## assume that the last bathtub abundances will apply for the projection period
			p2 <- p1
			p2$ENV.INDEX <- p1$ENV.INDEX[length(p1$ENV.INDEX)]
			tt.tubs <- numeric(p2$NTUBS)
			for(i in 1:length(tt.tubs)){
				tt.tubs[i]<-p1$BATHTUB.ABUNDANCE[[i]][length(p1$BATHTUB.ABUNDANCE[[i]])]
			}
			tt.tubs <- data.frame(t(tt.tubs))
			p2$BATHTUB.ABUNDANCE<-bt.abund.var(nyears=nyears,ntrials=ntrials,btmeans=tt.tubs)


#################################################
####### RUN THE MONTE-CARLO SIMULATIONS #########
			
			if (fishing.option==0) {
				## sim with no fishing
				tmpfile <- paste(output.file.path,input.objects[b],".0.0.mc",sep="")

				if (OVERWRITE==TRUE | file.exists(tmpfile)==FALSE) {
					cat("Running gamma = 0",fill=TRUE)
					tmp <- paste(input.objects[b],".",0,".",0,".","mc",sep="")	
					## UPDATE HERE: Use one for unpixelated or pixelated input
					if(calendar==TRUE){
					  if(pixel==TRUE){
					    assign(tmp, ssmu.mc.pixel(p2, saved.state=tt.ss, ggp=GGP.in, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=NULL, sd.krill.Rdev=sd.krill.Rdev, plot.now=FALSE))
					  }else{assign(tmp, ssmu.mc(p2, saved.state=tt.ss, ggp=GGP.in, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=NULL, sd.krill.Rdev=sd.krill.Rdev, plot.now=FALSE))}
					}else{
					  if(pixel==TRUE){
					    assign(tmp, ssmu.mc.pixel(p2, ggp=GGP.in, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=NULL, sd.krill.Rdev=sd.krill.Rdev, plot.now=FALSE))
					  }else{assign(tmp, ssmu.mc(p2, ggp=GGP.in, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=NULL, sd.krill.Rdev=sd.krill.Rdev, plot.now=FALSE))}
					}
          #
					save(list=tmp, file=tmpfile)
					rm(tmp)
				}
#
			} else {
				## fishing occurring over the agreed range of fractions of gamma
				# now set up the vectors of object and file names for saving the results of each simulation
				# naming convention: (path/)paramprefix.fishingoption.ggammafractions.mc

			  # STILL NEED TO FINISH THIS AUTOMATE THIS UPDATE to run MPAs (ESK): .g is normal, .2g is nopix or MPA (.nopix), .3g is pixelated but no MPA (.nompa)
				if(pixel==FALSE){
				  mc.objects <- paste(input.objects[b],".",fishing.option,".2g",gamma.fractions,".mc",sep="")
				}else{
				  if(SCENARIO=="MPA"||SCENARIO=="FBM"){
				    mc.objects <- paste(input.objects[b],".",fishing.option,".g",gamma.fractions,".mc",sep="")
				  }
				  if(SCENARIO=="base"){
				    mc.objects <- paste(input.objects[b],".",fishing.option,".3g",gamma.fractions,".mc",sep="")
				  }
				}
				mc.files <- paste(output.file.path,mc.objects,sep="")
				
				if (OVERWRITE==FALSE) {
					aa <- which(file.exists(mc.files))	
					if (any(aa)) {
						mc.objects <- mc.objects[-aa]
						mc.files <- mc.files[-aa]
					}
					if (length(mc.files)==0) print("All files already processed for these fishing options>0")
				}			
				#remove objects and files that already exist if they don't need processing

				print(paste(length(mc.files),"FILES TO PROCESS: "))
				print("==========================================")
				print(mc.files)

				if (!length(mc.files)==0) { #if there's anything to process ...
					for(k in seq(mc.objects)) {#seq(gamma.fractions)){
						pos <- regexpr("g.*.mc", mc.objects[k])
						gamma.fraction <- as.numeric(substr(mc.objects[k],pos+1,pos+attr(pos,"match.length")-4))
						#get gamma.fraction from the filename ... replacement for when not processing all files
								
						cat("Running gamma = ",paste(actual.gamma,"*", gamma.fraction),fill=TRUE)
						
						# Calendar
						if(calendar==TRUE){
						  if(pixel==TRUE){
						    assign(mc.objects[k], ssmu.mc.pixel(p2, saved.state=tt.ss, safe.mode=FALSE, ggp=GGP.in, ntrials=ntrials, nyears=nyears, fishing.option=fishing.option, start.fishing.yr=start.fishing.yr, stop.fishing.yr=stop.fishing.yr, actual.gamma=actual.gamma*gamma.fraction, sd.krill.Rdev=sd.krill.Rdev,plot.now=FALSE))
						  } else {
						    assign(mc.objects[k], ssmu.mc(p2, saved.state=tt.ss, safe.mode=FALSE, ggp=GGP.in, ntrials=ntrials, nyears=nyears, fishing.option=fishing.option, start.fishing.yr=start.fishing.yr, stop.fishing.yr=stop.fishing.yr, actual.gamma=actual.gamma*gamma.fraction, sd.krill.Rdev=sd.krill.Rdev,plot.now=FALSE))
						  }
						} else {
						  if(pixel==TRUE){
						    assign(mc.objects[k], ssmu.mc.pixel(p2, safe.mode=FALSE, ggp=GGP.in, ntrials=ntrials, nyears=nyears, fishing.option=fishing.option, start.fishing.yr=start.fishing.yr, stop.fishing.yr=stop.fishing.yr, actual.gamma=actual.gamma*gamma.fraction, sd.krill.Rdev=sd.krill.Rdev,plot.now=FALSE))
						  } else {
						    assign(mc.objects[k], ssmu.mc(p2, safe.mode=FALSE, ggp=GGP.in, ntrials=ntrials, nyears=nyears, fishing.option=fishing.option, start.fishing.yr=start.fishing.yr, stop.fishing.yr=stop.fishing.yr, actual.gamma=actual.gamma*gamma.fraction, sd.krill.Rdev=sd.krill.Rdev,plot.now=FALSE))
						  }
						}
						save(list=mc.objects[k],file=mc.files[k])
					}	
				} else {
					print("nothing to process")
				}
				
			}

			####### END OF MONTE-CARLO SIMS ##############
      gc()  # attempt to clean up memory
		} # parameter loop
	} # end of fishing options loop
}	

