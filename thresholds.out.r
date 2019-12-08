thresholds.out <- function(input.objects = c("mlt", "mst", "nst", "nlt"), gamma=1, to.year=NULL, mpa=TRUE, save=TRUE){
  # quick function to save and combine threshold violations - ESK Jan 2019
  # can also use this function to pull threshold violations from any year in a model run
### NB!! Need to update parameter paths and areas/fishing for SSMUs if these change
  
## INPUT ##
  # "input.objects": outcomes of four parameterizations to average
  # "gamma" = yeild multiplier
  # "to.year" = Total thresholds violations are pulled up to this year. 
  #          SET THIS TO NULL to get total thresholds from the entire model run (DEFAULT)
  # "mpa" = if this is for an MPA or not (default is TRUE)
  # "save" = whether to save in addition to outputting threshilds (default is TRUE)
  #
## OUTPUTout ##
  # Table of summed threshold violations, either as tallied by the model or to the year indicated by the user
  # First column is area (SSMU), second is violations of the base case reference ("No_Action"), and 3rd col is for with the Action
  #
  # lil reminder
  print("Hope you remembered to update parameter paths and the SSMU areas if needed!")
  #
  # base case scenario
  parameter.path1="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk_D1_nompa/"
  # scenario of interest
  parameter.path2="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk4_D1/"
  #
  for(b in seq(input.objects)){
    # MAKE SURE THESE ARE THE CORRECT FILE PATHS 
    if(mpa==TRUE){
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".1.3g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".1.3g",gamma,".mc", sep="")))
      #
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".1.g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".1.g",gamma,".mc", sep="")))      # pixelation and MPA
    } else {
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".3.g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".3.g",gamma,".mc", sep="")))
      #
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".3.g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".3.g",gamma,".mc", sep="")))      # pixelation and MPA
    }
    
# running for entire model run, so violations already calculated
    if(is.null(to.year)){
      if(mpa==TRUE){   # only loop the mpa runs if there is no to.year specified (because need to run on output by trial)
        base.in <- loop.pixel(base.in, mc=TRUE)
        mpa.in <- loop.pixel(mpa.in, mc=TRUE)
      }
      #
      base.tv <- base.in$fishery$threshold.violations
      mpa.tv <- mpa.in$fishery$threshold.violations
      #
    }
# if you want the threshold violations for a set year, they have to be calculated from the actual krill up to that year
    else{   
      #
      # all calculations will be by season; this indexes the indicated year * 2 seasons 
      year2<- to.year*2
      # but need to accommodate the first 2 recruitment years for krill
      year <- ((to.year+2)*2)
      #
      if(mpa==TRUE){
        nssmus<-mpa.in$setup$nareas-3     # get the number of pixels excluding BTs for krill - should be same for base.in
      } else{
        nssmus<-mpa.in$setup$nssmus     # get the number of ssmus for krill - should be same for base.in
      }
      ntrials<- mpa.in$setup$ntrials  # get number of trails - should also be the same
      #
      # Pull out the krill abundance for the model run up to the specified year and only for SSMUs (leave out bathtubs)
      # The recruitment years also have to be skipped 
      base.x <- base.in$N$krill[5:year,1:nssmus,]
      mpa.x <- mpa.in$N$krill[5:year,1:nssmus,]
      wbar.base.x <- base.in$wbar[1:year2, 1:nssmus,]
      wbar.mpa.x <- mpa.in$wbar[1:year2, 1:nssmus,]
      #
      # create matrix to hold number of threshold violations up the the year specified 
      if(mpa==TRUE){
        nssmus2<-mpa.in$setup$orig.SSMU   # also need to set these for the final density matrices which will be by SSMU as no-fishing pixels are excluded
        base.tv<- matrix(0, ntrials, nssmus2)
        mpa.tv<- matrix(0, ntrials, nssmus2)
      }else{
        base.tv<- matrix(0, ntrials, nssmus)
        mpa.tv<- matrix(0, ntrials, nssmus)
      }
      #
      # now calculate the krill densities by trial for each parameterization
      for(j in 1:ntrials){
        # First get biomass: abundance * wbar (wt of indiv krill)
        krill.base.biom<- base.x[,,j]*wbar.base.x[,,j]
        krill.mpa.biom<- mpa.x[,,j]*wbar.mpa.x[,,j]
        #
        if(mpa==TRUE){
          # set the areas and if fishing happens -- UPDATE HERE IF AREAS CHANGE (NB these are pixel areas!)
          mpa.areas<- c(407778600000,14221400000, 24300086000, 10759914000, 7491809600, 7576190400,10358684800,
                                     5225315200,7412695900,13604304100,15919260000,11527740000, 22969896600,12352103400,48730190400,
                                     9973809600,715722300000, 93277700000, 13853296200, 1715703800, 7009633800, 3241366200, 
                                     10644257200, 4309742800, 920000000000, 42119000000, 53735000000)
          npixels<-length(mpa.areas)
          # below denotes if the area is open to fishing or not - 1 = fishing, 0 = mpa, no fishing
          fish.here<- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1)
          #
          # Now calculate the krill density in each year (biomass/area for each pixel)
          nssmus2<-mpa.in$setup$orig.SSMU   
          krill.base.dens<- matrix(0, year2, nssmus2)
          krill.mpa.dens<- matrix(0, year2, nssmus2)
          ii<- 1
          for(i in 1:npixels){
            if(fish.here[i]>0){
              krill.base.dens[,ii]<- krill.base.biom[,i]/mpa.areas[i]
              krill.mpa.dens[,ii]<- krill.mpa.biom[,i]/mpa.areas[i]
              ii<- ii+1
            }
          }
        } else {
          # set the SSMU areas -- UPDATE HERE IF AREAS CHANGE
          ssmu.areas<- c(4.2200e+11, 3.5060e+10, 1.5068e+10, 1.5584e+10, 2.1017e+10, 2.7447e+10, 3.5322e+10, 5.8704e+10, 8.0900e+11, 1.5569e+10,
                         1.0251e+10, 1.4954e+10, 9.2000e+11, 4.2119e+10, 5.3735e+10)
          # Now calculate the krill density in each year (biomass/area for each SSMU)
          krill.base.dens<- matrix(0, year2, nssmus)
          krill.mpa.dens<- matrix(0, year2, nssmus)
          for(i in 1:nssmus){
            krill.base.dens[,i]<- krill.base.biom[,i]/ssmu.areas[i]
            krill.mpa.dens[,i]<- krill.mpa.biom[,i]/ssmu.areas[i]
          }
        }
        # sum up the times krill density is less than 15 g/m2
        base.tv[j,]<- apply(krill.base.dens, 2, FUN=function(x) sum(x<15))
        mpa.tv[j,]<- apply(krill.mpa.dens, 2, FUN=function(x) sum(x<15))
      }
    }
    assign(paste(input.objects[b],".base.tv",sep = ""),base.tv)
    assign(paste(input.objects[b],".mpa.tv",sep = ""),mpa.tv)
  }
  # average across the parameterizations
  base.tv.all <- (mlt.base.tv+mst.base.tv+nlt.base.tv+nst.base.tv)/4
  mpa.tv.all <- (mlt.mpa.tv+mst.mpa.tv+nlt.mpa.tv+nst.mpa.tv)/4
  # rm(mlt.base.tv,mst.base.tv,nlt.base.tv,nst.base.tv,mlt.mpa.tv,mst.mpa.tv,nlt.mpa.tv,nst.mpa.tv)
  #
  base.tv.final <- colMeans(base.tv.all)
  mpa.tv.final <- colMeans(mpa.tv.all)
  #
  out.tv <- cbind(c(1:15), base.tv.final, mpa.tv.final)
  colnames(out.tv)<- c("SSMU", "No_Action", "Action")
  if(save==TRUE){assign("out.thresholds", out.tv, .GlobalEnv)}
  out.tv
}

