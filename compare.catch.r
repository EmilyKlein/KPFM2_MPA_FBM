compare.catch <- function(input.objects = c("mst", "mlt", "nst", "nlt"), in.string = "catch", scalar="MPA", 
                          gamma=1, season="annual", get.tv=NULL){
  # FUNCTION TO PLOT AND SAVE   ABUNDANCES OF SPP GROUPS ACROSS SEVERAL SCENARIOS - ESK 2019
  # TO DO: update this so it also runs for FBM w/o being confusing
  # INPUT:  "in.string" = "catch" or "allocation"; default is "catch"
  #         "input.objects" = list of imput objects that represent the scenarios to be compared
  #         "scalar" = what is the y variable in the comparison, and this also helps name output by actio: 
  #                     "MPA" = MPA/No MPA, "No MPA" = No MPA/MPA, "No pix" = MPA/Not pixelated or MPA
  #                     "FBM3"/"FBM7" = FBM/No FBM for alternative 3/7, "No FBM3"/"No FBM7" = No FBM/FBM 3/7
  #         "gamma" = yield multiplier to address (1.0 = precautionary limit, 0.11 = trigger)
  #         "season" = the season of interest ("summer" or "winter"), or if seasons should be averaged for annual outcomes ("annual"); 
  #                    generally want to sum catch over the year, so default is annual
  #         "get.tv" = whether or not to also get threshold violations by calling thresholds.out code 
  #                     NULL = do not run (default); "end" = for end of model run, include number = the year to pull them (see that code for details)
  #                     NB!!!! If you are going to use this, MUST remember to go to that code and update parameter paths
  #
  # OUTPUT: Abundance or relative changes in abundance, depending on user choice 
  #
  #### FIRST PULL THE OUTPUT AND GET MEAN ACROSS TRIALS ####
  # a reminder to update parameter path - these need to be updated each time
  print("Hope you remembered to update the parameter paths! Just checking!")
  # 
  # base case or reference scenario
  parameter.path1="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk_D1_nompa/"
  # Scenario of interest
  parameter.path2="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk4_D1/"
  # 
  #
  for(b in seq(input.objects)){
    # Access the KPFM2 output - these are all different as they've been named differently in the code earlier
    if(scalar=="MPA"){
      # get the base case
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".1.3g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".1.3g",gamma,".mc", sep="")))  
      base.in <- loop.pixel(base.in)     # runs with an MPA need to be aggregated (looped) back to SSMU level 
      # Get the scenario of interest
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".1.g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".1.g",gamma,".mc", sep="")))      
      mpa.in <- loop.pixel(mpa.in)
    }
    if(scalar=="No MPA"){
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".1.g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".1.g",gamma,".mc", sep="")))  
      base.in <- loop.pixel(base.in) 
      #
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".1.3g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".1.3g",gamma,".mc", sep="")))      
      mpa.in <- loop.pixel(mpa.in)
    }
    if(scalar=="No pix"){
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".1.2g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".1.2g",gamma,".mc", sep="")))  
      #
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".1.g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".1.g",gamma,".mc", sep="")))      
      mpa.in <- loop.pixel(mpa.in)
    }
    if(scalar=="FBM3"|| scalar=="No FBM3"){
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".3.g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".3.g",gamma,".mc", sep=""))) 
      #
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".3.g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".3.g",gamma,".mc", sep="")))      
    }
    if(scalar=="FBM7" || scalar=="No FBM7"){
      eval(parse(text=paste("load('",parameter.path1, input.objects[b],".7.g",gamma,".mc')",sep="")))
      base.in <- eval(parse(text=paste(input.objects[b],".7.g",gamma,".mc", sep=""))) 
      #
      eval(parse(text=paste("load('",parameter.path2, input.objects[b],".7.g",gamma,".mc')",sep="")))
      mpa.in <- eval(parse(text=paste(input.objects[b],".7.g",gamma,".mc", sep="")))      
    }
    #
    # Then pull out what to analyze depending on call of "string"
    # NB: This code was originally written for MPA scenarios, so all the "action" cases are tagged "mpa"
    # They are saved as MPA or FBM so output will be clear
    if(in.string=="catch"){
      base.in.x <- eval(parse(text=paste("base.in$fishery$catch", sep="")))
      mpa.in.x <- eval(parse(text=paste("mpa.in$fishery$catch",sep="")))}
    if(in.string=="allocation"){
      base.in.x <- eval(parse(text=paste("base.in$fishery$allocation", sep="")))
      mpa.in.x <- eval(parse(text=paste("mpa.in$fishery$allocation",sep="")))}
    #
    assign(paste(input.objects[b],".base",sep = ""),base.in.x)
    assign(paste(input.objects[b],".mpa",sep = ""),mpa.in.x)
  }
  #  
  # create some stuff for late (indexed numbers should be the same across all params so can just pick one to use)
  ntrials <- mpa.in$setup$ntrials
  winter <- mpa.in$setup$ntimes 
  summer <- winter-1
  #  left this as a matrix in case want to add cols for some reason
  # (e.g. would like to pull in threshold violations here too)
  stuff <- matrix(0, ntrials, 1)
  # 
  total_stats<- matrix(0, 4, 2)
  colnames(total_stats)<-c("catch", "thresholds")
  rownames(total_stats)<-c("average", "sd_param","ave_trial", "sd_trial")
  #
  # Average across MC trials 
  mlt.base.out<-  apply(mlt.base, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  mst.base.out<-  apply(mst.base, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  nlt.base.out<-  apply(nlt.base, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  nst.base.out<-  apply(nst.base, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  #
  mlt.mpa.out<-  apply(mlt.mpa, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  mst.mpa.out<-  apply(mst.mpa, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  nst.mpa.out<-  apply(nst.mpa, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  nlt.mpa.out<-  apply(nlt.mpa, 2, FUN = function(x, prob){apply(x, 1, mean, na.rm = TRUE)})
  #
  # Get the variability across parameterizations
  boop <- rowSums(mlt.mpa.out)/rowSums(mlt.base.out)
  boop<-cbind(boop, rowSums(mst.mpa.out)/rowSums(mst.base.out))
  boop<-cbind(boop, rowSums(nlt.mpa.out)/rowSums(nlt.base.out))
  boop<-cbind(boop, rowSums(nst.mpa.out)/rowSums(nst.base.out))
  #
  if(season=="summer"){
    total_stats[2,1]<-sd(boop[summer,])
  }
  if(season=="winter"){
    total_stats[2,1]<-sd(boop[winter,])
  }
  if(season=="annual"){
    total_stats[2,1] <- sd((colSums(boop[summer:winter,]))/2)
  }
  #
  ## Average across parameterizations ##
  base.ave <- (mlt.base.out+mst.base.out+nlt.base.out+nst.base.out)/4
  mpa.ave <- (mlt.mpa.out+mst.mpa.out+nlt.mpa.out+nst.mpa.out)/4
  #
  #
  #### AVERAGE OVER SEASON OR TAKE ONE SEASON for each input scenario ####
  objects <- c("base.ave", "mpa.ave")
  for (jj in seq(objects)){
    input <- eval(parse(text=objects[jj]))
    rows <-  nrow(input)/2
    cols <- ncol(input)
    #
    if(season=="annual"){
      yy <- matrix(0, rows, cols)
      for (i in 1:cols){
        x <- 1
        y <- 2
        for (j in 1:rows){
          yy[j,i]<- mean(input[x:y,i])
          x<- x+2
          y <- y+2
        }
      }
      assign(paste(objects[jj]),yy)
    }
    else{
      yy <- matrix(0, rows, cols)
      for (i in 1:cols){
        if(season=="summer"){x<-1}   # this is the season to index, 1 for summer, 2 for winter
        if(season=="winter"){x<-2}
        for (j in 1:rows){
          yy[j,i]<- input[x,i]
          x <- x+2
        }
      }
      assign(paste(objects[jj],sep = ""),yy)
    }
  }
  # Save the variability across trials (but honestly I don't know this is useful? but already coded so... )
  base.ave2<-(mlt.base+mst.base+nlt.base+nst.base)/4
  mpa.ave2<-(mlt.mpa+mst.mpa+nlt.mpa+nst.mpa)/4
  #
  for(ii in 1:ntrials){
    if(season=="summer"){stuff[ii,1] <- sum(mpa.ave2[summer,,ii])/sum(base.ave2[summer,,ii])}
    if(season=="winter"){stuff[ii,1] <- sum(mpa.ave2[summer,,ii])/sum(base.ave2[summer,,ii])}
    if(season=="annual"){stuff[ii,1] <- (sum(mpa.ave2[summer:winter,,ii])/2)/(sum(base.ave2[summer:winter,,ii])/2)}
  }
  total_stats[3,1]<-mean(stuff[,1])
  total_stats[4,1]<-sd(stuff[,1])
  #
  # go get threshold violations if called
  if(is.null(get.tv)){
    total_stats[,2]<-NA
  }else{
    if(get.tv=="end"){
      tv.out<-thresholds.out(save=FALSE)
      total_stats[1,2]<- sum(tv.out[,3])/sum(tv.out[,2])
    }else{
      tv.out<-thresholds.out(to.year=get.tv, save=FALSE)
      total_stats[1,2]<- sum(tv.out[,3])/sum(tv.out[,2])
    }
    total_stats[2:4,2]<-NA
  }
  #
  assign(paste(scalar, "_catch_base", sep=""), base.ave, .GlobalEnv)
  assign(paste(scalar, "_catch_mpa", sep=""), mpa.ave, .GlobalEnv)
  if(!is.null(get.tv)){assign(paste(scalar, "_thresholds", sep=""), tv.out, .GlobalEnv)}
  #
  tt<-nrow(mpa.ave)
  total_stats[1,1]<-sum(mpa.ave[tt,])/sum(base.ave[tt,])
  #
  assign(paste(scalar,"_stats_catch", sep=""),total_stats, .GlobalEnv)
}



    
    

