compare.output <- function(spp.string = c("krill", "seals", "pengs", "whales", "fish"), input.objects = c("mst", "mlt", "nst", "nlt"), 
                        scalar="MPA", gamma=1, season="summer"){
  # FUNCTION TO PLOT AND SAVE   ABUNDANCES OF SPP GROUPS ACROSS SEVERAL SCENARIOS - ESK 2019
  # TO DO: update this so it also runs for FBM w/o being confusing
  # INPUT:  "spp.string" = list of species names to pull from the output of KPFM2
  #         "input.objects" = list of imput objects that represent the scenarios to be compared
  #         "scalar" = what is the y variable in the comparison, and this also helps name output by actio: 
  #                     "MPA" = MPA/No MPA, "No MPA" = No MPA/MPA, "No pix" = MPA/Not pixelated or MPA
  #                     "FBM3"/"FBM7" = FBM/No FBM for alternative 3/7, "No FBM3"/"No FBM7" = No FBM/FBM 3/7
  #         "gamma" = yield multiplier to address (1.0 = precautionary limit, 0.11 = trigger)
  #         "season" = the season of interest ("summer" or "winter"), or if seasons should be averaged for annual outcomes ("annual"); default is summer
  #
  # OUTPUT: Abundance or relative changes in abundance, depending on user choice 
  #
#### FIRST PULL THE OUTPUT ####
  ## THESE NEED TO BE UPDATED ##
  print("Hope you remembered to update the parameter paths! Just checking!")
  #
  # Base case or reference scenario
  parameter.path1="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk_US10_nompa/"
  # Scenario of interest
  parameter.path2="C:/Users/esklein/Desktop/SWFSC/FOOSA/risk4_US10/"
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
    # NB: This code was originally written for MPA scenarios, so all the "action" cases are tagged "mpa"
    # They are saved as MPA or FBM so output will be clear
    assign(paste(input.objects[b],".base.in",sep = ""),base.in)
    assign(paste(input.objects[b],".mpa.in",sep = ""),mpa.in)
  }
  #
### NOW GET RESULTS ACROSS TRIALS AND PARAMETERIZATIONS, WITH SOME STATISTICS
  #
  # create some stuff for late (indexed numbers should be the same across all params so can just pick one to use)
  ntrials <- mlt.mpa.in$setup$ntrials
  winter <- mlt.mpa.in$setup$ntimes 
  summer <- winter-1
  stuff <- matrix(0, ntrials, length(spp.string))
  aa<-0
  # matrix for housing total statistics
  total_stats<- matrix(0, 4, length(spp.string))
  colnames(total_stats)<-spp.string
  rownames(total_stats)<-c("average", "sd_param","ave_trial", "sd_trial")
  #
  # Pull in and assess abundances for each species individually 
  for(a in seq(spp.string)){
    aa<-aa+1
    for(bb in seq(input.objects)){
      base.in.x <- eval(parse(text=paste(input.objects[bb],".base.in$N$",spp.string[a],sep="")))
      mpa.in.x <- eval(parse(text=paste(input.objects[bb],".mpa.in$N$",spp.string[a],sep="")))
      assign(paste(input.objects[bb],".base",sep = ""),base.in.x)
      assign(paste(input.objects[bb],".mpa",sep = ""),mpa.in.x)
    }
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
      total_stats[2,aa]<-sd(boop[summer,])
    }
    if(season=="winter"){
      total_stats[2,aa]<-sd(boop[winter,])
    }
    if(season=="annual"){
      total_stats[2,aa] <- sd((rowSums(boop[summer:winter,]))/2)
    }
    #
    ## Average across parameterizations ##
    base.ave <- (mlt.base.out+mst.base.out+nlt.base.out+nst.base.out)/4
    mpa.ave <- (mlt.mpa.out+mst.mpa.out+nlt.mpa.out+nst.mpa.out)/4
    
    #### AVERAGE OVER SEASON OR TAKE ONE SEASON for each input scenario ####
    objects <- c("base.ave", "mpa.ave")
    for (jj in seq(objects)){
      input <- eval(parse(text=objects[jj]))
      rows <-  nrow(input)/2
      if(a=="krill"){cols <- ncol(input)-3}   # this needs to be -3 for krill to remove Btubs
      if(a!="krill"){cols <- ncol(input)}
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
      if(season=="summer"){stuff[ii,aa] <- sum(mpa.ave2[summer,,ii])/sum(base.ave2[summer,,ii])}
      if(season=="winter"){stuff[ii,aa] <- sum(mpa.ave2[summer,,ii])/sum(base.ave2[summer,,ii])}
      if(season=="annual"){stuff[ii,aa] <- (sum(mpa.ave2[summer:winter,,ii])/2)/(sum(base.ave2[summer:winter,,ii])/2)}
    }
    total_stats[3,aa]<-mean(stuff[,aa])
    total_stats[4,aa]<-sd(stuff[,aa])
    #
    assign(paste(spp.string[a], scalar, "base", sep="_"), base.ave, .GlobalEnv)
    assign(paste(spp.string[a], scalar, sep="_"), mpa.ave, .GlobalEnv)
    tt<-nrow(mpa.ave)
    total_stats[1,aa]<-sum(mpa.ave[tt,])/sum(base.ave[tt,])
  }
  assign(paste(scalar, "_stats_spp", sep=""),total_stats, .GlobalEnv)
}
  
