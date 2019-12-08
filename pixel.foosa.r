pixel.foosa <-function(input.data, areas.list, pixel.list=NULL, effort.dist=3, foraging=NULL){
  # Pixelates original parameter list into MPAs from original SSMUs (small-scale management units) - ESK May 2017. 
  # INPUT
      # "input.data" = .rdata file for data to be pixelated, this needs to be a file created via "import.all.parameters" or has similar structure
      # "areas.list" = matrix of (col 1) SSMUs+BTUBS, (2) how many pixel per SSMU (NB: BTubs are not pixelated), (3) # pixels ON for fishing, (4) # pixels OFF for fishing
      # "pixel.list" = optional file definining percentages for predator and krill updates - default is even distribution across pixels
      #                 (col 1) the pixel #, (col 2-6) proportion of: (2)  the original SSMU area the pixel is (must be 1 if no pixels for that SSMU), 
      #                 (3) historical catch, (4) pred pop that is in each pixel (if bred/recruit in only one pixel, must 1 for that and 0 for others), 
      #                 (5) season 1 pred foraging, (6) season 2 foraging, (7) krill init.density - this will usually be the same as area proportions (col 2), (7) krill Ralpha
      #                 IF NO CHANGE to pred or krill parameters, these cols should be 1 to loop the params over pixels
      #                 *** NOTE: pixel.list MUST include bathtubs at the end, one per btub, pop proportions = 0 unless btubs have pops in them and/or are pixelated
      # "effort.dist' = sets up how to displaced fishing will be redistributed:  
      #                 = 1 effort is redistributed equally across all other SSMUs/pixels
      #                 = 2 effort is redistributed equally across remaining SSMUs/pixels in Subarea
      #                 = 3 effort is (a) redistributed in nearby SSMUs for closed SSMUs - *** NB THIS IS PRE-DEFINED AND MUST  BE UPDATED AT LINE 85
      #                               OR (b) redistributed to open pixel by each SSMU if the SSMU is not completely closed
      #                 = 4 effort is redistributed by current distribution of effort across SSMUs
      # "foraging" = if foraging proportions change across all pixels, this is the matrix or list of matrices with those foraging proportions by species             
      #                 *** NOTE: This must be set up by species and seasons, with all species for season 1 together, followed by all species season 2
      #
  # OUTPUT: same format as from importat.all.parameters.r, ready for ssmu.ss.pixel() 

  nareas <- length(input.data$AREAS) # Set number of areas (SSMUS + BTs)
  nssmus <- length(input.data$HISTORICAL.CATCH) # Set number of SSMUs

  pixel.data<-list()  # Set up new list for data
  
### UPDATE AREAS ###
  areas <- NULL
  if(!is.null(pixel.list)){
    for (j in 1:nareas){
      areas1 <- NULL
      no.pixel = areas.list[[j,2]]
      for (k in 1:no.pixel){
        areas1[k] <- input.data[[1]][j]
      }
      areas <- c(areas, areas1)
    }
    pixels <- nrow(pixel.list)
    for (m in 1:pixels){
      areas[m] <- areas[m]*pixel.list[m,2]  # these should be loaded as proportions
    }
  } else {
    for (j in 1:nareas){
      areas1 <- NULL
      no.pixel = areas.list[[j,2]]
      for (k in 1:no.pixel){
        areas1[k] <- input.data[[1]][j]/no.pixel
      }
      areas <- c(areas, areas1)
    }
  }
  pixel.data[[1]] <- areas
  names(pixel.data)[[1]] <- "AREAS"

  ntubs <- input.data$NTUBS
  tot.areas <- length(pixel.data[[1]])  # get total number of areas (pixels + tubs)
  tot.pixels <- tot.areas - ntubs           # number of pixels, **not including bathtubs**
  
### UPDATE HISTORICAL CATCH by pixel. 
##  NB THIS CODE WILL NEED TO BE RE-ASSESSED FOR >2 PIXELS/SSMU 
## First we loop catch over pixels
  hcatch <- NULL
  if(!is.null(pixel.list)){  # if pixel.list is used, catch is pixelated proportional to pixel area size
    for (j in 1:nssmus){
      hcatch1 <- NULL
      no.pixel = areas.list[[j,2]]
      for (k in 1:no.pixel){
        hcatch1[k] <- input.data[[2]][j]    
      }
      hcatch <- c(hcatch, hcatch1)
    }
    hcatch <- hcatch*pixel.list[1:tot.pixels,2] 
    add.catch.p <- hcatch   # will use this for adding catch later
#
# Now make sure there is in no catch in the closed (MPA) areas
    for(jj in 1:tot.pixels){
      if(pixel.list[jj,3]==0){
        hcatch[jj]<-0
      }
    }
  } else { # if no pixel.list, use the number of pixels and assume even distribution across both
    for(j in 1:nssmus){
      hcatch1<- NULL
      if(areas.list[[j,4]]!=0){
        no.pixel = areas.list[[j,2]]
        pixels <- areas.list[[j,2]]
        pixel.on <- areas.list[[j,3]]
        pixel.off <- pixel.on + 1
        for (k in 1:pixel.on){
          hcatch1[k] <- input.data[[2]][j]/no.pixel
        }
        for(k in pixel.off:pixels){
          hcatch1[k]<-0
        }
        hcatch <- c(hcatch, hcatch1)
      } else {
        hcatch1 <- NULL
        no.pixel = areas.list[[j,2]]
        for (k in 1:no.pixel){
          hcatch1[k] <- input.data[[2]][j]/no.pixel
        }
        hcatch <- c(hcatch, hcatch1)
      }
    }
# Now need to also get add.catch for use later
    add.catch.p <- NULL
    for (j in 1:nssmus){
      hcatch2 <- NULL
      no.pixel = areas.list[[j,2]]
      for (k in 1:no.pixel){
        hcatch2[k] <- input.data[[2]][j]/no.pixel
      }
      add.catch.p <- c(add.catch.p, hcatch2)
    }
  } 
  #
  add.catch.p.final <- NULL
  for(mm in 1:tot.pixels){
    if(pixel.list[mm,3]==0){   
      add.catch.p.final[mm] <- add.catch.p[mm]  # just get the MPA demand to be redistributed
    }
  }
  if(!is.null(add.catch.p.final)){   # if there is no MPA, add.catch.p.final will not update, and neither will the rest of this
    add.catch.p.final[is.na(add.catch.p.final)]<-0  # replace NAs with 0s...
    end <- length(add.catch.p.final)
    add.catch.p.final<- c(add.catch.p.final[2:end],0)    # ...and shift forward to the non-MPA spots so can just add things up - THIS ALSO WILL NEED TO BE RE-ASSESSED in scenario with >2 pixels/SSMU
  #
  #  
  # Demand redistributed over total area - add.catch divided by total number of original SSMUs (in this case open areas)
    if(effort.dist==1){
      add.catch.p.final<- add.catch.p.final/nssmus  # divide up the MPA demand by total number of open areas (no. orig SSMUs)
      tot.add.catch.p <- sum(add.catch.p.final)     # Each area gets this additional amount added to it, i.e. demand from each closed area
      for(c in 1:length(hcatch)){
        if(hcatch[c]!=0){
          hcatch[c]<- hcatch[c]+tot.add.catch.p
        }
      }
    }
  # Demand equally distributed by Subarea  --- THIS MUST BE UPDATED AS CODE DOES NOT KNOW WHERE SUBAREAS ARE
    if (effort.dist==2){
      for(i in 1:15){   # This is "i in 1:16" for D1MPA, "i in 1:15" for US10
        add.catch.p.final[i]<- add.catch.p.final[i]/8       # divide up the MPA demand by total number of open areas (# orig SSMUs in the Subarea)
      }
      # This loop is just for D1MPA - comment out for US10 (only one pixel in this subarea, so use line 326 below)
      # for(i in 17:24){
      #   add.catch.p.final[i]<- add.catch.p.final[i]/4       # divide up the MPA demand by total number of open areas (# orig SSMUs in the Subarea)
      # }
      tot.add.catch.p1 <- sum(add.catch.p.final[1:15])     # Each area gets this additional amount added to it, i.e. demand from each closed area
      # Line below is for D1MPA only
      # tot.add.catch.p2 <- sum(add.catch.p.final[17:24])     # Each area gets this additional amount added to it, i.e. demand from each closed area
      # Line below is for Us10 only
      tot.add.catch.p2<-add.catch.p.final[16]/4
      #
      for(c in 1:15){   # This is "c in 1:16" for D1MPA, "c in 1:15" for US10 
        if(hcatch[c]!=0){
                 hcatch[c]<- hcatch[c]+tot.add.catch.p1
        }
      }
      for(cc in 16:20){   # This is 17:24 for Dom1, 16:20  for US10
        if(hcatch[cc]!=0){
          hcatch[cc]<- hcatch[cc]+tot.add.catch.p2
        }
      }
    }
    #
    if(effort.dist==3){ 
      x <- length(hcatch) # hcatch and add.catch will be different lengths as add.catch is from just closed pixels 
      y <- length(add.catch.p.final)
      z <- x-y
      add.catch.p.final <- c(add.catch.p.final, (rep(0,z)))
      hcatch <- hcatch+add.catch.p.final
    }
  
  # Amount based on current distribution of effort
    if (effort.dist==4){
      tot.catch <- sum(input.data$HISTORICAL.CATCH)
      eff.proportion <- NULL 
      for (j in 1:nssmus){
        eff.prop.a <- NULL
        no.pixel = areas.list[[j,2]]
        for (k in 1:no.pixel){
          eff.prop.a[k] <- input.data$HISTORICAL.CATCH[j]/tot.catch  # need to loop over pixels for use later
        } 
        eff.proportion <- c(eff.proportion, eff.prop.a)
      }
      for(jj in 1:(length(eff.proportion))){
        if (pixel.list[[jj,3]]==0){         # Pixels where fishing is "off" - so puts zeros here for next step
            eff.proportion[jj] <- 0
        }
      }
      nmpa <- length(add.catch.p.final)
      add.catch.4 <- vector(mode="numeric", length=tot.pixels)
      for (t in 1:tot.pixels){
        for(tt in 1:nmpa){
          add <- eff.proportion[t]*add.catch.p.final[tt]
          add.catch.4[t]<-add.catch.4[t] + add
        }
      }
      hcatch <- hcatch+add.catch.4
    }
  }
  pixel.data[[2]] <- hcatch
  names(pixel.data)[[2]] <- "HISTORICAL.CATCH"


##### UPDATE PREDATORS #####

  no.spp <- 4  
  
  for (i in 1:no.spp){
    new <- list()
    if(!is.null(pixel.list)){
      for (j in 1:nssmus){
        no.pixel <- areas.list[[j,2]]
        for (k in 1:no.pixel){
          x <- length(new)+1
          new[x]<- input.data[[2+i]][j]

          #Need to update the columns for the foraging matrix
          y <- NULL
          for (m in 1:nareas){
            to.pixel <- areas.list[[m,2]]
            y1 <- matrix(0,2,to.pixel)
            for (l in 1:to.pixel) {
              y1[,l] <- new[[x]]$foraging.matrix[,m]
            }
            y <- cbind(y, y1)
          }
          new[[x]]$foraging.matrix <- y
          names(new)[x] <-c(paste0("ssmu.",j, ".",k))
        }
        pixel.data[[2+i]] <- new
        names(pixel.data)[[2+i]] <- names(input.data)[[2+i]]
      }
      for (l in 1:tot.pixels){
        if(pixel.list[[l,3+i]]==0){
          pixel.data[[2+i]][[l]]$init.value <- 0
          pixel.data[[2+i]][[l]]$Jphi <- NA
          pixel.data[[2+i]][[l]]$M[1:2] <- NA
          pixel.data[[2+i]][[l]]$Mswitch[1:2] <- NA
          pixel.data[[2+i]][[l]]$Mprop[1:2] <- NA
          pixel.data[[2+i]][[l]]$Ralpha[1:2] <- NA
          pixel.data[[2+i]][[l]]$RSpeak[1:2] <- NA
          pixel.data[[2+i]][[l]]$RRpeak[1:2] <- NA
          pixel.data[[2+i]][[l]]$Rphi[1:2] <- NA
          pixel.data[[2+i]][[l]]$Qq[1:2] <- NA
          pixel.data[[2+i]][[l]]$QQmax[1:2] <- NA
          pixel.data[[2+i]][[l]]$Qk5[1:2] <- NA
          pixel.data[[2+i]][[l]]$foraging.matrix[,]<- NA
        } else {
          # cols in pixel.list: (1)pixel, (2)areas, (3)catch, (4-7)pred params, (8)pred forag season 1, (9)pred forag season 2, (10 & 11)krill
          pixel.data[[2+i]][[l]]$init.value <- pixel.data[[2+i]][[l]]$init.value * pixel.list[[l,3+i]]
  
          # Foraging matrix a bit more complicated as it has to be scaled by areas (col 2 in pixel.list)
          # Also need to allow foraging to change via "foraging" input if included
          if (!is.null(foraging)){
            for (xx in 1:tot.areas){
              pixel.data[[2+i]][[l]]$foraging.matrix[1,xx] <- pixel.data[[2+i]][[l]]$foraging.matrix[1,xx] * foraging[[i]][l,xx]   # season 1
              pixel.data[[2+i]][[l]]$foraging.matrix[2,xx] <- pixel.data[[2+i]][[l]]$foraging.matrix[2,xx] * foraging[[i+4]][l,xx]   # season 2
            }
          } else{
            for (xx in 1:tot.areas){
              pixel.data[[2+i]][[l]]$foraging.matrix[1,xx] <- pixel.data[[2+i]][[l]]$foraging.matrix[1,xx] * pixel.list[[xx,8]]   # season 1
              pixel.data[[2+i]][[l]]$foraging.matrix[2,xx] <- pixel.data[[2+i]][[l]]$foraging.matrix[2,xx] * pixel.list[[xx,9]]   # season 2
            }
          }
        }
      }
    } else {
      for (j in 1:nssmus){
        no.pixel <- areas.list[[j,2]]
        for(k in 1:no.pixel){
          x <- length(new)+1
          new[x] <- input.data[[2+i]][j]
          
          y <- NULL    # Foraging matrix
          for (m in 1:nareas){
            to.pixel <- areas.list[[m,2]]
            y1 <- matrix(0,2,to.pixel)
            for (l in 1:to.pixel) {
              y1[,l] <- new[[x]]$foraging.matrix[,m]/to.pixel
            }
            y <- cbind(y, y1)
          } 
          new[[x]]$foraging.matrix <- y
  
          if(k< no.pixel){   #Cancel out preds in pixels where fishing happens (first set of pixels - preds are retained in final pixel)
            new[[x]]$init.value <- 0
            new[[x]]$Jphi <- NA
            new[[x]]$M[1:2] <- NA
            new[[x]]$Mswitch[1:2] <- NA
            new[[x]]$Mprop[1:2] <- NA
            new[[x]]$Ralpha[1:2] <- NA
            new[[x]]$RSpeak[1:2] <- NA
            new[[x]]$RRpeak[1:2] <- NA
            new[[x]]$Rphi[1:2] <- NA
            new[[x]]$Qq[1:2] <- NA
            new[[x]]$QQmax[1:2] <- NA
            new[[x]]$Qk5[1:2] <- NA
            new[[x]]$foraging.matrix[,]<- NA
          }
          names(new)[x] <-c(paste0("ssmu.",j, ".",k))
        }
      }
      pixel.data[[2+i]] <- new
      names(pixel.data)[[2+i]] <- names(input.data)[[2+i]]
    }
  }


##### UPDATE KRILL #####

  new <- list()
  if(!is.null(pixel.list)){
    for (j in 1:nssmus){   # First update the SSMUs
      no.pixel <- areas.list[[j,2]]
      for (k in 1:no.pixel) {
        x <- length(new)+1
        new[x] <- input.data[[7]][j]
        names(new)[x] <-c(paste0("ssmu.",j, ".",k))
      }
    }
    a <- nssmus+1
    for (j in a:nareas){    # Now update the bathtubs so they have a different name as with initial input data
      x <- length(new)+1
      new[x] <- input.data[[7]][j]
      names(new)[x] <-c(paste0("btub.",j))
    }  
    for (m in 1:tot.pixels){
      new[[m]]$Ralpha <- new[[m]]$Ralpha * pixel.list[[m,11]] # update by proportion the pixel is of the original SSMU area
    }
  } else {
    for (j in 1:nssmus){   # First update the SSMUs
      no.pixel <- areas.list[[j,2]]
      for (k in 1:no.pixel) {
        x <- length(new)+1
        new[x] <- input.data[[7]][j]

  # UPDATE krill aspects - Ralpha is by areas, initial density is reassessed in the pixel.data if needed, else = 1
        new[[x]]$Ralpha <- new[[x]]$Ralpha/no.pixel
        names(new)[x] <-c(paste0("ssmu.",j, ".",k))
      }
    }
    a <- nssmus+1
    for (j in a:nareas){    # Now update the bathtubs so they have a different name as with initial input data
      x <- length(new)+1
      new[x] <- input.data[[7]][j]
      names(new)[x] <-c(paste0("btub.",j))
    }  
  }
  pixel.data[[7]] <- new
  names(pixel.data)[[7]] <- names(input.data)[[7]]


#### UPDATE CATCH SET-UP - loop over number of pixels per SSMU ####

  catch <- NULL
  if(!is.null(pixel.list)){
    for (j in 1:nssmus){
      no.pixel <- areas.list[[j,2]]
      catch1 <- matrix(0,2,no.pixel)
      for (k in 1:no.pixel){
        catch1[,k] <- input.data[[8]][,j]
      }
      catch <- cbind(catch, catch1)
    }
    for (m in 1:tot.pixels){
      catch[1,m] <- catch[1,m]*pixel.list[m,3]   
      catch[2,m] <- catch[2,m]*pixel.list[m,3]  
    }
  } else {
    for (j in 1:nssmus){
      no.pixel <- areas.list[[j,2]]
      catch1 <- matrix(0,2,no.pixel)
      for (k in 1:no.pixel){
        catch1[,k] <- input.data[[8]][,j]/no.pixel   
      }
      catch <- cbind(catch, catch1)
    }
  }

  pixel.data[[8]] <- catch
  names(pixel.data)[[8]] <- names(input.data)[[8]]

  
#### UPDATE OPTION 5 ####
# this may need to be adjusted later, we haven't used Option 5 yet, so not sure I've got everything correct in terms of updates

  pixel.data[[9]] <- input.data[[9]]
  names(pixel.data)[[9]] <- names(input.data)[[9]]
  pixel.data[[9]][[1]] <- c(1:(tot.areas)) # option5.areas

  # loop other aspects over number of pixels by SSMU, to $base.option 
  for (m in 2:7){
    x <- NULL
    x1 <- NULL
    for (j in 1:nareas){
      no.pixel = areas.list[[j,2]]
      for (k in 1:no.pixel){
        x1[k] <- input.data[[9]][[m]][j]
      }
      x <- c(x, x1)
    }
  pixel.data[[9]][[m]]<- x
  }

#### UPDATE THRESHOLD DENSITY AND AVAILABLE FRACTION - loop over number of pixels ####
  for (m in 10:11){
    y <- length((input.data[[m]]))
    x <- NULL
    for (j in 1:y){
      x1 <- NULL
      no.pixel = areas.list[[j,2]]
      for (k in 1:no.pixel){
        x1[k] <- input.data[[m]][j]
      }
      x <- c(x, x1)
    }
  pixel.data[[m]]<- x
  names(pixel.data)[[m]] <- names(input.data)[[m]]
  }

#### UPDATE V ARRAY ####

  p_in <- input.data$V.ARRAY
  
  ## Now distribute this across the pixels
  pixel.data[[12]] <- array(0, dim=c(tot.areas, tot.areas, 2))
  names(pixel.data)[12] <- names(input.data)[12]
  x <- NULL
  y <- NULL
  if(!is.null(pixel.list)){
    for (j in 1:nareas){
      no.pixel = areas.list[[j,2]]
      x1 <- matrix(0,nareas, no.pixel)
      y1 <- matrix(0,nareas, no.pixel)
      for (k in 1:no.pixel){
        x1[,k] <- (p_in[,j,1])
        y1[,k] <- (p_in[,j,2])
      }
      x <- cbind(x, x1)
      y <- cbind(y, y1)
    }
    for (j in 1:nareas){  # update by pixel area
      for(m in 1:tot.areas){
        x[j,m]<-x[j,m]*pixel.list[m,2]
        y[j,m]<-y[j,m]*pixel.list[m,2]
      }
    }
   } else{ 
    for (j in 1:nareas){      # First expand by columns
      no.pixel <- areas.list[[j,2]]
      x1 <- matrix(0,nareas, no.pixel)
      y1 <- matrix(0,nareas, no.pixel)
      for (k in 1:no.pixel){
        x1[,k] <- (p_in[,j,1])/no.pixel
        y1[,k] <- (p_in[,j,2])/no.pixel
      }
      x <- cbind(x, x1)
      y <- cbind(y, y1)
    }
  }
  v_summer <- x
  v_winter <- y
  
  # Loop over the "from" pixels
  a <- NULL
  b <- NULL
  for (j in 1:nareas){
    no.pixel = areas.list[[j,2]]
    a1 <- matrix(0,no.pixel, tot.areas)
    b1 <- matrix(0,no.pixel, tot.areas)
    for (k in 1:no.pixel){
      a1[k,] <- v_summer[j,]
      b1[k,] <- v_winter[j,]
    }
    a <- rbind(a, a1)
    b <- rbind(b, b1)
  }
  pixel.data[[12]][,,1]<- a
  pixel.data[[12]][,,2]<- b

#### UPDATE COMPETITION MATRIX ####
  x <- NULL
  for (j in 1:nareas){
    no.pixel = areas.list[[j,2]]
    x1 <- matrix(0,no.pixel,ncol(input.data[[13]]))
    for (k in 1:no.pixel){
      x1[k,] <- input.data[[13]][j,]
    }
    x <- rbind(x, x1)
  }
  pixel.data[[13]]<- x
  names(pixel.data)[[13]] <- names(input.data)[[13]]


#### UPDATE REMAINING DATA - should double-check none of these need to be updated ####

  pixel.data[[14]] <- input.data[[14]]  # Bathtub abundance - This will need to be updated if we want pixels in btubs for some reason
  names(pixel.data)[[14]] <- names(input.data)[[14]]
  pixel.data[[15]] <- input.data[[15]]  # Environmental index
  names(pixel.data)[[15]] <- names(input.data)[[15]]
  series <- length(input.data[[15]])/2
  pixel.data[[16]] <- input.data[[16]]  # Number of bathtubs (NTUBS)
  names(pixel.data)[[16]] <- names(input.data)[[16]]
  pixel.data[[17]] <- input.data[[17]]  # Number of seasons (NSEASONS)
  names(pixel.data)[[17]] <- names(input.data)[[17]]
  pixel.data[[18]] <- input.data[[18]]  # Krill RAge
  names(pixel.data)[[18]] <- names(input.data)[[18]]
  pixel.data[[19]] <- input.data[[19]]  # Initial season (INIT.SEASON)
  names(pixel.data)[[19]] <- names(input.data)[[19]]
## Keep track of a few things for plotting later  
  pixel.data[[20]] <- nssmus            # Keep track of original number of SSMUs
  names(pixel.data)[[20]] <- "ORIG.SSMU"
  pixel.data[[21]] <- areas.list[,2]            # Keep track of original number of SSMUs
  names(pixel.data)[[21]] <- "PIXEL.LIST"

## output what has been loaded ##
  print(paste("The setup says you have ", nssmus, " SSMU(s) divided into ", tot.pixels, " PIXELS, ", ntubs, " BATHTUB(s), ",
              series, " YEAR(s) of time series data, and ", pixel.data[[17]], " SEASON(s) specified", sep=""))

  pixel.data
  
}

