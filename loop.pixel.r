loop.pixel <- function(ss.object, mc=TRUE){
# This code loops the fishery and appropriate predator aspects from a MPA scenario 
# back to the original SSMUs (e.g. for risk assessmentin papers)
#
# INPUT
#   ss.object = the KPFM2 object to be aggregated
#   mc = whethere this object was created via Monte Carlo trials or not (default is TRUE)
# OUTPUT
#   out.ssmu1 = KPFM2 output at the original SSMU level
#
  nssmus <- ss.object$setup$orig.SSMU
  ntubs <- ss.object$setup$ntubs
  pxl.list <- ss.object$setup$pixel.list
  out.ssmu1 <- ss.object
  no.spp <- 5  
  pxl.listB <- pxl.list[1:15]
  no.sppB <- 4  # to index predator parameters
  #
  if(mc==TRUE){
    ntrials <- ss.object$setup$ntrials
    #
    ## Loop the predator parameters back to SSMUs - these just need to be indexed not added
    for (i in 1:no.sppB){
      for(m in 1:ntrials){
        if(i==3){  # Note that whales are outside the MPA in the pelagic areas so indexed differently
          y <- 0
          for (j in 1:nssmus){
            x <- y+1      # skip to column for next SSMU
            k <- pxl.listB[j]
            if(k==1){
              out.ssmu1$Q$demand[[i]][,j,m] <- ss.object$Q$demand[[i]][,x,m]
              out.ssmu1$Q$consumption[[i]][,j,m] <- ss.object$Q$consumption[[i]][,x,m]
              y <- x
            }else{
              y <- x+k-1  # all pixel cols for that SSMU
              out.ssmu1$Q$demand[[i]][,j,m] <- ss.object$Q$demand[[i]][,x,m]
              out.ssmu1$Q$consumption[[i]][,j,m] <- ss.object$Q$consumption[[i]][,x,m]
            }
          }
        }else{
          y <- 0
          for (j in 1:nssmus){
            x <- y+1      # skip to column for next SSMU
            k <- pxl.listB[j]
            if(k==1){
              out.ssmu1$Q$demand[[i]][,j,m] <- ss.object$Q$demand[[i]][,x,m]
              out.ssmu1$Q$consumption[[i]][,j,m] <- ss.object$Q$consumption[[i]][,x,m]
              y <- x
            }else{
              y <- x+k-1  # all pixel cols for that SSMU
              out.ssmu1$Q$demand[[i]][,j,m] <- ss.object$Q$demand[[i]][,y,m]
              out.ssmu1$Q$consumption[[i]][,j,m] <- ss.object$Q$consumption[[i]][,y,m]
            }
          }
        }
      }
      out.ssmu1$Q$demand[[i]] <- out.ssmu1$Q$demand[[i]][,1:nssmus,]
      out.ssmu1$Q$consumption[[i]] <- out.ssmu1$Q$consumption[[i]][,1:nssmus,]
    }
    # Not loop over predator and krill numbers and recruitment, which need to be added across the SSMUs
    # (although in our current MPAs there will be zeros in the offshore areas)
    for (i in 1:no.spp){
      for(m in 1:ntrials){
        y <- 0
        for (j in 1:nssmus){
          x <- y+1      # skip to column for next SSMU
          k <- pxl.list[j]
          if(k==1){
            out.ssmu1$N[[i]][,j,m] <- ss.object$N[[i]][,x,m]
            out.ssmu1$R$R[[i]][,j,m] <- ss.object$R$R[[i]][,x,m]
            y <- x
          }else{
            y <- x+k-1  # all pixel cols for that SSMU
            out.ssmu1$N[[i]][,j,m] <- rowSums(ss.object$N[[i]][,x:y, m])
            out.ssmu1$R$R[[i]][,j,m] <- rowSums(ss.object$R$R[[i]][,x:y, m])
          }
        }
      }
      if(i==1){  # krill have the bathtubs!
        tot<-nssmus+ntubs
        out.ssmu1$N[[i]] <- out.ssmu1$N[[i]][,1:tot,]
        out.ssmu1$R$R[[i]] <- out.ssmu1$R$R[[i]][,1:nssmus,]
      }else{
        out.ssmu1$N[[i]] <- out.ssmu1$N[[i]][,1:nssmus,]
        out.ssmu1$R$R[[i]] <- out.ssmu1$R$R[[i]][,1:nssmus,]
      }
    }
    # Now loop in the fishery allocations, catch, and threshold violations
    for (m in 1:ntrials){
      y <- 0
      for (j in 1:nssmus){
        x <- y+1      # skip to column for next SSMU
        k <- pxl.list[j]
        if(k==1){
          out.ssmu1$fishery$allocation[,j,m] <- ss.object$fishery$allocation[,x,m]
          out.ssmu1$fishery$catch[,j,m] <- ss.object$fishery$catch[,x,m]
          out.ssmu1$fishery$threshold.violations[m,j] <- ss.object$fishery$threshold.violations[m,x]
          y <- x
        }else{
          y <-x+k-1
          out.ssmu1$fishery$allocation[,j,m] <- rowSums(ss.object$fishery$allocation[,x:y,m])
          out.ssmu1$fishery$catch[,j,m] <- rowSums(ss.object$fishery$catch[,x:y,m])
          out.ssmu1$fishery$threshold.violations[m,j] <- (ss.object$fishery$threshold.violations[m,x])
        }
      }
      out.ssmu1$fishery$allocation <- out.ssmu1$fishery$allocation[,1:nssmus,]
      out.ssmu1$fishery$catch <- out.ssmu1$fishery$catch[,1:nssmus,]
      out.ssmu1$fishery$threshold.violations <- out.ssmu1$fishery$threshold.violations[,1:nssmus]
    }
  }
  # Single-run output
  if (mc==FALSE){
    for (i in 1:no.spp){
      y <- 0
      for (j in 1:nssmus){
        x <- y+1      # skip to column for next SSMU
        k <- pxl.list[j]
        if(k==1){
          out.ssmu1$N[[i]][,j] <- ss.object$N[[i]][,x]
          out.ssmu1$R$R[[i]][,j] <- ss.object$R$R[[i]][,x]
          y <- x
        }else{
          y <- x+k-1  # all pixel cols for that SSMU
          out.ssmu1$N[[i]][,j] <- rowSums(ss.object$N[[i]][,x:y])
          out.ssmu1$R$R[[i]][,j] <- rowSums(ss.object$R$R[[i]][,x:y])
        }
      }
      if(i==1){  # Careful! This assumes the first species group is krill, and output includes btubs!
        out.ssmu1$N[[i]] <- cbind(out.ssmu1$N[[i]][,1:nssmus], ss.object$N[[i]][,(nareas-ntubs+1):nareas])
        out.ssmu1$R$R[[i]] <- out.ssmu1$R$R[[i]][,1:nssmus]
      }
      if(i>1){
        out.ssmu1$N[[i]] <- out.ssmu1$N[[i]][,1:nssmus]
        out.ssmu1$R$R[[i]] <- out.ssmu1$R$R[[i]][,1:nssmus]
      }
    }
    y <- 0
    for (j in 1:nssmus){
      x <- y+1      # skip to column for next SSMU
      k <- pxl.list[j]
      if(k==1){
        out.ssmu1$fishery$allocation[,j] <- ss.object$fishery$allocation[,x]
        out.ssmu1$fishery$catch[,j] <- ss.object$fishery$catch[,x]
        y<-x
      }else{
        y <-x+k-1
        out.ssmu1$fishery$allocation[,j] <- rowSums(ss.object$fishery$allocation[,x:y])
        out.ssmu1$fishery$catch[,j] <- rowSums(ss.object$fishery$catch[,x:y])
      }
    }
    out.ssmu1$fishery$allocation <- out.ssmu1$fishery$allocation[,1:nssmus]
    out.ssmu1$fishery$catch <- out.ssmu1$fishery$catch[,1:nssmus]
  }
  out.ssmu1$setup$nareas <- nssmus + ntubs
  out.ssmu1$setup$nssmus <- nssmus
  out.ssmu1
}