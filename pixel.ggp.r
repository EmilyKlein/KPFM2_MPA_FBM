pixel.ggp <- function(GGP, areas.list){
  # This is a short code to pixelate the GGP input so it can be used with pixelated model runs
  # ESK April 2018
  #
  nssmus <- ncol(GGP)              # number of original ssmus
  pix_ggp <- NULL
  for (j in 1:nssmus){
    no.pixel <- areas.list[j,2]
    x<-matrix(0,nrow(GGP),no.pixel)
    for (k in 1:no.pixel){
      x[,k]<- GGP[,j]
    }
    pix_ggp<-cbind(pix_ggp,x)
  }
  pix_ggp
}