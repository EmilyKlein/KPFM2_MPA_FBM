get.gamma.fractions <- function(fishing.option) {
  
  # to run a range of possible fishing levels, use the below script but note this can take some time
	# gamma.fractions <- c(0.02, 0.04, 0.06, 0.08, 0.1, 0.11, seq(from=0.12,to=.22, by=.02), seq(from=.3,to=1.2,by=.1)) 
  # 
  # to just run the TAC as in Klein and Watters papers use this one
  gamma.fractions <- 1.0

	return(gamma.fractions)
}
