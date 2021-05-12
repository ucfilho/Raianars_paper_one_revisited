library('DEoptimR')


dixonpr <- function(xx)
{
  ##########################################################################
  #
  # DIXON-PRICE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  x1 <- xx[1]
  d <- length(xx)
  term1 <- (x1-1)^2
	
  ii <- c(2:d)
  xi <- xx[2:d]
  xold <- xx[1:(d-1)]
  sum <- sum(ii * (2*xi^2 - xold)^2)
	
  y <- term1 + sum
  return(y)
}



dim=30
RUNS=50
ITE=2000
Bounds=50

NPAR=100
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), dixonpr,
         tol = 1e-20,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Dixon Price JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')


# Dixon Price JDE DIM= 30 RUNS= 50 ITE= 2000 Bounds -10 10 
# MEAN= 0.6666667 
# MAX 0.6666667 
# MIN= 0.6666667 
# STD 6.728968e-17 

# 0.6666667 
# 6.728968e-17
# 0.6666667 

