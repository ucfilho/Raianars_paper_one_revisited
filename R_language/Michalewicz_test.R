library('DEoptimR')


Michalewicz  <- function(xx, m=10)
{
  ##########################################################################
  #
  # MICHALEWICZ FUNCTION
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
  # INPUTS:
  #
  # xx = c(x1, x2)
  # m = constant (optional), with default value 10
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(sin(xx) * (sin(ii*xx^2/pi))^(2*m))
	
  y <- -sum
  return(y)
}

dim=30
RUNS=3
ITE=5000
Particles = 200
Bounds_low=0
Bounds_up=pi
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(Bounds_low, dim), rep(Bounds_up, dim), Michalewicz ,
         tol = 1e-20,NP=Particles, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Michalewicz JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',Bounds_low,Bounds_up,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
