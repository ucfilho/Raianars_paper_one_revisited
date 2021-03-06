library('DEoptimR')

Solomon <- function(x)
{
  n = length(x)
  sumx2=0
  sqrtsx2=0
  for(i in 1:n)
  {
    sumx2=sumx2+x[i]^2
  }
  sqrtsx2=sumx2^0.5
  y = 1- cos(2* pi*sqrtsx2)+(0.1 * sqrtsx2)
  return(y)
}

dim=30
RUNS=50
ITE=2000
Bounds=100

NPAR=100
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Solomon,
         tol = 1e-40,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Solomon JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')


# Solomon JDE DIM= 30 RUNS= 50 ITE= 2000 Bounds -100 100 
# MEAN= 0.1547932 
# MAX 0.1998733 
# MIN= 0.09987335 
# STD 0.04883797 

# 0.1547932 
# 0.04883797 
# 0.09987335 
