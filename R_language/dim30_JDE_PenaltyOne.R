library('DEoptimR')


PenaltyOne= function(x)
{

    n = length(x); a = 10 ; k = 100 ; m =4;
    sumy=0
    sumu=0
    
    for (i in 1:(n-1))
    {
        yi = 1 + 1.0/4*(x[i]+1)
        yip = 1 + 1.0/4*(x[i+1]+1)
        sumy=sumy+(yi-1)**2*(1+10*(sin(pi*yip))**2)
        if( x[i] > a)
        {
            sumu = sumu + k*(x[i]-a)**m
        }
        else if( x[i] < -a)
        {
            sumu = sumu + k*(-x[i]-a)**m
        }
        else
        {
            sumu = sumu 
        }
     }   
    y0 = 1 + 1.0/4*(x[1]+1)
    yn = 1 + 1.0/4*(x[n-1]+1)
    fun = pi/n*(10*(sin(pi*y0))**2 + sumy + (yn-1)**2 ) + sumu 

return(fun)
}

dim=30
RUNS=50
ITE=2000
Bounds=50
Boundsn=-50
NPAR=100
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(Boundsn, dim), rep(Bounds, dim), PenaltyOne,
         tol = 1e-20,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('PenaltyOne JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')

# PenaltyOne JDE DIM= 30 RUNS= 50 ITE= 2000 Bounds -50 50 
# MEAN= 7.177638e-21 
# MAX 1.25791e-20 
# MIN= 4.254947e-21 
# STD 1.829447e-21 

# 7.177638e-21 
# 1.829447e-21 
# 4.254947e-21 

