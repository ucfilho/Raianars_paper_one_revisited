library('DEoptimR')


PenaltyTwo= function(x)
{

    n = length(x); a = 10 ; k = 100 ; m =4;
    sumx=0
    sumu=0
    
    for (i in 1:(n-1))
    {
        sumx=sumx+   (x[i]-1)**2*(1+( sin(3*pi*x[i+1]))**2)
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
     fun = 0.1*( (sin(3*pi*x[1]))**2 + sumx + (x[n-1]-1)**2*(1+(sin(2*pi*x[n-1]))**2) ) + sumu 

return(fun)
}

dim=10
RUNS=50
ITE=2000
Bounds=50

NPAR=100
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), PenaltyTwo,
         tol = 1e-20,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('PenaltyTwo JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds',-Bounds,Bounds,'\n')
cat('# MEAN=',MEAN,'\n')
cat('# MAX',MAX,'\n')
cat('# MIN=',MIN,'\n')
cat('# STD',STD,'\n')


cat(MEAN,'\n')
cat(STD,'\n')
cat(MIN,'\n')

PenaltyTwo JDE DIM= 10 RUNS= 50 ITE= 2000 Bounds -50 50 
# MEAN= 2.412568e-21 
# MAX 4.500444e-21 
# MIN= 1.147559e-21 
# STD 9.602737e-22 

# 2.412568e-21 
# 9.602737e-22 
# 1.147559e-21 
