library('DEoptimR')

HappyCat = function(x)
  {
   Alpha=1. / 8
   Num=length(x)
   s=0
   for( i in 1:Num){s=s+x[i]*x[i]}
   fun=((s - length(x))**2)**Alpha +   
      (s / 2 + sum(x)) / length(x) + 0.5 
   return(fun )
    
  }


dim=30
RUNS=50
ITE=2000
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-32, dim), rep(32, dim), Ackley,
         tol = 1e-20,NP=100, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Happy Cat JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
