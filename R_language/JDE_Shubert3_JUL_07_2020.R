library('DEoptimR')

Shubert3= function(x)
  {
  Num=length(x)
  fun=0
  for (i in 1:Num)
   { 
   for (j in 1:5) {  fun = fun+   j * sin(((j + 1) * x[ i]) + j) }
   }

  return(fun)
  }
# Global Minimum: 0 , domain=[-10,10]

dim=30
RUNS=50
ITE=2000
NPAR=100
Bounds=10
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Shubert3 ,
         tol = 1e-100,NP=100, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Shubert3 JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
