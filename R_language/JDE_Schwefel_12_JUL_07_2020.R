library('DEoptimR')

Schwefel_12= function(x)
  {
  Num=length(x)
  sum2=0
  for (k in 1:Num)
  {
        sum1=0
        for (i in 1:k)  { sum1=sum1+x[i] }
        sum2=sum2+sum1**2
   }
  fun=sum2
  return(fun)
  }
# f(x)=0 x=(0,0) [−100, 100]

dim=30
RUNS=50
ITE=2000
Bounds=100
NPAR=100
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Schwefel_12 ,
         tol = 1e-100,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Schwefel_12 JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
