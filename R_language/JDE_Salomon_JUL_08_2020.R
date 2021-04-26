library('DEoptimR')


Salomon= function(x)
  {
  Num=length(x)
  sumx2=0
  sqrtsx2=0

  for (i in 1:Num)
  {
   sumx2=sumx2+x[i]**2
   sqrtsx2=sumx2**0.5
   }
   fun=1- cos(2*pi*sqrtsx2)+(0.1 * sqrtsx2)
   return(fun)
  }
# f(x)=0 x=(0,0) , d=[-10,10]

dim=10
RUNS=50
ITE=2000
Bounds=2
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Salomon ,
         tol = 1e-100,NP=100, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Salomon JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
