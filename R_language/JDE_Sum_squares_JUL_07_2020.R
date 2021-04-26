library('DEoptimR')


Sum_squares= function(x)
  {
  Num=length(x)
  fun=0
  for (i in 1:Num)
  {
    fun = fun+ i *x[i]**2
   }
  return(fun)
  }
# f(x)=0 x=(0,0) , d=[-10,10]

dim=10
RUNS=50
ITE=2000
Bounds=10
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Sum_squares ,
         tol = 1e-100,NP=100, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Sum_squares JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
