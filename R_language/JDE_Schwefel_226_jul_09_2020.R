library('DEoptimR')
Schwefel_226= function(x)
  {
  Num=length(x)
	Sum = 0.0

	for (i in 1:Num)
	  {
		Sum =Sum+ x[i]*sin((abs(x[i]))**0.5)
		}
	fun=418.982887272433799807913601398*Num-Sum
  return(fun)
  }
  
# # f(xi)= 0 for xi = 420.968746 for i=1,...,n  ;  xi in [-500,500] 

ITE=2000
dim=30
RUNS=50
Y=0;X=0
NPAR=100
Bounds=500

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Schwefel_226,
         tol = 1e-200,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Schwefel_226 JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds, Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
