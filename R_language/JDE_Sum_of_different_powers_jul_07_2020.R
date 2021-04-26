library('DEoptimR')

Sum_of_different_powers= function(x)
  {
  Num=length(x)
  S=0

	for (i in 1:Num)
	  {
	  S=S+abs(x[i])**(i+1) 
		}
	fun=S
  return(fun)
  }
# f(x)=0 x=(0,0) [−1,1]


dim=30
RUNS=50
NPAR=100
ITE=2000
Bounds=1
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Sum_of_different_powers,
         tol = 1e-100,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Sum_of_different_powers JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
