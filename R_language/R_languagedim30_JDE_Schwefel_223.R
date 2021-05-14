library('DEoptimR')
Schwefel_223= function(x)
  {
  Num=length(x)
  sumx=0
	for (i in 1:Num)
	{
         sumx=sumx+x[i]^10
	}
	fun=sumx
  return(fun)
  }
# f(x)=0 x=(0,0) [−10, 10]
dim=30
RUNS=50
ITE=2000
NPAR=100
Bounds=10
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Schwefel_223,
         tol = 1e-100,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  cat('i =',i,'fobj =',Y[i],'\n')
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Schwefel_223 JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')


# Schwefel_223 JDE DIM= 30 RUNS= 50 ITE= 2000 Bounds= 10 10 
# MEAN= 1.21958e-101 
# MAX 3.511928e-101 
# MIN= 1.020002e-102 
# STD 8.899292e-102 


# Schwefel_223 JDE DIM= 30 RUNS= 50 ITE= 2000 Bounds= 10 10 
# 1.21958e-101 
# 8.899292e-102 
# 1.020002e-102 
