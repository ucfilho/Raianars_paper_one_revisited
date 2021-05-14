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

# f(x)=0 x=(0,0) [−10, 10]
dim=30
RUNS=50
ITE=2000
NPAR=100
Bounds=100
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Schwefel_223,
         tol = 0,NP=NPAR, trace = FALSE,  maxiter =ITE)
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


Schwefel_223 JDE DIM= 30 RUNS= 50 ITE= 2000 Bounds= 10 10 
# MEAN= 7.041517e-138 
# MAX 3.520724e-136 
# MIN= 2.20799e-198 
# STD 4.979054e-137 

# 7.041517e-138 
# 4.979054e-137 
# 2.20799e-198 
