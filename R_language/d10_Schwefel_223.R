library('DEoptimR')


Schwefel_223= function(x)
  {
  Num=length(x)
  sumx10=0


  for (i in 1:Num)
  {
   sumx10=sumx10+x[i]**10

   }
   fun=sumx10
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

# Schwefel_223 JDE DIM= 10 RUNS= 50 ITE= 2000 Bounds= 100 100 
# MEAN= 0 
# MAX 0 
# MIN= 0 
# STD 0 

# 0 
# 0 
# 0 
