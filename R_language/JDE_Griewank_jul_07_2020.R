---
title: "R Notebook"
output: html_notebook
---
```{r}
library('DEoptimR')

Griewank= function(x)
  {
  Num=length(x)
  prod_cosx_i05=1
  sumx1=0
  sumx2=0

	for (i in 1:Num)
	  {
	  sumx1=sumx1+x[i]
	  sumx2=sumx2+x[i]*x[i]
	  prod_cosx_i05=prod_cosx_i05*cos(x[i]/(i**0.5))
		}
	fun=1+sumx2/4000 - prod_cosx_i05
  return(fun)
  }
# f(x)=0 x=(0,0) [−600,600]


dim=30
RUNS=50
ITE=2000
Bounds=600
NPAR=100
Y=0;X=0

for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), Griewank,
         tol = 1e-20,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value

  }



MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Griewank JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')


```






