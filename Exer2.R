set.seed(5361)
t = c(0.0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)
n = c(10^2, 10^3, 10^4)
approx = matrix(0,3,length(t))
for(i in 1:3)
{   
   dat = rnorm(n[i])
   for(j in 1:length(t))
      approx[i,j] = mean( dat <= t[j])
}
result = as.data.frame(rbind(approx,pnorm(t)))
row.names(result) = c('n = 100','n = 1000','n = 10000','True Value')

re = 100
approx = matrix(0,re,length(t))
RepDat = vector("list", length(n))
for(i in 1:3)
{   
   for(j in 1:re)
   {   
      dat = rnorm(n[i])
      for(k in 1:length(t))
         approx[j,k] = mean( dat <= t[k]) - pnorm(t[k])
   }   
   RepDat[[i]] = approx
}

par(mfrow = c(3,3))
for(i in 1:9)
   boxplot(RepDat[[1]][,i],RepDat[[2]][,i],RepDat[[3]][,i],names=c('n = 100','n = 1000','n = 10000'))

