# Looking at Sigma
if(run){
  MultModel = "
  model{
  prec.Sigma~dwish(Vsig[,],n)
  Sigma[1:n,1:n] <- inverse(prec.Sigma[,])
  
  mu[1:n]~dmnorm(mu0[],Vmu)
  
  for(i in 1:N){
  Y[i,1:n]~dmnorm(mu[],prec.Sigma[,])
  for(j in 1:n){
  X[i,j]~dnorm(Y[i,j],10000000)
  }
  
  }
}"

  # Without na's 
  j.data <- gdata
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  init = NULL
  j.model   <- jags.model (file = textConnection(MultModel),data = data,inits = init,n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("Sigma"),n.iter = 10000)
  s.ena = j.out
  
  
  # With na's 
  
  j.data <- gdata.na
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(X=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  init = NULL
  j.model   <- jags.model (file = textConnection(MultModel), data = data, inits = init, n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model, variable.names= c("Sigma"), n.iter = 10000)
  s.ina = j.out 
}

save(s.ena, s.ina, file = "Sigma.Rdata")

s.ena.df <- as.data.frame(as.matrix(s.ena))
s.ina.df <- as.data.frame(as.matrix(s.ina))

par(mfrow = c(6,6))
Sigma.ena <- as.data.frame(matrix(NA,6,6))
for(i in 1:6){
  for(j in 1:6){
    idx <- which(colnames(s.ena.df)==paste0("Sigma[",i,",",j,"]"))
    Sigma.ena[i,j] <- mean(s.ena.df[,idx])
    plot(density(abs(s.ena.df[,idx])),main=colnames(s.ena.df)[idx], xlim=c(abs(var(gdata)[i,j])-.01, max(abs(s.ena.df[,idx]))))
    abline(v=abs(Sigma.ena[i,j]),col=3, lwd=2)
    abline(v=abs(var(gdata)[i,j]), lty=2, col=2, lwd=2)
    colnames(Sigma.ena)<- rownames(Sigma.ena) <- c("Log.LL","Log.LMA","Log.Amass","Log.Nmass","Log.Pmass","Log.Rmass")
  }
}
xtable(Sigma.ena, digits=4)
xtable(var(gdata), digits=4)
print(abs(Sigma.ena) >= abs(var(gdata)))

par(mfrow = c(6,6))
Sigma.ina <- as.data.frame(matrix(NA,6,6))
for(i in 1:6){
  for(j in 1:6){
    idx <- which(colnames(s.ina.df)==paste0("Sigma[",i,",",j,"]"))
    Sigma.ina[i,j] <- mean(s.ina.df[,idx])
    plot(density(abs(s.ina.df[,idx])),main=colnames(s.ina.df)[idx], xlim=c(abs(var(gdata)[i,j])-.01, max(abs(s.ina.df[,idx]))))
    abline(v=abs(Sigma.ina[i,j]),col=3, lwd=2)
    abline(v=abs(var(gdata)[i,j]), lty=2, col=2, lwd=2)
    colnames(Sigma.ina)<- rownames(Sigma.ina) <- c("Log.LL","Log.LMA","Log.Amass","Log.Nmass","Log.Pmass","Log.Rmass")
  }
}
xtable(Sigma.ina, digits=4)

print(abs(Sigma.ina) >= abs(var(gdata)))

print(abs(Sigma.ina) >= abs(Sigma.ena))






# #######################
# ## I have no idea what this is 
# 
# pairs(out4.df[,37:42], panel=function(x,y){
#   points(x,y)
#   fit <- lm(y~x)
#   p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
#   if(p < .01){abline(fit, col='red',lwd=2)}
#   legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
# })
# 
# pairs(gdata, panel=function(x,y){
# #  points(x,y)
# #   fit <- lm(y~x)
# #   p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
# #   if(p < .01){abline(fit, col='red',lwd=2)}
# #   par(new=TRUE)
#   den <- (kde2d(x, y, n = length(x)))
#   z <- den$z
#   CI <- quantile(z,c(.5,.95)) 
#   contour(den, col = "red", levels=CI ,add = TRUE) 
#   #legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
# })
# 
# pairs(out4.df[,37:42], panel=function(x,y){
#   # points(x,y)
#   #   fit <- lm(y~x)
#   #   p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
#   #   if(p < .01){abline(fit, col='red',lwd=2)}
#   #   par(new=TRUE)
#   den <- (kde2d(x, y, n = length(x)))
#   z <- den$z
#   CI <- quantile(z,c(.5,.95)) 
#   contour(den, col = "red", levels=CI ,add = TRUE) 
#   #legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
# })
# print((cov2cor(as.matrix(Sigma)))^2)
# print((cor(gdata))^2)
# print(abs((cov2cor(as.matrix(Sigma)))^2) >= abs((cor(gdata))^2))
