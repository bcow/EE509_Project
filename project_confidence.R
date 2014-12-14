library(car)
library(xtable)

v1 = c(); v2 = c();
for(i in 1:5){
  v1 = c(v1, rep(i, 6-i))
  v2 = c(v2, (i+1):6)
}


k = length(out1.df[,1])
par(mfrow=c(3,2))

ratios = as.data.frame(matrix(NA, 15, 4))
colnames(ratios) <- c("Univ w/o NA", "Multi w/o NA", "Univ, w/ NA", "Multi, w/ NA")


for(N in 1:15){
  
  xr <- range(c(DEN.1[[N]]$x1, DEN.3[[N]]$x1, DEN.2[[N]]$x1, DEN.4[[N]]$x1))
  yr <- range(c(DEN.1[[N]]$x2, DEN.3[[N]]$x2, DEN.2[[N]]$x2, DEN.4[[N]]$x2))
  plot(xr[1]-.1,xr[2], xlim=c(xr[1]-.1,xr[2]), ylim=c(yr[1]-.1,yr[2]), col="white",
       xlab = colnames(out1.df[,v1])[N],
       ylab = colnames(out1.df[,v2])[N])
  
  # ellipses 
  
  dataEllipse(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="red", center.pch="o")
  dataEllipse(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="blue", center.pch="o")
  dataEllipse(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=2, col="red", center.pch="o")
  h <- dataEllipse(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=2, col="blue", center.pch="o")
  
  # eigenvectors
  n = k
  p = 2
  M1 = cbind(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k])
  M2 = cbind(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k])
  M3 = cbind(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k])
  M4 = cbind(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k])
  
  ctr1 = colMeans(M1); S1 = cov(M1)
  ctr2 = colMeans(M2); S2 = cov(M2)
  ctr3 = colMeans(M3); S3 = cov(M3)
  ctr4 = colMeans(M4); S4 = cov(M4)
  
  for(i in 1:4){
    eval(parse(text = paste0("S = S",i)))
    eval(parse(text = paste0("ctr = ctr",i)))
    
    eigVal  <- eigen(S)$values
    eigVec  <- eigen(S)$vectors
    eigScl  <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
    xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
    yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
    if(i == 1 | i == 3){matlines(xMat, yMat, lty=1, lwd=1, col="red")}
    else{matlines(xMat, yMat, lty=1, lwd=1, col="blue")}
    
    l1 <- 2*sqrt(eigVal[1])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
    l2 <- 2*sqrt(eigVal[2])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
    ratios[N,i] <- sqrt(eigVal[1])/sqrt(eigVal[2])
  }
  
  
  # densities 
  pdfx.1 <- density(out1.df[,v1[N]])
  pdfx.2 <- density(out2.df[,v1[N]])
  pdfx.3 <- density(out3.df[,v1[N]])
  pdfx.4 <- density(out4.df[,v1[N]])
  pdfy.1 <- density(out1.df[,v2[N]])
  pdfy.2 <- density(out2.df[,v2[N]])
  pdfy.3 <- density(out3.df[,v2[N]])
  pdfy.4 <- density(out4.df[,v2[N]])
  
  par(new=TRUE)
  plot(pdfx.1$x, pdfx.1$y, col="red", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,15*max(pdfx.1$y)), type="l", lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(pdfx.2$x, pdfx.2$y, col="blue", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,10*max(pdfx.2$y)), lty=2)
  lines(pdfx.3$x, pdfx.3$y, col="red", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,10*max(pdfx.3$y)))
  lines(pdfx.4$x, pdfx.4$y, col="blue", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,10*max(pdfx.4$y)))

  par(new=TRUE)
  plot(pdfy.1$y, pdfy.1$x, col="red", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]), type="l", lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(pdfy.2$y, pdfy.2$x, col="blue",xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]), lty=2)
  lines(pdfy.3$y, pdfy.3$x, col="red", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]))
  lines(pdfy.4$y, pdfy.4$x, col="blue", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]))
  
  txt <- paste(colnames(out1.df[,v1])[N],"vs",colnames(out1.df[,v2])[N])
  rownames(ratios)[N] <- txt
  title(txt)
}

xtable(ratios)

ratios[,1] < ratios[,2]
ratios[,3] < ratios[,4]



par(mfrow = c(1,1))
N=6
xr <- range(c(DEN.1[[N]]$x1, DEN.3[[N]]$x1, DEN.2[[N]]$x1, DEN.4[[N]]$x1))
yr <- range(c(DEN.1[[N]]$x2, DEN.3[[N]]$x2, DEN.2[[N]]$x2, DEN.4[[N]]$x2))
plot(xr[1]-.1,xr[2], xlim=c(xr[1]-.1,xr[2]), ylim=c(yr[1]-.1,yr[2]), col="white",
     xlab = colnames(out1.df[,v1])[N],
     ylab = colnames(out1.df[,v2])[N])

# ellipses 

dataEllipse(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="red", center.pch="o")
dataEllipse(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="blue", center.pch="o")
dataEllipse(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=2, col="red", center.pch="o")
h <- dataEllipse(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=2, col="blue", center.pch="o")

# eigenvectors
n = k
p = 2
M1 = cbind(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k])
M2 = cbind(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k])
M3 = cbind(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k])
M4 = cbind(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k])

ctr1 = colMeans(M1); S1 = cov(M1)
ctr2 = colMeans(M2); S2 = cov(M2)
ctr3 = colMeans(M3); S3 = cov(M3)
ctr4 = colMeans(M4); S4 = cov(M4)

for(i in 1:4){
  eval(parse(text = paste0("S = S",i)))
  eval(parse(text = paste0("ctr = ctr",i)))
  
  eigVal  <- eigen(S)$values
  eigVec  <- eigen(S)$vectors
  eigScl  <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
  xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
  yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
  if(i == 1 | i == 3){matlines(xMat, yMat, lty=1, lwd=1, col="red")}
  else{matlines(xMat, yMat, lty=1, lwd=1, col="blue")}
  
  l1 <- 2*sqrt(eigVal[1])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
  l2 <- 2*sqrt(eigVal[2])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
  ratios[N,i] <- sqrt(eigVal[1])/sqrt(eigVal[2])
}


# densities 
pdfx.1 <- density(out1.df[,v1[N]])
pdfx.2 <- density(out2.df[,v1[N]])
pdfx.3 <- density(out3.df[,v1[N]])
pdfx.4 <- density(out4.df[,v1[N]])
pdfy.1 <- density(out1.df[,v2[N]])
pdfy.2 <- density(out2.df[,v2[N]])
pdfy.3 <- density(out3.df[,v2[N]])
pdfy.4 <- density(out4.df[,v2[N]])

par(new=TRUE)
plot(pdfx.1$x, pdfx.1$y, col="red", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,15*max(pdfx.1$y)), type="l", lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(pdfx.2$x, pdfx.2$y, col="blue", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,10*max(pdfx.2$y)), lty=2)
lines(pdfx.3$x, pdfx.3$y, col="red", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,10*max(pdfx.3$y)))
lines(pdfx.4$x, pdfx.4$y, col="blue", xlim=c(xr[1]-.1,xr[2]), ylim=c(0,10*max(pdfx.4$y)))

par(new=TRUE)
plot(pdfy.1$y, pdfy.1$x, col="red", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]), type="l", lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(pdfy.2$y, pdfy.2$x, col="blue",xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]), lty=2)
lines(pdfy.3$y, pdfy.3$x, col="red", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]))
lines(pdfy.4$y, pdfy.4$x, col="blue", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]))

txt <- paste(colnames(out1.df[,v1])[N],"vs",colnames(out1.df[,v2])[N])
rownames(ratios)[N] <- txt
title(txt)