library(KernSmooth)
library(MASS)
library(car)

v1 = c(); v2 = c();
for(i in 1:5){
  v1 = c(v1, rep(i, 6-i))
  v2 = c(v2, (i+1):6)
}

par(mfcol = c(1,1))
k = length(out1.df[,1])

DEN.1 <- list()
for(N in 1:length(v1)){
  x <- out1.df[,v1[N]][1:k]
  y <- out1.df[,v2[N]][1:k]
  den <- bkde2D(cbind(x,y), bandwidth = c(bandwidth.nrd(x),bandwidth.nrd(y)))
  DEN.1 <- append(DEN.1, list(den))
}
DEN.2 <- list()
for(N in 1:length(v1)){
  x <- out2.df[,v1[N]][1:k]
  y <- out2.df[,v2[N]][1:k]
  den <- bkde2D(cbind(x,y), bandwidth = c(bandwidth.nrd(x),bandwidth.nrd(y)))
  DEN.2 <- append(DEN.2, list(den))
}
DEN.3 <- list()
for(N in 1:length(v1)){
  x <- out3.df[,v1[N]][1:k]
  y <- out3.df[,v2[N]][1:k]
  den <- bkde2D(cbind(x,y), bandwidth = c(bandwidth.nrd(x),bandwidth.nrd(y)))
  DEN.3 <- append(DEN.3, list(den))
}
DEN.4 <- list()
for(N in 1:length(v1)){
  x <- out4.df[,v1[N]][1:k]
  y <- out4.df[,v2[N]][1:k]
  den <- bkde2D(cbind(x,y), bandwidth = c(bandwidth.nrd(x),bandwidth.nrd(y)))
  DEN.4 <- append(DEN.4, list(den))
}

par(mfrow=c(2,2))
for(N in 1:15){
  z.1 <- DEN.1[[N]]$fhat; CI.1 <- quantile(z.1,c(.5)) 
  z.2 <- DEN.2[[N]]$fhat; CI.2 <- quantile(z.2,c(.5)) 
  z.3 <- DEN.3[[N]]$fhat; CI.3 <- quantile(z.3,c(.5)) 
  z.4 <- DEN.4[[N]]$fhat; CI.4 <- quantile(z.4,c(.5)) 
  
  xr <- range(c(DEN.1[[N]]$x1, DEN.3[[N]]$x1, DEN.2[[N]]$x1, DEN.4[[N]]$x1))
  yr <- range(c(DEN.1[[N]]$x2, DEN.3[[N]]$x2, DEN.2[[N]]$x2, DEN.4[[N]]$x2))
  
  pdfx.1 <- density(out1.df[,v1[N]])
  pdfx.2 <- density(out2.df[,v1[N]])
  pdfx.3 <- density(out3.df[,v1[N]])
  pdfx.4 <- density(out4.df[,v1[N]])
  pdfy.1 <- density(out1.df[,v2[N]])
  pdfy.2 <- density(out2.df[,v2[N]])
  pdfy.3 <- density(out3.df[,v2[N]])
  pdfy.4 <- density(out4.df[,v2[N]])
  
  contour(DEN.1[[N]]$x1,DEN.1[[N]]$x2,DEN.1[[N]]$fhat,levels=CI.1, labels=c(""), xlim=c(xr[1]-.1,xr[2]), ylim=c(yr[1]-.1,yr[2]), lty=2, lwd=2, col="white")
  contour(DEN.2[[N]]$x1,DEN.2[[N]]$x2,DEN.2[[N]]$fhat,levels=CI.3, labels=c(""), add=TRUE, lty=2, lwd=2, col="blue")
  contour(DEN.3[[N]]$x1,DEN.3[[N]]$x2,DEN.3[[N]]$fhat,levels=CI.2, labels=c(""), add=TRUE, lty=1, lwd=2, col="red")
  contour(DEN.4[[N]]$x1,DEN.4[[N]]$x2,DEN.4[[N]]$fhat,levels=CI.4, labels=c(""), add=TRUE, lty=1, lwd=2, col="blue")
  
  dataEllipse(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="red", center.pch="o")
  dataEllipse(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="blue", center.pch="o")
  dataEllipse(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=2, col="red", center.pch="o")
  dataEllipse(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=2, col="blue", center.pch="o")
  
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
  
  
  title(paste(colnames(out1.df[,v1])[N],"vs",colnames(out1.df[,v2])[N]))
}




par(mfrow=c(2,2))
for(N in 1:15){
  
  xr <- range(c(DEN.1[[N]]$x1, DEN.3[[N]]$x1, DEN.2[[N]]$x1, DEN.4[[N]]$x1))
  yr <- range(c(DEN.1[[N]]$x2, DEN.3[[N]]$x2, DEN.2[[N]]$x2, DEN.4[[N]]$x2))
  plot(xr[1]-.1,xr[2], xlim=c(xr[1]-.1,xr[2]), ylim=c(yr[1]-.1,yr[2]), lty=2, lwd=2, col="white")
  
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
    matlines(xMat, yMat, lty=1, lwd=2, col="green")
    l1 <- 2*sqrt(eigVal[1])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
    l2 <- 2*sqrt(eigVal[2])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
    print(sqrt(eigVal[1])/sqrt(eigVal[2]))  
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
  #   abline(v = quantile(out1.df[,v1[N]],c(.05,.5,.95)), lty=2, lwd=1, col="red")
  #   abline(v = quantile(out2.df[,v1[N]],c(.05,.95)), lty=2, lwd=1, col="blue")
  #   abline(v = quantile(out3.df[,v1[N]],c(.05,.95)), lty=1, lwd=1, col="red")
  #   abline(v = quantile(out4.df[,v1[N]],c(.05,.95)), lty=1, lwd=1, col="blue")
  
  par(new=TRUE)
  plot(pdfy.1$y, pdfy.1$x, col="red", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]), type="l", lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(pdfy.2$y, pdfy.2$x, col="blue",xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]), lty=2)
  lines(pdfy.3$y, pdfy.3$x, col="red", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]))
  lines(pdfy.4$y, pdfy.4$x, col="blue", xlim=c(0,xr[2]*300), ylim=c(yr[1]-.1,yr[2]))
  #   abline(h = quantile(out1.df[,v2[N]],c(.05,.5,.95)), lty=2, lwd=1, col="red")
  #   abline(h = quantile(out2.df[,v2[N]],c(.05,.95)), lty=2, lwd=1, col="blue")
  #   abline(h = quantile(out3.df[,v2[N]],c(.05,.95)), lty=1, lwd=1, col="red")
  #   abline(h = quantile(out4.df[,v2[N]],c(.05,.95)), lty=1, lwd=1, col="blue")
    
  title(paste(colnames(out1.df[,v1])[N],"vs",colnames(out1.df[,v2])[N]))
}



# plot(xr[1]-.1,xr[2], xlim=c(xr[1]-.1,xr[2]), ylim=c(yr[1]-.1,yr[2]), lty=2, lwd=2, col="white")
# dataEllipse(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="red", center.pch="o")



# eigVal  <- eigen(A)$values
# eigVec  <- eigen(A)$vectors
# eigScl  <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
# xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
# yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
# ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) # normal ellipse
# ellRot  <- eigVec %*% t(ellBase)                                          # rotated ellipse
# #plot((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, type="l", lwd=2)
# # matlines(xMat, yMat, lty=1, lwd=2, col="green")
# # points(ctr[1], ctr[2], pch=4, col="red", lwd=3)
# 
# plot(xr[1]-.1,xr[2], xlim=c(xr[1]-.1,xr[2]), ylim=c(yr[1]-.1,yr[2]), lty=2, lwd=2, col="white")
# dataEllipse(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=2, lwd=2, col="red", center.pch="o")
# lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, lwd=1)
# matlines(xMat, yMat, lty=1, lwd=2, col="green")
# points(ctr[1], ctr[2], pch=4, col="red", lwd=3)
