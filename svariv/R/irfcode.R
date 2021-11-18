#' The code used to get and print irfs
#'
#' The following code is to obtain irfs for monthly data
#' p=1 lags
#'
data_v = cbind(y, x, iv, c1, c2, c3, c4)
data.df = as.data.frame(data_v)
data.df$year = (1:nrow(data.df) - 1) %/% 12

irfcomp = function(mylist,ind){

  mydf = do.call(rbind,mylist[ind])
  var.model <- VAR(mydf[, c("y", "x", "c1", "c2", "c3", "c4")], p = 1, type = "const")
  shock <- svariv(var.model, mydf$iv, "x")

  MA <- Phi(var.model, 12)
  irfs <- t(apply(MA, 3, function(x) x %*% shock))
  colnames(irfs) <- names(shock)
  irfs
  return(irfs)
}
irfs1 = irfcomp(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),irfcomp,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)

plot(response.time, irfs1[,1], xlim=c(1, 12), ylim=c(-1.0, 1.0), xlab = "", ylab = "Response", main = "", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs1[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()
