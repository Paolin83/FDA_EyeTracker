library(imputeTS)
library(fda)
fun_register<-function(data,lambda,knots,plot=FALSE,register=TRUE){
  #data<-test1
  #  lambda=2
  #knots=16
  # NOT RUN {
  #See the analyses of the growth data for examples.
  ##
  ## 1.  smooth the growth data for the Berkeley boys
  ##
  # Specify smoothing weight
    
    # Set up a B-spline basis for smoothing the discrete data
    norder <- 4
    # Specify what to smooth, namely the rate of change of curvature
    grado_pen<-  int2Lfd(max(0, norder-2))
    
    nbasis<- knots + norder - 2
    rng<- range(data$t,na.rm=T)
    wbasis <- create.bspline.basis(rangeval=rng,nbasis=nbasis, norder=norder)
    cvec0 <- matrix(0,nbasis,0)
    Wfd0 <- fd(cvec0, wbasis)
    growfdPar2_3 <- fdPar(Wfd0,  grado_pen,lambda)
    datafd_all   <- with(data, smooth.basis(t, t(x_mat), growfdPar2_3)$fd)
    gcv<-sum(with(data, smooth.basis(t, t(x_mat), growfdPar2_3)$gcv))
    data_reg<-datafd_all
  if(register==TRUE){
  # Register the curves directly (set 1 for the first deratives)
  
  data_pre_reg <- deriv(datafd_all, 0)
  #  Define the target function as the mean of the first nBoys records
  mean_all = mean.fd(data_pre_reg)

  data_reg <- register.fd(mean_all, data_pre_reg)
  }
  #  plot each curve.  Click on the R Graphics window to show each plot.
  #  The left panel contains:
  #    -- the unregistered curve (dashed blue line)
  #    -- the target function (dashed red line)
  #    -- the registered curve (solid blue line)
  #  The right panel contains:
  #    -- the warping function h(t)
  #    -- the linear function corresponding to no warping
  #plotreg.fd(smB.reg.0)
  if (plot==TRUE) plot(data_reg$regfd)

list(data_reg=data_reg,gcv=gcv,data_no_reg=datafd_all)  
}
  
  
sampling_regular<-function(x,id,t,n,ma=TRUE,window,scale=TRUE){
  #x<-db_tf1$p_s
  #id<-db_tf1$id
  #t<-db_tf1$time
  #n<-100
  m<-length(levels(id))
  t_new<-seq(window[1],window[2],len=n)
  step<-diff(t_new)[1]/2
  x_mat<-matrix(NA,m,n)
  for(i in 1:m){
    for(j in 1:n){ 
      x_mat[i,j]<-mean(x[t>=(t_new[j]-step) & t <=(t_new[j]+step) & id %in% levels(id)[i] ],na.rm=T)}}
  x_mat<-t(na_ma(t(x_mat)))
  if(scale) x_mat<-t(scale(t(x_mat)))
  list(x_mat=x_mat,t=t_new)
}

plot_cluster_curves<-function(data.fd,cluster){
fd.curves<-NULL
for(i in 1:max(cluster)){
fd.curves[[i]]<-mean.fd(data.fd$regfd[cluster==i])
}
plot(fd.curves[[1]],ylim=c(-2.5,2.5),lwd=2)
for(i in 1:max(cluster)){
plot(fd.curves[[i]],add=T,col=i,lwd=2)
}
}

plot_cluster_curves2<-function(data.fd,cluster,lwd=1){
  fd.curves<-NULL
  for(i in 1:max(cluster)){
    fd.curves[[i]]<-mean.fd(data.fd[cluster==i])
  }
  plot(fd.curves[[1]],ylim=c(-2.5,2.5),col=i,lwd=lwd)
  for(i in 1:max(cluster)){
    plot(fd.curves[[i]],add=T,col=i,lwd=lwd)
  }
}