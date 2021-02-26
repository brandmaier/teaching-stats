ballsplot <- function(num.marked, num.visible=100, title="") {
  n.rows <- 10
  n.cols <- 10
  df <- data.frame(x=rep(1:n.cols,n.rows),y=rep(1:n.rows,each=n.cols))
  df$z <- factor(c(rep(1,n.rows*n.cols-num.marked),rep(0,num.marked)))
  if (num.visible < 100) {
    #  dfsub<- df[-((num.visible+1):100),]
    dfsub <- df[-(1:(100-num.visible)),]
  } else {
    dfsub <- df
  }
  ggplot(data=df, aes(x=x,y=y))+
    geom_point(size=10,color="white")+
    geom_point(size=10,data=dfsub,aes(color=z))+
    #cowplot::theme_nothing()+
    theme(legend.position = "none") +
    theme(axis.text = element_blank())+
    theme(panel.background = element_rect(fill = "white"))+
    theme(axis.title = element_blank())+
    theme(axis.line = element_blank())+
    theme(axis.ticks = element_blank())
  #ggtitle(title)
  
}


compute.ppv <- function(power, alpha, r) {
  power*r/(power*r+alpha*(1-r))
}

plot_withinbetween <- function(
  with.between=TRUE, 
  with.within=TRUE) {
  
  set.seed(234049)
  # simulate within and between person data
  between.cor <- .7
  within.cor <- -.7
  n.between <- 5
  n.within <- 20
  between.cov <- between.cor*sqrt(15)
  person.means <- MASS::mvrnorm(n=n.between, mu=c(100,3), Sigma=matrix(c(15,between.cov,between.cov,1),nrow=2))
  #person.means <- matrix(c(
  #    100,1.7,
  #    100, 2.7,
  #    
  #), ncol=5, byrow=TRUE)
  
  alldat <- matrix(NA, nrow=n.between*n.within,ncol=3)
  
  betdat <- data.frame(ID=1:n.between,person.means)
  colnames(betdat)<-c("ID","IQ","Alkohol")
  betdat$ID<-factor(betdat$ID)
  
  for (i in 1:n.between) {
    idx <- ((i-1)*n.within+1):(i*n.within)
    alldat[idx,2:3]<-
      matrix(person.means[i,],nrow=n.within,ncol=2,byrow=TRUE)+
      MASS::mvrnorm(n=n.within, mu=c(0,0), Sigma=matrix(c(1,within.cor,within.cor,1),nrow=2))
    alldat[idx,1]<-i
  }
  
  colnames(alldat)<- c("ID","IQ","Alkohol")
  alldat<-data.frame(alldat)
  alldat$ID<-factor(alldat$ID)
  
  gp <- ggplot(alldat, aes(y=IQ,x=Alkohol, group=ID))+geom_point(aes(color=ID))+
    geom_smooth(aes(color=ID),method="lm",se=FALSE)+ xlab("Alkohol")+ ylab("IQ")
  
  if (with.between) {
    gp<-gp+  geom_smooth(data=betdat,aes(group=NULL), method="lm",se=FALSE, color="black")+
      geom_point(data=betdat,aes(color=ID,x=Alkohol,y=IQ),size=5,color="black")+
      geom_point(data=betdat,aes(color=ID,x=Alkohol,y=IQ),size=4)
  }
  
  gp <- gp + theme_light()
  
  return(gp)
  
}