library(ggplot2)

#prior_mu<- 0
#prior_sd <- 2
#data_mu <- 2
#data_sd <- 1
#n <- 1

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

bayes_update <- function(prior_mu, prior_sd, data_mu, data_sd, n) {
  varx <- data_sd^2
  
  a <- 1/(prior_sd^2)
  b <- n/varx
  
  post_mu <- (a*prior_mu+b*data_mu)/(a+b)
  post_sd <- sqrt(1/(a+b))
  
#  post_mu <- (varx*prior_mu+n*prior_sd^2*data_mu)/(n*prior_sd^2+data_sd^2)
#  post_sd <- (varx*prior_sd^2)/(n*prior_sd^2+varx)
  
  return(c(post_mu,post_sd))
}

bayes_plot<-function(prior_mu=0, prior_sd=1, 
                     data_mu=2, data_sd=1, 
                     posterior_mu=NULL, posterior_sd=NULL,
                     n=NULL, xlim=NULL) {
  
  # if (is.null(posterior_mu)) {
  #    posterior_sd <- 1/(1/prior_sd+1/(data_sd/n))
  #    posterior_mu <- posterior_sd^2*( prior_mu/prior_sd^2+ data_mu/(data_sd^2/n) )
  #  }
  if (is.null(xlim)) {
    xmin <- min(prior_mu-prior_sd*2, data_mu-data_sd*2, posterior_mu-posterior_sd*2)
    xmax <- max(prior_mu+prior_sd*2, data_mu+data_sd*2, posterior_mu+posterior_sd*2)
    xlim<-c(xmin,xmax)
  }
  
  library(tidyverse)
  x <- seq(xlim[1],xlim[2],0.1)
  prior <- dnorm(x, prior_mu, prior_sd)
  likelihood <- dnorm(x, data_mu, data_sd)
  posterior <- dnorm(x, posterior_mu, posterior_sd)
  df <- data.frame(x, `a Priori`=prior, Likelihood=likelihood, Posterior=posterior) %>% tibble
  dflong <- df %>% tidyr::pivot_longer(a.Priori:Posterior)
  
  #dflong <- dflong %>% filter(name=="posterior")
  
  gp<-ggplot(data=dflong, mapping=aes(fill=name,x=x,ymin=0,ymax=value,group=name))+
    geom_ribbon(alpha=.5,color="black")+
    ggthemes::theme_clean()+
    ggplot2::xlim(xlim[1],xlim[2])
  (gp)
  return(gp)
}


bayes_plot_deprecated<-function(prior_mu=0, prior_sd=1, 
                     data_mu=2, data_sd=1, 
                     posterior_mu=NULL, posterior_sd=NULL,
                     n=NULL, xlim=NULL) {
  
 # if (is.null(posterior_mu)) {
#    posterior_sd <- 1/(1/prior_sd+1/(data_sd/n))
#    posterior_mu <- posterior_sd^2*( prior_mu/prior_sd^2+ data_mu/(data_sd^2/n) )
#  }
  
  huron <- data.frame(xx=1:100)
  
  gp<-ggplot(data=huron, mapping=aes(fill=xx))+
    geom_area(stat = "function", 
              fun = function(x){dnorm(x,mean=prior_mu,sd=prior_sd)}, 
              fill = "grey80", col="black", xlim = c(-100, 100), alpha=.8, aes(fill="black")) +
    
    geom_area(stat = "function", fun = function(x){dnorm(x,mean=data_mu,sd=data_sd)},
              fill = "#00998a", col="black",xlim=c(-100,100), alpha=.8, aes(fill="red"))+
    
    geom_area(stat = "function", fun = function(x){dnorm(x,mean=posterior_mu,
                                                         sd=posterior_sd)},
              fill = "#99BB8a", col="black",xlim=c(-100,100), alpha=.8)+
    
    geom_vline(xintercept=posterior_mu,lty=2)+
    
    ggthemes::theme_clean()+
    xlab("Parameter")+
    ylab("")+
    #labs(fill="HI")+
    scale_fill_manual(values=c("grey80","#00998a","#99BB8a"),
                       labels=c("A Priori","Likelihood","A Posteriori"))
  #  scale_fill_identity(guide = "legend")
  #  guide_legend()
  #NULL
  
  if (!is.null(xlim)) {
    gp <- gp + ggplot2::xlim(xlim)
  }
  
  return(gp)
}

#bayes_plot(prior_mu=0, prior_sd=1,
#           data_mu=2,data_sd=1, n=300)

