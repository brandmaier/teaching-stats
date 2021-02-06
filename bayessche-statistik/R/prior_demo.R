
prior.demo <- function(prior) {
  
  x<- seq(0,1,.01)
  xl<-"x"
  if (prior==1) {
    prior <- ifelse(x<.5,0,1)*2
    xl<-"Ratewahrscheinlichkeit"
  } else if (prior==2) {
    prior <- dnorm(x,mean=.2,sd=.1)^2
  } else if (prior == 3) {
    prior <- 1
  }
  
  likelihood <- dnorm(x, mean=.5,sd=.1)
  
  posterior <- prior*likelihood
  
  library(tidyverse)
  
  df <- data.frame(x,Prior=prior, Likelihood=likelihood, Posterior=posterior)
  
  dflong <- df %>% pivot_longer(Prior:Posterior) 
  dflong$name <- factor(dflong$name,ordered=FALSE)
  dflong$name <- relevel(dflong$name, "Prior")
  dflong %>% ggplot(aes(x=x,y=value,group=name))+
    geom_line()+ facet_wrap(~name) + ggthemes::theme_clean()+
    ylab("Dichte")
  
  dflong %>% ggplot(aes(x=x,y=value,group=name,fill=name))+
    geom_line()+ ggthemes::theme_clean()+
    geom_ribbon(aes(ymin=0,ymax=value),alpha=.7)+
    ylab("Dichte")+
    xlab(xl)+
    theme(axis.title.x=element_text(size=rel(2))) +
    theme(axis.title.y=element_text(size=rel(2)))+
    labs(fill="Verteilung")
    
  
}

prior.demo(2)
