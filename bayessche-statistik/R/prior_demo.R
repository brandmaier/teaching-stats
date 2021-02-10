
prior.demo <- function(prior) {

  pos_correction <- 2  

  x<- seq(0,1,.01)
  likelihood <- dnorm(x, mean=.7,sd=.25)
  xl<-"x"
  if (prior==1) {
    x <- seq(-100,500,1)
    likelihood <- dnorm(x, mean=150,sd=100)
    prior <- ifelse(x<0,0,1) /500
    pos_correction <- 600
    xl<-"Reaktionszeit [min]"
  } else if (prior==2) {
#    prior <- dchisq(x,df=2)
    prior <- dnorm(x, mean=.2,sd=.2)
    xl <- "Wirksamkeit (Symptomreduktion)"
  } else if (prior == 3) {
    prior <- 1
  } else if (prior == 4) {
    prior <- 1
  }
  

  posterior <- prior*likelihood
  posterior <- posterior*pos_correction
  

  
  library(tidyverse)
  
  df <- data.frame(x,Prior=prior, Likelihood=likelihood, Posterior=posterior)
  
  dflong <- df %>% pivot_longer(Prior:Posterior) 
 # dflong$name <- factor(dflong$name,ordered=FALSE)
#  dflong$name <- relevel(dflong$name, "Prior")
  dflong %>% ggplot(aes(x=x,y=value,group=name))+
    geom_line()+ facet_wrap(~name) + ggthemes::theme_clean()+
    ylab("Dichte")
  
  gp <- dflong %>% ggplot(aes(x=x,y=value,group=name,fill=name))+
    geom_line()+ 
    #ggthemes::theme_clean()+
    geom_ribbon(aes(ymin=0,ymax=value),alpha=.7)+
    #ylab("Dichte")+
    ylab("")+
    xlab(xl)+
    #theme(axis.title.x=element_text(size=rel(2))) +
    #theme(axis.title.y=element_text(size=rel(2)))+
    labs(fill="Verteilung")+
    theme(text=element_text(size=25)) # increase all font sizes
  
    
  return(gp)
}

prior.demo(1)
