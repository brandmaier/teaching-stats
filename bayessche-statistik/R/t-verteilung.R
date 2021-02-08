library(tidyverse)

#set.seed(334)
#obs <- rnorm(20, mean = .4)
obs <- iqdat$x

mytest <- t.test(obs, mu=100)

x<-seq(-5,5,0.01)

tval <- dt(x,df = length(obs)-1)
tvalfilled <- ifelse(x>mytest$statistic,tval,0)

df <- as_tibble(data.frame(x, tval, tvalfilled))

#pivot_longer(df,2:3)
plt <- df %>% ggplot(aes(y=tval,x=x))+
  geom_line()+ geom_vline(xintercept=mytest$statistic,lty=2)+
  geom_ribbon(aes(ymax=tvalfilled,ymin=0), fill="red")+
  xlab("Statistik")+ ylab("Dichte")+
  #ggtitle("t-Verteilung")+
  geom_text(x=mytest$statistic,hjust=-.5, size=8,
            y=.3,label=paste0("t = ",round(mytest$statistic,2) ))+
 # ggthemes::theme_hc()+
  theme(text=element_text(size=25)) # increase all font sizes

plot(plt)

pval <- mytest$p.value
