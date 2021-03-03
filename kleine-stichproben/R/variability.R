variabilityplot <- function() {
set.seed(132983)
simt<-function(n) {
iq <- rnorm(n, 100, 15)
tt<-t.test(iq, mu=100)
return(c(mean=tt$estimate, tt$conf.int))
}
numeach <- 10
ns <- c(rep(10,numeach),rep(500,numeach))
result <- data.frame(1:(2*numeach),ns,t(sapply(ns, simt)))
names(result) <- c("id","N","mean","ci_low","ci_high")
result$N <- factor(result$N)
result$pred_ci_low <- 100 - 15/sqrt(ns)
result$pred_ci_high <- 100 + 15/sqrt(ns)
ggplot(result)+  
  geom_ribbon(aes(x=id,ymin=pred_ci_low, ymax=pred_ci_high), fill="#BBBBBB")+
  geom_hline(yintercept = 100,lty=2)+geom_bar(stat="identity",
                        alpha=.75,
                        aes(x=id,y=mean,fill=N),inherit.aes = FALSE, position=position_dodge())+
#  geom_errorbar(aes(ymin=ci_low,ymax=ci_high,x=id))+theme_light()+
  
  theme_light()+xlab("")+ylab("Mittlerer IQ")+
  theme(legend.text = element_text(size=26))+
  coord_cartesian(ylim=c(90,110))+theme(axis.title=element_text(size=26),
                                        axis.text = element_text(size=20) )+
  #scale_y_continuous(limits = c(90,110))+
NULL
}
