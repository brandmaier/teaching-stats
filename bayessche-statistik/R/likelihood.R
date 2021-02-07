x <- iqdat$x

dist_means <- c(80,mean(x), 120)
dist_sds <- c(10, sqrt(var(x)), 10)

lls <- c(0,0,0)

df <- data.frame(x=seq(60,140,.1))
for (i in 1:3) {
  #y1 <- dnorm(x, mean = dis)
  lls[i] <- sum(log(dnorm(iqdat$x,mean = dist_means[i], sd=dist_sds[i])))
  df[,paste0("Modell ",i)] <- dnorm(df$x, dist_means[i], dist_sds[i])
}

lls <- round(lls, 1)

gp <- df %>% pivot_longer(-1,names_to = "Modell") %>% 
  ggplot(aes(x=x,y=value,group=Modell,color=Modell,fill=Modell))+
  geom_line()+geom_ribbon(aes(ymin=0,ymax=value),alpha=.8)+
  geom_text(data=data.frame(name=paste0("Modell ",1:3),lls, x=dist_means,y=c(0.05,0.045,0.05)),
            aes(x=x,y=y,label=lls,color=name), size=10,
            inherit.aes = FALSE)+ labs(fill="Modell")+
  xlab("Beobachtungen")+theme(legend.position = "none") +
  theme(text=element_text(size=25))+ylab("Likelihood")+
  geom_point(data=iqdat,aes(y=0,x=x,fill=NULL,group=NULL),color="black",size=8)

#for ()

plot(gp)