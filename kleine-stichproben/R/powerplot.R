power_plot <- function(plot_alt=TRUE, mn=100) {

x <- mn+seq(-4,6,.01)
y_h0 <- dnorm(x = x, mean=mn)
y_h0_p <- ifelse( pnorm(x,mean = mn)>=0.95, y_h0,0)
y_h1 <- dnorm(x = x, mean=(mn+3))
df <- data_frame(x,y_h0,y_h1,y_h0_p)
y_h1_p <- ifelse( pnorm(x, mean=mn)>=.95, y_h1,0)
gp <- ggplot(df)+
  ggthemes::theme_clean()+ylab("Effekt")+ylab("Mutmaßlichkeit")+xlab("Effekt")+
  geom_ribbon(aes(ymin=0,ymax=y_h0_p,x=x),fill="lightblue")+
geom_line(aes(x=x,y=y_h0),lwd=2,color="darkgrey")+
  theme(axis.title=element_text(size=26),
        axis.text = element_text(size=20) ) +
  geom_vline(xintercept=qnorm(0.95,mean=mn),lty=2)


if (plot_alt) {
  gp <- gp + geom_ribbon(aes(ymin=0,ymax=y_h1_p,x=x),fill="orange",alpha=.9)+
  geom_line(aes(x=x,y=y_h1),lwd=2)
  
  NULL
}

gp <- gp +   geom_text(label="Kritischer Wert",x=qnorm(0.95,mean=mn),y=0.41,size=5,hjust=-.1)


return(gp)
}