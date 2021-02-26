power_plot <- function(plot_alt=TRUE) {

x <- seq(-4,6,.1)
y_h0 <- dnorm(x = x)
y_h0_p <- ifelse( pnorm(x)>0.95, y_h0,0)
y_h1 <- dnorm(x = x, mean=3)
df <- data_frame(x,y_h0,y_h1,y_h0_p)
y_h1_p <- ifelse( pnorm(x)>.95, y_h1,0)
gp <- ggplot(df)+
  ggthemes::theme_clean()+ylab("Effekt")+ylab("Mutmaßlichkeit")+xlab("Effekt")+
  geom_ribbon(aes(ymin=0,ymax=y_h0_p,x=x),fill="lightblue")+
geom_line(aes(x=x,y=y_h0),lwd=2,color="darkgrey")+
  theme(axis.title=element_text(size=26),
        axis.text = element_text(size=20) ) 


if (plot_alt) {
  gp <- gp + geom_ribbon(aes(ymin=0,ymax=y_h1_p,x=x),fill="orange")+
  geom_line(aes(x=x,y=y_h1),lwd=2)
  
  NULL
}

return(gp)
}