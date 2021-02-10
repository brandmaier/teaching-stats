# posterior.R
# draw posterior + mean + HDI
x <- seq(60,140,1)

y <- dnorm(x, 106 ,10)

q1 <- qnorm(0.025, 106,10)
q2 <- qnorm(0.975, 106,10)
y2 <- ifelse(x>q1 & x<q2,y,0)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(3)

df <- data.frame(x,y,y2)

gp <- ggplot(df, aes(x=x,y=y))+geom_area(fill=cols[2])+
  #  geom_area(alpha=.2)+ 
  
#  geom_area(aes(y=y2))+
geom_vline(xintercept = 106,lwd=1.2,lty=2)+
  geom_errorbar(aes(xmin=q1,xmax=q2,y=0.006,width=0.005))+
  xlab("Mittlerer IQ")+
#  theme(axis.title.x=element_text(size=rel(2))) 
  theme(text=element_text(size=25))+ylab("")
NULL
plot(gp)