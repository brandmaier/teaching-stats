# posterior.R
# draw posterior + mean + HDI
x <- seq(60,140,1)

y <- dnorm(x,100,10)

q1 <- qnorm(0.025, 100,10)
q2 <- qnorm(0.975, 100,10)
y2 <- ifelse(x>q1 & x<q2,y,0)

df <- data.frame(x,y,y2)

gp <- ggplot(df, aes(x=x,y=y))+geom_area(ymin=0,ymax=y,fill=.7)+
    geom_area()+ 
  geom_area(aes(y=y2),fill="blue")+
geom_vline(xintercept = 100,lwd=2,lty=2)+
NULL
plot(gp)