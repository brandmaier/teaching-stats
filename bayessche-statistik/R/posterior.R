# posterior.R
# draw posterior + mean + HDI
x <- seq(70,130,1)

y <- dnorm(x,100,10)


df <- data.frame(x,y)

ggplot(df, aes(x=x,y=y))+geom_area(ymin=0,ymax=y)