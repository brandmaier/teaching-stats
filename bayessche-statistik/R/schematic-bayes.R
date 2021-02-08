library(ggx)

if (!exists("prior_sd")) {
  prior_sd <- 15
}



set.seed(334)
obs <- rnorm(10, mean = 130)

x <- seq(50,160,1)

yprior <- dnorm(x, mean=100,sd=prior_sd)
likelihood <- sapply(x, function(m){ sum(dnorm(obs, mean=m, sd=10 )) })

#if (exists("nfakefac")) {
#  likelihood = likelihood*nfakefac
#}

posterior <- yprior*likelihood

#plot(x,posterior)
#plot(x,yprior)
#plot(x,likelihood)

yprior <- yprior/sum(yprior)
posterior <- posterior/sum(posterior)
likelihood <- likelihood/sum(likelihood)

df <- cbind(x,Prior=yprior, Posterior=posterior, Likelihood=likelihood)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(3)

sz <- 8

library(tidyverse)
plt <- as_tibble(df) %>% pivot_longer(-x) %>% ggplot(aes(x=x,y=value,group=name,color=name))+geom_line()+
  geom_ribbon(aes(ymax=value,ymin=0,fill=name),alpha=.7)+
  xlab("Mittlerer IQ")+ylab("Dichte")+
  gg_("hide legend")+
  gg_("increase x-axis font size to 20")+
  gg_("increase y-axis font size to 20")+
  geom_text(x=135,y=0.045, label="Posterior",col=cols[2],size=sz)+
geom_text(x=150,y=0.03, label="Likelihood",col=cols[1],size=sz)+
  geom_text(x=95,y=0.03, label="Prior",col=cols[3], size=sz)+
  theme(text=element_text(size=25))+ylab("")


plot(plt)
