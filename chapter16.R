library(dslabs)
library(tidyverse)
library(dplyr)
data("heights")
x <- heights %>% filter(sex=="Male")%>% .$height
mean(x)
sd(x)
N <- 50
X <- sample(x,N,replace = TRUE)
X
mean(X)
sd(X)


se <- sd(X)/sqrt(N)
se
X_hat <- mean(X)
z<- qnorm(1-((1-0.95)/2))
ci <- c(X_hat-z*se_hat , X_hat + z*se_hat)
ci

mu <- mean(x)
B <- 10000
res <- replicate(B,{
  X- sample(x,N,replace = TRUE)
  z<- qnorm(1-((1-0.95)/2))
  se <- sd(X)/sqrt(N)
  avg <- mean(X)
  interval <- c(avg-z*se,avg+z*se)
  between(mu,avg-z*se,avg+z*se)
})

mean(res)

Pr_AB <- (Pr_A *Pr_BA) / Pr_B

library(tidyverse)
options(digits=3)
library(dslabs)
data(brexit_polls)
p <- 0.481 # Remain supporters votes
d <- 2*p-1 #  actual spread
N <- 1500
1500*p
se <- sqrt(N*p*(1-p))
sqrt(p*(1-p)/N)
d

head(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(X_hat =(spread+1)/2)
mean(brexit_polls$spread)
se_hat <- 2*sqrt(brexit_polls$X_hat * (1-brexit_polls$X_hat)/sqrt(brexit_polls$samplesize))
sd(brexit_polls$spread)
mean(brexit_polls$X_hat)
sqrt(0.52*(1-0.52)/4772)
c(0.52-1.96*0.00723,0.52+1.96*0.00723)


brexit_polls <- brexit_polls %>%
  mutate(X_hat =(spread+1)/2)
p <- 0.481
sd(brexit_polls$X_hat)
head(brexit_polls)

june_polls <- brexit_polls%>%
  filter(enddate >= "2016-06-01")
june_polls
june_polls <- brexit_polls%>%
  filter(enddate >= "2016-06-01")
june_polls

june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(X_hat*(1-X_hat)/samplesize),
         se = 2*se_x_hat,
         lower = spread-1.96*se_x_hat,
         upper = spread+1.96*se_x_hat,
         hit = lower<= -0.038 & upper >= -0.038)

june_polls%>%group_by(pollster)%>%
  arrange(hit)%>%
  summarise(avg=mean(hit))

june_polls%>%ggplot(aes(poll_type,spread))+
  geom_boxplot()+
  geom_point()


june_polls%>%group_by(poll_type)%>%
  summarise(N= sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat= (spread+1)/2,
            lb = spread-1.96*(2*sqrt(p_hat*(1-p_hat)/N)),
            up = spread+1.96*(2*sqrt(p_hat*(1-p_hat)/N)))


brexit_hit

brexit_hit <- brexit_hit%>%group_by(poll_type,hit)%>%
  filter(poll_type %in% c("Online","Telephone", na.rm= TRUE))%>%
  summarise(hits = n())%>%
  spread(poll_type,hit)

brexit_hit

chisq_test <- brexit_hit %>%
  select(hits)%>%
  chisq.test() %>% .$p.value
chisq_test
brexit_polls %>%
  ggplot(aes(enddate,spread))+
  geom_point(aes(enddate,spread),show.legend = FALSE, color=poll_type, alpha = 0.4)+
  geom_smooth(method="loess", span=0.4)+
  geom_abline()


brexit_long<- brexit_polls %>%
  gather(vote,proportion,"remain":"undecided")%>%
  mutate(vote=factor(vote))
brexit_long%>%
  ggplot(aes(enddate,proportion,color=vote))+
  geom_smooth(method="loess", span=0.3)