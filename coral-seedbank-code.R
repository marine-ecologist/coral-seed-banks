rm(list=ls())
set.seed(69)

library(ggplot2)
library(truncnorm)
library(tidyverse)
library(moments)


### Fig 1a. Acropora recruit densities ------------

data_density <- read.csv("settlement_compiled_impacted.csv", header=T) %>%
                        mutate(numdate = RetrievalYear + (RetrievalMonth / 12)) %>%
                        group_by(numdate) %>%
                        summarise(total=mean(TotalDensity_100cm2)) %>%
                        mutate(y=1)

ggplot(data=data_density, aes(x=numdate, y=y)) + theme_bw() +
      geom_point(aes(size=total, color=total), alpha=0.7) + 
      scale_size(range = c(0, 20)) + 
      scale_colour_gradient2(low="darkred", mid="red", high="green") + xlim(2012, 2020) +
      geom_point(aes(size=0.2), alpha=0.1) + xlab("Year")


### Fig 1a. Acropora recruit densities ------------

data_cover <- read.csv("Coral trajectories.csv", header=T) %>%
                        mutate(numdate = Year + (Month / 12)) %>%
                        group_by(numdate) %>%
                        summarise(coral.cover=mean(cover), cover.se=sd(cover)/sqrt(n()))

ggplot() + theme_bw() + xlab("Year") + xlim(2012,2020) +
    geom_point(data=data_cover, aes(x=numdate, y=coral.cover)) +
    geom_errorbar(data=data_cover, aes(x=numdate, ymin=coral.cover-cover.se, ymax=coral.cover+cover.se))


### Fig 2a. Acropora recruit size distributions ------------

data <- read.csv("JuvAcrData.csv", header=T) %>% filter(Site=="LightM" | Site=="LightN"| Site=="SDO"| Site=="ES")

impacted <- data %>% filter(Disturbance=="Impacted")
unimpacted<- data %>% filter(Disturbance=="Unimpacted")

ggplot(data %>% na.omit(), aes(x=MaxDiam_mm, fill=Disturbance)) +
      geom_density(alpha=0.5) + ggtitle("") + xlim(0,40) + theme_bw() +
      geom_vline(aes(xintercept = median(impacted$MaxDiam_mm, na.rm=TRUE)), colour="red") +
      geom_vline(aes(xintercept = median(unimpacted$MaxDiam_mm, na.rm=TRUE)), colour="blue") +
      xlab("Maximum recruit diameter") + ylab("Probability density")

### Fig 2b. Acropora recruit densities  ------------

data2 <- data %>% mutate_if(is.numeric, replace_na, 0) %>% 
                  mutate(Abund = MaxDiam_mm) %>%
                  mutate(across(c(Abund), ~+as.logical(.x))) %>%
                  group_by(Disturbance) %>% 
                  mutate(Abund.m2=Abund*16) %>% # convert 0.0625_m2 to m2 
                  summarise(Abundance=mean(Abund.m2), stdev=sd(Abund.m2), n=n(), se=stdev/sqrt(n)) 
               

ggplot(data2, aes(x = Disturbance, y = Abundance, fill=Disturbance)) +
      geom_bar(position=position_dodge(), stat="identity", colour="black", size=0.5)+
      geom_errorbar(aes(ymin=Abundance-se, ymax=Abundance+se), size=0.5, width=.0, position=position_dodge(.9)) +
      ylab("Juvenile Acropora abundance m-2") + theme(legend.title = element_blank()) + theme_bw() 


### Figure 2c ------------

# Extract data from Table 2 in Victor et al 2009
# corals measured in 2005, 7 years after the bleaching

# simulate distribution of coral sizes per species from mean, n and SE
# use truncated normal distributions

#this version
east.deep.hyacinthus <- rtruncnorm(4, a=2, b=Inf, mean = 12.6, sd = 7.1)
east.deep.cerealis <- rtruncnorm(31, a=2, b=Inf, mean = 7.1, sd = 2.4)
east.deep.nasuta <- rtruncnorm(9, a=2, b=Inf, mean = 6.6, sd = 1.2)
east.deep.selago <- rtruncnorm(4, a=2, b=Inf, mean = 16.5, sd = 9.7)
west.deep.hyacinthus <- rtruncnorm(22, a=2, b=Inf, mean = 21.8, sd = 11.7)
west.deep.cerealis <- rtruncnorm(30, a=2, b=Inf, mean = 19.1, sd = 10.1)
west.deep.nasuta <- rtruncnorm(5, a=2, b=Inf, mean = 18, sd = 8.3)
west.deep.selago <- rtruncnorm(42, a=2, b=Inf, mean = 29.9, sd = 9.7)

postbleaching <- c(east.deep.hyacinthus,east.deep.cerealis,east.deep.nasuta,east.deep.selago,
                   west.deep.hyacinthus,west.deep.cerealis,west.deep.nasuta,west.deep.selago) %>% 
                   as.data.frame() 

postbleaching.eastern <- c(east.deep.hyacinthus,east.deep.cerealis,east.deep.nasuta,east.deep.selago) %>% 
                          as.data.frame()

colonysizes <- read.csv("Coral sizes.csv")  # import 2019 survey data


ggplot() + theme_bw() + xlim(0,120) +
  geom_density(data=postbleaching.eastern, aes(x=.), bw=5, alpha=.2, fill="#fee000") +
  geom_density(data=colonysizes, aes(x=width), bw=5, alpha=.2, fill="#0db2b2") +
  geom_vline(aes(xintercept = median(postbleaching.eastern$.))) +
  geom_vline(aes(xintercept = median(colonysizes$width))) + 
  xlab("Maximum colony width (cm)") + ylab("Probability density")
