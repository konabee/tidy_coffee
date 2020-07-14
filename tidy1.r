## This code was written by Chia Liu for TidyTuesday, on July 13, 2020 ## 

library(tidyverse)
library(ggridges)
library(ggplot2)
library(png)
library(grid)
library(ggpubr)

tuesdata <- tidytuesdayR::tt_load(2020, week = 28)
coffee_ratings <- tuesdata$coffee_ratings 

## just keep columns I need ## 
positions<-c(1,2,4)
d2<-coffee_ratings[,positions]
colnames(d2)<-c('points','country')

## clump USA observations together; simplify Tanzania ##
d2$country[d2$country %in% c('United States','United States (Hawaii)','United States (Puerto Rico)')]<-'USA (Mostly Hawaii)'
d2$country[d2$country=='Tanzania, United Republic Of']<-'Tanzania'

## keep only countries with more than 10+ beans ## 
a<-data.frame(table(d2$country)) 
a<-subset(a, Freq>=10)
d2<-d2[d2$country %in% a$Var1,]

## create my own palette; choose earth tones ##
mypal<-c('goldenrod3','rosybrown3','burlywood4','darkgreen')

## import background image ##
img<-readPNG('C:/Users/Chia/Desktop/coffeepic3.png')


d2 %>% 
  filter(points>50) %>% ## get rid of 1 observation that seems aberrant, with 0 point ## 
  mutate(country=fct_reorder(.f=country,.x=points,.fun=median),## arrange by median ##
         region=case_when(
           country %in% c('Ethiopia','Kenya','Uganda','Tanzania','Malawi')~'Africa',
           country %in% c('Costa Rica','Colombia','El Salvador','Brazil','Peru','Guatemala','Honduras',
                          'Mexico','Nicaragua')~'Latin America',
           country %in% c('China','Thailand','Indonesia','Taiwan','India')~'Asia',
           country == 'USA (Mostly Hawaii)' ~ 'North America')) %>% ## categorize by region ##
  ggplot(aes(x=points, y=country, fill=region))+
  background_image(img)+
  geom_density_ridges(color='indianred4', scale=3)+
  scale_fill_manual(aes(fill=region),values=mypal)+
  scale_x_continuous(breaks=seq(60,95,5))+
  theme_ridges(grid=F, line_size=0.1, font_size=12, font_family='mono')+
  labs(title='Whose beans reign supreme?',x='Distribution of Total Scores',y=NULL,
       caption='Author: C.Liu for TidyTuesday')+
  theme(
    plot.caption = element_text(hjust = 1, size=8, color='darkolivegreen4'),
    axis.title.x= element_text(hjust=0.5, size=10))+
  guides(fill=guide_legend(title=NULL), size=9)

ggsave('tt1.png',width=9,height=6)


