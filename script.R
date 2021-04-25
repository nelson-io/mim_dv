#libs
library(tidyverse)
library(hrbrthemes)

#set df
df <- tibble(year = 2010:2019,
             attrition_rate = c(.097,.02,.01,.07,.151,.056,.123,.045,.082,.091),
             AVG = rep('AVG', 10))


#plot base

p = ggplot(df, aes(x=as.factor(year), y=attrition_rate))+
  xlab("Year")+
  ylab('Attrition Rate') +
  ggtitle("Attrition Rate by Year")+
  scale_y_continuous(labels = scales::percent, breaks = c(0,.025,.05,.075,.1,.125, .15))+
  theme_bw()

# plots with hline


hl <-list( 
  geom_hline(aes(yintercept = mean(attrition_rate), color = AVG), linetype="dashed", size = 1, alpha = .5),
  scale_color_manual(values = 'red'),
  theme(legend.title = element_blank()))

#plot 1 - bars
bars <- geom_col(col = 'white', fill = 'steelblue')


p1 <- p +
  bars

p12 <- p1 +
  hl
  

# plot 2 - connected scatter
cscatter <- list(geom_line(aes(group = 1), color="grey") ,
                   geom_point(shape=21, color="black", fill="#69b3a2", size=6))

p2 <- p +
  cscatter

p22 <- p2 +
  hl
   


#plot 3 - lollipop
lollipop <- list(geom_segment( aes(x=as.factor(year), xend= as.factor(year), y=0, yend=attrition_rate)),
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2))

p3 <- p+
  lollipop

p32 <- p3 +
  hl

#plot 4 - TS

tsplt <- geom_line(aes(group = 1),color = 'steelblue', size = 1)

p4 <- p+
  tsplt

p42 <- p4 +
  hl
  

#save


for (i in setdiff(ls(), c('p','phl'))) {
  if (is.ggplot(get(i))) {
    ggsave(plot = get(i),filename = paste0('plots/',i,'.jpg'),dpi = 'retina')
  }
}


