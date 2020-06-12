getwd()
setwd("/home/pagzal/projects/uda-da-r/project")

library(tidyverse)
library(ggplot2)
library(lubridate)
#install.packages("gridExtra")
library(gridExtra)
library(hexbin)

# base R
#wine <- read.csv("wineQualityWhites.csv")

# readr / tidyverse
# whitewine <- read_csv("wineQualityWhites.csv", 
#                       col_types = cols(X1 = col_skip(), 
#                                        quality = col_integer()))
# rename(whitewine, c("X1" = "id"))


whitewine <- read_csv("wineQualityWhites.csv", 
                      col_types = cols(quality = col_integer()))
whitewine <- reshape::rename(whitewine, c(X1 = "ID"))


# ph vs. quality


redwine <- read_csv("wineQualityReds.csv", col_types = cols(X1 = col_skip()))
# drop first (index) column
#whitewine <- subset(whitewine, select = -X1)
#redwine <- subset(redwine, select = -X1)

wine <- whitewine

#mutate(mtcars, gpm= 1/mpg)
wine$qlev <- ifelse(wine$quality <= 4, "bad", ifelse(wine$quality >= 8, "good", "medium"))
names(wine)
summary(wine)

# funktioniert!

wine %>%
  # sample_n(100) %>%
  gather(key="wine.att", value="measurement", -c(ID, qlev))%>%
  ggplot() +
  geom_histogram(mapping=aes(measurement)) +
  facet_wrap(vars(wine.att), scales='free')

# x-log
# bimodale verteilung bei residual sugar kommt gut raus

wine %>%
  # sample_n(100) %>%
  gather(key="wine.att", value="measurement", -c(ID, qlev))%>%
  ggplot() +
  geom_histogram(mapping=aes(measurement)) +
  scale_x_log10() +
  facet_wrap(vars(wine.att), scales='free')

wine %>%
  # sample_n(100) %>%
  gather(key="wine.att", value="measurement", -c(ID, qlev))%>%
  ggplot() +
  geom_histogram(mapping=aes(measurement)) +
  scale_x_log10() +
  scale_y_log10() +  
  facet_wrap(vars(wine.att), scales='free')

wine %>%
  # sample_n(100) %>%
  gather(key="wine.att", value="measurement", -c(ID, qlev))%>%
  ggplot() +
  geom_histogram(mapping=aes(measurement)) +
  scale_y_log10() +
  facet_wrap(vars(wine.att), scales='free')

coord_cartesian(quantile(wine$volatile.acidity, probs=c(0.0, 0.98), names=FALSE)) 
#wine %>%
#  select(-c(ID, qlev)) %>%
#  summarise_all(c(min, max, mean, median))


q_alc <- wine %>% group_by(quality) %>% 
  summarise(mean = mean(alcohol), median = median(alcohol), n = n())


# JAWOLL!
# lässt aber nicht viel erkennen

wine %>%
  #sample_n(300) %>%
  gather(key="wine.att", value="measurement", -c(ID, qlev, quality))%>%
  ggplot(aes(x=measurement, y=quality)) +
    #geom_jitter(width=0, height=0.5) +
    geom_point(alpha=0.1) +
    # scale_x_sqrt() +
    #scale_x_log10() +
    geom_smooth() +
    #geom_quantile() +
    facet_wrap(vars(wine.att), scales='free')



ggplot(data=q_alc) +
  geom_line(aes(y=quality, x=mean))

histos <- list()
colnames = names(wine)
for (i in 1:length(colnames)) {
  col <- as.name(colnames[i])
  col
  histos[[i]] <- ggplot(data=wine) +
    geom_histogram(mapping=aes(x=col))
}
# expand an vector
grid.arrange(grobs=histos, ncol=4)


q <- quantile(wine$fixed.acidity, probs=c(0.01, 0.99), names=FALSE)
p1 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(fixed.acidity), binwidth=(q[2] - q[1])/30) +
  coord_cartesian(q) 
p1

p2 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(volatile.acidity), binwidth = 0.02) +
  coord_cartesian(quantile(wine$volatile.acidity, probs=c(0.0, 0.98), names=FALSE)) 
p2

p3 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(citric.acid), binwidth=0.02) +
  coord_cartesian(c(0, 1)) 
p3

p4 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(residual.sugar), bins = 80) +
  coord_cartesian(c(0, 25))
p4

p5 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(chlorides), bins=160) +
  coord_cartesian(c(0, 0.1))
p5

p6 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(free.sulfur.dioxide))

p7 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(total.sulfur.dioxide))

p8 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(density))

p9 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(pH))

p10 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(sulphates))

p11 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(alcohol))

p12 <- ggplot(data=wine) +
  geom_histogram(mapping=aes(quality))

histos <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) 
grid.arrange(grobs=histos, ncol=4)
# grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol=4)

ggplot(data=wine) + 
  geom_point(mapping=aes(x=alcohol, y=pH, color=quality))

wine$delta_so2 <- wine$total.sulfur.dioxide - wine$free.sulfur.dioxide
ggplot(wine, aes(delta_so2)) + geom_histogram()

ggplot(data=wine) + 
  geom_point(mapping=aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide, color=quality),   alpha=(0.1)) 

# sulphates are important ?! 
# the plot is not good - use a heatmap instead?
#mutate(mtcars, gpm= 1/mpg)
wine$qlev <- ifelse(wine$quality <= 4, "bad", ifelse(wine$quality >= 8, "good", "medium"))
wine %>%
ggplot(aes(sulphates, quality)) + 
  geom_jitter(alpha=0.2) 

ggplot(data=wine, aes(quality, sulphates)) + 
  # geom_bin2d() 
  geom_violin() 

# das sieht gut aus: zeigt, wo viele daten liegen
# wenn ich jetzt noch durchschnittliche quality zeigen könnte?
ggplot(data=wine, aes(alcohol, sulphates)) + 
  geom_hex() 

ggplot(data=wine, aes(alcohol, chlorides)) + 
  geom_hex() 

ggplot(data=wine, aes(alcohol, sulphates)) + 
  geom_bin2d() 

# interessanter plot!
wine %>%
  ggplot(aes(pH)) + 
    geom_density() +
    facet_grid(rows=vars(quality))

# interessanter plot!
wine %>%
  ggplot(aes(chlorides)) + 
  geom_density() +
  facet_grid(rows=vars(quality))

# interessanter plot!
wine %>%
  ggplot(aes(sulphates)) + 
  geom_density() +
  facet_grid(rows=vars(quality))
# ok -> median berechen. quality vs. median

# hmmm. da kommt auch nicht viel bei raus.
wine %>%
  select(c(sulphates, quality)) %>%
  group_by(quality) %>%
  summarise(median_sulphates = median(sulphates)) %>%
  ggplot(aes(x=median_sulphates, y=quality)) +
  geom_line()

#der plot hier ist interessant
wine$qfac <- factor(wine$quality)
ggplot(data=wine, aes(qfac, alcohol)) + 
  geom_boxplot() 

ggplot(data=wine, aes(qfac, density)) + 
  geom_boxplot() 

# boxplot besser ...
ggplot(data=wine, aes(qfac, alcohol)) + 
  geom_violin() 
# correlation here!
ggplot(data=wine) + 
  geom_point(mapping=aes(x=density, y=alcohol))

# http://www.r-tutor.com/

# r_xy = s_xy / (s_x * s_y)
# r_xy ... correlation coefficient
# s_xy ... covariance
# s_x, s_y ... sample std deviations
cor(wine$alcohol, wine$density)     

wine2 <- filter(wine, residual.sugar < 40 & citric.acid < 1 & fixed.acidity < 12 & free.sulfur.dioxide < 200)
cor(wine2$alcohol, wine2$density)  
ggplot(data=wine2) + 
  geom_point(mapping=aes(x=density, y=alcohol))

ggplot(data=wine2) + 
  geom_hex(mapping=aes(x=density, y=residual.sugar))
# rule of thumb (pearson):
# |r| > 0.3 ... meaningful
#       0.5     moderate
#       0.8     large
cor.test(wine$residual.sugar, wine$quality, method = "pearson")

cor.test(wine$alcohol, wine$quality, method = "pearson")
cor.test(wine$alcohol, wine$quality, method = "spearman")

# linear model
density.lm = lm(density ~ alcohol, data=wine)
coefficients(density.lm)

density.lm

# coeff of determination r**2
summary(density.lm) #$r.squared 

by(wine$residual.sugar, wine$qlevel, summary)

# make a small smaple to speed up scatterplot
idx <- sample.int(nrow(wine), 100)
wine.sample <- wine[idx, ]

# scatterplot matrix
# se also: https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
pairs(wine.sample)

library(ggplot2)
library(GGally)
ggpairs(wine.sample)

ggpairs(wine)

ggpairs(select(wine2, -quality))
# remove density, pH
ggpairs(select(wine2, -quality, -pH, -density, -total.sulfur.dioxide))


dim(wine)

# ordered factor for quality

#wine$qlevel <- quality_fac

ggplot(data=wine) + 
  geom_point(mapping=aes(x=alcohol, y=pH, color=quality_fac), 
             alpha=0.7
             )

quality_fac <- ordered(wine$quality, levels = 0:10)
wine %>%
  add_column(qlevel = quality_fac) %>%
  ggplot() + 
  geom_point(mapping=aes(x=alcohol, y=volatile.acidity, color=qlevel), alpha=0.1)

# idea: use jitter on quality
# idea: sum up ingredients -> what's the remainder?
# quality should be an int, or ordered factor?

# how to count values for quality, make relative frequencies?
abs_freq <- table(wine$quality)
rel_freq <- abs_freq / nrow(wine)

#qfac <- factor(x=wine$quality, levels = 1:10)

table(wine$quality)
sum(wine$quality)

ggplot(data=wine) +
  geom_boxplot(mapping=aes(quality))

qlev <- ifelse(wine$quality <= 4, "bad", ifelse(wine$quality >= 8, "good", "medium"))
#qfac <- ordered(qlev, levels = c("bad", "medium", "good"))

table(qlev)
wine$qlevel <- ordered(qlev, levels = c("bad", "medium", "good"))
table(wine$qlevel)

ggplot(data=wine) + 
  geom_point(mapping=aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide, color=qlevel), alpha=1) 

# good and bad ones only
ggplot(data=filter(wine, qlevel == "good" | qlevel == "bad")) + 
  geom_point(mapping=aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide, color=qlevel), alpha=1) 

# good and bad ones only
ggplot(data=filter(wine, qlevel == "good" | qlevel == "bad")) + 
  geom_point(mapping=aes(x=alcohol, y=volatile.acidity, color=qlevel), alpha=1) 

# variante 1
# wine.goodbad <- filter(subset(wine, select = -quality), qlevel == "good" | qlevel == "bad")

# variante 2
# wine.goodbad <- filter(wine, qlevel %in% c("good", "bad"))
# wine.goodbad <- select(wine.goodbad, -quality)

# variante 3
wine.goodbad <- wine %>%
  filter(qlevel %in% c("good", "bad")) %>%
  select(-quality)

ggpairs(wine.goodbad)

# angelehnt an: http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(wine.goodbad, #pch = 19,  cex = 0.5,
      col = my_cols[wine.goodbad$qlevel],
      lower.panel=NULL)

# question: linear model density -> alcohol, sugar etc.
c()

cut(wine$fixed.acidity)

# make fixed acidity a factor
faf <- cut(wine$fixed.acidity, 
           quantile(wine$fixed.acidity, names=FALSE))
wine$qf <- factor(wine$quality)
vaf <- cut(wine$volatile.acidity, 
           quantile(wine$volatile.acidity, names=FALSE))

ggplot(aes(x=fixed.acidity, y=volatile.acidity), data=wine) +
  geom_line(aes(color=qf), stat='summary', position=median)

wine.typical %>%
  select(-ID) %>%
  ggpairs()

# correlation
so2_ratio <- wine$free.sulfur.dioxide / wine$total.sulfur.dioxide
ggplot(so2_ratio) +
  geom_point() +
  geom_abline(slope=1, intercept=0)

wine %>%
  add_column(so2_ratio = wine$free.sulfur.dioxide / wine$total.sulfur.dioxide) %>%
  ggplot(aes(y=quality, x=so2_ratio)) + 
  #geom_histogram()
  geom_point() +
  geom_smooth() 

wine %>%
  #sample_n(20) %>%
  select(-c(ID, quality)) %>%
  ggpairs(upper = list(continuous = wrap("cor", size=2))) +
  theme_grey(base_size = 6)



phmodel <- lm(pH ~ citric.acid + volatile.acidity + fixed.acidity, data=wine)
summary(phmodel)

qmodel <- lm(quality ~ alcohol + chlorides + total.sulfur.dioxide + volatile.acidity + sulphates, data=wine) 
summary(qmodel)

dmodel <- lm(density ~ alcohol + chlorides + sulphates + total.sulfur.dioxide + residual.sugar + citric.acid + fixed.acidity + volatile.acidity, data=wine) 
summary(dmodel)

gph <- glm(pH ~ citric.acid + volatile.acidity + fixed.acidity, data=wine)
summary(gph)
