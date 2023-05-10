
#read in data for dendros
d133 <- read.csv("Cleaning/dendro_133_cleaned.csv")
d133 <- d133 %>% mutate(ts = ymd_hms(ts))

d140 <- read.csv("Cleaning/dendro_140_cleaned.csv")
d140 <- d140 %>% mutate(ts = ymd_hms(ts))


#subset dendro 133 to equalize
d133 <- d133[-1,]

str(d140)

combined_dendro <- cbind(d133,d140)
#try plotting combined (unsuccessful)
ggplot((subset(combined_dendro, ts < as.POSIXct("2022-10-23 00:00") & ts>as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color="grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color ="red")

#plotting 140
ggplot((subset(d140, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")

plot140<-ggplot((subset(d140, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
                 geom_line(aes(y=value), color = "grey40")+
                 geom_line(aes(y=gro_tot), color = "seagreen")+
                 geom_line(aes(y=twd), color = "red")
                 
#labeling the graph 140
print(plot140+labs(x="Date of 2022", y="Displacement Value (um)",title="Growth, Displacement Value, and Tree Water Deficit 
of Douglas-Fir 140"))

#add legend but this isn't working.
#option 1
gglegend('right', legend=c("Growth", "Displacement of Dendrometer", "Tree Water Deficit (TWD)"),col=c("seagreen", "grey40", "red"), lty=1)

#option 2
colors <- c("Growth" = "seagreen", "Tree Water Deficit (TWD)" = "red", "Displacement of Dendrometer" = "grey40")
df <- gather(d140, key = measure, value = Rate, 
             c("Growth", "Tree Water Deficit (TWD)", "Displacement of Dendrometer"))
ggplot((subset(d140, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")

#option 3
install.packages("tidyr")

library(tidyr)
df <- gather(by_year_percentage, key = measure, value = Rate, 
             c("deathpercentage", "tamponadepercentage", "protaminepercentage"))


#plotting 133
ggplot((subset(d133, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")

plot_133<-ggplot((subset(d133, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")

print(plot_133+labs(x="Date of 2022", y="Displacement Value (um)",title="Growth, Displacement Value, and Tree Water Deficit 
of Douglas-Fir 133"))


