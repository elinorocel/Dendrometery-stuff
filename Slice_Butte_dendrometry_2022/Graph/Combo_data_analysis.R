
#read in data for dendros
d133 <- read.csv("Cleaning/dendro_133_cleaned.csv")
d133 <- d133 %>% mutate(ts = ymd_hms(ts))

d140 <- read.csv("Cleaning/dendro_140_cleaned.csv")
d140 <- d140 %>% mutate


#subset dendro 133 to equalize
d133 <- d133[-1,]

str(d140)

combined_dendro <- cbind(d133,d140)


#plotting
ggplot((subset(d140, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")
xlab(“Date”)
p + ylab(“Change in Radius (um)”)

ggplot((subset(d133, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey70")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red")



# overlay line plot 
lines(sample_data$x, sample_data$y2, col='green', lwd=2)
lines(sample_data$x, sample_data$y3, col='red', lwd=1)
lines(sample_data$x, sample_data$y4, col='blue', lty="dashed")