ggplot((subset(d_140_L3, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y=value), color = "grey40")+
  geom_line(aes(y=gro_tot), color = "seagreen")+
  geom_line(aes(y=twd), color = "red4")

ggplot_add(lines((subset(d_133_L3, ts < as.POSIXct("2022-10-23 00:00") & ts > as.POSIXct("2022-06-29 00:00"))), aes(x=ts))+
  geom_line(aes(y2=value), color = "grey70")+
  geom_line(aes(y2=gro_tot), color = "green")+
  geom_line(aes(y2=twd), color = "red"))



# overlay line plot 
lines(sample_data$x, sample_data$y2, col='green', lwd=2)
lines(sample_data$x, sample_data$y3, col='red', lwd=1)
lines(sample_data$x, sample_data$y4, col='blue', lty="dashed")