library(ggplot2)
library(patchwork)

df = read.csv("//chss.datastore.ed.ac.uk/chss/ppls/users/scox2/Papers/Dementia Protocol LBC1936/CumPlotData.csv", header=T, sep=",")
df$date <- as.Date(df$consensus_diagnosis_date, format="%d/%m/%Y")
df$Wavedate <- as.Date(df$Wdate, format="%d/%m/%Y")

a = ggplot(df,aes(date))+
stat_bin(aes(y=cumsum(..count..)),geom="line",bins=100, size=2, color="dodgerblue4") +
ylim (0,118) + 
xlim(as.Date(c('02/07/2006', '30/11/2022'), format="%d/%m/%Y") ) +
labs(y = "Cumulative acsertained cases", x = "Year")

b = ggplot(df,aes(Wavedate))+
stat_bin(aes(y=cumsum(..count..)),geom="line",bins=10, size=2, color="dodgerblue2")+
ylim (0,118) + 
xlim(as.Date(c('02/07/2006', '30/11/2022'), format="%d/%m/%Y") ) +
labs(y = "Cumulative self-report cases", x = "Year")
a/b

pdf("//chss.datastore.ed.ac.uk/chss/ppls/users/scox2/Papers/Dementia Protocol LBC1936/CumPlot.pdf", width=7, height=7)
a/b
dev.off()


