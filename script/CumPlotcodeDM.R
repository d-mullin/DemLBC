library(ggplot2)
library(patchwork)
library(tidyverse)
theme_set(theme_bw()) # b&w theme for plot


df <- read_csv("processed_data/CumPlotData.csv")
df$date <- as.Date(df$consensus_diagnosis_date, format="%d/%m/%Y")
df$Wavedate <- as.Date(df$Wdate, format="%d/%m/%Y")

a = ggplot(df,aes(date))+
stat_bin(aes(y=cumsum(..count..)),geom="line",bins=100, size=1, color="black") +
ylim (0,118) + 
xlim(as.Date(c('02/07/2006', '30/11/2022'), format="%d/%m/%Y") ) +
labs(y = "Cumulative dementia outcomes", x = "Year")

b = ggplot(df,aes(Wavedate))+
stat_bin(aes(y=cumsum(..count..)),geom="line",bins=10, size=1, color="grey")+
ylim (0,118) + 
xlim(as.Date(c('02/07/2006', '30/11/2022'), format="%d/%m/%Y") ) +
labs(y = "Cumulative dementia outcomes", x = "Year")

pdf("//Users/dmullin/Dropbox/Academic/PhD/LBC/Dementia Ascertainment/DemLBC/figures/CumPlotA.pdf", width=7, height=3.5)
a
dev.off()

pdf("//Users/dmullin/Dropbox/Academic/PhD/LBC/Dementia Ascertainment/DemLBC/figures/CumPlotB.pdf", width=7, height=3.5)
b
dev.off()

pdf("//Users/dmullin/Dropbox/Academic/PhD/LBC/Dementia Ascertainment/DemLBC/figures/CumPlotBoth.pdf", width=7, height=3.5)
a/b
dev.off()


