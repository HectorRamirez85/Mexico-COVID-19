#### Libraries ####
require(ggplot2)

#### Directories #####
setwd("G:\\Mi unidad\\Covid19\\Mexico-COVID-19")


table <- read.csv("Mexico_COVID19.csv")


dim(table)
str(table)
head(table)


subtable <- table[-(1:35),] # removing the first 35 days without cases


# plot(table$Pos, type="o", pch=19, las=1, ylab="Infected",
#      main="Infected", xlab="time (days)")

dim(subtable)

pdf("MexicoInfectionEvolution.pdf",11,9)
for(i in 1:nrow(subtable)){
  print(i)
  ggplot(subtable[i,]) +
    ylab("Infected people") + xlab("Date") + ggtitle("Mexico infected people") +
    ylim(c(0,250)) +
    geom_line(mapping=aes(x=Fecha, y=Pos, group=1)) +
    geom_point(mapping=aes(x=Fecha, y=Pos, group=1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = 100, lty=2) +
    geom_line(mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
    geom_point(mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
    geom_line(mapping=aes(x=Fecha, y=Pos_I, group=1), col="blue") +
    geom_point(mapping=aes(x=Fecha, y=Pos_I, group=1), col="blue") +
    geom_line(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
    geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
    geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2) # day with the first local infected person
}

dev.off()

pdf("MexicoInfectionToday.pdf", 11, 9)
ggplot(subtable) +
  ylab("Infected people") + xlab("Date") + ggtitle("Mexico infected people") +
  ylim(c(0,250))+
  geom_line(mapping=aes(x=Fecha, y=Pos, group=1)) +
  geom_point(mapping=aes(x=Fecha, y=Pos, group=1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 100, lty=2) +
  geom_line(mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
  geom_point(mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
  geom_line(mapping=aes(x=Fecha, y=Pos_I, group=1), col="blue") +
  geom_point(mapping=aes(x=Fecha, y=Pos_I, group=1), col="blue") +
  geom_line(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
  geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
  geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2) # day with the first local infected person

dev.off()

