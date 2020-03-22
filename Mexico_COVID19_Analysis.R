#### Libraries ####
require(ggplot2)

#### Directories #####
wd <- "G:\\Mi unidad\\Covid19\\Mexico-COVID-19"
set(wd)

graphs <- paste0(wd,"\\Graphs")


table <- read.csv("Mexico_COVID19.csv")


dim(table)
str(table)
head(table)


subtable <- table[-(1:35),] # removing the first 35 days without cases


# plot(table$Pos, type="o", pch=19, las=1, ylab="Infected",
#      main="Infected", xlab="time (days)")

dim(subtable)

setwd(graphs)

#### Today ####
pdf(paste0("MexicoInfectionToday_",subtable$Fecha[nrow(subtable)],".pdf"), 11, 9)
par(mar=c(7,7,7,7))
ggplot(subtable) +
  ylab("Detected cases") + xlab("Date") + ggtitle("Covid-19 detected cases in Mexico") +
  geom_line(mapping=aes(x=Fecha, y=Pos, group=1)) +
  geom_point(mapping=aes(x=Fecha, y=Pos, group=1)) +
  geom_hline(yintercept = 100, lty=2) +
  geom_line(mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
  geom_point(mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
  geom_line(mapping=aes(x=Fecha, y=Pos_I, group=1), col="blue") +
  geom_point(mapping=aes(x=Fecha, y=Pos_I, group=1), col="blue") +
  geom_line(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
  geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
  geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2) + # day with the first local infected person
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=24, hjust = 0.5))
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  # scale_fill_discrete(name="Mexico", labels=c("A", "B", "C","D"))
dev.off()


#### Hoy ####
pdf(paste0("MexicoInfeccionHoy_",subtable$Fecha[nrow(subtable)],".pdf"), 11, 9)
ggplot(subtable) +
  ylab("Casos detectados") + xlab("Fecha") + ggtitle("Casos detectados de Covid-19 en México") +
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
  geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2) + # day with the first local infected person
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size=24))
# scale_fill_discrete(name="Mexico", labels=c("A", "B", "C","D"))
dev.off()

#### IN DEVELOPMENT ####


#### Temporal evolution ####
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
    geom_line(mapping=aes(x=Fecha, y=Pos_L, gsroup=1), col="darkgreen") +
    geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1), col="darkgreen") +
    geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2) # day with the first local infected person
}
dev.off()
