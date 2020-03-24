#### Libraries ####
require(ggplot2)

#### Directories #####
wd <- "G:\\Mi unidad\\Covid19\\Mexico-COVID-19"
graphs <- paste0(wd,"\\Graphs")
setwd(wd)


#### Read table ####
table <- read.csv("Mexico_COVID19.csv")

dim(table)
str(table)
head(table)

subtable <- table[-(1:35),] # removing the first 35 days without cases
dim(subtable)
head(subtable)


##### Parameters ####
lineWd <- 2 # line width
cxSize <- 3 # cex size
ref <- 0.7 # vertical and horizontal reference lines

setwd(graphs)


#### Today, daily charts ####
pdf(paste0("MexicoInfectionToday_",subtable$Fecha[nrow(subtable)],".pdf"), 13, 9)
par(mar=c(7,7,7,7))
print(paste("Day", nrow(subtable)-1, "since 1st. detected case")) # Day since the first detected case

cases <- c("Total" = "black", "Imported" = "blue", "Local" = "darkgreen", "Deaths" = "red", "Recovered" = "purple") 
ggplot(subtable) +
  # ylim(c(0,1000)) +
  ggtitle(paste("COVID-19 in Mexico", subtable$Fecha[nrow(subtable)])) +
  geom_line(mapping=aes(x=Fecha, y=Pos, group=1, color="Total"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos, group=1, color="Total"), cex=cxSize) +
  geom_hline(yintercept = 100, lty=2, size=ref) +
  geom_line(mapping=aes(x=Fecha, y=Recovered, group=1, color="Recovered"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Recovered, group=1, color="Recovered"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Deceased, group=1, color="Deaths"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Deceased, group=1, color="Deaths"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Pos_I, group=1, color="Imported"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos_I, group=1, color="Imported"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Pos_L, group=1, color="Local"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1, color="Local"), cex=cxSize) +
  # geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2, size=ref) + # day with the first local infected person
  # geom_vline(xintercept=which(subtable$Fecha == "2020-03-19"), col="red", lty=2, size=ref) + # day with the first dead person
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=24,face="bold"),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(size=30, hjust = 0.5, face="bold"),
        legend.text = element_text(size=20), 
        legend.title = element_blank()) +
        # legend.title = element_text(size=24,face="bold")) +
  scale_color_manual(values = cases, limits = names(cases)) +
  labs(x = "Date",
       y = "Confirmed cases")
dev.off()


#### Hoy, gráficos diarios ####

pdf(paste0("MexicoInfeccionHoy_",subtable$Fecha[nrow(subtable)],".pdf"), 13, 9)
par(mar=c(7,7,7,7))
print(paste("Día", nrow(subtable)-1, "desde el 1er. caso detectado")) # Día desde el primer caso detectado


casos <- c("Totales" = "black", "Importados" = "blue", "Locales" = "darkgreen", "Defunciones" = "red", "Recuperados" = "purple") 
ggplot(subtable) +
  # ylim(c(0,1000)) +
  # ggtitle("Casos detectados de Covid-19 en México") +
  ggtitle(paste("COVID-19 en México", subtable$Fecha[nrow(subtable)])) +
  geom_line(mapping=aes(x=Fecha, y=Pos, group=1, color="Totales"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos, group=1, color="Totales"), cex=cxSize) +
  geom_hline(yintercept = 100, lty=2, size=ref) +
  geom_line(mapping=aes(x=Fecha, y=Recovered, group=1, color="Recuperados"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Recovered, group=1, color="Recuperados"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Deceased, group=1, color="Defunciones"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Deceased, group=1, color="Defunciones"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Pos_I, group=1, color="Importados"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos_I, group=1, color="Importados"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Pos_L, group=1, color="Locales"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1, color="Locales"), cex=cxSize) +
  # geom_vline(xintercept=which(subtable$Fecha == "2020-03-13"), col="black", lty=2, size=ref) + # day with the first local infected person
  # geom_vline(xintercept=which(subtable$Fecha == "2020-03-19"), col="red", lty=2, size=ref) + # day with the first dead person
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=24,face="bold"),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(size=30, hjust = 0.5, face="bold"),
        legend.text = element_text(size=20), 
        legend.title = element_blank()) +
  scale_color_manual(values = casos, limits = names(casos)) +
  labs(x = "Fecha",
       y = "Casos confirmados")
dev.off()





#### IN DEVELOPMENT ####

subtable$CMX[nrow(subtable)] / subtable$Pos[nrow(subtable)] * 100



#### Adjust a Gaussian model to the total cases curve and try to predict where will it be in a few days ####


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
