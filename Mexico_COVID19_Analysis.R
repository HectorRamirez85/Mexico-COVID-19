#### Libraries ####
require(ggplot2)

#### Directories #####
wd <- "G:\\Mi unidad\\Covid19\\Mexico-COVID-19"
setwd(wd)

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

## today detected
subtable[nrow(subtable),]
subtable$Pos[nrow(subtable)]

head(subtable)


##### Parameters ####
lineWd <- 2 # line width
cxSize <- 3 # cex size
ref <- 0.7 # vertical and horizontal reference lines



#### Today ####
pdf(paste0("MexicoInfectionToday_",subtable$Fecha[nrow(subtable)],".pdf"), 11, 9)
par(mar=c(7,7,7,7))
print(paste("Day", nrow(subtable)-1, "since 1st. detected case")) # Day since the first detected case

cases <- c("Total" = "black", "Imported" = "blue", "Locals" = "darkgreen", "Deaths" = "red") 
ggplot(subtable) +
  # ylim(c(0,1000)) +
  ggtitle(paste("Covid-19 in Mexico", subtable$Fecha[nrow(subtable)])) +
  geom_line(mapping=aes(x=Fecha, y=Pos, group=1, color="Total"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos, group=1, color="Total"), cex=cxSize) +
  geom_hline(yintercept = 100, lty=2, size=ref) +
  geom_line(mapping=aes(x=Fecha, y=Deceased, group=1, color="Deaths"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Deceased, group=1, color="Deaths"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Pos_I, group=1, color="Imported"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos_I, group=1, color="Imported"), cex=cxSize) +
  geom_line(mapping=aes(x=Fecha, y=Pos_L, group=1, color="Locals"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos_L, group=1, color="Locals"), cex=cxSize) +
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
       y = "Detected cases")
dev.off()


#### Hoy ####

pdf(paste0("MexicoInfeccionHoy_",subtable$Fecha[nrow(subtable)],".pdf"), 11, 9)
par(mar=c(7,7,7,7))
print(paste("Día", nrow(subtable)-1, "desde el 1er. caso detectado")) # Día desde el primer caso detectado

# a <- paste("Total =", subtable$Pos[nrow(subtable)])


casos <- c("Total" = "black", "Importados" = "blue", "Locales" = "darkgreen", "Decesos" = "red") 
ggplot(subtable) +
  # ylim(c(0,1000)) +
  # ggtitle("Casos detectados de Covid-19 en México") +
  ggtitle(paste("Covid-19 en México", subtable$Fecha[nrow(subtable)])) +
  geom_line(mapping=aes(x=Fecha, y=Pos, group=1, color="Total"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Pos, group=1, color="Total"), cex=cxSize) +
  geom_hline(yintercept = 100, lty=2, size=ref) +
  geom_line(mapping=aes(x=Fecha, y=Deceased, group=1, color="Decesos"), size=lineWd) +
  geom_point(mapping=aes(x=Fecha, y=Deceased, group=1, color="Decesos"), cex=cxSize) +
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
       y = "Casos detectados")
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
