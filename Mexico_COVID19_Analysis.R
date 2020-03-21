#### Libraries ####
require(ggplot2)

#### Directories #####
setwd("G:\\Mi unidad\\Covid19\\Mexico-COVID-19")


table <- read.csv("Mexico_COVID19.csv")


dim(table)
str(table)
head(table)


subtable <- table[-(1:35),] # removing the first 35 days


# plot(table$Pos, type="o", pch=19, las=1, ylab="Infected",
#      main="Infected", xlab="time (days)")

ggplot() +
  ylab("Infected people") + xlab("Date") + ggtitle("Mexico infected people") +
  geom_line(subtable, mapping=aes(x=Fecha, y=Pos, group=1)) +
  geom_point(subtable, mapping=aes(x=Fecha, y=Pos, group=1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 100, lty=2) +
  geom_line(subtable, mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
  geom_point(subtable, mapping=aes(x=Fecha, y=Deceased, group=1), col="red") +
  scale_color_discrete(name = "Y series", labels = c("Deceased", "Pos"))


# legend imported, locals, total, deads

