setwd("G:\\Mi unidad\\Covid19\\Mexico-COVID-19")


table <- read.csv("Mexico_COVID19.csv")


dim(table)
str(table)


table[nrow(table),]

grep("_L", names(table))
grep("_I", names(table))

plot(table$Pos, type="o", pch=19, las=1, ylab="Infectados",
     main="Infectados México", xlab="tiempo (días)")
