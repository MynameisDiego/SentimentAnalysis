library(dplyr)
#install.packages("data.table")
library(data.table)
library(formattable)
#install.packages("formattable")
library(ggplot2)
library(reshape2)
library(scales)

datos = read.csv("C:/Users/Diego/Downloads/DataR_unnest_1.csv")
datos=datos %>% mutate(frames=row_number())
eliminar = c("start","duration","interval","x","y","width", "height", "id")
datos = datos[,!(names(datos) %in% eliminar)]
datos = datos %>% select(frames, everything())

View(datos)

for (i in 1:nrow(datos))
  {

    datos$scores.neutral[i] = round(datos$scores.neutral[i]*100,2)
    datos$scores.anger[i] = round(datos$scores.anger[i]*100,2)
    datos$scores.contempt[i] = round(datos$scores.contempt[i]*100,2)
    datos$scores.happiness[i] = round(datos$scores.happiness[i]*100,2)
    datos$scores.disgust[i] = round(datos$scores.disgust[i]*100,2)
    datos$scores.sadness[i] = round(datos$scores.sadness[i]*100,2)
    datos$scores.surprise[i] = round(datos$scores.surprise[i]*100,2)
    datos$scores.fear[i] = round(datos$scores.fear[i]*100,2)
}

datos = setnames(datos, c("Frames","Neutral", "Felicidad", "Sorpresa", "Tristeza","Ira", "Disgusto",
                          "Miedo", "Desprecio"))

write.csv(datos,"datos.csv")


typeof(datos[2])



prom1 = mean(datos[[2]])
prom2 = mean(datos[[3]])
prom3 = mean(datos[[4]])
prom4 = mean(datos[[5]])
prom5 = mean(datos[[6]])
prom6 = mean(datos[[7]])
prom7 = mean(datos[[8]])
prom8 = mean(datos[[9]])

nombres = c("Neutral", "Felicidad", "Sorpresa", "Tristeza", "Ira", "Disgusto", "Miedo", "Desprecio")
promedio= c(prom1, prom2, prom3, prom4, prom5, prom6, prom7, prom8)

dataPromedio = data.frame(nombres,promedio)

bp =ggplot(dataPromedio, aes(x = "", y=promedio, fill=nombres))+
  geom_bar(width = 1, stat = "identity")
pie = bp + coord_polar("y", start=0) + 
  geom_text(aes(y=promedio/8 + c(0,cumsum(promedio)[-length(promedio)]),
                label = percent(promedio/100)), size=1)

datosTotal = melt(datos, id = "Frames") 
ggplot(data = datosTotal, aes(x=Frames, y=value, colour=variable))+
  geom_line(size = 1) + geom_point(size = 0.5) +
  scale_color_brewer(type = "qual", palette = 7) + theme_bw()+
  theme(plot.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size=10)) + 
  scale_x_continuous("Frames", expand = c(0,0)) + scale_y_continuous("Emociones", expand = c(0,0))






