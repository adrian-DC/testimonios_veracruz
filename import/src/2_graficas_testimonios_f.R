rm(list=ls())
setwd("~")

#-------------------------------------#
#  Gráficas Memorial de Testimonios   #
#  Mariana S.                         #
#-------------------------------------#

require(pacman)
p_load(tidyverse, readxl, treemapify, rgdal,rgeos,ggrepel,ggmosaic)

# Personal
  out <- "/Users/marianasolano/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/out"
grafs <- "/Users/marianasolano/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/grafs"

# Oficina
  out <- "/Users/carolinatorreblanca/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/out"
grafs <- "/Users/carolinatorreblanca/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/grafs"
  shp <- "/Users/carolinatorreblanca/Dropbox (Data Cívica A.C.)/Data Cívica/Libreria/Shapefiles/municipios"

#Abrimos la base
data <- read.csv(paste0(out,"/base_testimonios.csv"),fileEncoding = "UTF-8", stringsAsFactors = F)

#Algunos arreglos antes de comenzar a graficar
data$grupo_edad[data$edad < 4] <- "S/D"
data$grupo_edad[data$edad < 18] <- "Menores de 18 años"
data$grupo_edad[data$edad >= 18 & data$edad < 24 ] <- "De 18 a 23 años"
data$grupo_edad[data$edad >= 24 & data$edad < 30 ] <- "De 24 a 29 años"
data$grupo_edad[data$edad >= 30 & data$edad < 45 ] <- "De 30 a 44 años"
data$grupo_edad[data$edad >= 45 & data$edad < 60 ] <- "De 45 a 59 años"
data$grupo_edad[data$edad >= 60 & data$edad < 120 ] <- "De 60 años o más"
data$grupo_edad[data$edad > 120] <- "S/D"
data <- mutate(data, grupo_edad=replace_na(grupo_edad,"S/D"))
data <- mutate(data, edad=replace_na(edad,"S/D"))

# Colectivo Treemap 
tempo <- data %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(part_colectivo) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))  

ggplot(tempo, aes(area=porcentaje, fill=part_colectivo, label=paste(part_colectivo, "\n", paste0("(",porcentaje,"%", ")")))) + 
       geom_treemap() +  scale_fill_manual(values=c("#fc8d59", "#ffffbf", "#91cf60")) +
       geom_treemap_text(place = "centre", grow = F, color ="black") +
       labs(title="¿La persona declarante pertenece a algún colectivo?", 
           caption="", fill="") +
       theme_bw(base_size = 10) +
       theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72")) +
       coord_fixed() +
       guides(fill=F)

ggsave(paste0(grafs,"/1_colectivo",".svg"), width = 12, height =12)


#####Sexo de las víctimas

tempo <- data %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(sexo) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))  

ggplot(tempo, aes(area=porcentaje, fill=sexo, label=paste(total, paste0("(",porcentaje,"%", ")")))) + 
        geom_treemap() +  scale_fill_manual(values=c("#fc8d59", "#ffffbf", "#91cf60")) +
        geom_treemap_text(place = "centre", grow = F, color ="black") +
        labs(title="Sexo de la víctima", 
             caption="", fill="") +
        theme_bw(base_size = 10) +
        theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72")) +
        coord_fixed() 

ggsave(paste0(grafs,"/1_2_sexo_totales",".svg"), width = 12, height =12)


tempo <- data %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(colectivo) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))  

ggplot(tempo, aes(area=porcentaje, fill=colectivo, label=paste(colectivo, "\n", paste0("(",porcentaje,"%", ")")))) + 
       geom_treemap() +  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#ffffbf","#e6f598","#abdda4")) +
       geom_treemap_text(place = "centre", grow = F, color ="black") +
       labs(title="De las personas declarantes que afirmaron pertenecer a algún colectivo, ¿a cuál pertenecen?", 
             caption="",fill="") +
       theme_bw(base_size = 10) +
       theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
             legend.position = "bottom",
             legend.background = element_rect(linetype="solid",color="black")) +
       coord_fixed()

ggsave(paste0(grafs,"/2_colectivo",".svg"), width = 12, height =12)


# Sexo y edad de la víctima 
tempo <- data %>% 
         mutate(total=1) %>% 
         group_by(grupo_edad, sexo) %>%
         filter(grupo_edad!="S/D") %>%
         filter(sexo!="No Especificado") %>%
         mutate(total_estatal=sum(total)) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$grupo_edad <- factor(tempo$grupo_edad,levels=c("Menores de 18 años","De 18 a 23 años","De 24 a 29 años","De 30 a 44 años",
                                                     "De 45 a 59 años","De 60 años o más"))

ggplot(tempo,aes(x=sexo,y=grupo_edad,fill=porcentaje)) +
        geom_tile(color="black") + geom_text(aes(label=paste0(porcentaje,"%"))) +
        scale_fill_continuous(low="#d9f0a3", high="#005a32",
                              breaks=seq(0, 35, 15),
                              limits=c(0,35)) +
        labs(title="Porcentaje de las víctimas declaradas como desaparecidas por sexo y edad", subtitle= "", 
             x="", y="", fill="") +
        coord_fixed() +
        theme_bw() + 
        theme(title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Didot"),
              plot.caption = element_text(color= "black", size=9,face="bold" ))
      
ggsave(paste0(grafs,"/3_edad",".svg"), width = 12, height =12)

## Sexo y escolaridad
tempo <- data %>% 
         mutate(total=1) %>% 
         group_by(escol, sexo) %>%
         filter(sexo!="No Especificado") %>%
         mutate(total_estatal=sum(total)) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$escol <- factor(tempo$escol,levels=c("Primaria","Secundaria","Bachillerato","Carrera técnica","Licenciatura","Posgrado", "Sin información"))

ggplot(tempo,aes(x=sexo,y=escol,fill=porcentaje)) +
       geom_tile(color="black") + 
       geom_text(aes(label=paste0(porcentaje,"%"))) +
       scale_fill_continuous(low="#fde0dd", high="#c51b8a",
                             breaks=seq(0, 30, 10),
                             limits=c(0,30)) +
       labs(title="Porcentaje de las víctimas declaradas como desaparecidas por sexo y escolaridad", 
            subtitle= "", x="", y="", fill="", caption="") +
       coord_fixed() +
       theme_bw() + 
       theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
             plot.caption = element_text(color= "black", size=9, face="bold"))

ggsave(paste0(grafs,"/4_escolaridad",".svg"), width = 12, height =12) 

# Sexo y estado civil
tempo <- data %>% 
         mutate(total=1) %>% 
         group_by(edo_civil, sexo) %>%
         filter(sexo!="No Especificado") %>%
         filter(edo_civil!="Sin información") %>%
         mutate(total_estatal=sum(total)) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))


tempo$edo_civil <- factor(tempo$edo_civil,levels=c("Viuda(o)","Divorciada(o)","Separada(o)","Unión libre","Casada(o)","Soltera(o)"))

ggplot(tempo,aes(x=sexo,y=edo_civil,fill=porcentaje)) +
        geom_tile(color="black") + geom_text(aes(label=paste0(porcentaje,"%"))) +
        scale_fill_continuous(low="#bfd3e6", high="#6e016b") +
        labs(title="Porcentajes de víctimas declaradas como desaparecidas por sexo y estado civil", subtitle= "",
             x="", y="", fill="Porcentaje", caption="") +
        coord_fixed() +
        theme_bw() + 
        theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
              plot.caption = element_text(color= "black", size=9,face="bold"))

ggsave(paste0(grafs,"/5_edocivil",".svg"), width = 12, height =12)

# Porcentaje de víctimas por edad y ocupación *agregar un spine pero dejar esta para que Jacobo y Andrés puedan ver los porcentajes
tempo <- data %>% 
         mutate(total=1) %>% 
         group_by(ocupacion, grupo_edad) %>%
         filter(grupo_edad!="S/D") %>%
         filter(ocupacion!="Sin información") %>%
         mutate(total_estatal=sum(total)) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$grupo_edad <- factor(tempo$grupo_edad,levels=c("Menores de 18 años","De 18 a 23 años","De 24 a 29 años","De 30 a 44 años",
                                                     "De 45 a 59 años","De 60 años o más"))

ggplot(tempo,aes(x=grupo_edad,y=reorder(ocupacion, +porcentaje),fill=porcentaje)) +
       geom_tile(color="black") + geom_text(aes(label=paste0(porcentaje,"%"))) +
       scale_fill_continuous(low="#bfd3e6", high="#6e016b") +
       labs(title="Porcentajes de víctimas declaradas como desaparecidas por grupo de edad y ocupación", subtitle= "",
                x="", y="", fill="Porcentaje", caption="") +
       coord_fixed() +
       theme_bw() + 
       theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
             plot.caption = element_text(color= "black", size=9,face="bold" ))

ggsave(paste0(grafs,"/6_edad_ocup",".svg"), width = 12, height =12)

###### Gráfica extra, ¿qué pasa si quitamos "otros"?
tempo <- data %>% 
         mutate(total=1) %>% 
         group_by(ocupacion, grupo_edad) %>%
         filter(grupo_edad!="S/D") %>%
         filter(ocupacion!="Sin información", ocupacion!="Otros") %>%
         mutate(total_estatal=sum(total)) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$grupo_edad <- factor(tempo$grupo_edad,levels=c("Menores de 18 años","De 18 a 23 años","De 24 a 29 años","De 30 a 44 años",
                                                     "De 45 a 59 años","De 60 años o más"))

ggplot(tempo,aes(x=grupo_edad,y=reorder(ocupacion, +porcentaje),fill=porcentaje)) +
      geom_tile(color="black") + geom_text(aes(label=paste0(porcentaje,"%"))) +
      scale_fill_continuous(low="#bfd3e6", high="#6e016b") +
      labs(title="Porcentajes de víctimas declaradas como desaparecidas por grupo de edad y ocupación", 
           subtitle= "sin ocupaciones sin clasificar",
           x="", y="", fill="", caption="") +
      coord_fixed() +
      theme_bw() + 
      theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
            plot.caption = element_text(color= "black", size=9,face="bold" ))

ggsave(paste0(grafs,"/6_1_edad_ocup_sin_otros",".svg"), width = 12, height =12)

# Porcentaje de víctimas por edad y escolaridad *cambiar por spine pero dejar esta por los porcentajes
tempo <- data %>% 
         group_by(escol, grupo_edad) %>%
         mutate(total=1) %>% 
         filter(grupo_edad!="S/D") %>%
         filter(escol!="Sin información") %>%
         mutate(total_estatal=sum(total)) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))
 
tempo$escol <- factor(tempo$escol,levels=c("Primaria","Secundaria","Bachillerato","Carrera técnica","Licenciatura","Posgrado", "Sin información"))
tempo$grupo_edad <- factor(tempo$grupo_edad,levels=c("Menores de 18 años","De 18 a 23 años","De 24 a 29 años","De 30 a 44 años",
                                                     "De 45 a 59 años","De 60 años o más"))

ggplot(tempo,aes(x=grupo_edad,y=escol,fill=porcentaje)) +
        geom_tile(color="black") + geom_text(aes(label=paste0(porcentaje,"%"))) +
        scale_fill_continuous(low="#bfd3e6", high="#6e016b") +
        labs(title="Porcentaje de las víctimas declaradas como desaparecidas por grupo de edad y escolaridad", subtitle= "",
             x="", y="", fill="Porcentaje", caption="") +
        coord_fixed() +
        theme_bw() + 
        theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
              plot.caption = element_text(color= "black", size=9,face="bold" ))

ggsave(paste0(grafs,"/7_edad_escol",".svg"), width = 12, height =12)

# Porcentaje de víctimas por sexo y grupos de edad
tempo <- data %>% 
         mutate(total=1) %>% 
         group_by(escol, grupo_edad, sexo) %>%
         filter(grupo_edad!="S/D") %>%
         filter(escol!="Sin información") %>%
         filter(sexo!="No Especificado") %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$grupo_edad <- factor(tempo$grupo_edad,levels=c("Menores de 18 años","De 18 a 23 años","De 24 a 29 años","De 30 a 44 años",
                                                     "De 45 a 59 años","De 60 años o más"))
ggplot(tempo) +
        geom_mosaic(aes(weight=porcentaje,x = product(sexo, grupo_edad), fill=sexo), na.rm=TRUE) +
        labs(title="Porcentaje de las víctimas declaradas como desaparecidas por sexo y grupos de edad",
             x="",y="",fill="") +
        theme_bw () +
        theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72"),
              plot.caption = element_text(color= "black", size=9,face="bold" ),
        axis.text.x = element_text(color="black", angle=90, size=10, hjust=1))

ggsave(paste(grafs,"8_edad_sexo.svg",sep="/"),width = 22,height = 12)


## Lugar de desaparición
tempo <- data %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(lugar) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

ggplot(tempo, aes(area=porcentaje, fill=lugar, label=paste(lugar, "\n", paste0("(",porcentaje,"%", ")")))) + 
        geom_treemap() +  
        scale_fill_manual(values=c("#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#4575b4")) +
        geom_treemap_text(place = "centre", grow = F, color ="black") +
        labs(title="Lugar donde ocurrió la desaparición de la víctima", caption="") +
        theme_bw(base_size = 10) +
        theme(title = element_text(color = "black", size=11, hjust=0, face="bold", family="Bodoni 72"),
              plot.caption = element_text(color= "black", size=9,face="bold", family = "Bodoni 72")) +
        coord_fixed() 

ggsave(paste(grafs,"9_lugar.svg",sep="/"),width = 22,height = 12)


########### Barras #############

## ¿Alguién presenció el momento de la desaparición?
tempo <- data %>% 
          mutate(total=1,total_estatal=sum(total)) %>% 
          group_by(testigo) %>%
          summarize(total=sum(total)) %>% 
          ungroup() %>% 
          mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$testigo <- factor(tempo$testigo,levels=c("Sí", "No", "Se desconoce"))

ggplot(tempo, aes(x = reorder(testigo,-porcentaje), y= porcentaje, fill=testigo)) +
        geom_bar(stat="identity", position="dodge") + 
        geom_text(aes(label=porcentaje), position=position_dodge(width=1), vjust=-1) + 
        coord_flip()+
        labs(title ="¿Alguien presenció el momento en que la persona fue privada de su libertad?", 
             subtitle="", x = "", y = "", fill = "", caption = "") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90, hjust =1),
              title  = element_text(face="bold", size=14, family = "Bodoni 72")) 

ggsave(paste(grafs,"10_testigos.svg",sep="/"),width = 22,height = 12)


##Desaparición individual o colectiva
tempo <- data %>% 
        mutate(total=1,total_estatal=sum(total)) %>% 
        group_by(tipo_desap) %>%
        summarize(total=sum(total)) %>% 
        ungroup() %>% 
        mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$tipo_desap <- factor(tempo$tipo_desap,levels=c("Se desconoce","Individual", "Colectiva"))

ggplot(tempo, aes(x = tipo_desap, y= porcentaje, fill=tipo_desap)) +
        geom_bar(stat="identity", position="dodge") + 
        geom_text(aes(label=porcentaje), position=position_dodge(width=1), vjust=-1) + 
        scale_fill_manual(values=c("#1b7837","#762a83", "#d73027")) +
        labs(title ="El hecho ocurrió de manera...", 
             subtitle="", x = "", y = "Porcentaje", fill="" , caption = "") + 
        theme_minimal() + coord_flip()
        theme(axis.text.x = element_text(angle=90, hjust =1),
              title  = element_text(face="bold", size=14, family = "Bodoni 72"))

ggsave(paste(grafs,"11_tipo_desap.svg",sep="/"),width = 22,height = 12)


##¿Ocurrió un desplazamiento después de la desaparición?
tempo <- data %>% 
        mutate(total=1,total_estatal=sum(total)) %>% 
        group_by(despl) %>%
        summarize(total=sum(total)) %>% 
        ungroup() %>% 
        mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo$despl <- factor(tempo$despl, levels=c("Sí", "No", "No respondió"))

ggplot(tempo, aes(x = reorder(despl, -porcentaje), y= porcentaje, fill= despl)) +
        geom_bar(stat="identity") + 
        geom_text(aes(label=porcentaje), position=position_stack(vjust = 0.5)) + 
        labs(title ="¿Ocurrió un desplazamiento por motivo de la desaparición?", 
             subtitle="", x = "", y = "Porcentaje", fill = "", caption = "") + 
        theme_bw() + coord_flip()
        theme(axis.text.x = element_text(angle=180, hjust =1, face = "bold", family="Bodoni 72"),
              title  = element_text(face="bold", size=14)) 

ggsave(paste(grafs,"12_desplazamiento.svg",sep="/"),width = 22,height = 12)


########################
#  Mapas y municipios  #
########################

p_load(ggrepel,sf,ggpubr)

##Abrimos mapa
shape_mun <- st_read(paste(shp,"MUNICIPIOS.shp",sep="/"), stringsAsFactors = F)
shape_mun<-filter(shape_mun, CVE_ENT==30)

shape_mun$NOM_MUN <- gsub("\xe1", "á", shape_mun$NOM_MUN)
shape_mun$NOM_MUN <- gsub("\xe9", "é", shape_mun$NOM_MUN)
shape_mun$NOM_MUN <- gsub("\xed", "í", shape_mun$NOM_MUN)
shape_mun$NOM_MUN <- gsub("\xf3", "ó", shape_mun$NOM_MUN)
shape_mun$NOM_MUN <- gsub("\xf1", "ñ", shape_mun$NOM_MUN)
shape_mun$NOM_MUN <- gsub("\xfa", "ú", shape_mun$NOM_MUN)

tempo <- data %>%
        group_by(municipio_de_desaparicion)%>% 
        mutate(total=1,total_estatal=sum(total)) %>% 
        summarize(total=sum(total)) %>% 
        ungroup() 


mun <- shape_mun %>% 
       left_join(.,tempo,by=c("NOM_MUN"="municipio_de_desaparicion"))


#Graficamos en el mapita

ggplot(mun) +
      geom_sf(aes(fill=total),color="white") +
      scale_fill_viridis_c(direction=-1,option="viridis") +
      labs(title="Total de desapariciones por municipio",
           subtitle="municipio donde ocurrió la desaparición", fill="") +
      theme_bw() +
      coord_sf()

ggsave(paste(grafs,"13_municipio.png",sep="/"),width = 22,height = 12)

#Barras
tempo <- data %>%
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(municipio_de_desaparicion)%>% 
         summarize(total=sum(total)) %>% 
         ungroup() 

tempo <- filter(tempo, municipio_de_desaparicion!="", municipio_de_desaparicion!="No respondió", !is.na(municipio_de_desaparicion))

ggplot(tempo, aes(x = reorder(municipio_de_desaparicion, -total), y = total, fill = municipio_de_desaparicion)) +
       geom_bar(stat="identity") + 
       geom_text(aes(label=total), position=position_stack(vjust = 0.5)) + 
       labs(title ="Total de desapariciones por municipio", 
            subtitle="Municipio donde se declara la desaparición de la víctima", x = "", y = "", fill="") + 
       theme_bw() + 
       coord_flip() +
       theme(axis.text.x = element_text(angle=0, hjust =1, face = "bold"),
                  title  = element_text(face="bold", size=14, family="Bodoni 72")) +
      guides(fill=F)

ggsave(paste(grafs,"13.1_municipios.svg",sep="/"),width = 22,height = 12)

##Por municipio de residencia 
tempo <- data %>%
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(municipio_de_residencia)%>% 
         summarize(total=sum(total)) %>% 
         ungroup() 

tempo <- filter(tempo, municipio_de_residencia!="", municipio_de_residencia!="No respondió", !is.na(municipio_de_residencia))

ggplot(tempo, aes(x = reorder(municipio_de_residencia, -total), y = total, fill = municipio_de_residencia)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=total), position=position_stack(vjust = 0.5)) + 
  labs(title ="Municipio de residencia de la víctima declarada como desaparecida", 
       subtitle="", x = "", y = "", fill="") + 
  theme_bw() + 
  coord_flip() +
  theme(axis.text.x = element_text(angle=0, hjust =1, face = "bold"),
        title  = element_text(face="bold", size=14, family="Bodoni 72")) +
  guides(fill=F)

ggsave(paste(grafs,"13.2_municipio_de_residencia.svg",sep="/"),width = 22,height = 12)


####Por años
tempo <- data %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(anio_ocur) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))

tempo <- filter(tempo, anio_ocur!="", anio_ocur!="No respondió", !is.na(anio_ocur))

ggplot(tempo, aes(x = anio_ocur, y = porcentaje, fill = porcentaje)) +
        geom_bar(stat="identity") + 
        geom_text(aes(label=porcentaje), position=position_stack(vjust = 0.5)) + 
        labs(title ="Porcentaje de desapariciones por año de ocurrencia", 
             subtitle="Año en el que ocurre la desaparición", x = "", y = "", fill="") + 
        theme_bw() + 
        coord_flip() +
        theme(axis.text.x = element_text(angle=0, hjust =1, face = "bold"),
              title  = element_text(face="bold", size=14, family="Bodoni 72")) +
        guides(fill=F)

ggsave(paste(grafs,"17_anio_ocurrencia.svg",sep="/"),width = 22,height = 12)



#### Autoridades ####
prueba <- select(data, c(perpetrador, aut1:aut7))
prueba$id <- 1:nrow(prueba)

prueba <- gather(prueba, autoridad, quien, aut1:aut7)
prueba <- filter(prueba, perpetrador!="No respondió")

aver <- prueba %>%
        mutate(total=1,total_estatal=sum(total),quien=str_trim(quien,side=c("both"))) %>% 
        group_by(quien)%>% 
        summarize(total=sum(total)) %>% 
        ungroup() 
  
aver <- filter(aver, quien!="", quien!="No respondió", !is.na(quien))

ggplot(aver, aes(x = reorder(quien, -total), y = total, fill = quien)) +
       geom_bar(stat="identity") + 
       geom_text(aes(label=total), position=position_stack(vjust = 0.5)) + 
       labs(title ="¿Quién cree que pudo haber participado en la desaparición?", 
            subtitle="", x = "", y = "", caption = "", fill="") + 
       theme_bw() + coord_flip()
       theme(axis.text.x = element_text(angle=180, hjust =1, face = "bold"),
             title  = element_text(face="bold", size=14, family="Bodoni 72")) 
       
ggsave(paste(grafs,"14_autoridad_totales.svg",sep="/"),width = 22,height = 12)
 
      
           
#Ahora con porcentajes 
aver <- filter(aver, quien!="", quien!="No respondió", !is.na(quien))

aver <- prueba %>%
        mutate(total=1,total_estatal=sum(total),quien=str_trim(quien,side=c("both"))) %>% 
        group_by(quien)%>% 
        summarize(total=sum(total)) %>% 
        ungroup() %>%
        mutate(total_estatal=sum(total), porcentaje=round(total*100/158, digits = 2))

aver <- filter(aver, quien!="", quien!="No respondió", !is.na(quien))
 
ggplot(aver, aes(x = reorder(quien, -porcentaje), y= porcentaje, fill= quien)) +
       geom_bar(stat="identity") + 
       geom_text(aes(label=porcentaje), position=position_stack(vjust = 0.5)) + 
       labs(title ="¿Quién cree que pudo haber participado en la desaparición?", 
             subtitle="Porcentaje de las autoridades declaradas como sospechosas en la desaparición por parte de la persona declarante", 
             x = "", y = "", caption = "Las columnas pueden sumar más de 100% porque indican más de una opción", fill="") + 
       theme_bw() + coord_flip() +
       theme(title = element_text(color = "black", size=11, hjust=0, face="bold", family="Bodoni 72"),
             plot.caption = element_text(color= "black", size=9, family = "Bodoni 72")) +

ggsave(paste(grafs,"15_autoridad_porcentajes.svg",sep="/"),width = 22,height = 12)



###
# Por último, queremos saber si las personas desaparecen en su mismo municipio de residencia
###

data$mismo_mun <- ifelse(data$municipio_de_desaparicion==data$municipio_de_residencia, T, F)

data <- mutate(data,mismo_mun=case_when(mismo_mun=="TRUE" ~ "Si",
                                        mismo_mun=="FALSE" ~ "No"))

tempo <- data %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(mismo_mun) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))  

ggplot(tempo, aes(area=porcentaje, fill=mismo_mun, label=paste(mismo_mun, "\n", paste0("(",porcentaje,"%", ")")))) + 
       geom_treemap() +  scale_fill_manual(values=c("#fc8d59", "#ffffbf", "#91cf60")) +
       geom_treemap_text(place = "centre", grow = F, color ="black") +
       labs(title="¿La víctima fue desaparecida en su mismo municipio de residencia?", 
            subtitle="El municipio de residencia es el mismo que el municipio de desaparición", fill="") +
       theme_bw(base_size = 10) +
       theme(plot.title = element_text(color = "black", size=11, hjust=0, face="bold", family = "Bodoni 72")) +
       coord_fixed() +
       guides(fill=F)
      
      ggsave(paste0(grafs,"/16_mismo_muncipio",".svg"), width = 12, height =12)

##### Una más, autoridades y "otros"
pruebas <- data 
pruebas <- select(data, perpetrador)
pruebas$id <- 1:nrow(pruebas)

pruebas <- pruebas %>%
           filter(str_detect(perpetrador, "Otro"))


aver <-  pruebas %>% 
         mutate(total=1,total_estatal=sum(total)) %>% 
         group_by(perpetrador) %>%
         summarize(total=sum(total)) %>% 
         ungroup() %>% 
         mutate(total_estatal=sum(total),porcentaje=round(total*100/total_estatal,digits=2))  


ggplot(aver, aes(x = perpetrador, y = porcentaje, fill = porcentaje)) +
       geom_bar(stat="identity") + 
       geom_text(aes(label=porcentaje), position=position_stack(vjust = 0.5)) + 
       labs(title ="¿Quién cree que pudo haber participado en la desaparición?", 
            subtitle="Participación de particulares y agentes del Estado", x = "", y = "", caption = "", fill="") + 
       theme_bw() + coord_flip()
       theme(axis.text.x = element_text(angle=180, hjust =1, face = "bold"),
            title  = element_text(face="bold", size=14, family="Bodoni 72")) 
      
      ggsave(paste(grafs,"18_autoridad_otros.svg",sep="/"),width = 22,height = 12)


