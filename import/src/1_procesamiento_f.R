rm(list=ls())
setwd("~")

#################################################
# Procesamiento de testimonios *la buena*       #
# Autor: Mariana S.                             #          
# Junio 2019                                    #          
################################################

require(pacman)
p_load(tidyverse,stringi,lubridate,janitor,anytime)

# Personal
inp <- "/Users/marianasolano/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/inp"
out <- "/Users/marianasolano/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/out"

# Oficina
inp <- "/Users/dc_adrix/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/inp" 
out <- "/Users/dc_adrix/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/Memoria (Testimonios)/Datos/out" 

# Abrimos la base
data <- read.csv(paste0(inp,"/Datos-Testimonios.csv"),fileEncoding = "UTF-8", stringsAsFactors = F) %>% 
        clean_names()

# Nos quedamos con las preguntas de la primera versión del cuestionario + la 56
data <- select(data, participa_en_alguna_organizacion, nombre_de_la_organizacion, nombre_de_la_victima:
                     quien_cree_que_pudo_haber_participado_en_la_desaparicion_desaparicio_n_cometida_por_particulares)


#------- Chequemos y recodifiquemos —-----#

# Colectivo
data$nombre_de_la_organizacion <- str_to_title(data$nombre_de_la_organizacion)
data <- data %>% 
         mutate(nombre_de_la_organizacion=case_when(str_detect(nombre_de_la_organizacion,"Solecito|Solectio") == T ~ "Solecito",
                                             str_detect(nombre_de_la_organizacion,"Belén|González") == T ~ "Madres en Búsqueda Belén González",
                                             str_detect(nombre_de_la_organizacion,"Madres En Búsqueda") == T ~ "Madres en Búsqueda Belén González",
                                             str_detect(nombre_de_la_organizacion,"Fbmh|Buscadores|Herrera|Fbmh") == T ~ "Familiares en Búsqueda María Herrera",
                                             str_detect(nombre_de_la_organizacion,"Buscando A Nuestros|Grupo Buscando") == T ~ "Buscando a Nuestros Desaparecidos y Desaparecidas",
                                             str_detect(nombre_de_la_organizacion,"Orizaba|Familias De Desaparecidos") == T ~ "Familias de Desaparecidos Orizaba-Córdoba",
                                             nombre_de_la_organizacion=="" ~ "Sin información",
                                             TRUE ~ "Otros"))

data <- rename(data,colectivo=nombre_de_la_organizacion)

data <- rename(data,part_colectivo=participa_en_alguna_organizacion) %>% 
        mutate(part_colectivo=case_when(part_colectivo==""~"No Respondió",T~part_colectivo))

# Ocupación
data <- rename(data, ocupacion=a_que_se_dedicaba) %>% 
         mutate(ocupacion=case_when(str_detect(ocupacion,"Vende|Venta|Vendía|Comerc|Comerr") == T ~ "Comerciante",
                                    str_detect(ocupacion,"Estudia|Estudiante|Carrera De Medicina") == T ~ "Estudiante", 
                                    str_detect(ocupacion,"Empleado|Empleada|Trabajador|Trabajo|Almacen|
							  Compañía|En Un|Meser|Administr|Administr|
						          Servicio De|Estilista|Edecán|Reciclaje|
							  Repartidor|Tramitrador|Vigilante") == T ~ "Empleado particular",
                                    str_detect(ocupacion,"Trailero|Taxista|Trailero|Fletero|Camiones|
							  Tráiler") == T ~ "Chofer",
                                    str_detect(ocupacion,"Electri|Eléctrico|Soldador|Mecánico|
							  Automotriz|Reparación") == T ~ "Técnicos",
                                    ocupacion=="" ~ "Sin información",
                                    TRUE ~ "Otros")) 

# Sexo
table(data$sexo_de_la_victima)
data <- rename(data,sexo=sexo_de_la_victima) %>% 
        mutate(sexo=case_when(sexo=="Otro"~"No Especificado",T~sexo))

# Escolaridad
data <- rename(data,escol=nivel_educativo) %>% 
        mutate(escol=case_when(escol==""~"Sin información",T~escol))

# Estado civil 
data <- rename(data, edo_civil=estado_civil)%>% 
        mutate(edo_civil=case_when(edo_civil==""~"Sin información",T~edo_civil))

# Cálculo de la edad de la víctima  
data <- mutate(data,fecha_de_nacimiento=ymd(fecha_de_nacimiento),
         fecha_exacta_de_desaparicion=ymd(fecha_exacta_de_desaparicion),
         edad=interval(fecha_de_nacimiento,fecha_exacta_de_desaparicion),
         edad=time_length(edad,unit="year"))
data$edad <- round(as.numeric(data$edad),0)


# Año de desaparición
data$anio_ocur = substr(data$fecha_exacta_de_desaparicion, 1, 4)

# Lugar de desaparición
data <- rename(data,lugar=lugar_de_desaparicion) %>% 
        mutate(lugar=case_when(lugar==""~"Se desconoce",T~lugar))

# Alguien presenció el momento de la desaparición 
data <- rename(data,testigo=alguien_presencio_el_momento_en_el_que_la_persona_fue_privada_de_su_libertad) %>% 
        mutate(testigo=case_when(testigo==""~"Se desconoce",T~testigo))

# Hecho individual o colectivo
data <- rename(data,tipo_desap=fue_un_hecho_individual_o_colectivo) %>% 
        mutate(tipo_desap=case_when(tipo_desap==""~"Se desconoce",T~tipo_desap))

# Ocurrió un desplazamiento?
data <- rename(data,despl=ocurrio_un_desplazamiento) %>% 
        mutate(despl=case_when(despl==""~"No respondió",T~despl))

# Quién cree que pudo haber participado en la desaparición
data <- rename(data, perpetrador=quien_cree_que_pudo_haber_participado_en_la_desaparicion_desaparicion_forzada) %>% 
        mutate(perpetrador=case_when(perpetrador==""~"No respondió",T~perpetrador)) %>%
        separate(perpetrador, into=c("aut1", "aut2", "aut3", "aut4", "aut5", "aut6", "aut7"), sep=",", remove=F)
       
# Preparamos la base para guardarla
base <- select(data, part_colectivo, colectivo, sexo, edad, entidad_de_residencia:escol, ocupacion, estado_de_desaparicion, municipio_de_desaparicion, 
                     anio_ocur, lugar, testigo, tipo_desap, despl, perpetrador, aut1:aut7)

write.csv(base, paste0(out, "/base_testimonios.csv"), row.names = F, fileEncoding = "UTF-8")




#### Datos adicinales ###

#--- Sobre dependientes económicos
aver <- data %>%
        select(dependientes_economicos) %>%
        rename(familia=dependientes_economicos)

aver <- aver %>%
        group_by(familia) %>%
        ungroup() %>%
        mutate(familia=case_when(familia==""~"No respondió",T~familia)) %>%
        separate(familia, into=c("fam1", "fam2", "fam3", "fam4", "fam5", "fam6"), sep=",", remove=F)

aver <- aver %>%
        group_by(familia) %>%
        ungroup() %>%
        separate(fam1, into=c("tipo1", "dep1"), sep="-", remove=T) %>%
        separate(fam2, into=c("tipo2", "dep2"), sep="-", remove=T) %>%
        separate(fam3, into=c("tipo3", "dep3"), sep="-", remove=T) %>%
        separate(fam4, into=c("tipo4", "dep4"), sep="-", remove=T) %>%
        separate(fam5, into=c("tipo5", "dep5"), sep="-", remove=T) %>%
        separate(fam6, into=c("tipo6", "dep6"), sep="-", remove=T) 


prueba <- aver %>%
        mutate(total=1,total_estatal=sum(total),familia=str_trim(familia,side=c("both"))) %>% 
        group_by(familia)%>% 
        summarize(total=sum(total)) %>% 
        ungroup() 

#De las víctimas desaparecidas, 201 tenían dependientes económicos y 92 no
