# Análisis de desarrollos verticales en Tijuana


knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

library(easypackages)
my_packages <- c("readxl", "tidyverse", "ggmap", "xtable")
libraries(my_packages)

datos <- read_excel("Bd_verticales_tij.xlsx")


#########################################################
#GRAFICAS DE DESARROLLOS VERTICALES REPORTE##############
#########################################################

##############
#MAPA#########
##############

tj <- c(-117.021211, 32.518561)
tj_map <- get_map(location = tj, source = "google", maptype = "roadmap", zoom = 12)

dt <- datos %>%
  mutate_all(funs(toupper)) %>%
  select(nombre_desarrollo, Latitud, Longitud) %>%
  filter(!nombre_desarrollo %in% c("Bajalta")) %>%
  mutate(Latitud = as.numeric(Latitud),
         Longitud = as.numeric(Longitud)) %>%
  filter(!is.na(Latitud))


ggmap(tj_map, extent = "panel", maprange = FALSE) + 
  geom_density2d(data = dt, aes(x = Longitud, y = Latitud)) + 
  stat_density2d(data = dt, aes(x = Longitud, y = Latitud, alpha = ..level.., fill = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  geom_point(data = dt, aes(x = Longitud, y = Latitud, colour = factor(nombre_desarrollo))) + 
  theme_minimal() + 
  ggtitle("Desarrollos verticales en Tijuana") + 
  theme(legend.position = "none", plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666", size = 11))

ggsave(filename = "map_densidad.jpg")

ggsav

ggmap(tj_map, extent = "panel") + 
  geom_point(data = dt, aes(x = Longitud, y = Latitud, colour = factor(nombre_desarrollo))) +
  geom_density2d(data = dt, aes(x = Longitud, y = Latitud, position = "identity")) +
  scale_color_manual(values = c("#ca2128", "#f6932f", "#6ebe4c", "#2e9fd9", "#a74e9d", "#6f3da3", "#a0121d", "#ed6223", "#2b9245", "#0d709a", "#862884", "#4f2584", "#797273", "#540e11", "#2128ca"), name = "Desarrollo") +
  ggtitle("Desarrollos verticales en Tijuana") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666", size = 11)) +
  theme(legend.title = element_text(family = "Helvetica", face = "bold", color = "#666666", size = 10))

ggsave(filename = "map_desarrollos.png")


######################################
#COSTO MEDIO DEL DEPARTAMENTO#########
######################################

cm_dep <- dat %>%
  mutate(precio_en_dolares = as.numeric(precio_en_pesos)) %>%
  filter(!is.na(precio_en_dolares)) %>%
  mutate(nombre_desarrollo = toupper(nombre_desarrollo)) %>%
  group_by(nombre_desarrollo) %>%
  summarise(m.price = mean(precio_en_dolares)) %>%
  ggplot(aes(x = reorder(nombre_desarrollo, m.price), m.price)) +
  geom_col(aes(fill = m.price)) +
  coord_flip() +
  ggtitle("Costo medio del departamento en Dólares") +
  scale_y_continuous(breaks = c(0, 2500000, 5000000, 7500000, 10000000, 12500000), labels = c("0","250k", "500k", "750k",  "1mi.", "1,25mi.")) +
  ylab(label = NULL) + 
  xlab(label = NULL) +
  theme_minimal() + 
  theme(legend.position = "none",plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666"), axis.text = element_text(family = "Helvetica", face = "bold"))

ggsave(filename = "cm_dep.png")

######################################
#COSTO MEDIO POR METRO CUADRADO#########
######################################

cm_depm2 <- dat %>%
  select(nombre_desarrollo, metros_cuadrados, precio_en_dolares) %>%
  mutate(precio = as.numeric(precio_en_dolares),
         metros = as.numeric(metros_cuadrados),
         pxm = precio / metros) %>%
  mutate(nombre_desarrollo = toupper(nombre_desarrollo)) %>%
  filter(!is.na(pxm)) %>%
  group_by(nombre_desarrollo) %>%
  summarise(m.price = mean(pxm, na.rm = TRUE))
  ggplot(aes(x = reorder(nombre_desarrollo, m.price), y = m.price)) +
  geom_col(aes(fill = m.price)) +
  coord_flip() +
  xlab(label = NULL) +
  ylab(label = NULL) +
  ggtitle("Costo medio por metro cuadrado") + 
  scale_y_continuous( breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000), labels = c("0", "500", "1,000" ,"1,500", "2,000", "2,500", "3,000")) + 
  theme_minimal() + 
  theme(legend.position = "none", plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666"), axis.text = element_text(family = "Helvetica", face = "bold"))

ggsave(filename = "cm_depm2.jpg")

######################################
#COMPARATIVO AMENIDADES###############
######################################

cm_amen <- dat %>%
  select(-desarrollador, -nombre_de_la_tipologia, -numero_de_habitaciones, -Niveles_departamento, -metros_cuadrados, -T.C., -precio_en_pesos, -precio_en_dolares, -pesos_por_metro_cuadrado, -numero_de_unidades) %>%
  filter(!nombre_desarrollo %in% c("Horizonte")) %>%
  mutate(nombre_desarrollo = toupper(nombre_desarrollo)) %>%
  distinct() %>%
  gather(amenidad, numero, 2:44) %>%
  group_by(amenidad) %>%
  mutate(n = sum(numero)) %>%
  ungroup() %>%
  group_by(nombre_desarrollo) %>%
  summarise(n2 = sum(numero)) %>%
  ggplot(aes(x = reorder(nombre_desarrollo, n2), y = n2)) + 
  geom_col(aes(fill = n2)) +
  coord_flip() +
  ylab(label = NULL) +
  xlab(label = NULL) +
  theme_minimal() + 
  theme(legend.position = "none", plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666"), axis.text = element_text(family = "Helvetica", face = "bold")) +
  ggtitle("Desarrollos con más amenidades")

ggsave(filename = "cm_amen.png")

######################################
#COMPARATIVO AMENIDADES###############
######################################

r_price_am <- dat %>%
  select(-desarrollador, -nombre_de_la_tipologia, -numero_de_habitaciones, -Niveles_departamento, -metros_cuadrados, -T.C., -precio_en_pesos, -pesos_por_metro_cuadrado, -numero_de_unidades) %>%
  group_by(nombre_desarrollo) %>%
  mutate(precio_en_dolares = as.numeric(precio_en_dolares),
         m.price = mean(precio_en_dolares, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(amenidad, valor, c(Alberca, sun_deck, asadores, suites_de_huespedes, salon_de_juegos, lounge, terraza, gimnasio, lavanderia, elevadores_de_alta_velocidad, estacionamiento_para_visitas, bodegas, sky_deck, salon_de_usos_multiples, fogateros, cafe_y_patio, area_de_carga_y_descarga, recepcion_con_front_desk, concierge, tecnologia_residencial, control_de_acceso, Raquetbol, Jacuzzi, Sauna, corredor_de_lectura, zona_barre, studio_yoga, cancha_de_tenis, area_de_bar_con_tv_para_eventos_deportivos, Room_services_de_cocina_privada, Sistema_de_audio_bose_en_areas_comunes, area_de_juegos_para_ninos, guarderia, pet_yard, acondicionado_para_silla_de_ruedas, `WI-FI gratis en espacios comunes`, alberca_con_tobogan, salon_para_tareas, pista_de_patinaje, chapoteadero_para_bebes, Brincolin, Circuito_de_crossfit, terraza_para_ejercicio_al_aire_libre)) %>%
  select(-precio_en_dolares) %>%
  filter(valor != 0) %>%
  group_by(nombre_desarrollo, amenidad) %>%
  mutate(m = sum(valor),
         tot = 43,
         prc = m / tot) %>%
  ungroup() %>%
  mutate(nombre_desarrollo = toupper(nombre_desarrollo)) %>%
  select(-valor, -m,-tot, -amenidad) %>%
  ggplot(aes(m.price, prc)) + geom_point(aes(colour = nombre_desarrollo)) +
  geom_smooth(method = "lm") +
  xlab(label = "Precio promedio") +
  ylab(label = "Porcentaje de amenidades respecto al total") + 
  theme_minimal() +
  ggtitle("Relación precio promedio y número de amenidades") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666"), axis.text = element_text(family = "Helvetica"), legend.position = "none")

ggsave(filename = "r_price_am.png")

######################################
#PRINCIPALES AMENIDADES###############
######################################

p_amen <- dat %>% select(-desarrollador, -nombre_de_la_tipologia, -numero_de_habitaciones, -Niveles_departamento, -metros_cuadrados, -T.C., -precio_en_pesos, -precio_en_dolares, -pesos_por_metro_cuadrado, -numero_de_unidades) %>%
  gather(amenidad, valor, c(Alberca, sun_deck, asadores, suites_de_huespedes, salon_de_juegos, lounge, terraza, gimnasio, lavanderia, elevadores_de_alta_velocidad, estacionamiento_para_visitas, bodegas, sky_deck, salon_de_usos_multiples, fogateros, cafe_y_patio, area_de_carga_y_descarga, recepcion_con_front_desk, concierge, tecnologia_residencial, control_de_acceso, Raquetbol, Jacuzzi, Sauna, corredor_de_lectura, zona_barre, studio_yoga, cancha_de_tenis, area_de_bar_con_tv_para_eventos_deportivos, Room_services_de_cocina_privada, Sistema_de_audio_bose_en_areas_comunes, area_de_juegos_para_ninos, guarderia, pet_yard, acondicionado_para_silla_de_ruedas, `WI-FI gratis en espacios comunes`, alberca_con_tobogan, salon_para_tareas, pista_de_patinaje, chapoteadero_para_bebes, Brincolin, Circuito_de_crossfit, terraza_para_ejercicio_al_aire_libre)) %>% filter(valor != 0 ) %>%
  distinct() %>%
  group_by(amenidad) %>%
  summarise( m = sum(valor)) %>%
  arrange(desc(m)) %>%
  filter(m > 3) %>%
  ggplot(aes(x = reorder(amenidad, m), y = m)) + geom_col(aes(fill = m)) +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "none") + 
  ylab(label = NULL) + 
  xlab(label = NULL) +
  scale_x_discrete(labels = c("gimnasio" = "Gimnasio", "terraza" = "Terraza", "salon_de_juegos" = "Salón de juegos", "asadores" = "Asadores", "Alberca" = "Alberca", "sun_deck" = "Sun Deck", "control_de_acceso" = "Control de acceso", "fogateros" = "Fogateros", "bodegas" = "Bodegas", "area_de_juegos_para_ninos" = "Área de juegos infantiles", "salon_de_usos_multiples" = "Salón de usos múltiples", "lounge" = "Lounge", "recepcion_con_front_desk" = "Front Desk", "cafe_y_patio" = "Café y Patio", "area_de_bar_con_tv_para_eventos_deportivos"= "Área de bar con Tv")) +
  ggtitle("Principales amenidades") + 
  theme(plot.title = element_text(family = "Helvetica", face = "bold", color = "#666666"), axis.text = element_text(family = "Helvetica"))

ggsave(filename = "p_amen.jpg")




  addtorow2 <- list()
addtorow2$pos <- list(0)
addtorow2$command <- c("n & Amenidades & Frecuencia\\\\\n")

tabla3 <- xtable(dat %>% select(-desarrollador, -nombre_de_la_tipologia, -numero_de_habitaciones, -Niveles_departamento, -metros_cuadrados, -T.C., -precio_en_pesos, -precio_en_dolares, -pesos_por_metro_cuadrado, -numero_de_unidades) %>%
                   gather(amenidad, valor, c(Alberca, sun_deck, asadores, suites_de_huespedes, salon_de_juegos, lounge, terraza, gimnasio, lavanderia, elevadores_de_alta_velocidad, estacionamiento_para_visitas, bodegas, sky_deck, salon_de_usos_multiples, fogateros, cafe_y_patio, area_de_carga_y_descarga, recepcion_con_front_desk, concierge, tecnologia_residencial, control_de_acceso, Raquetbol, Jacuzzi, Sauna, corredor_de_lectura, zona_barre, studio_yoga, cancha_de_tenis, area_de_bar_con_tv_para_eventos_deportivos, Room_services_de_cocina_privada, Sistema_de_audio_bose_en_areas_comunes, area_de_juegos_para_ninos, guarderia, pet_yard, acondicionado_para_silla_de_ruedas, `WI-FI gratis en espacios comunes`, alberca_con_tobogan, salon_para_tareas, pista_de_patinaje, chapoteadero_para_bebes, Brincolin, Circuito_de_crossfit, terraza_para_ejercicio_al_aire_libre)) %>% filter(valor != 0 ) %>%
                   distinct() %>%
                   group_by(amenidad) %>%
                   summarise( m = sum(valor)) %>%
                   arrange(desc(m)) %>%
                   mutate(amenidad = fct_recode(amenidad ,
                                                "suites_de_huespedes"                    = "Suites para huéspedes",
                                                "lavanderia"                             = "Lavandería",
                                                "elevadores_de_alta_velocidad"           = "Elevadores de alta velocidad",
                                                "estacionamiento_para_visitas"           = "Estacionamiento para visitas",
                                                "sky_deck"                               = "Sky Deck",
                                                "corredor_de_lectura"                    = "Corredor de Lectura",
                                                "zona_barre"                             = "Zona Barré" ,
                                                "studio_yoga"                            = "Studio yoga",
                                                "cancha_de_tenis"                        = "Cancha de tenis",
                                                "Room_services_de_cocina_privada"        = "Room service con cocina privada" ,
                                                "Sistema_de_audio_bose_en_areas_comunes" = "Audio BOSE en áreas comúnes" ,
                                                "guarderia"                              = "Guardería",
                                                "pet_yard"                               = "Pet yard",
                                                "acondicionado_para_silla_de_ruedas"     = "Acondicionado para silla de ruedas",
                                                "alberca_con_tobogan"                    = "Alberca con Tobogan",
                                                "salon_para_tareas"                      = "Salón de tareas" ,
                                                "pista_de_patinaje"                      = "Pista de patinaje",
                                                "chapoteadero_para_bebes"                = "Chapoteadero para bebés",
                                                "Brincolin"                              = "Brincolín",
                                                "Circuito_de_crossfit"                   = "Circuito de Crossfit",
                                                "terraza_para_ejercicio_al_aire_libre"   = "Terraza para ejercicio al aire libre" )), caption = "Amenidades con menor frecuencia", digits = 0)

print.xtable(tabla3, caption.placement = "top", comment = FALSE, format.args = list(big.mark = ","), include.colnames = FALSE)