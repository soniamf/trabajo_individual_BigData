
library(tidyverse)
library(sf)
library(tmap)
library(gt)
library(ggplot2)
library(ggplotify)
library(ggplotlyExtra)
library(patchwork)
library(plotly)
install.packages('vembedr')
library(vembedr)
library(tidyr)

my_url <- 'https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t26/p070/p01/serie/l0/02002.csv_bdsc?nocab=1'
curl::curl_download(my_url, './pruebas/datos_ine.csv')

medioambiente_datos <- rio::import('./pruebas/datos_ine.csv')

medioambiente_datos <- janitor::clean_names(medioambiente_datos)
names(medioambiente_datos)

df_wide <- medioambiente_datos %>%
  rename(sector_actividad = sector_de_actividad_economica) %>%
  rename(tipo_de_equipo = tipo_de_equipo_e_instalacion) %>%
  rename(ine_ccaa = comunidades_y_ciudades_autonomas)

df <- df_wide %>%
  filter(sector_actividad == 'Total actividades') %>%
  filter(tipo_de_equipo == 'INVERSIÓN TOTAL')



# MAPA. Inversión en protección ambiental en 2021 por comunidad autonoma. NO ME SALE, mapa fallido.... :(


df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias

df_geo_prov <- df_geo_prov %>%
  select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)

df_new <- df %>%
  filter(periodo == 2019) %>%
  filter(ambito_medioambiental == 'Total') %>%
  filter(ine_ccaa %in% c('01 Andalucía', '02 Aragón', '03 Asturias, Principado de', '04 Balears, Illes', '05 Canarias', '06 Cantabria', '07 Castilla y León', '08 Castilla - La Mancha', '09 Cataluña', '10 Comunitat Valenciana', '11 Extremadura', '12 Galicia', '13 Madrid, Comunidad de', '	
14 Murcia, Región de', '	
15 Navarra, Comunidad Foral de', '	
16 País Vasco', '17 Rioja, La')) %>%
  tidyr::separate(ine_ccaa, into = c("ine_ccaa", 'ine_ccaa.n'), sep = " 
", extra = "merge") %>%
  select(periodo, total, ine_ccaa, ambito_medioambiental)

df_ok <- left_join(df_geo_prov, df_new, by = c("ine_ccaa" = "ine_ccaa"))

p <- ggplot() + 
  geom_sf(data = df_ok, 
          aes(geometry = geometry, fill = total), 
          color = "white", size = 0.09) 

p + scale_fill_distiller(palette = 11, name = NULL) +
  labs(title = "Inversión en proteccion ambiental en 2021 " , caption = "Datos provenientes del INE") + theme_void()


p

# GRAFICO DE BARRAS
# Como afecto el covid-19 a la inversion en proteccion medioambiental en Aragon. Observamos la diferencia desde 2019, antes del covid,  hasta 2021, despues del covid.

df_4 <-  df %>% filter(ine_ccaa == "02 Aragón") %>%
  filter(ambito_medioambiental == 'Total')

p4 <- ggplot(data = df_4, 
             aes(x = reorder (periodo, total), y = total)) + 
  geom_bar(fill = "#82E0AA", stat = 'identity', position = 'dodge') + 
  labs(title = "Inversion en proteccion medioambiental en Aragón", 
       caption = "Datos provenientes del INE", 
       x = "Años", 
       y = "Inversión en euros") + 
  theme_light()

p4 + geom_smooth(color = "black") 

p4


#FACET
#evolucion 2015-2020 de la inversión en proteger y gestionar los residuos a nivel nacional.

df_3 <- df %>% 
  filter(ambito_medioambiental == 'Gestión de residuos') %>%
  filter(ine_ccaa == '') %>%
  filter(periodo %in% c(2015, 2016, 2017, 2018, 2019, 2020)) %>%
  select(ambito_medioambiental, ine_ccaa, periodo, total)

p3 <- ggplot(df_3, aes(x = '', y = total, color = 'ambito_medioambiental')) +
  geom_point() + 
  geom_smooth() + facet_wrap(vars(periodo), nrow = 3, ncol =3) + 
  labs(title = "Evolución de la inversion en la gestión de residuos", caption = "Datos provenientes del INE", x = "Años", y = "Inversión en euros")

ggplotly(p3)


p3


# TABLA
# que comunidad autonoma ha invertido mas en proteccion para el medioambiente en 2020

df_5 <- df %>% filter(periodo == 2020) %>% 
  top_n(1, total) %>% 
  group_by(ine_ccaa) %>% 
  select(ine_ccaa, periodo, total) %>% 
  ungroup()

img_locales <- c("./imagenes/foto.svg")

df_img <- cbind(df_5, img_locales)  

tt_img <- df_img %>% 
  gt() 

tt_img %>% 
  gt::text_transform(locations = cells_body(columns = c(img_locales)), 
                     fn = function(x){gt::local_image(x, height = 30)}) %>% 
  tab_header(title = "Comunidad Autonoma con mas gasto en proteccion ambiental en 2020") %>% 
  cols_label(periodo = "Año", total = "Inversion en euros", ine_ccaa = "Comunidad Autonoma", img_locales = "") %>% 
  opt_table_font(font = google_font("Fira Mono")) %>% 
  tab_options(column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") 



# TABLA 2. A modo resumen

df_7 <- df %>% 
  filter (ambito_medioambiental %in% c('Protección del aire y el clima', 'Protección y descontaminación de suelos, aguas subterráneas y superficiales
', 'Protección de la biodiversidad y los paisajes', 'Protección de la biodiversidad y los paisajes')) %>%
  filter(ine_ccaa %in% c('01 Andalucía', '02 Aragón', '03 Asturias, Principado de', '04 Balears, Illes', '05 Canarias', '06 Cantabria', '07 Castilla y León', '08 Castilla - La Mancha', '09 Cataluña', '10 Comunitat Valenciana', '11 Extremadura', '12 Galicia', '13 Madrid, Comunidad de', '	
14 Murcia, Región de', '	
15 Navarra, Comunidad Foral de', '	
16 País Vasco', '17 Rioja, La')) %>%
  filter(periodo %in% c(2018, 2019, 2020, 2021)) %>%
  select(ambito_medioambiental, ine_ccaa, periodo, total)

DT::datatable(df_7, filter = 'top', 
              
              options = list(pageLength = 7, autoWidth = TRUE ))

df_7 %>%
  
  tibble::as_tibble() %>%
  
  DT::datatable(filter = 'top', options = list(pageLength = 7, autoWidth = TRUE))
