library(dplyr)
library(stringr)
library(tidyr)

source('~/6_varios/alap/src/lecturas.R', echo=TRUE)
 
# sesiones_2012 <- lecturas_2012 %>% 
#   filter(codigo_sesion == 'SR') %>% 
#   mutate(nombre= str_extract_all(autores_lectura, "^.+(?=,)"))

# Sesiones 2010: 45
# Sesiones 2012: 63
# Sesiones 2014: 56

sesiones <- rbind(lecturas_2014, lecturas_2012, lecturas_2010) %>% 
  filter(codigo_sesion == 'SR')
  

pba <- sesiones %>% 
  select(id_sesion, anio, lectura) %>% 
  bind_cols(str_split_fixed(sesiones$autores_lectura, ";", n=Inf) %>% as_tibble()) %>% 
  pivot_longer(4:14, names_to = "nro_autor", values_to = "autores_aux") %>% 
  filter(autores_aux != "") %>% 
  mutate(nro_autor = str_extract(nro_autor, "\\d+"),
         apellido  = str_extract(autores_aux, "^[^,]+(?=,)"),
         nombre    = str_extract(autores_aux, "(?<=,)[^,]+(?=,)"),
         filiacion = str_sub(autores_aux, nchar(apellido) + nchar(nombre) + 3, nchar(autores_aux)),
         filiacion = case_when(anio == '2010' ~ str_extract(autores_aux, "(?<=\\().+(?=\\))"),
                               TRUE ~ filiacion),
         nombre    = case_when(anio == '2010' ~ str_extract(autores_aux, "(?<=,)[^[:punct:]]+(?=[:punct:])"),
                               TRUE ~ nombre))


sesiones %>% select(id_sesion, anio) %>% distinct() %>% group_by(anio) %>% summarise(n = n())

# Falta 1 sesion de 2010 y 3 de 2012

