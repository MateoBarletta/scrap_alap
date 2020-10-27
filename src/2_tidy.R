library(stringr)
library(dplyr)
library(tidyr)

source('~/6_varios/alap/src/1_scrap.R', echo=TRUE)

# Año 2010
particip_tidy_2010 <- particip_2010 %>% 
  transmute(raw = V1,
            digit = str_detect(V1, "\\d")) %>% 
  filter(digit==TRUE) %>% 
  mutate(completo  = str_replace_all(raw, pattern = "^[:blank:]+[:upper:]", replacement=""),
         identidad = str_extract_all(completo, pattern = ".+(?=;)")) %>% 
  transmute(nombres   = str_extract_all(identidad, pattern = "(?<=,).+"),
            apellidos = str_extract_all(identidad, pattern = ".+(?=,)"),
            sesion    = str_extract_all(completo, pattern = "(?<=;\\s).+(?=,)")) %>%
  separate(sesion, c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), sep=",\\s+") %>% 
  pivot_longer(3:11, values_to = "sesion") %>% 
  select(-name) %>% 
  filter(!is.na(sesion))

# sesiones_2010 <- sesiones_2010[-1,] 

sesiones_aux <- sesiones_2010[-1,] %>% 
  mutate(global = is.na(V2),
         tipo   = str_replace_all(V1, "\\s+", ""),
         codigo = case_when(global ==T & tipo=="Apertura/Opening" ~ "AP",
                            global ==T & tipo=="ConferenciaInaugural" ~ "CI",
                            global ==T & tipo=="ComunicaciónOral" ~ "CO",
                            global ==T & tipo=="Clausura/ClosingCeremony" ~ "CL",
                            global ==T & tipo=="SesióndePoster" ~ "PO",
                            global ==T & tipo=="Panel" ~ "PA",
                            global ==T & tipo=="Plenaria" ~ "PL",
                            global ==T & tipo=="SesionesRegulares" ~ "SR",
                            global ==T & tipo=="MesaRedonda" ~ "MR"))

clasif_sesiones <- sesiones_aux %>% 
  filter(is.na(V2)) %>% 
  transmute(codigo,
            tipo_sesion = V1)

for (i in 1:nrow(sesiones_aux)){
  
  if (is.na(sesiones_aux[i,'codigo']) == TRUE) {
    
    sesiones_aux[i,'codigo'] = sesiones_aux[i-1,'codigo']
    
  }
  
}

sesiones_tidy_2010 <- sesiones_aux %>% 
  filter(global==F) %>% 
  left_join(clasif_sesiones, by='codigo') %>% 
  transmute(sesion = str_c(codigo, ".", tipo),
            codigo, tipo_sesion,
            titulo = V2) %>% 
  distinct() 

sacar_2010 <- sesiones_tidy_2010 %>% 
  filter(sesion == 'CO.08') %>% 
  filter(titulo == "Salud y Envejecimiento en América Latina")

sesiones_tidy_2010 <- sesiones_tidy_2010 %>% 
  anti_join(sacar_2010)

df_2010 <- left_join(sesiones_tidy_2010, particip_tidy_2010, by='sesion') %>% 
  mutate(anio='2010')

rm(sacar_2010, sesiones_2010, particip_2010, sesiones_tidy_2010, particip_tidy_2010, sesiones_aux, i)

# Año 2012
ap_2012 <- particip_2012[12:nrow(particip_2012),] %>% 
  mutate(raw       = V1,
         completo  = str_replace_all(raw, pattern = "^[:upper:][:blank:]", replacement = ""),
         identidad = str_replace_all(completo, pattern = "\\s\\s.+", replacement = ""),
         ap        = str_detect(completo, pattern = "AP\\.")) %>%
  filter(ap==TRUE) %>% 
  filter(str_detect(raw, "^[:upper:][:blank:]")==F) %>% 
  transmute(nombres   = str_extract_all(identidad, "^.+\\s"),
            apellidos = str_extract_all(identidad, "\\s.+$"),
            sesion    = "AP.01")

particip_tidy_2012 <- particip_2012[12:nrow(particip_2012),] %>% 
  mutate(raw       = V1,
         completo  = str_replace_all(raw, pattern = "^[:upper:][:blank:]", replacement = ""),
         identidad = str_replace_all(completo, pattern = "\\s\\s.+", replacement = ""),
         ap        = str_detect(completo, pattern = "AP\\.")) %>%
  filter(ap==FALSE) %>% 
  transmute(nombres   = str_extract_all(identidad, pattern = "(?<=,).+"),
            apellidos = str_extract_all(identidad, pattern = ".+(?=,)"),
            sesion    = str_extract_all(completo, pattern = "\\s\\s.+")) %>%
  separate(sesion, c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), sep="\\s+") %>% 
  pivot_longer(3:11, values_to = "sesion") %>%
  mutate(filtro = nchar(sesion)) %>% 
  filter(filtro>0) %>% 
  select(-name, -filtro)  %>% 
  bind_rows(ap_2012) %>% 
  mutate(sesion = str_replace_all(sesion, " ", ""))

sesiones_aux <- sesiones_2012[3:nrow(sesiones_2012),] %>% 
  mutate(global = is.na(V2),
         tipo   = str_replace_all(V1, "\\s+", ""),
         codigo = case_when(global ==T & tipo=="Apertura/Opening" ~ "AP",
                            global ==T & tipo=="ConferenciaInaugural" ~ "CI",
                            global ==T & tipo=="ComunicaciónOral" ~ "CO",
                            global ==T & tipo=="Clausura/ClosingCeremony" ~ "CL",
                            global ==T & tipo=="Sesióndeposter" ~ "PO",
                            global ==T & tipo=="Panel" ~ "PA",
                            global ==T & tipo=="Plenaria" ~ "PL",
                            global ==T & tipo=="Sesiónregular" ~ "SR",
                            global ==T & tipo=="MesaRedonda" ~ "MR"))


for (i in 1:nrow(sesiones_aux)){
  
  if (is.na(sesiones_aux[i,'codigo']) == TRUE) {
    
    sesiones_aux[i,'codigo'] = sesiones_aux[i-1,'codigo']
    
  }
  
}

sesiones_tidy_2012 <- sesiones_aux %>% 
  filter(global==F) %>% 
  left_join(clasif_sesiones, by='codigo') %>% 
  transmute(sesion = str_c(codigo, ".", tipo),
            sesion = str_replace_all(sesion, " ", ""),
            codigo, tipo_sesion,
            titulo = V2) %>% 
  distinct() 

df_2012 <- left_join(sesiones_tidy_2012, particip_tidy_2012, by='sesion') %>% 
  mutate(anio='2012')

rm(sesiones_2012, particip_2012, sesiones_tidy_2012, particip_tidy_2012, sesiones_aux, i, ap_2012)


# Año 2014
particip_tidy_2014 <- particip_2014[6:nrow(particip_2014),] %>% 
  as_tibble() %>% 
  mutate(raw       = as.character(value),
         completo  = str_replace_all(raw, pattern = "^[:upper:][:blank:]", replacement = ""),
         identidad = str_replace_all(completo, pattern = "\\s\\s.+", replacement = "")) %>%
  transmute(nombres   = str_extract_all(identidad, pattern = "(?<=,).+"),
            apellidos = str_extract_all(identidad, pattern = ".+(?=,)"),
            sesion    = str_extract_all(completo, pattern = "\\s\\s.+")) %>%
  separate(sesion, c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), sep="\\s+") %>% 
  pivot_longer(3:11, values_to = "sesion") %>%
  mutate(filtro = nchar(sesion)) %>% 
  filter(filtro>0) %>% 
  select(-name, -filtro) %>%  
  mutate(sesion = str_replace_all(sesion, " ", ""))


sesiones_aux <- sesiones_2014[3:nrow(sesiones_2014),] %>% 
  mutate(global = is.na(V2),
         tipo   = str_replace_all(V1, "\\s+", ""),
         codigo = case_when(global ==T & tipo=="Apertura/Opening" ~ "AP",
                            global ==T & tipo=="ConferenciaInaugural" ~ "CI",
                            global ==T & tipo=="ComunicaciónOral" ~ "CO",
                            global ==T & tipo=="Clausura/ClosingCeremony" ~ "CL",
                            global ==T & tipo=="Sesióndeposter" ~ "PO",
                            global ==T & tipo=="Panel" ~ "PA",
                            global ==T & tipo=="Plenaria" ~ "PL",
                            global ==T & tipo=="Sesiónregular" ~ "SR",
                            global ==T & tipo=="MesaRedonda" ~ "MR",
                            global ==T & tipo=="Proyectosespeciales" ~ "SE"))


for (i in 1:nrow(sesiones_aux)){
  
  if (is.na(sesiones_aux[i,'codigo']) == TRUE) {
    
    sesiones_aux[i,'codigo'] = sesiones_aux[i-1,'codigo']
    
  }
  
}

sesiones_tidy_2014 <- sesiones_aux %>% 
  filter(global==F) %>% 
  left_join(clasif_sesiones, by='codigo') %>% 
  transmute(sesion = str_c(codigo, ".", tipo),
            sesion = str_replace_all(sesion, " ", ""),
            codigo, tipo_sesion,
            titulo = V2) %>% 
  distinct() 

df_2014 <- left_join(sesiones_tidy_2014, particip_tidy_2014, by='sesion') %>% 
  mutate(anio='2014')

rm(sesiones_2014, particip_2014, sesiones_tidy_2014, particip_tidy_2014, sesiones_aux, i)

df_sesiones_alap <- bind_rows(df_2010, df_2012, df_2014)

rm(df_2010, df_2012, df_2014)

# WriteXLS::WriteXLS(df_sesiones_alap, "data/df_sesiones_alap_2010_2014.xls")  
