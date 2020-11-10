tildes <- function(texto){
  chartr("áéíóú","aeiou", texto)
}

links <- lista_links_2012

lecturas <- tibble()

for (i in 1:length(links)){
  
  tabla <- paste0("www.alapop.org", links[i]) %>% 
    getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
    XML::readHTMLTable()
  
  ver <- tabla[[length(tabla)]] %>% 
    as_tibble()
  
  aux <-  rbind(ver[-1,3], "") %>% 
    transmute(aux_autores = V3)
  
  ver2 <- ver %>%
    cbind(aux) %>% 
    mutate(tipo_sesion   = str_extract(ver[[2,1]], ".+(?=:\\s?)"),
           nro_sesion    = str_extract(ver[[2,1]], "\\d\\d(?=\\s-)"),
           nro_sesion    = case_when(is.na(tipo_sesion) ~ str_extract(ver[[1,1]], "\\d\\d(?=\\s-)"), TRUE ~ nro_sesion),
           tipo_sesion   = case_when(is.na(tipo_sesion) ~ str_extract(ver[[1,1]], ".+(?=:\\s?)"), TRUE ~ tipo_sesion),
           tipo_sesion   = str_replace_all(tipo_sesion, "\\s", ""),
           tipo_sesion   = str_to_lower(tipo_sesion),
           tipo_sesion   = tildes(tipo_sesion),
           codigo_sesion = case_when(tipo_sesion == "sesionregular"        ~ "SR",
                                     tipo_sesion == "sesionesregulares"    ~ "SR",
                                     tipo_sesion == "sesiondeposter"       ~ "SP",
                                     tipo_sesion == "apertura/opening"     ~ "AO",
                                     tipo_sesion == "conferenciainaugural" ~ "CI",
                                     tipo_sesion == "mesaredonda"          ~ "MR",
                                     tipo_sesion == "sesiontematica"       ~ "ST",
                                     tipo_sesion == "proyectosespeciales"  ~ "PE",
                                     tipo_sesion == "comunicacionoral"     ~ "CO",
                                     tipo_sesion == "panel"                ~ "PN",
                                     tipo_sesion == "plenaria"             ~ "PL"),
           id_sesion = paste0(codigo_sesion, ".", nro_sesion),
           aux_lectura = str_detect(V2, "\\d\\."),
           lectura = str_extract_all(V2, "\\d+"),
           titulo_lectura = V3,
           autores_lectura = aux_autores) %>% 
    filter(aux_lectura == TRUE) %>% 
    select(id_sesion, codigo_sesion, tipo_sesion, lectura, titulo_lectura, autores_lectura)
  
  lecturas <- rbind(lecturas, ver2)
}


rm(aux, tabla, ver, ver2, i)