library(stringr)
library(dplyr)
library(XML)
library(RCurl)

# Paths
path_sesiones_2014 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1761&Itemid=781"
path_sesiones_2012 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1552&Itemid=894"
path_sesiones_2010 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1448&Itemid=339"


# Funcion auxiliar tildes
tildes <- function(texto){
  chartr("áéíóú","aeiou", texto)
}

# Defino funcion scrapping
get_sesiones <- function(lista_links) {
  
  links <- lista_links
  
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
             codigo_sesion = case_when(tipo_sesion == "sesionregular"            ~ "SR",
                                       tipo_sesion == "sesionesregulares"        ~ "SR",
                                       tipo_sesion == "sesiondeposter"           ~ "SP",
                                       tipo_sesion == "apertura/opening"         ~ "AO",
                                       tipo_sesion == "clausura/closingceremony" ~ "CL",
                                       tipo_sesion == "conferenciainaugural"     ~ "CI",
                                       tipo_sesion == "mesaredonda"              ~ "MR",
                                       tipo_sesion == "proyectosespeciales"      ~ "PE",
                                       tipo_sesion == "comunicacionoral"         ~ "CO",
                                       tipo_sesion == "panel"                    ~ "PN",
                                       tipo_sesion == "plenaria"                 ~ "PL"),
             id_sesion = paste0(codigo_sesion, ".", nro_sesion),
             aux_lectura = str_detect(V2, "\\d\\."),
             lectura = str_extract_all(V2, "\\d+"),
             titulo_lectura = V3,
             autores_lectura = aux_autores) %>% 
      filter(aux_lectura == TRUE) %>% 
      select(id_sesion, codigo_sesion, tipo_sesion, lectura, titulo_lectura, autores_lectura)
    
    lecturas <- rbind(lecturas, ver2)
  }
  
  lecturas
}


# 2014
links_sesiones <- path_sesiones_2014 %>% 
  getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
  XML::getHTMLLinks() 

links_2014 <- tibble(
  links   = links_sesiones,
  article = str_detect(links, "781$")) %>% 
  filter(article==TRUE)

lista_links_2014 <- links_2014$links[7:nrow(links_2014)]
# paste0("www.alapop.org", links_2014[3,1])

lecturas_2014 <- get_sesiones(lista_links_2014) %>% 
  mutate(anio='2014')


#saveRDS(lecturas_2014, "data/lecturas_2014.rds")
rm(lecturas, links_2014, links_sesiones, lista_links_2014, path_sesiones_2014)

# 2012
links_sesiones <- path_sesiones_2012 %>% 
  getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
  XML::getHTMLLinks() 

links_2012 <- tibble(
  links   = links_sesiones,
  article = str_detect(links, "894$")) %>% 
  filter(article==TRUE) %>% 
  arrange(links)

lista_links_2012 <- links_2012$links[6:nrow(links_2012)]

lecturas_2012 <- get_sesiones(lista_links_2012) %>% 
  mutate(anio = '2012')

#saveRDS(lecturas_2012, "data/lecturas_2012.rds")
rm(lecturas, links_2012, links_sesiones, lista_links_2012, path_sesiones_2012)

# 2010
links_sesiones <- path_sesiones_2010 %>% 
  getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
  XML::getHTMLLinks() 

links_2010 <- tibble(
  links   = links_sesiones,
  article = str_detect(links, "339$")) %>% 
  filter(article==TRUE) %>% 
  arrange(links)

lista_links_2010 <- c(links_2010$links[2:3], links_2010$links[8:nrow(links_2010)])

lecturas_2010 <- get_sesiones(lista_links_2010) %>% 
  mutate(anio = '2010',
         nrow = row_number(),
         id_sesion     = case_when(id_sesion == "NA.NA" ~ "SR.33", 
                                   id_sesion == "CO.08" & nrow < 100 ~ "SR.08",
                                   TRUE ~ id_sesion),
         tipo_sesion   = case_when(is.na(tipo_sesion)    ~ "sesionesregulares", 
                                   id_sesion == "SR.08"  ~ "sesionesregulares",
                                   TRUE ~ tipo_sesion),
         codigo_sesion = case_when(is.na(codigo_sesion)  ~ "SR", 
                                   id_sesion == "SR.08"  ~ "SR",
                                   TRUE ~ codigo_sesion)) %>% 
  select(-nrow)

#saveRDS(lecturas_2010, "data/lecturas_2010.rds")
rm(lecturas, links_2010, links_sesiones, lista_links_2010, path_sesiones_2010)

rm(get_sesiones, tildes)

# # 2008
# path_sesiones_2008 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&id=128&Itemid=895"
# 
# links_sesiones <- path_sesiones_2008 %>% 
#   getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
#   XML::getHTMLLinks() 
# 
# links_2008 <- tibble(
#   links   = links_sesiones,
#   article = str_detect(links, "Itemid=\\d+$")) %>% 
#   filter(article==TRUE) %>% 
#   arrange(links)
# 
# lista_links_2008 <- links_2008$links
# 
# lecturas_2008 <- get_sessiones(lista_links_2008) %>% 
#   mutate(anio = '2008',
#          id_sesion     = case_when(id_sesion == "NA.NA" ~ "SR.33", TRUE ~ id_sesion),
#          tipo_sesion   = case_when(is.na(tipo_sesion)    ~ "sesionesregulares", TRUE ~ tipo_sesion),
#          codigo_sesion = case_when(is.na(codigo_sesion)  ~ "SR", TRUE ~ codigo_sesion))
# 
# saveRDS(lecturas_2008, "data/lecturas_2008.rds")
# rm(lecturas, links_2008, links, links_sesiones, lista_links_2008, path_sesiones_2008)


# # 2016
# path_sesiones_2014 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1761&Itemid=781"
# 
# links_sesiones <- path_sesiones_2014 %>% 
#   getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
#   XML::getHTMLLinks() 
# 
# links_2014 <- tibble(
#   links   = links_sesiones,
#   article = str_detect(links, "781$")) %>% 
#   filter(article==TRUE)
# 
# lista_links_2014 <- links_2014$links[7:nrow(links_2014)]
# # paste0("www.alapop.org", links_2014[3,1])
# 
# lecturas_2014 <- get_sessiones(lista_links_2014) %>% 
#   mutate(anio='2014')
# 
# 
# #saveRDS(lecturas_2014, "data/lecturas_2014.rds")
# rm(lecturas, links_2014, links, links_sesiones, lista_links_2014, path_sesiones_2014)