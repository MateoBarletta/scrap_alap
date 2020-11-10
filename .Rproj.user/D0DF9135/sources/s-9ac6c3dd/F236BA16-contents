library(dplyr)
library(XML)
library(RCurl)

### path de las sesiones e indices de participantes

# path_sesiones_2016 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1663&Itemid=891"

path_sesiones_2014 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1761&Itemid=781"
path_particip_2014 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1762&Itemid=781"

path_sesiones_2012 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1552&Itemid=894"
path_particip_2012 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1553&Itemid=894"

path_sesiones_2010 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1448&Itemid=339"
path_particip_2010 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&layout=edit&id=1449&Itemid=339"

path_sesiones_2008 <- "http://www.alapop.org/alap/index.php?option=com_content&view=article&id=128&Itemid=895"

# funcion para scrapear las tablas de las sesiones
scrapea_sesiones <- function(path) {
  
  tabla <- path %>% 
    getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
    readHTMLTable()
  
  tabla[[length(tabla)]]
  
}

# loopeo entre los paths
paths_sesiones   <- c(path_sesiones_2008, path_sesiones_2010, path_sesiones_2012, path_sesiones_2014)
nombres_sesiones <- c("sesiones_2008", "sesiones_2010", "sesiones_2012", "sesiones_2014")


for(i in 1:length(paths_sesiones)){
  
  assign(nombres_sesiones[[i]], scrapea_sesiones(paths_sesiones[[i]]))
  
}

rm(i, path_sesiones_2010, path_sesiones_2012, path_sesiones_2014, paths_sesiones, nombres_sesiones, scrapea_sesiones)

# indices de participantes
tabla_2010 <- path_particip_2010 %>% 
  getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
  readHTMLTable()

tabla_2012 <- path_particip_2012 %>% 
  getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
  readHTMLTable()

tabla_2014 <- path_particip_2014 %>% 
  getURL(.opts = list(ssl.verifypeer = FALSE)) %>% 
  readHTMLTable()

particip_2010 <- tabla_2010[[3]]
particip_2012 <- tabla_2012[[1]]
particip_2014 <- tabla_2014[[5]]

rm(tabla_2010, tabla_2012, tabla_2014, path_particip_2010, path_particip_2012, path_particip_2014)

# sesiones <- list(sesiones_2010, sesiones_2012, sesiones_2014)
# particip <- list(particip_2010, particip_2012, particip_2014)
# saveRDS(sesiones, "data/sesiones.rds")
# saveRDS(particip, "data/participantes.rds")
