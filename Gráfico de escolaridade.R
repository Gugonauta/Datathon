#Pacotes
library(dplyr)
library(ggplot2) 
library(sf) 
library(leaflet) 
library(RColorBrewer)

# Banco TSE perfil_eleitorado_ATUAL
banco <- read.csv2("C:/Users/User/Desktop/UFRGS/Datathon/Banco_de_dados/perfil_eleitorado_ATUAL.csv")

# Banco do estilo shapefile q pegamos no IBGE, http://downloads.ibge.gov.br/downloads_geociencias.htm
#organizacao_do_territorio >>> 
#malhas_territoriais >>> 
#malhas_municipais >>> 
#municipio_2015 >>> 
#Brasil >>> 
#BR >>> 
#br_unidades_da_federacao.zip

codigo_uf <- st_read(dsn = 'C:/Users/User/Desktop/UFRGS/Datathon/Banco_de_dados/BRUFE250GC_SIR.shp', quiet = T)

banco_estados_escolaridade <- banco %>%
  group_by(UF, GRAU_DE_ESCOLARIDADE) %>%
  summarise(QTD_ELEITORES_NO_PERFIL = sum(QTD_ELEITORES_NO_PERFIL))

banco_estados_escolaridade_clean <- banco_estados_escolaridade[which(banco_estados_escolaridade$GRAU_DE_ESCOLARIDADE != "NÃO INFORMADO"),1:3]

banco_estados_escolaridade_clean$GRAU_DE_ESCOLARIDADE = factor(banco_estados_escolaridade_clean$GRAU_DE_ESCOLARIDADE,
                                                               levels = levels(banco_estados_escolaridade_clean$GRAU_DE_ESCOLARIDADE),
                                                               labels = c(1,4,3,6,5,2,-1,8,7))
# Media ponderada
banco_estados_escolaridade_ponderado = banco_estados_escolaridade_clean %>%
  group_by(UF) %>%
  summarise(peso = sum(as.numeric(GRAU_DE_ESCOLARIDADE)*QTD_ELEITORES_NO_PERFIL)/sum(QTD_ELEITORES_NO_PERFIL))

banco_estados_escolaridade_ponderado$UF = factor(banco_estados_escolaridade_ponderado$UF,
                                                 levels = levels(banco_estados_escolaridade_ponderado$UF),
                                                 labels = c('ACRE','ALAGOAS','AMAZONAS','AMAPÁ','BAHIA','CEARÁ','DISTRITO FEDERAL','ESPÍRITO SANTO',
                                                            'GOIÁS','MARANHÃO','MINAS GERAIS','MATO GROSSO DO SUL','MATO GROSSO','PARÁ',
                                                            'PARAÍBA','PERNAMBUCO','PIAUÍ','PARANÁ','RIO DE JANEIRO','RIO GRANDE DO NORTE',
                                                            'RONDÔNIA','RORAIMA','RIO GRANDE DO SUL', 'SANTA CATARINA', 'SERGIPE',
                                                            'SÃO PAULO', 'TOCANTINS', 'ZZ'))
escolaridade <- merge(banco_estados_escolaridade_ponderado,codigo_uf, by.x = "UF", by.y = "NM_ESTADO")

escolaridade <- st_as_sf(escolaridade)

bins <- c(4, 4.2, 4.4, 4.6, 4.8, 5)
pal <- colorBin("YlOrRd", domain = escolaridade$peso, bins = bins)

leaflet(escolaridade) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal(peso), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = sprintf("%s - peso: %s", b$SG_.UF, round(escolaridade$peso, 3)),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~peso, opacity = 0.7, title = NULL,
            position = "bottomright")










