#Pacotes
library(dplyr)
library(ggplot2) 
library(sf) 
library(leaflet) 
library(RColorBrewer)

# Banco do TSE RESULTADOS agrupado com todos estados, Lucas que nos ajudou a juntar 
banco <- read.csv2("C:/Users/User/Desktop/UFRGS/Datathon/Banco_de_dados/VOTOS_BRASIL.csv")

banco <- banco %>%
  filter(NR_VOTAVEL!=95&NR_VOTAVEL!=96) %>%
  group_by(SG_.UF) %>%
  mutate(PORCENTAGEM=QT_VOTOS/sum(QT_VOTOS))
  
banco_b <- banco %>%
  filter(NR_VOTAVEL==17)

banco_h <- banco %>%
  filter(NR_VOTAVEL==13)


# Banco do estilo shapefile q pegamos no IBGE, http://downloads.ibge.gov.br/downloads_geociencias.htm
#organizacao_do_territorio >>> 
#malhas_territoriais >>> 
#malhas_municipais >>> 
#municipio_2015 >>> 
#Brasil >>> 
#BR >>> 
#br_unidades_da_federacao.zip

codigo_uf <- st_read(dsn = 'C:/Users/User/Desktop/UFRGS/Datathon/Banco_de_dados/BRUFE250GC_SIR.shp', quiet = T)

banco_b$SG_.UF = factor(banco_b$SG_.UF,                  levels = levels(banco_b$SG_.UF),
                        labels = c('ACRE','ALAGOAS','AMAZONAS','AMAPÁ','BAHIA','CEARÁ','DISTRITO FEDERAL','ESPÍRITO SANTO',
                                   'GOIÁS','MARANHÃO','MINAS GERAIS','MATO GROSSO DO SUL','MATO GROSSO','PARÁ',
                                   'PARAÍBA','PERNAMBUCO','PIAUÍ','PARANÁ','RIO DE JANEIRO','RIO GRANDE DO NORTE',
                                   'RONDÔNIA','RORAIMA','RIO GRANDE DO SUL', 'SANTA CATARINA', 'SERGIPE',
                                   'SÃO PAULO', 'TOCANTINS', 'ZZ'))

banco_h$SG_.UF = factor(banco_h$SG_.UF,                  levels = levels(banco_h$SG_.UF),
                                                 labels = c('ACRE','ALAGOAS','AMAZONAS','AMAPÁ','BAHIA','CEARÁ','DISTRITO FEDERAL','ESPÍRITO SANTO',
                                                            'GOIÁS','MARANHÃO','MINAS GERAIS','MATO GROSSO DO SUL','MATO GROSSO','PARÁ',
                                                            'PARAÍBA','PERNAMBUCO','PIAUÍ','PARANÁ','RIO DE JANEIRO','RIO GRANDE DO NORTE',
                                                            'RONDÔNIA','RORAIMA','RIO GRANDE DO SUL', 'SANTA CATARINA', 'SERGIPE',
                                                            'SÃO PAULO', 'TOCANTINS', 'ZZ'))

b <- merge(banco_b,codigo_uf, by.x = "SG_.UF", by.y = "NM_ESTADO")
b <- st_as_sf(b)

h <-  merge(banco_h,codigo_uf, by.x = "SG_.UF", by.y = "NM_ESTADO")
h = st_as_sf(h)


bins <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
pal <- colorBin("YlOrRd", domain = b$PORCENTAGEM, bins = bins)

# Comando pego do script da oficina de mapas https://www.ufrgs.br/datathon/datathon-spatial-v02.zip
# Copiei igual ao que ele fez no script, só troquei os bancos praticamente
# Gráfico bolsonaro
leaflet(b) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal(PORCENTAGEM), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = sprintf("%s - PORCENTAGEM: %s", b$SG_.UF, round(b$PORCENTAGEM, 3)),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~PORCENTAGEM, opacity = 0.7, title = NULL,
            position = "bottomright")

pal_h <- colorBin("YlOrRd", domain = h$PORCENTAGEM, bins = bins)

#Gráfico Haddad

leaflet(h) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal_h(PORCENTAGEM), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = sprintf("%s - PORCENTAGEM: %s", h$SG_.UF, round(h$PORCENTAGEM, 3)),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_h, values = ~PORCENTAGEM, opacity = 0.7, title = NULL,
            position = "bottomright")












