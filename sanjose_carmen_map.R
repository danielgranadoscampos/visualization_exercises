library(osmdata)
library(tidyverse)
library(sysfonts)
library(showtext)

# Mapa de San José Central, basado en:
# Kaduk (2021, Jan. 18). Taras Kaduk: Print Personalized Street Maps Using R. 
# From https://taraskaduk.com/posts/2021-01-18-print-street-maps/


# Coordenadas  ------------------------------------------------------------

coordenadas_carmen <- getbb("Carmen San Jose Costa Rica")

coordenadas <- cbind(coordenadas_carmen[,1] - 0.005,
                     coordenadas_carmen[,2] + 0.005)

colnames(coordenadas) <- c("min", "max")



# Features ----------------------------------------------------------------

#Carreteras
carreteras <- coordenadas %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = c("motorway", "primary", 
                                             "primary_link","secondary", 
                                             "secondary_link", "tertiary", 
                                             "tertiary_link")) %>%
  osmdata_sf()

#Calles
calles <- coordenadas%>%
  opq()%>%
  add_osm_feature(key = "highway", value = c("residential", "living_street",
                                             "unclassified","service", 
                                             "footway")) %>%
  osmdata_sf()

# Vías
rieles <- coordenadas%>%
  opq() %>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf()

# Parques
parques <- coordenadas%>%
  opq() %>%
  add_osm_feature(key = "leisure", value = c("park")) %>%
  osmdata_sf()

#Bosques
bosques <- coordenadas%>%
  opq() %>%
  add_osm_feature(key = "natural", value = "wood") %>%
  osmdata_sf()
#Simon
simon <- coordenadas %>% 
  opq() %>% 
  add_osm_feature(key = "tourism", value = c("zoo")) %>%
  osmdata_sf()

water <- coordenadas%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("canal", "river")) %>%
  osmdata_sf()


# Theme -------------------------------------------------------------------

#Font
font_add(family = "bold", "Didot-HTF-B06-Bold.ttf")
showtext_auto()

#Theme
blank_theme <- theme(axis.line=element_blank(),
                     axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(), 
                     axis.title.y=element_blank(),
                     legend.position = "none",
                     plot.background=element_rect(fill = "#171a1c"),
                     panel.grid.minor=element_blank(),
                     panel.background=element_blank(),
                     panel.grid.major=element_blank(),
                     plot.margin = unit(c(t=2,r=2,b=2,l=2), "cm"),
                     plot.caption = element_text(color = "#FF7597", size = 120, 
                                                 hjust = .5, family = "bold"),
                     panel.border = element_blank()
)


# Grafico -----------------------------------------------------------------

map <- ggplot() +
  blank_theme +
  labs(caption = 'Carmen, San José, Costa Rica') +
  # carreteras
  geom_sf(data = carreteras$osm_lines,
          inherit.aes = FALSE,
          color = "#5d0ed5", 
          size = 1.7,       
          alpha = 0.8) +    
  # calles
  geom_sf(data = calles$osm_lines,
          inherit.aes = FALSE,
          color = "#51a095",
          size = 0.7,
          alpha = 0.6) +
  #rieles
  geom_sf(data = rieles$osm_lines,
          inherit.aes = FALSE,
          color = "#51a095",
          size = 0.5,
          alpha = 0.6) +
  # rios
  geom_sf(data = water$osm_lines,
          inherit.aes = FALSE,
          color = "#FF7597",
          size = 2.3,
          alpha = 0.5) +
  # parques
  # geom_sf(data = parques$osm_polygons,
  #         inherit.aes = FALSE,
  #         colour ="#F3ABFF",
  #         fill = NA) +
  # geom_sf(data = parques$osm_multipolygons,
  #         inherit.aes = FALSE,
  #         fill = "grey10",
  #         colour = NA,
  #         alpha = 0.3) +
  # simon
# geom_sf(data = simon$osm_polygons,
#         inherit.aes = FALSE,
#         colour = "#F3ABFF",
#         fill = NA) +
# geom_sf(data = simon$osm_multipolygons,
#         inherit.aes = FALSE,
#         fill = "grey10",
#         colour = NA,
#         alpha = 0.3) +
#bosque
# geom_sf(data = bosques$osm_polygons,
#         inherit.aes = FALSE,
#         colour = "#F3ABFF",
#         fill = NA,
#         alpha = 0.5) +
#extent to display
coord_sf(xlim = c(coordenadas[1],coordenadas[3]),
         ylim = c(coordenadas[2],coordenadas[4]),
         expand = FALSE)

map


# Guardar -----------------------------------------------------------------


ggsave("r_map.png", plot=map, width = 300, height = 300, units = "mm", dpi = "retina")
ggsave("map.svg", plot=map)