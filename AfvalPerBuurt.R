# Data taken from
# https://dataplatform.nl/#/data/2346f748-e7e7-4e6d-b630-1009a3c0e9d8
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2022
# https://www.cbs.nl/nl-nl/maatwerk/2021/31/kerncijfers-wijken-en-buurten-2021

# Packages installation
install.packages("rgeoda")
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")

# Load  packages
library(sf)
library(ggplot2)
library(dplyr)

# Kerncijfers
kern = read_excel("./.data/kwb-2021.xls") %>% 
  filter(recs == "Buurt" & gm_naam == "Utrecht")

FILTER_ONLY_POLLUTION = function(c)
  startsWith(c, 'Afval en vervuiling openbare ruimte;') |
  c == 'Anders nl; met betrekking tot afval' |
  c == 'Anders nl; met betrekking tot vervuiling'

# Reports
reports_sf = read.csv(file = './.data/reports-utrecht.csv') %>%
  mutate(year = as.integer(format(as.POSIXct(begindatum), format='%Y'))) %>%
  filter(year == 2022) %>% 
  filter(FILTER_ONLY_POLLUTION(categorie)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3857)

# Neighborhoods without industrial areas
utr_bu_woon_sf = st_read("./.data/WijkBuurtkaart_2022_v0/SHP/CBS_buurten2022.shp") %>%
  filter(GM_NAAM == 'Utrecht')  %>%
  filter(!startsWith(BU_NAAM, 'Bedrijven')) %>% 
  merge(kern[,c('a_inw', 'gwb_code')], by.x='BU_CODE', by.y = 'gwb_code') %>% 
  mutate(a_inw = as.integer(a_inw)) %>%
  st_transform(crs = 3857)


# Joining

reports_joined = st_join(reports_sf, utr_bu_woon_sf) %>% 
  st_drop_geometry() %>%
  filter(!is.na(BU_CODE)) %>%
  group_by(BU_CODE) %>% 
  summarise(n = sum(aantal_stemmen)) %>% 
  merge(
    utr_bu_woon_sf
    , 
    by='BU_CODE') %>%
  mutate(report_index = 100 * n / a_inw) %>%
  mutate(report_cat = as.factor(ntile(report_index, 4))) %>% 
  arrange(-report_index)

FILL_COLORS = c("#bdcf32", "#ede15b", "#ef9b20", "#ea5545")


# All neighborhoods bar chart
ggplot(
  head(reports_joined, 104), 
  aes(x = report_index, y=reorder(BU_NAAM, -report_index), fill = report_cat)) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  geom_bar(stat="identity", width=0.8) +
  theme(axis.text.y = element_text(lineheight = 3.4, size = 6)) +
  coord_fixed(ratio = 1.5) +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "All neighbourhoods excl. industrial areas",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )


# All areas map
ggplot() +
  geom_sf(
    data = (
      reports_joined  %>% st_as_sf
    ),
    size = 0.3,
    color = "white"
  ) +
  aes(fill = report_cat) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "All neighbourhoods excl. industrial areas",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )


# Selected areas map
SOME_AREAS_FILTER = function(bn)
  startsWith(bn, "Nieuw Engeland") | 
  startsWith(bn, "Lombok") |
  startsWith(bn, "Laan van Nieuw-Guinea") |
  startsWith(bn, "Oog in Al") |
  startsWith(bn, "Halve Maan") |
  startsWith(bn, "Welgelegen") | 
  startsWith(bn, "Kanaleneiland-Noord") |
  startsWith(bn, "Transwijk-Noord")

ggplot() +
  geom_sf(
    data = (reports_joined  %>% st_as_sf %>% filter(SOME_AREAS_FILTER(BU_NAAM))),
    size = 0.3,
    color = "white"
  ) +
  geom_sf_text(data = reports_joined  %>% st_as_sf %>% filter(SOME_AREAS_FILTER(BU_NAAM)), aes(label = paste(substr(BU_NAAM, 0, 20), '...', "\n", round(report_index, 2))), nudge_y = 100) +
  aes(fill = report_cat) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "Selected neighbourhoods",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )

