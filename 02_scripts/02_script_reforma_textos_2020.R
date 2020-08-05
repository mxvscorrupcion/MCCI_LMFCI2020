# Paquetes ----
require(foreign)
require(mxmaps)
require(haven)
require(srvyr)
require(ggpubr)
require(gtable)
require(scales)
require(grid)
require(gridExtra)
require(tidyverse)

# Paleta de colores ----
# #'000c2d' (azul oscuro)
# #'f72732' (rojo mcci)
# #'fff72d' (crema)
# #'ffffff' (blanco)
# #'333333' (texto)

mcci_discrete <- c(
  '#000c2d', '#0E9A9D', '#8a8c91', '#ecd756', '#f72732'
)

partidos <- c(
  "#0079bc", # PAN
  "#e93d44", # PRI
  "#ffde00", # PRD
  "#ee3d44", # PT
  "#87c344", # PVEM
  "#fd8204", # MC
  "#b85756", # MORENA
  "#743388", # INDEPENDIENTE
  "#8a8c91", # NS / NC
  "#606265", # LEYENDAS
  "#b8babd"  # NO HA DECIDIDO
)

# Directorios ----
inp <- "Github/MCCI_LMFCI2020/01_datos/"
out <- "Github/MCCI_LMFCI2020/03_gráficas/04_gráficas_en_textos/"

# Mapa para gráficas ----
load(paste0(inp, "mxmap.RData"))
mxmap <- mxstate.map %>% 
  left_join(
    mxmaps::df_mxstate %>% 
      mutate(
        circ = case_when(
          state_abbr == "BC" ~ "01",
          state_abbr == "BCS" ~ "01",
          state_abbr == "CHIH" ~ "01",
          state_abbr == "DGO" ~ "01",
          state_abbr == "JAL" ~ "01",
          state_abbr == "NAY" ~ "01",
          state_abbr == "SIN" ~ "01",
          state_abbr == "SON" ~ "01",
          state_abbr == "AGS" ~ "02",
          state_abbr == "COAH" ~ "02",
          state_abbr == "GTO" ~ "02",
          state_abbr == "NL" ~ "02",
          state_abbr == "QRO" ~ "02",
          state_abbr == "SLP" ~ "02",
          state_abbr == "TAM" ~ "02",
          state_abbr == "ZAC" ~ "02",
          state_abbr == "CAMP" ~ "03",
          state_abbr == "CHPS" ~ "03",
          state_abbr == "OAX" ~ "03",
          state_abbr == "QROO" ~ "03",
          state_abbr == "TAB" ~ "03",
          state_abbr == "VER" ~ "03",
          state_abbr == "YUC" ~ "03",
          state_abbr == "CDMX" ~ "04",
          state_abbr == "GRO" ~ "04",
          state_abbr == "MOR" ~ "04",
          state_abbr == "PUE" ~ "04",
          state_abbr == "TLAX" ~ "04",
          state_abbr == "COL" ~ "05",
          state_abbr == "HGO" ~ "05",
          state_abbr == "MEX" ~ "05",
          state_abbr == "MICH" ~ "05",
          T ~ NA_character_
        )
      ) %>% 
      select(
        region, state_abbr, circ
      )
  )

leyenda <- 
  ggplot(mxmap, aes(long, lat, group=group, fill = as.numeric(circ))) +
  geom_polygon(color = "black", size = .2) +
  scale_fill_gradientn(colours = mcci_discrete) +
  theme_void() +
  guides(fill=F) +
  coord_map() 

# Datos ----
d <- readxl::read_excel(paste0(inp, "datos_gráficas.xlsx")) %>% 
  mutate(props = as.numeric(props))


# Gráficas ----
# * Texto 1 ----
# ** Gráfica 1.1 ----
a <- filter(d, id=="T1_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)
ggplot(a, aes(x = as.factor(año),
              y = props,
              label = paste0(props, "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 12) +
  facet_wrap(~str_wrap(variables, 40), scales = "free_x") +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1],mcci_discrete[2], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 1.2 ----
a <- filter(d, id=="T1_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 1.3 ----
a <- filter(d, id=="T1_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(col = reorder(variables, as.numeric(orden)),
              y = props,
              label = paste0(round(props, 2), "%"),
              x = as.numeric(año))) +
  geom_line(show.legend = T, size = 2) +  geom_point(size = 4) + 
  ggrepel::geom_label_repel(vjust = -0.3, size = 8, show.legend = F, col = mcci_discrete[1]) + 
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_color_manual("", values = c(mcci_discrete[1],mcci_discrete[2], mcci_discrete[4], mcci_discrete[5])) +
  scale_x_continuous(breaks = c(2018,2019,2020)) +
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "right")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 1.4 ----
a <- filter(d, id=="T1_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)


ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  facet_wrap(~as.factor(año)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 2  ----
# ** Gráfica 2.1 ----
a <- filter(d, id=="T2_1") 
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(str_wrap(variables, 25), as.numeric(orden)),
              y = props,
              label = paste0(props, "%"),
              fill = reorder(str_wrap(variables, 25), as.numeric(orden)))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 12) +
  facet_wrap(~str_wrap(v_id, 35)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.2 ----
a <- filter(d, id=="T2_2")
fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.3 ----
a <- filter(d, id=="T2_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(v_id, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(variables))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.2, size = 7, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[5], mcci_discrete[1])) +
  labs(title= str_wrap(fiuf, width = 65),
       caption = fiuffi) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "top")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 2.4 ----
a <- filter(d, id=="T2_4") 
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(str_wrap(variables, 25), as.numeric(orden)),
              y = props,
              label = paste0(props, "%"),
              fill = reorder(str_wrap(variables, 25), as.numeric(orden)))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(vjust = -0.5, size = 12, position = position_dodge2(width = 0.5)) +
  facet_wrap(~str_wrap(v_id, 35)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 2.5 ----
a <- filter(d, id=="T2_5")
fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")




# ** Gráfica 2.6 ----
a <- filter(d, id=="T2_6") %>%
  mutate(
    variables = case_when(
      variables == "Ns/Nc" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "No contesta" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "Ninguno" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "En blanco" ~ "No sabe / No contesta / Ninguno / En blanco",
      T ~ variables
    ),
    orden = as.numeric(orden)
  ) %>% 
  group_by(id, variables, orden, v_id, año, título, subtítulo, fuente) %>% 
  summarise(props = sum(props,na.rm = T)) 

fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 25), -orden),
              x = props,
              label = ifelse(props>0,paste0(round(props, 2), "%"),""),
              fill = reorder(str_wrap(variables, 25), -orden))) +
  geom_col(width = 0.8) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,40),
    breaks = seq(0,40,10),
    labels = paste0(
      as.character(seq(0,40,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = rev(partidos)) +
  labs(title= str_wrap(fiuf, width = 55),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 2.7 ----
a <- filter(d, id=="T2_7") %>%
  mutate(
    variables = case_when(
      variables == "Ns/Nc" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "No contesta" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "Ninguno" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "En blanco" ~ "No sabe / No contesta / Ninguno / En blanco",
      T ~ variables
    ),
    orden = as.numeric(orden)
  ) %>% 
  group_by(id, variables, orden, v_id, año, título, subtítulo, fuente) %>% 
  summarise(props = sum(props,na.rm = T)) 

fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 25), -orden),
              x = props,
              label = ifelse(props>0,paste0(round(props, 2), "%"),""),
              fill = reorder(str_wrap(variables, 25), -orden))) +
  geom_col(width = 0.8) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,40),
    breaks = seq(0,40,10),
    labels = paste0(
      as.character(seq(0,40,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = rev(partidos)) +
  labs(title= str_wrap(fiuf, width = 55),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.8 ----
a <- filter(d, id=="T2_8") %>%
  mutate(
    variables = case_when(
      variables == "Ns/Nc" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "No contesta" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "Ninguno" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "En blanco" ~ "No sabe / No contesta / Ninguno / En blanco",
      T ~ variables
    ),
    orden = as.numeric(orden)
  ) %>% 
  group_by(id, variables, orden, v_id, año, título, subtítulo, fuente) %>% 
  summarise(props = sum(props,na.rm = T)) 

fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 25), -orden),
              x = props,
              label = ifelse(props>0,paste0(round(props, 2), "%"),""),
              fill = reorder(str_wrap(variables, 25), -orden))) +
  geom_col(width = 0.8) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,45),
    breaks = seq(0,40,10),
    labels = paste0(
      as.character(seq(0,40,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = rev(partidos)) +
  labs(title= str_wrap(fiuf, width = 55),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.9 ----
a <- filter(d, id=="T2_9") %>%
  mutate(
    variables = case_when(
      variables == "Ns/Nc" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "No contesta" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "Ninguno" ~ "No sabe / No contesta / Ninguno / En blanco",
      variables == "En blanco" ~ "No sabe / No contesta / Ninguno / En blanco",
      T ~ variables
    ),
    orden = as.numeric(orden)
  ) %>% 
  group_by(id, variables, orden, v_id, año, título, subtítulo, fuente) %>% 
  summarise(props = sum(props,na.rm = T)) 

fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 25), -orden),
              x = props,
              label = ifelse(props>0,paste0(round(props, 2), "%"),""),
              fill = reorder(str_wrap(variables, 25), -orden))) +
  geom_col(width = 0.8) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,40),
    breaks = seq(0,40,10),
    labels = paste0(
      as.character(seq(0,40,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = rev(partidos)) +
  labs(title= str_wrap(fiuf, width = 55),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.10 ----
a <- filter(d, id=="T2_10") 
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(str_wrap(variables, 25), -as.numeric(orden)),
              y = props,
              label = paste0(props, "%"),
              fill = reorder(str_wrap(variables, 25), -as.numeric(orden)))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(vjust = -0.3, size = 12, position = position_dodge2(width = 0.5)) +
  facet_wrap(~str_wrap(v_id, 35)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[5], mcci_discrete[1])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.11 ----
a <- filter(d, id=="T2_11") 
fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.13 ----
a <- filter(d, id=="T2_13")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(fill = variables,
              x = props,
              label = paste0(round(props, 2), "%"),
              y = reorder(v_id, -as.numeric(orden)))) +
  geom_col(width = 0.8, position = position_stack()) + 
  geom_text(hjust = -0.03, size = 5, position = position_stack(vjust = 0.3), col = "#ffffff", fontface = "bold") +
  scale_x_continuous(
    limits = c(0,55),
    breaks = seq(0,50,10),
    labels = paste0(
      as.character(seq(0,50,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.spacing.y = unit(1.0, 'cm'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 10, dpi = 100, bg = "transparent")


# ** Gráfica 2.12 ----
a <- filter(d, id=="T2_12") %>%
  mutate(
    variables = ifelse(str_detect(variables, "And"), "Al presidente López Obrador", variables),
    integer_part = floor(props),
    missing_int = 100-sum(integer_part),
    decimals = props-integer_part,
    sorted = order(desc(decimals)),
    rounded = ifelse(sorted<=missing_int, integer_part+1, integer_part),
    orden = as.numeric(orden)
  ) %>% 
  arrange(orden)
fiuf <- unique(a$título)
fiuff <- unique(a$v_id)
fiuffi <- unique(a$fuente)

c <- round(a$rounded)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5],
                                   mcci_discrete[2],
                                   partidos[7],
                                   mcci_discrete[4],
                                   partidos[11],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# * Texto 3 ----
# ** Gráfica 3.1 ----
a <- filter(d, id=="T3_1") %>% 
  mutate(
    fecha = lubridate::floor_date(as.Date.numeric(x = año,origin = "1900-01-01"), "month")
  )
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(
  a,
  aes(
    x = as.Date(fecha), y = props, col = ifelse(str_starts(variables, "Mej"),"Ha mejorado / Mejorará","Ha empeorado / Empeorará")
  )
) +
  geom_line() + geom_point() +
  facet_wrap(~ reorder(v_id, orden)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_color_manual("",values = c("#f72732", "#000c2d")) +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%Y-%m"),
               limits = as.Date(c("2018-01-01","2020-02-01"))) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 3.2 ----
a <- filter(d, id=="T3_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(variables, as.numeric(orden)),
              y = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(vjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 22),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 3.3 ----
a <- filter(d, id=="T3_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(str_wrap(variables, 25), as.numeric(orden)),
              y = props,
              label = paste0(props, "%"),
              fill = reorder(str_wrap(variables, 25), as.numeric(orden)))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 12) +
  facet_wrap(~str_wrap(v_id, 35)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 3.4 ----
a <- filter(d, id=="T3_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = fct_rev(as.factor(str_wrap(v_id,25))),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = reorder(variables, -as.numeric(orden)))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", guide = guide_legend(reverse = TRUE),
                    values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = str_wrap(fiuff, 65),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 4 ----
# ** Gráfica 4.1 ----
a <- filter(d, id=="T4_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 4.2 ----
a <- filter(d, id=="T4_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)


b <- a %>% 
  filter(año == 2019) 
c <- b$props
names(c) <- paste0(b$variables, " [", b$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(b$orden, c))  

p1 <- 
  ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[2],
                                   mcci_discrete[4],
                                   "gray",
                                   mcci_discrete[3])) +
  labs(title= "2019") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "right")


b <- a %>% 
  filter(año == 2020) 
c <- b$props
names(c) <- paste0(b$variables, " [", b$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(b$orden, c))  

p2 <- 
  ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5],
                                   mcci_discrete[2],
                                   mcci_discrete[4],
                                   "gray",
                                   mcci_discrete[3])) +
  labs(title= "2020") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "right")

c_0 <- 
  ggarrange(p1, p2, ncol = 1,nrow = 2)

annotate_figure(c_0,
                top = text_grob(str_wrap(fiuf,75), size = 35, face = "bold" , hjust = 0.5, family = "Arial Narrow"),
                bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
)
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 4.3 ----
a <- filter(d, id=="T4_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = as.factor(año),
              y = round(props, 2),
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 12) +
  facet_wrap(~str_wrap(variables, 40), scales = "free_x") +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 37, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 4.4 ----
a <- filter(d, id=="T4_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 5 ----
# ** Gráfica 5.1 ----
a <- filter(d, id=="T5_1") %>% 
  mutate(
    props = ifelse(is.na(props), 0, props)
  )
fiuf <- unique(a$título)
fiuffi <- paste0(unique(a$fuente), "\nNota: En 2020 se agruparon las respuestas de 'ninguno' en la categoría 'Ns/Nc'; se agruparon las respuestas de 'todos' en una misma categoría con 'otros'.")

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = ifelse(props>0,paste0(round(props, 2), "%"),""),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.3, size = 5, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 5.2 ----
a <- filter(d, id=="T5_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- str_replace_all(unique(a$fuente), " ! ", "\n")

ggplot(a, aes(x = as.factor(año),
              y = round(props, 2),
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 7) +
  facet_wrap(~str_wrap(variables, 40), scales = "free_x") +
  scale_y_continuous(
    limits = c(0,20),
    breaks = seq(0,20,5),
    labels = paste0(
      as.character(seq(0,20,5)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1],mcci_discrete[2], mcci_discrete[4], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 37, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, 'cm'),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 5.3 ----
a <- filter(d, id=="T5_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = as.factor(año),
              y = round(props, 2),
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 7) +
  facet_wrap(~str_wrap(variables, 40), scales = "free_x") +
  scale_y_continuous(
    limits = c(0,30),
    breaks = seq(0,30,5),
    labels = paste0(
      as.character(seq(0,30,5)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1],mcci_discrete[2], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 37, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, 'cm'),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# * Texto 6 ----
# ** Gráfica 6.1 ----
a <- filter(d, id=="T6_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a) +
  geom_line(aes(col = reorder(variables, as.numeric(orden)),
                  y = props,
                  x = as.numeric(año)),
            show.legend = T, size = 2) +  
  ggrepel::geom_label_repel(
    data = a %>% filter(año==2019),
    aes(col = reorder(variables, as.numeric(orden)),
        y = props,
        label = paste0(round(props, 2), "%"),
        x = as.numeric(año)), 
    vjust = -0.3, size = 5, show.legend = F, col = mcci_discrete[1]
  ) + 
  geom_point(aes(col = reorder(variables, as.numeric(orden)),
                 y = props,
                 x = as.numeric(año)),
             size = 4) + 
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_color_manual("", values = c(mcci_discrete[1],mcci_discrete[5],mcci_discrete[2],
                                    mcci_discrete[4], mcci_discrete[3])) +
  scale_x_continuous(breaks = unique(a$año)) +
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "right")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 8, dpi = 100, bg = "transparent")

# ** Gráfica 6.2 ----
a <- filter(d, id=="T6_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  facet_wrap(~as.factor(año)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 6.3 ----
a <- filter(d, id=="T6_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(str_wrap(variables, 25), as.numeric(orden)),
              y = props,
              label = paste0(props, "%"),
              fill = reorder(str_wrap(variables, 25), as.numeric(orden)))) +
  geom_col(width = 0.5) + geom_text(vjust = -0.5, size = 12) +
  facet_wrap(~str_wrap(v_id, 35)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], 
                                   mcci_discrete[2], 
                                   mcci_discrete[5],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráficas 6.4 ----
a <- filter(d, id=="T6_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot() + 
  geom_point(data = a, 
             aes(
               x=props, 
               y= reorder(str_wrap(variables,25),-as.numeric(orden)), 
               col = as.factor(año)
             ), size = 3,
             colour_x=mcci_discrete[1], colour_xend = mcci_discrete[]) + 
  ggalt::geom_dumbbell(data = a %>% 
                         select(-c(id,título:tipo_gráfica))%>% 
                         pivot_wider(names_from = año, values_from = props), 
                       aes(
                         x=`2020`, 
                         xend=`2019`, 
                         y=reorder(str_wrap(variables,25),-as.numeric(orden)), 
                         y_end=reorder(str_wrap(variables,25),-as.numeric(orden))
                       ),
                       colour_x=mcci_discrete[1], colour_xend = mcci_discrete[5],
                       size_x = 4.5, size_xend = 4.5,
                       dot_guide = T,
                       colour = "gray", size = 2) + 
  geom_text(data = a %>% 
              select(-c(id,título:tipo_gráfica))%>% 
              pivot_wider(names_from = año, values_from = props), aes(x=`2020`, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(`2020`,"%")),
            color=mcci_discrete[1], size=6, vjust=-1, fontface="bold")  +
  geom_text(data = a %>% 
              select(-c(id,título:tipo_gráfica))%>% 
              pivot_wider(names_from = año, values_from = props), aes(x=`2019`, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(`2019`,"%")),
            color=mcci_discrete[5], size=6, vjust=2, fontface="bold") +
  scale_x_continuous(
    limits = c(25,100),
    breaks = seq(25,100,25),
    labels = paste0(
      as.character(seq(25,100,25)), "%"
    )
  ) +
  scale_color_manual(name = "", values = c(mcci_discrete[1], mcci_discrete[5]) )+  
  labs(title= str_wrap(fiuf, width = 80),
       caption = paste0(fiuffi, " | @ricalvan")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30 , face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(1.0, 'cm'),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "top")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 10, dpi = 100, bg = "transparent")

# ** Gráficas 6.5 ----
a <- filter(d, id=="T6_5")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot() + 
  geom_point(data = a, 
             aes(
               x=props, 
               y= reorder(str_wrap(variables,25),-as.numeric(orden)), 
               col = as.factor(año)
             ), size = 3,
             colour_x=mcci_discrete[1], colour_xend = mcci_discrete[]) + 
  ggalt::geom_dumbbell(data = a %>% 
                         select(-c(id,título:tipo_gráfica))%>% 
                         pivot_wider(names_from = año, values_from = props), 
                       aes(
                         x=`2020`, 
                         xend=`2019`, 
                         y=reorder(str_wrap(variables,25),-as.numeric(orden)), 
                         y_end=reorder(str_wrap(variables,25),-as.numeric(orden))
                       ),
                       colour_x=mcci_discrete[1], colour_xend = mcci_discrete[5],
                       size_x = 4.5, size_xend = 4.5,
                       dot_guide = T,
                       colour = "gray", size = 2) + 
  geom_text(data = a %>% 
              select(-c(id,título:tipo_gráfica))%>% 
              pivot_wider(names_from = año, values_from = props), aes(x=`2020`, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(`2020`,"%")),
            color=mcci_discrete[1], size=6, vjust=-1, fontface="bold")  +
  geom_text(data = a %>% 
              select(-c(id,título:tipo_gráfica))%>% 
              pivot_wider(names_from = año, values_from = props), aes(x=`2019`, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(`2019`,"%")),
            color=mcci_discrete[5], size=6, vjust=2, fontface="bold") +
  scale_x_continuous(
    limits = c(0,75),
    breaks = seq(0,75,25),
    labels = paste0(
      as.character(seq(0,75,25)), "%"
    )
  ) +
  scale_color_manual(name = "", values = c(mcci_discrete[1], mcci_discrete[5]) )+  
  labs(title= str_wrap(fiuf, width = 80),
       caption = paste0(fiuffi, " | @ricalvan")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30 , face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(1.0, 'cm'),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "top")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 10, dpi = 100, bg = "transparent")

# ** Gráficas 6.6 ----
a <- filter(d, id=="T6_6")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot() + 
  geom_point(data = a, 
             aes(
               x=props, 
               y= reorder(str_wrap(variables,25),-as.numeric(orden)), 
               col = as.factor(año)
             ), size = 3,
             colour_x=mcci_discrete[1], colour_xend = mcci_discrete[]) + 
  ggalt::geom_dumbbell(data = a %>% 
                         select(-c(id,título:tipo_gráfica))%>% 
                         pivot_wider(names_from = año, values_from = props), 
                       aes(
                         x=`2020`, 
                         xend=`2019`, 
                         y=reorder(str_wrap(variables,25),-as.numeric(orden)), 
                         y_end=reorder(str_wrap(variables,25),-as.numeric(orden))
                       ),
                       colour_x=mcci_discrete[1], colour_xend = mcci_discrete[5],
                       size_x = 4.5, size_xend = 4.5,
                       dot_guide = T,
                       colour = "gray", size = 2) + 
  geom_text(data = a %>% 
              select(-c(id,título:tipo_gráfica))%>% 
              pivot_wider(names_from = año, values_from = props), aes(x=`2020`, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(`2020`,"%")),
            color=mcci_discrete[1], size=6, vjust=-1, fontface="bold")  +
  geom_text(data = a %>% 
              select(-c(id,título:tipo_gráfica))%>% 
              pivot_wider(names_from = año, values_from = props), aes(x=`2019`, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(`2019`,"%")),
            color=mcci_discrete[5], size=6, vjust=2, fontface="bold") +
  scale_x_continuous(
    limits = c(0,75),
    breaks = seq(0,75,25),
    labels = paste0(
      as.character(seq(0,75,25)), "%"
    )
  ) +
  scale_color_manual(name = "", values = c(mcci_discrete[1], mcci_discrete[5]) )+  
  labs(title= str_wrap(fiuf, width = 80),
       caption = paste0(fiuffi, " | @ricalvan")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30 , face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(1.0, 'cm'),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "top")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 10, dpi = 100, bg = "transparent")


# * Texto 7 ----
# ** Gráfica 7.1 ----
a <- filter(d, id=="T7_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 25), as.numeric(props)),
              x = props,
              label = ifelse(props>0,paste0(round(props, 2), "%"),""),
              fill = props)) +
  geom_col(width = 0.8) + 
  geom_text(hjust = -0.2, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_gradient(high = '#000c2d', low = '#b2b6c0') +
  labs(title= str_wrap(fiuf, width = 55),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 20, dpi = 100, bg = "transparent")


# ** Gráfica 7.2 ----
a <- filter(d, id=="T7_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(fill = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              y = forcats::fct_rev(as.factor(año)))) +
  geom_col(width = 0.8, position = position_stack()) + 
  geom_text(hjust = -0.05, size = 6, position = position_stack(vjust = .5), col = "#ffffff", fontface = "bold") +
  facet_wrap(~as.factor(forcats::fct_rev(v_id)), ncol = 1) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.spacing.y = unit(1.0, 'cm'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 10, dpi = 100, bg = "transparent")

# ** Gráfica 7.3 ----
a <- filter(d, id=="T7_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(v_id))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.05, size = 6, position = position_dodge(width = 0.5)) +
  facet_wrap(~as.factor(v_id)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1],mcci_discrete[2], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.spacing.x = unit(1.0, 'cm'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 8 ----
# ** Gráfica 8.1 ----
a <- filter(d, id=="T8_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 25), -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(v_id))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.2, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,20),
    breaks = seq(0,20,5),
    labels = paste0(
      as.character(seq(0,20,5)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 8.2 ----
a <- filter(d, id=="T8_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 8.3 ----
a <- filter(d, id=="T8_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 30), -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(v_id))) +
  geom_col(width = 0.4, position = position_dodge2()) + geom_text(hjust = -0.2, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 8.4 ----
a <- filter(d, id=="T8_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)


ggplot(a, aes(y = reorder(variables, -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 8.5 ----
a <- filter(d, id=="T8_5")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)
c <- a$props
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[5],
                                   mcci_discrete[4],
                                   mcci_discrete[2],
                                   mcci_discrete[1],
                                   "#4c546c",
                                   "gray",
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 9 ----
# ** Gráfica 9.1 ----
a <- filter(d, id=="T9_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)


b <- a %>% 
  filter(v_id == "Empresas o negocios privados") 
c <- b$props
names(c) <- paste0(b$variables, " [", b$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(b$orden, c))  

p1 <- 
  ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[5],
                                   mcci_discrete[1],
                                   mcci_discrete[3])) +
  labs(title= unique(b$v_id)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")


b <- a %>% 
  filter(v_id == "Familiares o amigos en el gobierno") 
c <- b$props
names(c) <- paste0(b$variables, " [", b$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(b$orden, c))  

p2 <- 
  ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[5],
                                   mcci_discrete[1],
                                   mcci_discrete[3])) +
  labs(title= unique(b$v_id)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")

c_0 <- 
  ggarrange(p1, p2, ncol = 1,nrow = 2, common.legend = TRUE)

annotate_figure(c_0,
                top = text_grob(str_wrap(fiuf,75), size = 38, face = "bold" , hjust = 0.5, family = "Arial Narrow"),
                bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
)
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 9.2 ----
a <- filter(d, id=="T9_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(variables, as.numeric(orden)),
              y = round(props, 2),
              label = paste0(round(props, 2), "%"),
              fill = reorder(variables, as.numeric(orden)))) +
  geom_col(width = 0.5) + 
  geom_text(vjust = -0.2, size = 12) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1],mcci_discrete[5], mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 10 ----
# ** Gráfica 10.1 ----
a <- filter(d, id=="T10_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- unique(a$variables)[1]

p1 <- 
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = mcci_discrete[1]) +
  labs(title= paste0(unique(a$variables)[1], " [ ", unique(a$props)[1], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[2]), rep("no del", 100-unique(a$props)[2]))

p2 <- 
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[2],"white"
  )) +
  labs(title= paste0(unique(a$variables)[2], " [ ", unique(a$props)[2], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")


nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[3]), rep("no del", 100-unique(a$props)[3]))
p3 <- 
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[4],"white"
  )) +
  labs(title= paste0(unique(a$variables)[3], " [ ", unique(a$props)[3], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[4]), rep("no del", 100-unique(a$props)[4]))
p4 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[5],"white"
  )) +
  labs(title= paste0(unique(a$variables)[4], " [ ", unique(a$props)[4], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")

c_0 <- 
  ggarrange(p1, p2,p3,p4, ncol = 1,nrow = 4)

annotate_figure(c_0,
                top = text_grob(str_wrap(fiuf,75), size = 38, face = "bold" , hjust = 0.5, family = "Arial Narrow"),
                bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
)
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 10.2 ----
a <- filter(d, id=="T10_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- unique(a$variables)[1]

p1 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = mcci_discrete[1]) +
  labs(title= paste0(unique(a$variables)[1], " [ ", unique(a$props)[1], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[2]), rep("no del", 100-unique(a$props)[2]))

p2 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[4],"white"
  )) +
  labs(title= paste0(unique(a$variables)[2], " [ ", unique(a$props)[2], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")


nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[3]), rep("no del", 100-unique(a$props)[3]))
p3 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[5],"white"
  )) +
  labs(title= paste0(unique(a$variables)[3], " [ ", unique(a$props)[3], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")

c_0 <- 
  ggarrange(p1, p2,p3, ncol = 1,nrow = 3)

annotate_figure(c_0,
                top = text_grob(str_wrap(fiuf,75), size = 38, face = "bold" , hjust = 0.5, family = "Arial Narrow"),
                bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
)
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 10.3 ----
a <- filter(d, id=="T10_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(
  y = reorder(variables, props),
  x = props,
  fill = v_id,
  label = paste0(props, "%"),
)) +
  geom_col(position = position_stack()) + geom_text(hjust = -0.25) +
  scale_fill_manual("", values = c(
    mcci_discrete[3],mcci_discrete[5]
  )) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  labs(title= fiuf,
       subtitle = str_wrap(fiuff, 95),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 28, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 23, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 15, dpi = 100, bg = "transparent")


# * Texto 11 ----
# ** Gráfica 11.1 ----
a <- filter(d, id=="T11_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)
c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 11.2 ----
a <- filter(d, id=="T11_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)
c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", guide = guide_legend(reverse = TRUE),
                    values = c(mcci_discrete[1],
                                   mcci_discrete[2],
                                   mcci_discrete[4],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 11.3 ----
a <- filter(d, id=="T11_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(a, aes(x = reorder(str_wrap(variables, 25), as.numeric(orden)),
              y = round(props, 2),
              label = paste0(round(props, 2), "%"),
              fill = reorder(variables, as.numeric(orden)))) +
  geom_col(width = 0.5) + 
  geom_text(vjust = -0.2, size = 12) +
  scale_y_continuous(
    limits = c(0,50),
    breaks = seq(0,50,10),
    labels = paste0(
      as.character(seq(0,50,10)), "%"
    )
  ) + 
  scale_fill_manual("", guide = guide_legend(reverse = TRUE),
                    values = c(mcci_discrete[5],
                               mcci_discrete[4],
                               mcci_discrete[2],
                               mcci_discrete[1],
                               "#4c546c",
                               mcci_discrete[3]))+
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")
  
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 11.4 ----
a <- filter(d, id=="T11_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(col = reorder(variables, as.numeric(orden)),
              y = props,
              x = as.numeric(año))) +
  geom_line(show.legend = T, size = 2) +  geom_point(size = 4) + 
  scale_color_manual("", values = c(mcci_discrete[1],mcci_discrete[5])) +
  scale_x_continuous(breaks = seq(2008,2019,1)) +
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "right")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 11.5 ----
a <- filter(d, id=="T11_5")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)
c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[5],
                                   mcci_discrete[1],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 11.6 ----
a <- filter(d, id=="T11_6")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)
c <- round(a$props)
names(c) <- paste0(a$variables, " [", a$props, "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  
df$orden <- factor(rep(a$orden, c))  

ggplot(df, aes(x = x, y = y, fill = reorder(str_wrap(category, 25), as.numeric(orden)))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[5],
                                   mcci_discrete[1],
                                   mcci_discrete[3])) +
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 12 PENDIENTE ----
# 
# * Texto 13  ----
# ** Gráfica 13.1 ----
a <- filter(d, id=="T13_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a) +
  geom_line(aes(col = reorder(variables, as.numeric(orden)),
                y = props,
                x = as.numeric(año)),
            show.legend = T, size = 2) +  
  ggrepel::geom_label_repel(
    data = a %>% filter(año==2019),
    aes(col = reorder(variables, as.numeric(orden)),
        y = props,
        label = paste0(round(props, 2), "%"),
        x = as.numeric(año)), 
    vjust = -0.3, size = 5, show.legend = F, col = mcci_discrete[1]
  ) + 
  geom_point(aes(col = reorder(variables, as.numeric(orden)),
                 y = props,
                 x = as.numeric(año)),
             size = 4) + 
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_color_manual("", values = c(mcci_discrete[1],mcci_discrete[5],mcci_discrete[2],
                                    mcci_discrete[4], mcci_discrete[3])) +
  scale_x_continuous(breaks = unique(a$año)) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "right")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 15, height = 8, dpi = 100, bg = "transparent")

# ** Gráfica 13.2 ----
a <- filter(d, id=="T13_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(fill = reorder(variables, as.numeric(orden)),
              y = props,
              label = paste0(round(props, 2), "%"),
              x = fct_rev(str_wrap(subtítulo,20)))) +
  geom_col(width = 0.5, position = position_dodge2(width = 0.5)) + 
  geom_text(hjust = -0.2, size = 7, position = position_dodge2(width = 0.5)) +
  facet_wrap(~as.factor(año)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  coord_flip() +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(2, "lines"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "right")

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 13.3 ----
a <- filter(d, id=="T13_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(variables, as.numeric(orden)),
              y = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2(width = 0.5)) + 
  geom_text(vjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 13.4 ----
a <- filter(d, id=="T13_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = as.factor(año),
              y = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2(width = 0.5)) + 
  geom_text(vjust = -0.3, size = 9, position = position_dodge(width = 0.5)) +
  facet_wrap(~variables) +
  scale_y_continuous(
    limits = c(0,30),
    breaks = seq(0,30,10),
    labels = paste0(
      as.character(seq(0,30,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[2],mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 38, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 35, hjust = 0.5),
        plot.caption = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        strip.text.x = element_text(size = 30),
        panel.spacing.x = unit(2, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 13.5 ----
a <- filter(d, id=="T13_5")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(variables, as.numeric(orden)),
              y = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2(width = 0.5)) + 
  geom_text(vjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_y_continuous(
    limits = c(0,60),
    breaks = seq(0,60,10),
    labels = paste0(
      as.character(seq(0,60,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 13.6 ----
a <- filter(d, id=="T13_6")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 20), -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.1, size = 7, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,50),
    breaks = seq(0,50,10),
    labels = paste0(
      as.character(seq(0,50,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = paste0(fiuff,"\n"),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 13.7 ----
a <- filter(d, id=="T13_7")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 20), -as.numeric(orden)),
              x = round(props, 0),
              label = paste0(round(props, 0), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.1, size = 7, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,75),
    breaks = seq(0,75,10),
    labels = paste0(
      as.character(seq(0,75,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")



# * Texto 14 ----
# ** Gráfica 14.1 ----
a <- filter(d, id=="T14_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 20), -as.numeric(orden)),
              x = round(props, 0),
              label = paste0(round(props, 0), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.1, size = 7, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,35),
    breaks = seq(0,30,10),
    labels = paste0(
      as.character(seq(0,30,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = str_wrap(fiuffi, width = 75)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 14.2 ----
a <- filter(d, id=="T14_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(str_wrap(variables, 20), as.numeric(orden)),
              y = props,
              label = props,
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2(width = 0.5)) + 
  geom_text(vjust = -0.1, size = 10, position = position_dodge(width = 0.5)) +
  scale_y_continuous(
    limits = c(0,3),
    breaks = seq(0,3,0.5),
    labels = seq(0,3,0.5)
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 14.3 ----
a <- filter(d, id=="T14_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- "Fuente: Encuesta MCCI-Reforma | marzo 2019 y marzo 2020. \nNota: valores ajustados por inflación."

ggplot(a, aes(x = reorder(str_wrap(variables, 20), as.numeric(orden)),
              y = props,
              label = paste0("$",props),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2(width = 0.5)) + 
  geom_text(vjust = -0.1, size = 10, position = position_dodge(width = 0.5)) +
  scale_y_continuous(
    limits = c(0,1200)
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 14.4 ----
a <- filter(d, id=="T14_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a,
       aes(x = reorder(str_wrap(as.factor(variables), 15), as.numeric(variables)),
           y = round(props),
           fill = props,
           label = paste0(round(props,1), "%"))) +
  geom_col(width = 0.4) + 
  geom_text(size = 3, vjust = -0.5) +
  facet_wrap(~ reorder(v_id,as.numeric(orden))) +
  scale_y_continuous(
    limits = c(0,40),
    breaks = seq(0,40,10),
    labels = paste0(
      as.character(seq(0,40,10)), "%"
    )
  ) + 
  scale_fill_gradient(high = '#000c2d', low = '#b2b6c0') +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 15 ----
# ** Gráfica 15.1 ----
a <- filter(d, id=="T15_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(variables, as.numeric(props)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 15.2 ----
a <- filter(d, id=="T15_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(x = reorder(variables, as.numeric(orden)),
              y = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(año))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.03, size = 4, position = position_dodge(width = 0.5), angle = 90) +
  facet_wrap(~as.factor(str_wrap(v_id, 30))) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 32, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 30),
        panel.spacing.x = unit(1.0, 'cm'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# * Texto 16 ----
# ** Gráficas 16.1 ----
a <- filter(d, id=="T16_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)


ggplot(a, 
       aes(x = reorder(str_wrap(variables,15), abs(props), function(x){ sum(x) }), 
           y = round(props, 0), 
           label = paste0(abs(round(props, 0)), "%"),
           fill = v_id)) + 
  geom_bar(stat = "identity", width = .6) +
  geom_text(hjust = "outward", size = 8,
            position = position_dodge(width = 0)) +
  facet_grid(~ año) +
  scale_y_continuous(
    limits = c(-100,100),
    breaks = seq(-100,100,25),
    labels = paste0(
      c(as.character(seq(100,0,-25)),
        as.character(seq(25,100,25))), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 34, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(1.0, 'cm'),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom") +
  coord_flip() 
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráficas 16.2 ----
a <- filter(d, id=="T16_2")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- unique(a$variables)[1]

p1 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = mcci_discrete[1]) +
  labs(title= paste0(unique(a$variables)[1], " [ ", unique(a$props)[1], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")

df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[2]), rep("no del", 100-unique(a$props)[2]))

p2 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[4],"white"
  )) +
  labs(title= paste0(unique(a$variables)[2], " [ ", unique(a$props)[2], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")


nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- c(rep("del", unique(a$props)[3]), rep("no del", 100-unique(a$props)[3]))
p3 <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "#8a8c91", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(
    mcci_discrete[5],"white"
  )) +
  labs(title= paste0(unique(a$variables)[3], " [ ", unique(a$props)[3], " ]")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "none")


c_0 <- 
  ggarrange(p1, p2,p3, ncol = 1,nrow = 3)

annotate_figure(c_0,
                top = text_grob(str_wrap(fiuf,75), size = 38, face = "bold" , hjust = 0.5, family = "Arial Narrow"),
                bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
)
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráficas 16.3 ----
a <- filter(d, id=="T16_3")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)


p1 <- 
  ggplot(a %>% filter(v_id=="Hombres"), 
         aes(y = reorder(str_wrap(variables, 25), -as.numeric(orden)),
             x = props,
             label = paste0(round(props, 2), "%"),
             fill = as.factor(v_id))) + 
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = mcci_discrete[1]) +
  labs(title= unique(a$v_id)[1]) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")

p2 <- 
  ggplot(a %>% filter(v_id=="Mujeres"), 
         aes(y = reorder(str_wrap(variables, 25), -as.numeric(orden)),
             x = props,
             label = paste0(round(props, 2), "%"),
             fill = as.factor(v_id))) + 
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.3, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = mcci_discrete[5]) +
  labs(title= unique(a$v_id)[2]) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "italic"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")


c_0 <- 
  ggarrange(p1, p2, ncol = 2,nrow = 1)

annotate_figure(c_0,
                top = text_grob(str_wrap(fiuf,75), size = 38, face = "bold" , hjust = 0.5, family = "Arial Narrow"),
                bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
)

ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráficas 16.4 ----
a <- filter(d, id=="T16_4")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

b <- a %>% 
  select(-c(id,año,título:tipo_gráfica))%>% 
  pivot_wider(names_from = v_id, values_from = props)

ggplot(b, 
       aes(x=Hombres, xend=Mujeres, y=reorder(str_wrap(variables,25),-as.numeric(orden)), group=variables)) + 
  ggalt::geom_dumbbell(colour_x = mcci_discrete[1], colour_xend = mcci_discrete[5], colour = mcci_discrete[1],
                size_x = 6, size_xend = 6) + 
  geom_text(aes(x=Hombres, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(Hombres,"%")),
            color=mcci_discrete[1], size=7, vjust=2, fontface="bold")  +
  geom_text(aes(x=Mujeres, y=reorder(str_wrap(variables,25),-as.numeric(orden)), label=paste0(Mujeres,"%")),
            color=mcci_discrete[5], size=7, vjust=2, fontface="bold") +
  scale_x_continuous(
    limits = c(25,75),
    breaks = seq(25,75,10),
    labels = paste0(
      as.character(seq(25,75,10)), "%"
    )
  ) +
  scale_color_manual(name = "", values = c(mcci_discrete[1], mcci_discrete[5]) )+  
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = "En rojo se indican los porcentajes correspondientes a mujeres; en azul, lo correspondiente a hombres.",
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 34, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(1.0, 'cm'),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráficas 16.5 ----
a <- filter(d, id=="T16_5")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, 
       aes(x = str_wrap(v_id, width = 30), 
           fill = as.factor(año), 
           alpha = props,
           label = paste0(props, "%"),
           y = reorder(str_wrap(variables, width = 30), -as.numeric(orden)))) + 
  geom_tile() +
  geom_text(size = 10,
            col = "#ffffff") + 
  facet_wrap(~as.factor(año)) +
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5]))+
  scale_alpha_continuous("", range = c(.5,1)) +
  labs(title= str_wrap(fiuf, width = 80),
       subtitle = fiuff,
       caption = fiuffi) +
  theme_void() +
  theme(plot.title = element_text(size = 30, face = "bold",hjust = .5),
        plot.subtitle = element_text(size = 25,hjust = .5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 25),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        legend.position = "none") +
  coord_fixed()


ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# * Texto 17 ----
# ** Gráfica 17.1 ----
a <- filter(d, id=="T17_1")
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, aes(y = reorder(str_wrap(variables, 30), -as.numeric(orden)),
              x = props,
              label = paste0(round(props, 2), "%"),
              fill = as.factor(variables))) +
  geom_col(width = 0.2, position = position_dodge2()) + geom_text(hjust = -0.2, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[2], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráficas 17.3 ----
a <- filter(d, id=="T17_3") %>% 
  mutate(v_id = subtítulo,
         props = ifelse(v_id=="Hombre",props*-1,props))
fiuf <- unique(a$título)
fiuff <- unique(a$subtítulo)
fiuffi <- unique(a$fuente)

ggplot(a, 
       aes(x = reorder(str_wrap(variables,15), abs(props), function(x){ sum(x) }), 
           y = round(props, 0), 
           label = paste0(abs(round(props, 0)), "%"),
           fill = v_id)) + 
  geom_bar(stat = "identity", width = .6) +
  geom_text(hjust = "outward", size = 8,
            position = position_dodge(width = 0)) +
  facet_grid(~ año) +
  scale_y_continuous(
    limits = c(-100,100),
    breaks = seq(-100,100,25),
    labels = paste0(
      c(as.character(seq(100,0,-25)),
        as.character(seq(25,100,25))), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 34, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.text.x = element_text(size = 30, face = "italic"),
        panel.spacing.x = unit(1.0, 'cm'),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom") +
  coord_flip() 
ggsave(filename = paste0(
  out, unique(a$id), ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

beepr::beep(1)
