# Paquetes ----
require(foreign)
require(mxmaps)
require(haven)
require(srvyr)
require(dplyr)
require(ggpubr)
require(gtable)
require(grid)
require(gridExtra)
require(tidyverse)

# Funciones para renombrar ----
rename_fac <- function(x){
  paste0("fac_", x)
}

recode_names <- function(x){
  x = 
    case_when(
      x == "p1" ~ v_names[1],
      x == "p3" ~ v_names[2],
      x == "p4" ~ v_names[3],
      x == "p5" ~ v_names[4],
      x == "p6a" ~ v_names[5],
      x == "p6b" ~ v_names[6],
      x == "p6c" ~ v_names[7],
      x == "p6d" ~ v_names[8],
      x == "p7" ~ v_names[9],
      x == "p8a" ~ v_names[10],
      x == "p8b" ~ v_names[11],
      x == "p8c" ~ v_names[12],
      x == "p8d" ~ v_names[13],
      x == "p8e" ~ v_names[14],
      x == "p8f" ~ v_names[15],
      x == "p8g" ~ v_names[16],
      x == "p8h" ~ v_names[17],
      x == "p8i" ~ v_names[18],
      x == "p8j" ~ v_names[19],
      x == "p8k" ~ v_names[20],
      x == "p8l" ~ v_names[21],
      x == "p8m" ~ v_names[22],
      x == "p8n" ~ v_names[23],
      x == "p8o" ~ v_names[24],
      x == "p8p" ~ v_names[25],
      x == "p8q" ~ v_names[26],
      x == "p8r" ~ v_names[27],
      x == "p8s" ~ v_names[28],
      x == "p8t" ~ v_names[29],
      x == "p9a" ~ v_names[30],
      x == "p9b" ~ v_names[31],
      x == "p9c" ~ v_names[32],
      x == "p10" ~ v_names[33],
      x == "p11" ~ v_names[34],
      x == "p12" ~ v_names[35],
      x == "p13" ~ v_names[36],
      x == "p14" ~ v_names[37],
      x == "p15" ~ v_names[38],
      x == "p16" ~ v_names[39],
      x == "p17" ~ v_names[40],
      x == "p18" ~ v_names[41],
      x == "p19" ~ v_names[42],
      x == "p20" ~ v_names[43],
      x == "p21" ~ v_names[44],
      x == "p22" ~ v_names[45],
      x == "p23" ~ v_names[46],
      x == "p24" ~ v_names[47],
      x == "p25" ~ v_names[48],
      x == "p26" ~ v_names[49],
      x == "p27" ~ v_names[50],
      x == "p28" ~ v_names[51],
      x == "p29a" ~ v_names[52],
      x == "p29b" ~ v_names[53],
      x == "p29c" ~ v_names[54],
      x == "p29d" ~ v_names[55],
      x == "p29e" ~ v_names[56],
      x == "p29f" ~ v_names[57],
      x == "p29g" ~ v_names[58],
      x == "p30" ~ v_names[59],
      x == "p31" ~ v_names[60],
      x == "p32a" ~ v_names[61],
      x == "p32b" ~ v_names[62],
      x == "p32c" ~ v_names[63],
      x == "p33a" ~ v_names[64],
      x == "p33b" ~ v_names[65],
      x == "p33c" ~ v_names[66],
      x == "p33d" ~ v_names[67],
      x == "p33e" ~ v_names[68],
      x == "p34a" ~ v_names[69],
      x == "p34b" ~ v_names[70],
      x == "p34c" ~ v_names[71],
      x == "p34d" ~ v_names[72],
      x == "p34e" ~ v_names[73],
      x == "p34f" ~ v_names[74],
      x == "p34g" ~ v_names[75],
      x == "p35" ~ v_names[76],
      x == "p36" ~ v_names[77],
      x == "p37" ~ v_names[78],
      x == "p38" ~ v_names[79],
      x == "p39a" ~ v_names[80],
      x == "p39b" ~ v_names[81],
      x == "p39c" ~ v_names[82],
      x == "p39d" ~ v_names[83],
      x == "p39e" ~ v_names[84],
      x == "p39f" ~ v_names[85],
      x == "p39g" ~ v_names[86],
      x == "p40a" ~ v_names[87],
      x == "p40b" ~ v_names[88],
      x == "p40c" ~ v_names[89],
      x == "p40d" ~ v_names[90],
      x == "p40e" ~ v_names[91],
      x == "p40f" ~ v_names[92],
      x == "p40g" ~ v_names[93],
      x == "p40h" ~ v_names[94],
      x == "p41" ~ v_names[95],
      x == "p42" ~ v_names[96],
      x == "p43" ~ v_names[97],
      x == "boleta" ~ v_names[98],
      x == "p1r" ~ v_names[99],
      x == "p3r" ~ v_names[100]
    )
}

# Paleta de colores ----
# #'000c2d' (azul oscuro)
# #'f72732' (rojo mcci)
# #'fff72d' (crema)
# #'ffffff' (blanco)
# #'333333' (texto)

mcci_discrete <- c(
  '#000c2d', '#0E9A9D', '#8a8c91', '#ecd756', '#f72732'
)

# Directorios ----
inp <- "Github/MCCI_LMFCI2020/01_datos/"
out <- "Github/MCCI_LMFCI2020/03_gráficas/"

# Mapa para gráficas ----
load(paste0(inp, "mxmap.RData"))
mxmap <- mxstate.map %>% 
  left_join(
    df_mxstate %>% 
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
d <- read.spss(paste0(inp,"N202003_MCCI.sav"),to.data.frame = T) %>% 
  as.tibble()
colnames(d) <- tolower(colnames(d))
codebook_2020  <- tibble(etiqueta = colnames(d)) %>%
  mutate(pregunta = attr(d, "variable.labels"))

# Recodificación ----
v_names <- codebook_2020$pregunta[c(16:113,139,140)]
v_names <- tolower(v_names)
v_names <- trimws(v_names)
v_names <- str_replace_all(v_names, "[[:punct:]]", "")
v_names <- str_replace_all(v_names, " ", "_")
v_names <- paste0("v_",v_names)
v_names[97] <- "v_43_sumando_todo_lo_que_ganan_los_integrantes_de_este_hogar_cuánto_ganan_en_total_al_mes"
v_names[98] <- "v_44_si_hoy_hubiera_elecciones_para_diputados_federales_por_cuál_partido_votaría_usted"
v_names[99] <- paste0(v_names[99],"_simple")
v_names[100] <- paste0(v_names[2],"_simple")

data_2020 <- d %>% 
  rename_all(tolower) %>% 
  rename_at(vars(starts_with("pond")),
            funs(rename_fac)) %>% 
  rename_at(
    vars(starts_with("fac")),
    funs(rename_fac)
  ) %>% 
  rename_at(
    vars(starts_with("fe")),
    funs(rename_fac)
  ) %>% 
  rename(
    n_punto = punto
  )  %>% 
  rename_at(
    vars(starts_with("p")),
    funs(recode_names)
  ) %>% 
  rename_at(
    vars(starts_with("bol")),
    funs(recode_names)
  ) %>% 
  rename(
    weight = fac_fe_final_,
    cruce_sexo = b,
    cruce_grupos_edad = gedad,
    cruce_circunscripcion = circ
  ) %>% 
  select(
    cve_unica_sec:tipo, starts_with("cruce"), starts_with("v_"), weight
  ) %>% 
  mutate_at(
    .vars = vars(starts_with("v_")), 
    .funs = funs(factor)
  ) %>% 
  mutate_at(
    .vars = vars(starts_with("cruce_")),
    .funs = funs(factor)
  ) 
data_2020$weight <- as.numeric(data_2020$weight)
data_2020[data_2020 == "Ns/Nc"] <- NA

# Proporciones simples ----
# * Tabla de frecuencias ----
test <- data.frame()
for (i in 2:length(v_names)){
  tempo <- data_2020 %>% 
    select(v_names[i], weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(starts_with("v_")),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(var_v = starts_with("v_")) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(var_v) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  test <- bind_rows(test, tempo)
  rm(tempo)
}

prop_simples_2020 <- test

# * Gráficas ----
v_names_loop <- str_replace_all(v_names, "v_", "")
v_num <- sub("\\_.*", "", v_names_loop)
v_num[98] <- "voto_dip" 
v_num[99] <- "1_simple" 
v_num[100] <- "3_simple" 

vector_fiuf <- as.character(codebook_2020$pregunta[c(16:113,139,140)])
vector_fiuf <- sub('.*\\. ', '', vector_fiuf)
vector_fiuf[97] <- "Sumando todo lo que ganan los integrantes de este hogar, ¿cuánto ganan en total al mes?"
vector_fiuf[99] <- paste0(vector_fiuf[99]," (simple)")
vector_fiuf[100] <- paste0(vector_fiuf[2]," (simple)")

fiuffi <- "Fuente: Encuesta MCCI-Reforma | marzo 2020"

a <- prop_simples_2020

for(i in 2:length(v_names_loop)){
  fiuf <- vector_fiuf[i]
  ggplot(a %>% filter(v_id == v_names_loop[i]), 
         aes(x = reorder(str_wrap(var_v,10), -prop), 
             y = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             fill = prop)) + 
    geom_bar(stat = "identity", width = .3) +
    geom_text(size = 7, vjust = -0.5) +
    scale_y_continuous(
      limits = c(0,100),
      breaks = seq(0,100,25),
      labels = paste0(
        as.character(seq(0,100,25)), "%"
      )
    ) + 
    scale_fill_gradient(high = '#000c2d', low = '#b2b6c0') +
    labs(title= str_wrap(fiuf, width = 75),
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold"),
          plot.subtitle = element_text(size = 25),
          plot.caption = element_text(size = 20),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "none")
  
  
  ggsave(filename = paste0(
    out, "00_simples/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
}
beepr::beep(1)
rm(i, a)

# Proporciones por sexo ----
# * Tabla de frecuencias ----
test <- data.frame()
for (i in 2:length(v_names)){
  tempo <- data_2020 %>% 
    select(v_names[i],cruce_sexo, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(sexo = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(sexo) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  test <- bind_rows(test, tempo)
  rm(tempo)
}

prop_sexo <- test

# * Gráficas ----
fiuff <- "Proporciones por sexo"

a <- prop_sexo %>% 
  mutate(
    prop = ifelse(
      str_detect(sexo, "ombr"), prop*(-1), prop
    )
  )


for(i in 2:length(v_names_loop)){
  fiuf <- vector_fiuf[i]
  ggplot(a %>% filter(v_id == v_names_loop[i]), 
         aes(x = reorder(str_wrap(var_v,15), abs(prop), function(x){ sum(x) }), 
             y = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             fill = sexo)) + 
    geom_bar(stat = "identity", width = .6) +
    geom_text(hjust = "outward", size = 7,
              position = position_dodge(width = 0)) +
    scale_y_continuous(
      limits = c(-100,100),
      breaks = seq(-100,100,25),
      labels = paste0(
        c(as.character(seq(100,0,-25)),
          as.character(seq(25,100,25))), "%"
      )
    ) + 
    scale_fill_manual("", values = c("#000c2d", "#f72732")) +
    labs(title= str_wrap(fiuf, width = 75),
         subtitle = fiuff,
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold"),
          plot.subtitle = element_text(size = 25),
          plot.caption = element_text(size = 20),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "bottom") +
    coord_flip() 
  
  
  ggsave(filename = paste0(
    out, "01_sexo/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
}

rm(i, a)

beepr::beep(1)

# Proporciones por grupo de edad ----
# * Tabla de frecuencias ----
test <- data.frame()
for (i in 2:length(v_names)){
  tempo <- data_2020 %>% 
    select(v_names[i], cruce_grupos_edad, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(grupos_edad = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(grupos_edad) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  test <- bind_rows(test, tempo)
  rm(tempo)
}

prop_grupos_edad <- test

# * Gráficas ----
fiuff <- "Proporciones por grupos de edad"

a <- prop_grupos_edad 

for(i in 2:length(v_names)){
  fiuf <- vector_fiuf[i]
  b <- a %>% 
    filter(v_id == v_names_loop[i]) %>% 
    filter(!prop<0.01)
  ggplot(b, 
         aes(x = paste0(grupos_edad, "\naños"), 
             fill = round(prop*100, 2), 
             label = paste0(abs(round(prop*100, 2)), "%"),
             y = var_v)) + 
    geom_tile() +
    geom_text(size = ifelse(length(levels(as.factor(b$var_v)))>6, 2.5, 7), 
              col = "#ffffff") + 
    scale_fill_gradientn(colours = mcci_discrete) +
    labs(title= str_wrap(fiuf, width = 40),
         subtitle = fiuff,
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 25, face = "bold"),
          plot.subtitle = element_text(size = 25),
          plot.caption = element_text(size = 20),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = "none") +
    coord_fixed()
  
  ggsave(filename = paste0(
    out, "02_grupos_edad/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
  
  rm(b)
}

rm(i, a)
beepr::beep(1)

# Proporciones por circunscripción electoral ----
# * Tabla de frecuencias ----
test <- data.frame()
for (i in 2:length(v_names)){
  tempo <- data_2020 %>% 
    select(v_names[i], cruce_circunscripcion, weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(c(starts_with("cruce_"),
                              starts_with("v_"))),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(circunscripción = starts_with("cruce_"),
           var_v = starts_with("v_")
    ) %>% 
    mutate(
      v_id = str_replace_all(v_names[i], "v_", "")
    ) %>% 
    drop_na(circunscripción) %>% 
    filter(!var_v=="98") %>% filter(!var_v=="99")
  
  test <- bind_rows(test, tempo)
  rm(tempo)
}

prop_circ <- test

# * Gráficas ----
fiuff <- "Proporciones por circunscripción"

a <- prop_circ 

for(i in 2:length(v_names)){
  fiuf <- vector_fiuf[i]
  for(x in 1:5){
    tempo <- 
      ggplot(a %>% filter(v_id == v_names_loop[i], circunscripción==x), 
             aes(x = str_wrap(var_v,10), 
                 y = round(prop*100, 2), 
                 label = paste0(abs(round(prop*100, 2)), "%"))) + 
      geom_bar(stat = "identity", width = .3, fill = mcci_discrete[x]) +
      geom_text(size = 7, vjust = -0.5) +
      scale_y_continuous(
        limits = c(0,100),
        breaks = seq(0,100,25),
        labels = paste0(
          as.character(seq(0,100,25)), "%"
        )
      ) + 
      scale_fill_gradientn(colours = mcci_discrete) +
      labs(title= paste0("Circunscripción ", x)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 25, face = "italic",hjust = 0.5),
            plot.subtitle = element_text(size = 25),
            plot.caption = element_text(size = 20),
            plot.background = element_rect(fill = "transparent",colour = NA),
            text = element_text(family = "Arial Narrow"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12),
            legend.position = "none") 
    assign(paste0("c_", x), tempo)
  }
  
  c_0 <- 
    ggarrange(c_1 + theme(axis.text.y = element_text(size = 12)),
              c_2 + theme(axis.text.y = element_blank()),
              c_3 + theme(axis.text.y = element_blank()),
              c_4 + theme(axis.text.y = element_text(size = 12)),
              c_5 + theme(axis.text.y = element_blank()), 
              leyenda,ncol = 3,nrow = 2)
  
  
  
  annotate_figure(c_0,
                  top = text_grob(str_wrap(fiuf,75), size = 30, face = "bold", family = "Arial Narrow"),
                  bottom = text_grob(fiuffi,hjust = 1, x = 1, size = 20, family = "Arial Narrow")
  )
  
  ggsave(filename = paste0(
    out, "03_circunscripción/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
  
  rm(c_0,c_1,c_2,c_3,c_4,c_5)
}

beepr::beep(1)

# Guardar tablas de frecuencias ----
openxlsx::write.xlsx(
  prop_simples_2020, paste0(out, "00_simples/00_props_simples.xlsx")
)

openxlsx::write.xlsx(
  prop_sexo,paste0(out, "01_sexo/00_props_sexo.xlsx")
)

openxlsx::write.xlsx(
  prop_grupos_edad, paste0(out, "02_grupos_edad/00_props_gedad.xlsx")
)

openxlsx::write.xlsx(
  prop_circ, paste0(out, "03_circunscripción/00_props_circ.xlsx")
)

beepr::beep(1)
