library(tidyverse) 
library(sf)

sf_use_s2(F)

source("script/src_funcoes_renan.R")

set.seed(1)

# VALIDACAO -------------------------------------------------------------------------------------------------------


# compara etas -----------------------------------------------------------------------

# com outlier

ensemble <- read_csv("input/marvin/torre/eucflux_eta.csv") %>% 
  janitor::clean_names() %>% 
  rename(date = datetime,
         eta_model = e_ta_model) %>% 
  select(-e_ta_eucflux) %>% 
  rename(eta = eta_model) %>% 
  mutate(origem = "ensemble")

torre <- readxl::read_excel("input/Eucflux_Fluxos_Torre_2008_2018_clayton_2021.xlsx", skip = 4) %>% 
  janitor::clean_names() %>% 
  mutate(dia = as.Date(date)) %>% 
  group_by(dia) %>% 
  summarise(eta_torre = sum(transpiracao_mm)) %>% 
  rename(date = dia) %>% 
  rename(eta = eta_torre) %>% 
  mutate(origem = "torre")

penman <- readxl::read_excel("input/balanco_hidrico_PENMAN_eucflux.xlsx") %>% 
  janitor::clean_names() %>% 
  select(yyyymmdd, etr_mm_penman) %>% 
  rename(date = yyyymmdd,
         eta_penman = etr_mm_penman) %>% 
  mutate(origem = "penman",
         date = as.Date(date)) %>% 
  rename(eta = eta_penman) %>% 
  mutate(origem = "penman")

modis_terra <- read_csv("input/modis/filtered_scaled_ET_500m_terra.csv", 
                  col_names = c("full", "produto", "aquisicao", "coords", "processamento", "variavel", "valor")) %>% 
  select(produto, aquisicao, valor) %>% 
  mutate(ano = str_sub(aquisicao, 2, 5),
         julian = str_remove(aquisicao, "^..."),
         data = as.Date(julian, "%y%j"),
         valor = as.numeric(valor))
  
modis_aqua <- read_csv("input/modis/filtered_scaled_ET_500m_aqua.csv", 
                        col_names = c("full", "produto", "aquisicao", "coords", "processamento", "variavel", "valor")) %>% 
  select(produto, aquisicao, valor) %>% 
  mutate(ano = str_sub(aquisicao, 2, 5),
         julian = str_remove(aquisicao, "^..."),
         data = as.Date(julian, "%y%j"),
         valor = as.numeric(valor)) %>% 
  select(data_aqua = data,
         valor_aqua = valor)

modis <- bind_cols(modis_terra, modis_aqua) %>% 
  mutate(valor = if_else(is.na(valor), valor_aqua, valor)/8,
         origem = "modis") %>% 
  select(date = data,
         eta = valor, origem)
  
df <- bind_rows(ensemble, torre, penman, modis) %>% 
  filter(date >= as.Date("2013-01-01"),
         date <= as.Date("2019-01-01")) %>% 
  arrange(desc(origem), date)

ggplot(df %>% filter(origem %in% c("torre"))) + 
  geom_line(aes(date, eta, color = fct_inorder(origem)), size = 2, alpha = 0.8) + 
  scale_color_manual(name = "Origem ET", 
                     values = c("torre" = "#AF423F"),
                     labels = c("torre" = "Torre de fluxo")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 15)) +
  ylab("Evapotranspiração (mm/dia)") +
  xlab("Ano") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # legend.position = c(0.82, 0.86),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_raw_com_outlier.png",
       width = 14, height = 8)

# distribuição ET - distribuicao similar a normal, justifica IQR para outliers

ggplot(df %>% filter(origem == "torre")) +
  geom_histogram(aes(eta)) +
  ylab("Observações") +
  xlab("Evapotranspiração (mm/dia)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/outlier_eta_torre_hist.png",
       width = 8, height = 6)


# sem outlier

ensemble <- read_csv("input/marvin/torre/eucflux_eta.csv") %>% 
  janitor::clean_names() %>% 
  rename(date = datetime,
         eta_model = e_ta_model) %>% 
  mutate(eta_model = remove_outliers(eta_model)) %>%
  select(-e_ta_eucflux) %>% 
  rename(eta = eta_model) %>% 
  mutate(origem = "ensemble") %>% 
  mutate(eta = zoo::na.approx(eta)) %>% 
  filter(!eta < 1)

torre <- readxl::read_excel("input/Eucflux_Fluxos_Torre_2008_2018_clayton_2021.xlsx", skip = 4) %>% 
  janitor::clean_names() %>% 
  mutate(dia = as.Date(date)) %>% 
  group_by(dia) %>% 
  summarise(eta_torre = sum(transpiracao_mm)) %>% 
  rename(date = dia) %>% 
  mutate(eta_torre = remove_outliers(eta_torre)) %>%
  rename(eta = eta_torre) %>% 
  mutate(origem = "torre") %>% 
  mutate(eta = zoo::na.approx(eta)) %>% 
  filter(!eta < 1)

penman <- readxl::read_excel("input/balanco_hidrico_PENMAN_eucflux.xlsx") %>% 
  janitor::clean_names() %>% 
  select(yyyymmdd, etr_mm_penman) %>% 
  rename(date = yyyymmdd,
         eta_penman = etr_mm_penman) %>% 
  mutate(origem = "penman",
         date = as.Date(date)) %>% 
  mutate(eta_penman = remove_outliers(eta_penman)) %>%
  rename(eta = eta_penman) %>% 
  mutate(origem = "penman") %>% 
  mutate(eta = zoo::na.approx(eta)) %>% 
  filter(!eta < 1)

modis <- bind_cols(modis_terra, modis_aqua) %>% 
  mutate(valor = if_else(is.na(valor), valor_aqua, valor)/8,
         origem = "modis") %>% 
  select(date = data,
         eta = valor, origem) %>% 
  mutate(eta = remove_outliers(eta))

df <- bind_rows(ensemble, torre, penman, modis) %>% 
  filter(date >= as.Date("2013-01-01"),
         date <= as.Date("2019-01-01")) %>% 
  arrange(desc(origem), date)

ggplot(df %>% filter(origem %in% c("torre"))) + 
  geom_line(aes(date, eta, color = fct_inorder(origem)), size = 2, alpha = 0.8) + 
  scale_color_manual(name = "Origem ET", 
                     values = c("torre" = "#AF423F"),
                     labels = c("torre" = "Torre de fluxo")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 15)) +
  ylab("Evapotranspiração (mm/dia)") +
  xlab("Ano") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # legend.position = c(0.82, 0.86),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_raw_sem_outlier.png",
       width = 14, height = 8)

#compara torre com outras origens

ggplot(df %>% filter(origem %in% c("torre", "ensemble"))) + 
  geom_line(aes(date, eta, color = fct_inorder(origem)), size = 2, alpha = 0.8) + 
  scale_color_manual(name = "Origem ET", 
                     values = c("torre" = "#AF423F",
                                "ensemble" = "gray40"),
                     labels = c("torre" = "Torre de fluxo",
                                "ensemble" = "Ensemble")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 10)) +
  ylab("Evapotranspiração (mm/dia)") +
  xlab("Ano") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # legend.position = c(0.82, 0.86),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_torre_ensemble.png",
       width = 14, height = 8)

ggplot(df %>% filter(origem %in% c("torre", "modis"))) + 
  geom_line(aes(date, eta, color = fct_inorder(origem)), size = 2, alpha = 0.8) + 
  scale_color_manual(name = "Origem ET", 
                     values = c("torre" = "#AF423F",
                                "modis" = "gray30"),
                     labels = c("torre" = "Torre de fluxo",
                                "modis" = "Modis")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 10)) +
  ylab("Evapotranspiração (mm/dia)") +
  xlab("Ano") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # legend.position = c(0.82, 0.86),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_torre_modis.png",
       width = 14, height = 8)

ggplot(df %>% filter(origem %in% c("torre", "penman"))) + 
  geom_line(aes(date, eta, color = fct_inorder(origem)), size = 2, alpha = 0.8) + 
  scale_color_manual(name = "Origem ET", 
                     values = c("torre" = "#AF423F",
                                "penman" = "gray40"),
                     labels = c("torre" = "Torre de fluxo",
                                "penman" = "Penman-M.")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 10)) +
  ylab("Evapotranspiração (mm/dia)") +
  xlab("Ano") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # legend.position = c(0.82, 0.86),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_torre_penman.png",
       width = 14, height = 8)

# Teste T entre as ETs ----------------------------------------

df_t <- df %>% 
  pivot_wider(names_from = origem, values_from = eta)

torre_ensemble <- t.test(df_t$torre, df_t$ensemble, paired = F)
torre_ensemble
resultado_t_test(torre_ensemble)

torre_penman <- t.test(df_t$torre, df_t$penman, paired = F)
torre_penman
resultado_t_test(torre_penman)

torre_modis <- t.test(df_t$torre, df_t$modis, paired = F)
torre_modis
resultado_t_test(torre_modis)


# correlacao de evapos DIA MES ANO --------------------------------------------------------------------------------

df <- bind_rows(ensemble, torre, penman, modis) %>% 
  pivot_wider(names_from = origem, values_from = eta)

df_dia <- df %>% 
  mutate(x = torre,
         y = ensemble)

ggplot(df_dia, aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  xlab("ETa Torre (mm/dia)") +
  ylab("ETa Ensemble (mm/dia)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_corr_dia.png",
       width = 8, height = 6)

df_mes <- df %>% 
  filter(!is.na(torre)) %>%
  mutate(ano_mes = str_extract(date, "^.......")) %>% 
  group_by(ano_mes) %>% 
  summarise(ensemble = sum(ensemble, na.rm = T),
            torre = sum(torre, na.rm = T)) %>% 
  mutate(x = torre,
         y = ensemble)

ggplot(df_mes, aes(x, y)) + 
  geom_point(alpha = 0.3, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  xlab("ETa Torre (mm/mes)") +
  ylab("ETa Ensemble (mm/mes)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_corr_mes.png",
       width = 8, height = 6)

# rain <- readxl::read_excel("input/marvin/torre/eucflux_estacao.xlsx") %>% 
#   janitor::clean_names() %>% 
#   filter(!is.na(dia)) %>% 
#   select(date = dia, 
#          ppt = rain) %>% 
#   mutate(date = as.Date(date),
#          ppt = as.numeric(ppt))

df_ano <- df %>% 
  filter(!is.na(torre)) %>% 
  # left_join(rain) %>% 
  mutate(ano = str_extract(date, "^....") %>% as.numeric()) %>% 
  group_by(ano) %>% 
  summarise(ensemble = sum(ensemble, na.rm = T),
            torre = sum(torre, na.rm = T)) %>% 
  mutate(x = torre,
         y = ensemble)
  #           ppt = sum(ppt, na.rm = T)) %>% 
  # mutate(et_ppt_ensemble = ensemble/ppt,
  #        et_ppt_torre = torre/ppt)
df_ano

ggplot(df_ano, aes(torre, ensemble)) + 
  geom_point(alpha = 0.3, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  xlab("ETa Torre (mm/ano)") +
  ylab("ETa Ensemble (mm/ano)") +
  # geom_line(aes(date, e_ta_eucflux), color = "red") + 
  # geom_line(aes(date, eta_torre), color = "blue") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_corr_ano.png",
       width = 8, height = 6)




# ANALISE AMBIENTE ------------------------------------------------------------------------------------------------


# carrega parcelas de inventario de interesse ---------------------------------------------------------------------

# parcelas_raw <- readxl::read_excel("input/10 07 2023 - Base Parcelas - MS - 2023.xlsx")
# 
# parcelas_ica <- parcelas_raw %>% #head(10000) %>%
#   janitor::clean_names() %>% 
#   mutate(narvha = as.numeric(narvha)) %>%
#   mutate(vtcc = as.numeric(vtcc)) %>%
#   filter(datamedicao >= "2015-07-01",
#          datamedicao <= "2023-12-31") %>%
#   mutate(key = paste0(up_c_r, "_", numparcela)) %>%
#   arrange(up_c_r, numparcela, datamedicao) %>%
#   group_by(key) %>%
#   nest() %>%
#   mutate(new = map(data, ~mutate(.x,
#                                  vtcc_anterior = lag(vtcc),
#                                  ica = vtcc - vtcc_anterior,
#                                  datamedicao_anterior = as.Date(lag(datamedicao))))) %>%
#   ungroup() %>%
#   select(new) %>%
#   unnest(new) %>%
#   mutate(data_plantio = as.Date(case_when(rotacao == 1 ~ dataplantio,  rotacao > 1 ~ datainirotacao)),
#          datamedicao = as.Date(datamedicao),
#          # idade_inv = as.numeric((datamedicao - data_plantio)) / 364.25
#          ) %>% # use a time delta function
#   mutate(#ima = vtcc/idinv,
#          dias_corridos = datamedicao - datamedicao_anterior) %>%
#   mutate(key = paste0(up_c_r, "_", numparcela)) %>%
#   select(key, up_c_r, latitude, longitude, numparcela,
#          data_plantio,
#          dataplantio, datainirotacao, rotacao,
#          area_up = arearotacao,
#          propriedade = desctipopropriedade,
#          umn = unidademanejo,
#          zonaclimatica,
#          zona_fisiografica = zona_fisiografica_zf,
#          macroambiente,
#          altitude = altitudeparcela,
#          matgen = descmatgen,
#          datamedicao,
#          tipoinventario = nometipoinventario,
#          tipoparcela, anoref, nummedicao, mhdom, narvha, pmortas, pdominadas, pquebra,
#          idinv,
#          imcom, imcim,
#          area_basal = ab,
#          vtcc,
#          imacc,
#          ica, vtcc_anterior, datamedicao_anterior, dias_corridos) %>% 
#   mutate(imcom = as.numeric(imcom),
#          imcim = as.numeric(imcim)) %>% 
#   mutate(gmato = 10/3 * (imcom - 1) * 10/9 * imcim)
# 
# embrapra <- raster::raster("input/br_available_water_100-200cm_pred_mm_cm.tif")
# 
# dc <- raster::raster("input/DC_MS/hand_15/")
# 
# altitude <- raster::raster("input/dem_srtm-30m_ms.tif")
# 
# parcelas_info_aux <- parcelas_ica %>%
#   distinct(key, .keep_all = T) %>%
#   mutate(longitude = as.double(longitude),
#          latitude = as.double(latitude)) %>%
#   filter(!is.na(longitude),
#          !is.na(latitude)) %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4674) %>%
#   st_buffer(convert_degree(30))
# 
# unf <- read_sf("input/mme_limite_unf.shp") %>%
#   filter(unf == "MS")
# 
# parcelas_info <- parcelas_info_aux %>%
#   select(-altitude) %>%
#   mutate(cad = exactextractr::exact_extract(embrapra, ., fun = "mean")*100,
#          dc = exactextractr::exact_extract(dc, ., fun = "mean"),
#          altitude = exactextractr::exact_extract(altitude, ., fun = "mean")) %>%
#   filter_polygon(unf) %>%
#   st_drop_geometry() %>%
#   select(key, cad, dc, altitude)
# 
# parcelas_ica_info <- parcelas_ica %>%
#   select(-altitude) %>%
#   left_join(parcelas_info) %>%
#   filter(key %in% parcelas_info$key)
# 
# write_csv(parcelas_ica_info, "marvin/ms_parcels_info.csv")

marvin_raw <- read_csv("input/marvin/eta_parcel_daily.csv")

marvin <- marvin_raw %>% 
  select(key, date_eta = datetime, eta = ETa) %>% 
  filter(date_eta >= as.Date('2015-07-01'))

marvin_key_aux <- marvin %>% pull(key) %>% unique()

merge_ppt <- read_csv("input/ms_parcelas_ppt_mensal.csv") %>% 
  filter(ano >= 2015) %>% #pois é quanto começam os dados de ETa
  transmute(key = key,
            date = as.Date(paste0(ano, "-", mes, "-15")),
            ppt_mes = ppt) %>% 
  mutate(ano_mes = str_extract(date, "^......."),
         key_ppt = paste0(key, ano_mes)) %>% 
  filter(date >= as.Date('2015-07-01'))

parcelas <- read_csv("marvin/ms_parcels_info.csv") %>% 
  filter(data_plantio >= as.Date('2015-07-01')) %>% #pois é quanto começam os dados de ETa
  select(-dataplantio, -datainirotacao) %>% 
  mutate(key_inv = paste0(key, "_", datamedicao)) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(rotacao == 1,
         # !propriedade == "FOMENTO",
         key %in% marvin_key_aux,
         pmortas == 0,
         # pdominadas == 0,
         # pquebra == 0,
         idinv <= 8) %>% 
  mutate(ordem = str_sub(umn, 4, 4),
         textura = str_sub(umn, 5, 5),
         solo = paste0(ordem, textura)) %>% 
  filter(!is.na(vtcc),
         !is.na(area_basal)) %>% 
  filter(!area_basal == 0) %>% 
  mutate(int = vtcc/area_basal) %>% 
  filter(int > 4,
         int < 15) %>% 
  group_by(key) %>% 
  nest() %>% 
  mutate(ultima_medicao = map(data, ~max(.$datamedicao))) %>% 
  unnest(ultima_medicao) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  filter(!vtcc > 450)

# matgen mask

matmask <- readxl::read_excel("output/matgen_mask.xlsx")

# matmask <- parcelas %>%
#   select(matgen) %>%
#   distinct(matgen, .keep_all = T) %>%
#   arrange(matgen) %>%
#   rowid_to_column() %>%
#   rename(id = rowid) %>%
#   mutate(id = str_pad(id, 2, pad = "0", side = "left"),
#          matgen_mask = paste0("GEN_", id)) %>%
#   select(-id)
# matmask
# 
# writexl::write_xlsx(matmask, "output/matgen_mask.xlsx")

# contagem de parcelas
completo <- read_csv("marvin/ms_parcels_info.csv") %>% 
  filter(key %in% marvin_key_aux)
x<-unique(completo$key)
length(x)

contagem <- parcelas %>%
  mutate(cod_faz = str_extract(up_c_r, "^...."),
         up = str_extract(up_c_r, "^......"))
x<-unique(contagem$cod_faz)
length(x)
x<-unique(contagem$up)
length(x)
x<-unique(contagem$key)
length(x)
x<-unique(contagem$key_inv)
length(x)
sum(distinct(contagem, up, .keep_all = T)$area_up)

# writexl::write_xlsx(parcelas, "output/ms_parcelas_filtradas.xlsx")


# qualidade de parcelas - area basal + vtcc -----------------------------------------------------------------------

parcelas <- read_csv("marvin/ms_parcels_info.csv") %>%
  select(-dataplantio, -datainirotacao) %>%
  mutate(key_inv = paste0(key, "_", datamedicao)) %>%
  distinct(key_inv, .keep_all = T) %>%
  filter(!is.na(vtcc),
         !is.na(area_basal))

ab <- parcelas

ggplot(ab, aes(area_basal, vtcc)) +
  geom_point(alpha = 0.3) +
  xlab("Área Basal (m³/ha)") +
  ylab("VTCC (m³/ha)") +
  theme_light(20) +
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_area_basal_completo.png",
       width = 8, height = 6)

parcelas <- read_csv("marvin/ms_parcels_info.csv") %>% 
  filter(data_plantio >= as.Date("2015-07-01")) %>%
  select(-dataplantio, -datainirotacao) %>% 
  mutate(key_inv = paste0(key, "_", datamedicao)) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(rotacao == 1,
         # !propriedade == "FOMENTO",
         key %in% marvin_key_aux,
         # pmortas == 0,
         # pdominadas == 0,
         # pquebra == 0,
         idinv <= 8) %>% 
  mutate(ordem = str_sub(umn, 4, 4),
         textura = str_sub(umn, 5, 5),
         solo = paste0(ordem, textura)) %>% 
  filter(!is.na(vtcc),
         !is.na(area_basal)) %>% 
  filter(!vtcc > 450)

ab <- parcelas

ggplot(ab, aes(area_basal, vtcc)) + 
  geom_point(alpha = 0.3) +
  xlab("Área Basal (m³/ha)") +
  ylab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_area_basal_idade_rotacao.png",
       width = 8, height = 6)

parcelas <- read_csv("marvin/ms_parcels_info.csv") %>% 
  filter(data_plantio >= as.Date("2015-07-01")) %>%
  select(-dataplantio, -datainirotacao) %>% 
  mutate(key_inv = paste0(key, "_", datamedicao)) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(rotacao == 1,
         # !propriedade == "FOMENTO",
         key %in% marvin_key_aux,
         # pmortas == 0,
         # pdominadas == 0,
         # pquebra == 0,
         idinv <= 8) %>% 
  mutate(ordem = str_sub(umn, 4, 4),
         textura = str_sub(umn, 5, 5),
         solo = paste0(ordem, textura)) %>% 
  filter(!is.na(vtcc),
         !is.na(area_basal)) %>% 
  filter(!area_basal == 0) %>% 
  mutate(int = vtcc/area_basal) %>% 
  filter(int > 4,
         int < 15) %>% 
  filter(!vtcc > 450)

ab <- parcelas

# ggplot(ab, aes(int)) + 
#   geom_histogram()

ggplot(ab, aes(area_basal, vtcc)) + 
  geom_point(alpha = 0.3) +
  xlab("Área Basal (m³/ha)") +
  ylab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_area_basal_filtrado_com_mortas.png",
       width = 8, height = 6)

parcelas <- read_csv("marvin/ms_parcels_info.csv") %>% 
  filter(data_plantio >= as.Date("2015-07-01")) %>%
  select(-dataplantio, -datainirotacao) %>% 
  mutate(key_inv = paste0(key, "_", datamedicao)) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(rotacao == 1,
         # !propriedade == "FOMENTO",
         key %in% marvin_key_aux,
         pmortas == 0,
         # pdominadas == 0,
         # pquebra == 0,
         idinv <= 8) %>% 
  mutate(ordem = str_sub(umn, 4, 4),
         textura = str_sub(umn, 5, 5),
         solo = paste0(ordem, textura)) %>% 
  filter(!is.na(vtcc),
         !is.na(area_basal)) %>% 
  filter(!area_basal == 0) %>% 
  mutate(int = vtcc/area_basal) %>% 
  filter(int > 4,
         int < 15) %>% 
  group_by(key) %>% 
  nest() %>% 
  mutate(ultima_medicao = map(data, ~max(.$datamedicao))) %>% 
  unnest(ultima_medicao) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  filter(!vtcc > 450)

ab <- parcelas

ggplot(ab, aes(area_basal, vtcc)) + 
  geom_point(alpha = 0.3) +
  xlab("Área Basal (m³/ha)") +
  ylab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_area_basal_filtrado_sem_mortas.png",
       width = 8, height = 6)


# idade distribuição --------------------------------------------------------------------------------------

parcelas_idade <- parcelas %>% 
  select(key_inv, up_c_r, idinv) %>% 
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2),
         classe_mid = as.numeric(paste0(classe_idinv, ".5"))) %>% 
  group_by(classe_idinv) %>% 
  nest() %>% 
  mutate(n = map_dbl(data, ~nrow(.x))) %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(n = replace(n, duplicated(n), NA))

ggplot(parcelas_idade) +
  geom_histogram(aes(idinv)) +
  geom_label(aes(x = classe_mid, y = 10000, label = n), size = 5.5) +
  scale_x_continuous(n.breaks = 8, limits = c(1, 8)) +
  geom_vline(xintercept = c(2, 3, 4, 5, 6, 7), color = "#AF423F", lwd = 1) +
  ylab("Observações") +
  xlab("Idade") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_hist.png",
       width = 8, height = 6)

parcelas_idade_ajustado <- parcelas %>% 
  select(key_inv, up_c_r, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.9, 2.1) ~ 2,
                                  between(idinv, 2.9, 3.1) ~ 3,
                                  between(idinv, 3.9, 4.1) ~ 4,
                                  TRUE ~ NA),
         classe_mid = as.numeric(paste0(classe_idinv, ".2"))) %>% 
  filter(!is.na(classe_idinv)) %>% 
  group_by(classe_idinv) %>% 
  nest() %>% 
  mutate(n = map_dbl(data, ~nrow(.x))) %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(n = replace(n, duplicated(n), NA))

ggplot(parcelas_idade_ajustado) +
  geom_histogram(aes(idinv)) +
  geom_label(aes(x = classe_mid, y = 5000, label = n), size = 5.5) +
  scale_x_continuous(breaks = seq(1, 5, 0.1), limits = c(1.8, 4.2)) +
  geom_vline(xintercept = c(2, 3, 4), color = "#AF423F", lwd = 1) +
  ylab("Observações") +
  xlab("Idade") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_hist_ajustado.png",
       width = 8, height = 6)

parcelas_idade_completo <- parcelas %>% 
  select(key_inv, up_c_r, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA),
         classe_mid = as.numeric(paste0(classe_idinv, ".5"))) %>% 
  filter(!is.na(classe_idinv)) %>% 
  group_by(classe_idinv) %>% 
  nest() %>% 
  mutate(n = map_dbl(data, ~nrow(.x))) %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(n = replace(n, duplicated(n), NA))

ggplot(parcelas_idade_completo) +
  geom_histogram(aes(idinv)) +
  geom_label(aes(x = classe_mid, y = 9000, label = n), size = 5.5) +
  scale_x_continuous(n.breaks = 8, limits = c(1, 8)) +
  geom_vline(xintercept = c(2, 3, 4, 5, 6, 7), color = "#AF423F", lwd = 1) +
  ylab("Observações") +
  xlab("Idade") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_hist_completo.png",
       width = 8, height = 6)

# anomalia de chuvas por fazenda ----------------------------------------------------------------------------------

merge_anomalia <- read_csv("input/ms_parcelas_ppt_mensal.csv") %>% 
  mutate(cod_faz = str_extract(key, "^...."),
         date = as.Date(paste0(ano, "-", mes, "-15"))) %>% 
  group_by(cod_faz, date) %>% 
  summarise(ppt_mes = mean(ppt, na.rm = T)) %>% 
  mutate(ano = lubridate::year(date)) %>% 
  group_by(cod_faz, ano) %>% 
  summarise(ppt_ano = sum(ppt_mes, na.rm = T)) %>% 
  ungroup()

aux_ppt_anom <- merge_anomalia %>% 
  group_by(ano) %>% 
  summarise(p = mean(ppt_ano)) %>% 
  ungroup() %>% 
  summarise(p = mean(p))

df <- merge_anomalia %>% 
  mutate(p_hist = aux_ppt_anom$p,
         p_anom = ppt_ano - p_hist) %>% 
  group_by(cod_faz) %>% 
  nest() %>% 
  mutate(ordena = map_dbl(data, ~sum(.x$p_anom))) %>% 
  unnest() %>% 
  ungroup()
df

ggplot(df)+
  geom_tile(aes(x = reorder(cod_faz, -ordena),
                y = factor(ano, levels = rev(unique(ano))), 
                fill = p_anom)) + #factor(ano, levels = rev(unique(ano)))
  scale_fill_viridis_c(option = "turbo", 
                       direction = -1, 
                       limits = c(-880, 880), 
                       na.value = "white") +
  xlab("Fazendas") +
  labs(fill = "Anomalia \nchuva (mm)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(size=20),
        axis.title.y = element_blank(),
        # legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, size = 6),
        axis.ticks.x = element_line(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/anomalia_chuva_fazenda.png",
       width = 24, height = 12)

# sensibilidade a colheita e plantio ----------------------------------------------------------------------------------

colheita <- readxl::read_excel("input/colheita/Consulta Colheita - CORPORATIVO-2020_12.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(tipo_intervencao %in% c("CORTE TOTAL")) %>% 
  select(up_c_r, data_fim_corte) %>% 
  filter(!is.na(data_fim_corte)) %>% 
  mutate(data_fim_corte = as.Date(data_fim_corte))
colheita

cad_22 <- readxl::read_excel("input/Situação Atual do Cadastro - CORPORATIVO - 2022_12.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(data_novo_plantio = as.Date(case_when(rotacao == 1 ~ data_de_plantio_conducao,  rotacao > 1 ~ inicio_rotacao))) %>% 
  select(up, data_novo_plantio)

parcelas_colheita <- read_csv("marvin/ms_parcels_info.csv") %>% 
  # filter(data_plantio >= as.Date('2015-07-01')) %>% #pois é quanto começam os dados de ETa
  select(-dataplantio, -datainirotacao) %>% 
  mutate(key_inv = paste0(key, "_", datamedicao)) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(rotacao == 1,
         # !propriedade == "FOMENTO",
         key %in% marvin_key_aux,
         pmortas == 0,
         # pdominadas == 0,
         # pquebra == 0,
         idinv <= 8) %>% 
  mutate(ordem = str_sub(umn, 4, 4),
         textura = str_sub(umn, 5, 5),
         solo = paste0(ordem, textura)) %>% 
  filter(!is.na(vtcc),
         !is.na(area_basal)) %>% 
  filter(!area_basal == 0) %>% 
  mutate(int = vtcc/area_basal) %>% 
  filter(int > 4,
         int < 15) %>% 
  group_by(key) %>% 
  nest() %>% 
  mutate(ultima_medicao = map(data, ~max(.$datamedicao))) %>% 
  unnest(ultima_medicao) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  filter(!vtcc > 450)

unique(sense$ano_mes)

set.seed(1)

sense <- parcelas_colheita %>% 
  select(key, up_c_r, gmato) %>% 
  inner_join(colheita) %>% 
  distinct(up_c_r, .keep_all = T) %>% 
  mutate(mes_corte = lubridate::month(data_fim_corte, label = T),
         ano_mes = str_extract(data_fim_corte, "^.......")) %>% 
  mutate(up = str_extract(up_c_r, "^......")) %>% 
  left_join(cad_22) %>% 
  mutate(tot = data_novo_plantio - data_fim_corte) %>% 
  filter(tot <= 370, tot >= 45) %>%
  group_by(ano_mes) %>% 
  nest() %>% 
  mutate(sorteio = map(data, ~sample_n(.x, 1))) %>% 
  select(-data) %>% 
  unnest(sorteio) %>% 
  ungroup() %>% 
  left_join(marvin) %>% 
  filter(date_eta >= as.Date("2019-01-01"),
         date_eta <= as.Date("2022-06-30"))
sense

ggplot(sense) +
  geom_line(aes(date_eta, eta), alpha = 0.8) +
  geom_vline(aes(xintercept = data_fim_corte), color = "#AF423F", lwd = 1) +
  geom_vline(aes(xintercept = data_novo_plantio), color = "#AF423F", lwd = 1) +
  scale_x_date(date_breaks = "4 month", 
               date_labels = "%Y-%m", 
               limits = c(as.Date('2019-01-01'), as.Date('2022-06-30'))) +
  facet_wrap(~mes_corte) +
  ylab("ETa Ensemble (mm/ano)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size=20, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/sensibilidade_colheita_plantio.png",
       width = 28, height = 16)

# decision tree ----------------------------------------------------------------------------------
# 
# x <- unique(parcelas$key_inv)
# aux_sample <- sample(unique(parcelas$key_inv), 47065) # 47065
# 
# parcelas_dt <- parcelas %>%
#   filter(key_inv %in% aux_sample)
# 
# aux_ac <- parcelas_dt %>%
#   select(key_inv) %>%
#   distinct(key_inv, .keep_all = T)
# 
# et_ac_1 <- aux_ac %>%
#   head(round(nrow(aux_ac)*0.5, 0)+1)
# 
# et_ac_2 <- aux_ac %>%
#   tail(round(nrow(aux_ac)*0.5, 0))
# 
# # teste <- bind_rows(et_ac_1, et_ac_2)
# #
# # all(aux_ac$key_inv %in% teste$key_inv)
# 
# parcelas_d_tree_1 <- parcelas_dt %>%
#   filter(key_inv %in% et_ac_1$key_inv) %>%
#   mutate(key = str_remove(key_inv, "...........$")) %>%
#   distinct(key_inv, .keep_all = T) %>%
#   left_join(marvin)
# 
# evapo_acumulado_1 <- parcelas_d_tree_1 %>% #sample_n(10) %>%
#   filter(date_eta >= data_plantio,
#          date_eta <= datamedicao) %>%
#   group_by(key_inv) %>%
#   summarise(eta = sum(eta, na.rm = T)) %>%
#   ungroup()
# 
# write_csv(evapo_acumulado_1, "output/evapo_acumulado_1.csv")
# 
# parcelas_d_tree_2 <- parcelas_dt %>%
#   filter(key_inv %in% et_ac_2$key_inv) %>%
#   mutate(key = str_remove(key_inv, "...........$")) %>%
#   distinct(key_inv, .keep_all = T) %>%
#   left_join(marvin)
# 
# evapo_acumulado_2 <- parcelas_d_tree_2 %>% #sample_n(10) %>%
#   filter(date_eta >= data_plantio,
#          date_eta <= datamedicao) %>%
#   group_by(key_inv) %>%
#   summarise(eta = sum(eta, na.rm = T)) %>%
#   ungroup()
# 
# write_csv(evapo_acumulado_2, "output/evapo_acumulado_2.csv")
# 
# evapo_acumulado <- bind_rows(evapo_acumulado_1, evapo_acumulado_2)
# 
# writexl::write_xlsx(evapo_acumulado, "output/evapo_acumulado_rpart.xlsx")
evapo_acumulado <- readxl::read_xlsx("output/evapo_acumulado_rpart.xlsx")

parcelas_no_inv <- parcelas %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_idinv)) %>% 
  select(-datamedicao:-dias_corridos, narvha)

# skimr::skim(parcelas_no_inv)

parcelas_tree <- evapo_acumulado %>% 
  inner_join(parcelas_no_inv) 

date_aux <- seq(as.Date(min(parcelas$data_plantio)), as.Date(max(parcelas$datamedicao)), by = '1 day') %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  rename(data_plantio = value,
         data_continua = rowid)

arv_parcelas <- parcelas_tree %>% 
  select(-key, -key_inv, -up_c_r, -numparcela, -propriedade) %>% 
  left_join(date_aux) %>% 
  select(-data_plantio) %>% 
  select(-data_continua, -ultima_medicao, -umn, -latitude, -longitude, -int, -area_up, -rotacao) %>% 
  select(-zona_fisiografica, -solo) %>% 
  # drop_na() %>% 
  mutate_if(is.character, as.factor) 

skimr::skim(arv_parcelas)

x <- names(arv_parcelas)

# file <- file("output/defesa/variaveis_decisiontree_ambiente.txt")
# writeLines(x, file)
# close(file)

arv <- rpart::rpart(data = arv_parcelas %>% filter(classe_idinv == 5),
                    formula = eta ~.,
                    method = "anova",
                    cp = 0.0005, maxdepth = 5, 
                    minsplit = nrow(arv_parcelas) * 0.05)
arv

rpart.plot::rpart.plot(arv, cex = 1)


# idade decision tree ----------------------------------------------------------

df_eta_idade <- parcelas %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(idinv <= 8) %>%
  inner_join(evapo_acumulado) %>% 
  group_by(idinv) %>% 
  summarise(eta = mean(eta, na.rm = T))

ggplot(df_eta_idade) + 
  geom_col(aes(idinv, eta)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Idade da Floresta (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_idade.png",
       width = 8, height = 6)

df_eta_idade_classe <- parcelas %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_idinv)) %>%
  inner_join(evapo_acumulado) %>% 
  group_by(classe_idinv) %>% 
  summarise(eta = mean(eta, na.rm = T))

ggplot(df_eta_idade_classe) + 
  geom_col(aes(classe_idinv, eta)) +
  geom_label(aes(classe_idinv, eta, label = round(eta,0)), vjust = 1.2) +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7)) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Classe de Idade (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_classe_idade.png",
       width = 8, height = 6)

df_idade_corr <- parcelas %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(idinv <= 8) %>%
  inner_join(evapo_acumulado) %>% 
  group_by(idinv) %>% 
  summarise(eta = mean(eta, na.rm = T)) %>% 
  mutate(x = idinv,
         y = eta)

ggplot(df_idade_corr, aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Idade (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_corr_idade.png",
       width = 8, height = 6)

# regressao ETa - analise do ambiente geral -------------------------------------------------------------------------

df_all_corr <- parcelas %>%
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_idinv)) %>% 
  left_join(evapo_acumulado) %>% 
  select(-datamedicao:-dias_corridos, narvha) %>% 
  select(-key, -key_inv, -up_c_r, -numparcela, -propriedade, -data_plantio) %>% 
  select(-ultima_medicao, -umn, -latitude, -longitude, -int, -area_up, -rotacao) %>% 
  select(-zona_fisiografica, -solo) %>% 
  drop_na() %>%
  mutate_if(is.character, as.factor)

# skimr::skim(df_all_corr)

# corr linear

line_corr_2 <- df_all_corr %>% 
  filter(classe_idinv == 2) %>% 
  select(-classe_idinv)

lm_line_corr_2 <- lm(formula = eta ~ ., data = line_corr_2)
r2 <- summary(lm_line_corr_2)
r2

pred_line_corr_2 <- data.frame(eta_pred = predict(lm_line_corr_2, line_corr_2)) %>% 
  as_tibble() %>% 
  bind_cols(line_corr_2)

ggplot(pred_line_corr_2, 
       aes(eta, eta_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 1250, y = 3300, 
           label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
           size = 6) +
  scale_x_continuous(limits = c(1000, 3500)) +
  scale_y_continuous(limits = c(1000, 3500)) +
  ylab("ETa Ensemble Predicted (mm/ciclo)") +
  xlab("ETa Ensemble (mm/ciclo)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_predict_corr_linear_classe_2.png",
       width = 8, height = 6)

line_corr_4 <- df_all_corr %>% 
  filter(classe_idinv == 4) %>% 
  select(-classe_idinv)

lm_line_corr_4 <- lm(formula = eta ~ ., data = line_corr_4)
r2 <- summary(lm_line_corr_4)
r2 

pred_line_corr_4 <- data.frame(eta_pred = predict(lm_line_corr_4, line_corr_4)) %>% 
  as_tibble() %>% 
  bind_cols(line_corr_4)

ggplot(pred_line_corr_4, 
       aes(eta, eta_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 3500, y = 6300, 
           label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
           size = 6) +
  scale_x_continuous(limits = c(3000, 6500)) +
  scale_y_continuous(limits = c(3000, 6500)) +
  ylab("ETa Ensemble Predicted (mm/ciclo)") +
  xlab("ETa Ensemble (mm/ciclo)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_predict_corr_linear_classe_4.png",
       width = 8, height = 6)

# corr polinomial

# x <- names(df_all_corr)
# file <- file("output/defesa/variaveis_poly_ambiente.txt")
# writeLines(x, file)
# close(file)

poly_corr_2 <- df_all_corr %>% 
  filter(classe_idinv == 2) %>% 
  select(-classe_idinv)

lm_poly_corr_2 <- lm(formula = eta ~ polym(zonaclimatica ,macroambiente, gmato, cad, dc, altitude,
                                           ordem, textura, matgen, narvha, 
                                           degree = 3, raw = TRUE), 
                     data = poly_corr_2)
r2 <- summary(lm_poly_corr_2)
r2

pred_poly_corr_2 <- data.frame(eta_pred = predict(lm_poly_corr_2, poly_corr_2)) %>% 
  as_tibble() %>% 
  bind_cols(poly_corr_2)

ggplot(pred_poly_corr_2, 
       aes(eta, eta_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 1250, y = 3300,  
           label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
           size = 6) +
  scale_x_continuous(limits = c(1000, 3500)) +
  scale_y_continuous(limits = c(1000, 3500)) +
  ylab("ETa Ensemble Predicted (mm/ciclo)") +
  xlab("ETa Ensemble (mm/ciclo)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_predict_corr_polinomial_classe_2.png",
       width = 8, height = 6)

poly_corr_4 <- df_all_corr %>% 
  filter(classe_idinv == 4) %>% 
  select(-classe_idinv)

lm_poly_corr_4 <- lm(formula = eta ~ polym(zonaclimatica, macroambiente, gmato, cad, dc, altitude,
                                           ordem, textura, matgen, narvha, 
                                           degree = 3, raw = TRUE), 
                     data = poly_corr_4)
r2 <- summary(lm_poly_corr_4)
r2

pred_poly_corr_4 <- data.frame(eta_pred = predict(lm_poly_corr_4, poly_corr_4)) %>% 
  as_tibble() %>% 
  bind_cols(poly_corr_4)

ggplot(pred_poly_corr_4, 
       aes(eta, eta_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 3500, y = 6300, 
           label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
           size = 6) +
  scale_x_continuous(limits = c(3000, 6500)) +
  scale_y_continuous(limits = c(3000, 6500)) +
  ylab("ETa Ensemble Predicted (mm/ciclo)") +
  xlab("ETa Ensemble (mm/ciclo)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_predict_corr_polinomial_classe_4.png",
       width = 8, height = 6)


# matgen --------------------------------------------------------------------------------------------

parcelas_matgen <- parcelas %>%
  select(key_inv, up_c_r, matgen, idinv, data_plantio) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>%
  group_by(matgen, classe_idinv) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~nrow(.x))) %>%
  filter(n >= 50) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(wrap_classe = paste0("Classe ", classe_idinv, " anos"))

x <- unique(parcelas_matgen$matgen)

ggplot(parcelas_matgen %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(reorder(matgen, -n)), stat="count") +
  ylab("Observações") +
  xlab("Material Genético") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/matgen_hist_classe_4.png",
       width = 8, height = 6)

df_matgen <- parcelas_matgen %>% 
  group_by(matgen, classe_idinv) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$eta, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_matgen %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(reorder(matgen, -eta_order), eta), alpha = 0.3) +
  geom_boxplot(aes(reorder(matgen, -eta_order), eta), alpha = 0.8) +
  geom_violin(aes(reorder(matgen, -eta_order), eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Material Genético") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/matgen_eta_classe_4.png",
       width = 8, height = 6)

df_matgen_corr <- df_matgen %>% 
  mutate(y = eta,
         x = as.factor(matgen))

ggplot(df_matgen_corr %>% filter(classe_idinv == 4), 
       aes(reorder(x, -eta_order), y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Material Genético") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/matgen_eta_corr_4.png",
       width = 8, height = 6)


# dc --------------------------------------------------------------------------------------------

parcelas_num <- parcelas %>%
  select(key_inv, up_c_r, dc, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) 

ggplot(parcelas_num %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(dc)) +
  ylab("Observações") +
  xlab("Zona Fisiográfica Contínua") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/dc_hist_classe_4.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_dc = case_when(dc > 75 ~ "ZF 4",
                               dc > 50 ~ "ZF 3",
                               dc > 25 ~ "ZF 2",
                               TRUE ~ "ZF 1")) 

ggplot(df_num %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(classe_dc, eta), alpha = 0.3) +
  geom_boxplot(aes(classe_dc, eta), alpha = 0.8) +
  geom_violin(aes(classe_dc, eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Zona Fisiográfica") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/dc_eta_classe_4.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = eta,
         x = dc)

ggplot(df_num_corr %>% filter(classe_idinv == 4), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Zona Fisiográfica") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/dc_eta_corr_4.png",
       width = 8, height = 6)


# cad --------------------------------------------------------------------------------------------

parcelas_num <- parcelas %>%
  select(key_inv, up_c_r, cad, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) 

ggplot(parcelas_num %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(cad)) +
  ylab("Observações") +
  xlab("CAD (mm/m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/cad_hist_classe_4.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_cad = case_when(cad > 150 ~ "> 150",
                                cad > 130 ~ "> 130",
                                cad > 110 ~ "> 110",
                               TRUE ~ "> 0")) 

ggplot(df_num %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(classe_cad, eta), alpha = 0.3) +
  geom_boxplot(aes(classe_cad, eta), alpha = 0.8) +
  geom_violin(aes(classe_cad, eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Classe de CAD (mm/m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/cad_eta_classe_4.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = eta,
         x = cad)

ggplot(df_num_corr %>% filter(classe_idinv == 4), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("CAD (mm/m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/cad_eta_corr_4.png",
       width = 8, height = 6)

# altitude --------------------------------------------------------------------------------------------

parcelas_num <- parcelas %>%
  select(key_inv, up_c_r, altitude, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) 

ggplot(parcelas_num %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(altitude)) +
  ylab("Observações") +
  xlab("Altitude (m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/altitude_hist_classe_4.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_altitude = case_when(altitude > 450 ~ "> 450",
                                     altitude > 400 ~ "> 400",
                                     altitude > 350 ~ "> 350",
                                     TRUE ~ "> 0")) 

ggplot(df_num %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(classe_altitude, eta), alpha = 0.3) +
  geom_boxplot(aes(classe_altitude, eta), alpha = 0.8) +
  geom_violin(aes(classe_altitude, eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Altitude (m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/altitude_eta_classe_4.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = eta,
         x = altitude)

ggplot(df_num_corr %>% filter(classe_idinv == 4), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Altitude (m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/altitude_eta_corr_4.png",
       width = 8, height = 6)

# macroambiente --------------------------------------------------------------------------------------------

parcelas_fac <- parcelas %>%
  select(key_inv, up_c_r, macroambiente, idinv, data_plantio) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) %>% 
  group_by(macroambiente, classe_idinv) %>%
  nest() %>%
  filter(!macroambiente == "NULL") %>% 
  mutate(n = map_dbl(data, ~nrow(.x))) %>%
  # filter(n >= 50) %>% 
  unnest(data) %>% 
  ungroup() 

ggplot(parcelas_fac %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(macroambiente), stat = "count") +
  ylab("Observações") +
  xlab("Macroambiente") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/macroambiente_hist_classe_4.png",
       width = 8, height = 6)

df_fac <- parcelas_fac %>% 
  group_by(macroambiente, classe_idinv) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$eta, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_fac %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(macroambiente, eta), alpha = 0.3) +
  geom_boxplot(aes(macroambiente, eta), alpha = 0.8) +
  geom_violin(aes(macroambiente, eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Macroambiente") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/macroambiente_eta_classe_4.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = eta,
         x = as.factor(macroambiente))

ggplot(df_fac_corr %>% filter(classe_idinv == 4), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Macroambiente") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/macroambiente_eta_corr_4.png",
       width = 8, height = 6)


# ordem --------------------------------------------------------------------------------------------

desc_solos <- readxl::read_excel("input/desc_solos.xlsx") %>% 
  select(-desc_embrapa)

parcelas_fac <- parcelas %>%
  select(key_inv, up_c_r, ordem, idinv, data_plantio) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) %>% 
  group_by(ordem, classe_idinv) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~nrow(.x))) %>%
  # filter(n >= 50) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  left_join(desc_solos) %>% 
  mutate(ordem = desc_ordem)

ggplot(parcelas_fac %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(reorder(ordem, -n)), stat = "count") +
  ylab("Observações") +
  xlab("Ordem do Solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/ordem_hist_classe_4.png",
       width = 8, height = 6)

df_fac <- parcelas_fac %>% 
  group_by(ordem, classe_idinv) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$eta, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_fac %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(reorder(ordem, -eta_order), eta), alpha = 0.3) +
  geom_boxplot(aes(reorder(ordem, -eta_order), eta), alpha = 0.8) +
  geom_violin(aes(reorder(ordem, -eta_order), eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Ordem do solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/ordem_eta_classe_4.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = eta,
         x = as.factor(ordem))

ggplot(df_fac_corr %>% filter(classe_idinv == 4), 
       aes(reorder(x, -eta_order), y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Ordem do solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/ordem_eta_corr_4.png",
       width = 8, height = 6)


# adensamento --------------------------------------------------------------------------------------------

parcelas_num <- parcelas %>%
  select(key_inv, up_c_r, narvha, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) 

ggplot(parcelas_num %>% filter(classe_idinv == 2)) +
  geom_histogram(aes(narvha)) +
  ylab("Observações") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/narvha_hist_classe_2.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ "> 1450",
                                   narvha > 1200 ~ "> 1200",
                                   narvha > 950 ~ "> 950",
                                   narvha > 700 ~ "> 700",
                                   TRUE ~ "> 0")) %>% 
  group_by(classe_narvha, classe_idinv) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$eta, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_num %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(reorder(classe_narvha, eta_order), eta), alpha = 0.3) +
  geom_boxplot(aes(reorder(classe_narvha, eta_order), eta), alpha = 0.8) +
  geom_violin(aes(reorder(classe_narvha, eta_order), eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/narvha_eta_classe_4.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = eta,
         x = narvha)

ggplot(df_num_corr %>% filter(classe_idinv == 4), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/narvha_eta_corr_4.png",
       width = 8, height = 6)


# grau mato --------------------------------------------------------------------------------------------

parcelas_num <- parcelas %>%
  select(key_inv, up_c_r, gmato, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2, 4)) %>%
  inner_join(evapo_acumulado) 

ggplot(parcelas_num %>% filter(classe_idinv == 4)) +
  geom_histogram(aes(gmato)) +
  ylab("Observações") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/gmato_hist_classe_4.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_gmato = case_when(gmato > 75 ~ "> 75",
                                   gmato > 50 ~ "> 50",
                                   gmato > 25 ~ "> 25",
                                   TRUE ~ "> 0")) %>% 
  group_by(classe_gmato, classe_idinv) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$eta, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_num %>% filter(classe_idinv == 4)) + 
  geom_jitter(aes(classe_gmato, eta), alpha = 0.3) +
  geom_boxplot(aes(classe_gmato, eta), alpha = 0.8) +
  geom_violin(aes(classe_gmato, eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/gmato_eta_classe_4.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = eta,
         x = gmato)

ggplot(df_num_corr %>% filter(classe_idinv == 2), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/gmato_eta_corr_2.png",
       width = 8, height = 6)


# adensamento com efeito grau mato --------------------------------------------------------------------

parcelas_num <- parcelas %>%
  select(key_inv, up_c_r, narvha, gmato, idinv) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(classe_idinv %in% c(2)) %>%
  inner_join(evapo_acumulado) %>% 
  mutate(classe_gmato = case_when(gmato >= 75 ~ "Grau Mato >= 75",
                                  gmato <= 25 ~ "Grau Mato <= 25",
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_gmato))

ggplot(parcelas_num %>% filter(classe_idinv == 2)) +
  geom_histogram(aes(narvha)) +
  ylab("Observações") +
  xlab("Nº árvores por hectare") +
  facet_wrap(~classe_gmato) + 
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=16),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/efeito_narva_gmato_hist_classe_2.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  group_by(classe_narvha) %>% 
  summarise(eta = mean(eta, na.rm = T),
            gmato = mean(gmato, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

coeff <- max(df_num$eta) / 100 #max(df_densidade_media$gmato)

ggplot(df_num) + 
  geom_col(aes(reorder(classe_narvha, ordena), eta), width = 0.5, alpha = 0.8) +
  geom_col(aes(reorder(classe_narvha, ordena), gmato*coeff), width = 0.2, alpha = 1, fill = "#4EA72E") + 
  scale_y_continuous(name = "ETa Ensemble (mm/ano)",
                     sec.axis = sec_axis(~./coeff, name = "Grau Mato (%)")) +
  # ylab("ETa Ensemble (mm/ano)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 20, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/efeito_narva_gmato_2_eixos.png",
       width = 8, height = 6)

df_num_paralelo <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  group_by(classe_narvha, classe_gmato) %>% 
  summarise(eta = mean(eta, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num_paralelo) + 
  geom_col(aes(reorder(classe_narvha, ordena), eta, 
               fill = classe_gmato), width = 0.5, alpha = 0.8, position = "dodge2") +
  scale_fill_manual(values = c("Grau Mato >= 75" = "#4EA72E",
                               "Grau Mato <= 25" = "#436F97"),
                    labels = c("Grau Mato >= 75" = ">= 75",
                               "Grau Mato <= 25" = "<= 25")) +  
  ylab("ETa Ensemble (mm/ano)") +
  xlab("Nº árvores por hectare") +
  labs(fill = "Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        strip.text = element_text(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # legend.position = c(0.8, 0.2),
        # axis.text.x = element_text(angle = 20, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/efeito_narva_gmato_2_barras.png",
       width = 8, height = 6)

df_num_jiter <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  mutate(classe_gmato = case_when(gmato > 75 ~ "Grau Mato >= 75",
                                  gmato > 50 ~ "> 50",
                                  gmato > 25 ~ "> 25",
                                  TRUE ~ "Grau Mato <= 25")) %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num_jiter %>% filter(classe_idinv == 2)) + 
  geom_jitter(aes(reorder(classe_narvha, ordena), eta), alpha = 0.3) +
  geom_boxplot(aes(reorder(classe_narvha, ordena), eta), alpha = 0.8) +
  geom_violin(aes(reorder(classe_narvha, ordena), eta), alpha = 0.1) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Nº árvores por hectare") +
  facet_wrap(~classe_gmato) +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=16),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        strip.text = element_text(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/efeito_narva_gmato_2_jiter.png",
       width = 8, height = 6)

df_num_corr <- parcelas_num %>% 
  mutate(y = eta,
         x = narvha)

ggplot(df_num_corr %>% filter(classe_idinv == 2), 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  facet_wrap(~classe_gmato) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 5, label.y = "bottom") +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/efeito_narva_gmato_2_corr.png",
       width = 8, height = 6)


# mapa altitude ---------------------------------------------------------------------------------------------------

parcelas_alt <- parcelas %>% 
  distinct(key, .keep_all = T) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4674)

mapa <- mapview::mapview(parcelas_alt, 
                         zcol = 'altitude', 
                         lwd = 0, 
                         alpha.regions = 0.8, 
                         layer.name = c("Altitude Parcelas"))

# mapa

mapview::mapshot(mapa, url = "output/defesa/mapa_altitude.html")



# ANALISE PRODUTIVIDADE - WUE ----------------------------------------------------------------------------


# WUE VTCC ----------------------------------------------------------------------------------------------------------

#Quarterly water use efficiency was calculated as the total monthly carbon increment (50% of stem biomass) (Stape et al.,                                                2010) for each replicate plot divided by the total amount of monthly water use.

# merge_ppt_wue <- merge_ppt %>% 
#   select(key_ppt, ppt_mes) %>% 
#   mutate(key_data = as.Date(paste0(str_extract(key_ppt, ".......$"), "-01")),
#          key = str_remove(key_ppt, ".......$"))
# 
# ppt_wue <- parcelas %>% #head(10) %>%
#   mutate(filtro_plantio = as.Date(paste0(str_extract(data_plantio, "^......."), "-01")),
#          filtro_medicao = as.Date(paste0(str_extract(datamedicao, "^......."), "-01"))) %>% 
#   # select(key, key_inv, filtro_plantio, filtro_medicao) %>% 
#   left_join(merge_ppt_wue) %>% 
#   filter(key_data >= filtro_plantio,
#          key_data <= filtro_medicao) %>% 
#   group_by(key_inv) %>% 
#   summarise(ppt = sum(ppt_mes, na.rm = T))
# 
# df_wue <- ppt_wue %>%
#   left_join(evapo_acumulado) 
# 
# writexl::write_xlsx(df_wue, "output/ppt_evapo_acumulado_rpart.xlsx")
# df_wue <- readxl::read_xlsx("output/ppt_evapo_acumulado_rpart.xlsx")

# não usamos a ppt

parcelas_wue <- parcelas %>% 
  left_join(evapo_acumulado) 

wue <- parcelas_wue %>% #select(key_inv, vtcc, eta, narvha) %>% 
  mutate(m2_arv = 10000/narvha) %>% 
  mutate(wue = (vtcc*0.25) / ((eta * m2_arv)/1000)) %>% 
  filter(wue > 0)


# ETa e VTCC dependentes de IDADE ------------------------------------------------------------

wue_eta_vtcc <- wue %>% 
  mutate(x = eta,
         y = vtcc)

ggplot(wue_eta_vtcc, aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("VTCC (m³/ha)") +
  xlab("ETa Ensemble Acumulada (mm/ciclo)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/eta_vtcc.png",
       width = 8, height = 6)

wue_idinv_vtcc <- wue %>% 
  mutate(x = idinv,
         y = vtcc)

ggplot(wue_idinv_vtcc, aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("VTCC (m³/ha)") +
  xlab("Idade da Floresta (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_vtcc.png",
       width = 8, height = 6)

wue_idinv_eta <- wue %>% 
  mutate(x = idinv,
         y = eta)

ggplot(wue_idinv_eta, aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("ETa Ensemble Acumulada (mm/ciclo)") +
  xlab("Idade da Floresta (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_eta.png",
       width = 8, height = 6)


# WUE independente de idade -------------------------------------------------------------

wue_idade <- wue %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(idinv <= 8) %>%
  group_by(idinv) %>% 
  summarise(wue = mean(wue, na.rm = T)) %>% 
  drop_na() %>% 
  mutate(x = idinv, 
         y = wue)

ggplot(wue_idade, aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Idade da Floresta (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_wue.png",
       width = 8, height = 6)

ggplot(wue_idade %>% filter(idinv >= 3), aes(x, y)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Idade da Floresta (anos)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_wue_sem_2_anos.png",
       width = 8, height = 6)

wue_classe_idade <- wue %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  mutate(classe_idinv = case_when(between(idinv, 1.94, 2.06) ~ 2,
                                  between(idinv, 2.94, 3.06) ~ 3,
                                  between(idinv, 3.94, 4.06) ~ 4,
                                  between(idinv, 4.94, 5.06) ~ 5,
                                  between(idinv, 5.94, 6.06) ~ 6,
                                  between(idinv, 6.94, 7.06) ~ 7,
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_idinv)) %>% 
  group_by(classe_idinv) %>% 
  summarise(wue = mean(wue, na.rm = T)) %>% 
  mutate(x = classe_idinv, 
         y = wue)

ggplot(wue_classe_idade) + 
  geom_col(aes(classe_idinv, wue)) +
  geom_label(aes(classe_idinv, wue, label = round(wue,2)), vjust = 1.2, size = 5.5) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Classe de Idade") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_wue_classe_idade.png",
       width = 8, height = 6)

wue_idade <- wue %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  filter(idinv <= 8) %>% 
  group_by(idinv) %>% 
  summarise(wue = mean(wue, na.rm = T)) %>% 
  mutate(x = idinv, 
         y = wue)

ggplot(wue_idade) + 
  geom_col(aes(idinv, wue)) +
  # geom_label(aes(idinv, wue, label = round(wue,2)), vjust = 1.2, size = 5.5) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Classe de Idade") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/idinv_idade.png",
       width = 8, height = 6)

# WUE e VTCC histogram --------------------------------------------------------------

wue_hist <- wue %>% 
  filter(idinv <= 8) %>% 
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2)

ggplot(wue_hist) + 
  geom_histogram(aes(wue)) +
  # scale_x_continuous(n.breaks = 4) +
  # geom_vline(xintercept = c(1400, 1200, 950, 700), color = "#AF423F", lwd = 1) +
  ylab("Observações") +
  xlab("WUE (Kg C/m³ água)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_hist.png",
       width = 8, height = 6)

ggplot(wue_hist) + 
  geom_histogram(aes(vtcc)) +
  # scale_x_continuous(n.breaks = 4) +
  # geom_vline(xintercept = c(1400, 1200, 950, 700), color = "#AF423F", lwd = 1) +
  ylab("Observações") +
  xlab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_hist.png",
       width = 8, height = 6)

# decision tree WUE -----------------------------------------------------------------------------

arv_wue <- wue %>% 
  filter(idinv <= 8) %>% 
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  select(-key, -key_inv, -up_c_r, -numparcela, -propriedade, -data_plantio) %>% 
  select(-ultima_medicao, -umn, -latitude, -longitude, -int, -area_up, -rotacao) %>% 
  select(-area_basal, -datamedicao:-nummedicao, -imcom, -imcim, -imacc:-dias_corridos, -m2_arv) %>% 
  select(-mhdom:-idinv, -vtcc, -eta) %>% 
  select(-zona_fisiografica, -solo)

arv <- rpart::rpart(data = arv_wue,
                    formula = wue ~.,
                    method = "anova",
                    cp = 0.0005, maxdepth = 5, 
                    minsplit = nrow(arv_wue) * 0.05)
arv

rpart.plot::rpart.plot(arv, cex = 0.8)


# regressao WUE analise do ambiente geral -------------------------------------------------------------------------

df_corr_wue <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  select(-datamedicao:-dias_corridos, narvha) %>% 
  select(-key, -key_inv, -up_c_r, -numparcela, -propriedade, -data_plantio) %>% 
  select(-ultima_medicao, -umn, -latitude, -longitude, -int, -area_up, -rotacao, -m2_arv) %>% 
  select(-classe_idinv, -eta, -zona_fisiografica, -solo) %>%
  drop_na() %>% 
  mutate_if(is.character, as.factor) 

# skimr::skim(df_corr_wue)

# corr linear

wue_line_corr <- lm(formula = wue ~ ., data = df_corr_wue)
r2 <- summary(wue_line_corr)
r2

wue_pred_line_corr <- data.frame(wue_pred = predict(wue_line_corr, df_corr_wue)) %>% 
  as_tibble() %>% 
  bind_cols(df_corr_wue)

# skimr::skim(wue_pred_line_corr)

ggplot(wue_pred_line_corr, 
       aes(wue, wue_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 0.25, y = 1.75, 
           label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
           size = 6) +
  scale_x_continuous(limits = c(0, 2.1)) +
  scale_y_continuous(limits = c(0, 2.1)) +
  ylab("WUE Predicted (Kg C/m³ água)") +
  xlab("WUE (Kg C/m³ água)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_predict_corr_linear.png",
       width = 8, height = 6)

wue_poly_corr <- lm(formula = wue ~ polym(zonaclimatica, macroambiente, gmato, cad, dc, altitude,
                                           ordem, textura, matgen, narvha, 
                                           degree = 3, raw = TRUE), 
                     data = df_corr_wue)
r2 <- summary(wue_poly_corr)
r2

wue_pred_poly_corr <- data.frame(wue_pred = predict(wue_poly_corr, df_corr_wue)) %>% 
  as_tibble() %>% 
  bind_cols(df_corr_wue)

ggplot(wue_pred_poly_corr, 
       aes(wue, wue_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 0.25, y = 1.75, 
           label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
           size = 6) +
  scale_x_continuous(limits = c(0, 2.1)) +
  scale_y_continuous(limits = c(0, 2.1)) +
  ylab("WUE Predicted (Kg C/m³ água)") +
  xlab("WUE (Kg C/m³ água)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_predict_corr_polinomial.png",
       width = 8, height = 6)


# WUE matgen -------------------------------------------------------------------------------------

parcelas_fac <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, matgen, data_plantio, wue) %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>%
  group_by(matgen) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~nrow(.x))) %>%
  filter(n >= 300) %>% 
  unnest(data) %>% 
  ungroup() 

ggplot(parcelas_fac) +
  geom_histogram(aes(reorder(matgen, -n)), stat="count") +
  ylab("Observações") +
  xlab("Material Genético") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_matgen_hist.png",
       width = 8, height = 6)

df_fac <- parcelas_fac %>% 
  group_by(matgen) %>% 
  nest() %>% 
  mutate(wue_order = map_dbl(data, ~mean(.x$wue, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_fac) + 
  geom_jitter(aes(reorder(matgen, -wue_order), wue), alpha = 0.3) +
  geom_boxplot(aes(reorder(matgen, -wue_order), wue), alpha = 0.8) +
  geom_violin(aes(reorder(matgen, -wue_order), wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Material Genético") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_matgen.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = wue,
         x = as.factor(matgen))

ggplot(df_fac_corr, 
       aes(reorder(x, -wue_order), y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Material Genético") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_matgen_corr.png",
       width = 8, height = 6)


# WUE dc -----------------------------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, dc, wue)

ggplot(parcelas_num) +
  geom_histogram(aes(dc)) +
  ylab("Observações") +
  xlab("Zona Fisiográfica Contínua") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_dc_hist.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_dc = case_when(dc > 75 ~ "ZF 4",
                               dc > 50 ~ "ZF 3",
                               dc > 25 ~ "ZF 2",
                               TRUE ~ "ZF 1")) 

ggplot(df_num) + 
  geom_jitter(aes(classe_dc, wue), alpha = 0.3) +
  geom_boxplot(aes(classe_dc, wue), alpha = 0.8) +
  geom_violin(aes(classe_dc, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Zona Fisiográfica") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_dc.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = wue,
         x = dc)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Zona Fisiográfica") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_dc_corr.png",
       width = 8, height = 6)


# WUE cad ---------------------------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, cad, wue) 

ggplot(parcelas_num) +
  geom_histogram(aes(cad)) +
  ylab("Observações") +
  xlab("CAD (mm/m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_cad_hist.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_cad = case_when(cad > 150 ~ "> 150",
                                cad > 130 ~ "> 130",
                                cad > 110 ~ "> 110",
                                TRUE ~ "> 0")) 

ggplot(df_num) + 
  geom_jitter(aes(classe_cad, wue), alpha = 0.3) +
  geom_boxplot(aes(classe_cad, wue), alpha = 0.8) +
  geom_violin(aes(classe_cad, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Classe de CAD (mm/m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_cad.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = wue,
         x = cad)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("CAD (mm/m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_cad_corr.png",
       width = 8, height = 6)


# WUE altitude ------------------------------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, altitude, wue)

ggplot(parcelas_num) +
  geom_histogram(aes(altitude)) +
  ylab("Observações") +
  xlab("Altitude (m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_altitude_hist.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_altitude = case_when(altitude > 450 ~ "> 450",
                                     altitude > 400 ~ "> 400",
                                     altitude > 350 ~ "> 350",
                                     TRUE ~ "> 0")) 

ggplot(df_num) + 
  geom_jitter(aes(classe_altitude, wue), alpha = 0.3) +
  geom_boxplot(aes(classe_altitude, wue), alpha = 0.8) +
  geom_violin(aes(classe_altitude, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Altitude (m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_altitude.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = wue,
         x = altitude)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Altitude (m)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_altitude_corr.png",
       width = 8, height = 6)


# WUE macroambiente -----------------------------------------------------------------------------------

parcelas_fac <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, macroambiente, wue) %>% 
  filter(!macroambiente == "NULL")

ggplot(parcelas_fac) +
  geom_histogram(aes(macroambiente), stat = "count") +
  ylab("Observações") +
  xlab("Macroambiente") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_macroambiente_hist.png",
       width = 8, height = 6)

df_fac <- parcelas_fac 

ggplot(df_fac) + 
  geom_jitter(aes(macroambiente, wue), alpha = 0.3) +
  geom_boxplot(aes(macroambiente, wue), alpha = 0.8) +
  geom_violin(aes(macroambiente, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Macroambiente") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_macroambiente.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = wue,
         x = as.factor(macroambiente))

ggplot(df_fac_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Macroambiente") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_macroambiente_corr.png",
       width = 8, height = 6)


# WUE ordem ------------------------------------------------------------------------------------

desc_solos <- readxl::read_excel("input/desc_solos.xlsx") %>% 
  select(-desc_embrapa)

parcelas_fac <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, ordem, wue) %>% 
  group_by(ordem) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~nrow(.x))) %>%
  unnest(data) %>% 
  ungroup() %>% 
  left_join(desc_solos) %>% 
  mutate(ordem = desc_ordem)

ggplot(parcelas_fac) +
  geom_histogram(aes(reorder(ordem, -n)), stat = "count") +
  ylab("Observações") +
  xlab("Ordem do Solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_ordem_hist.png",
       width = 8, height = 6)

df_fac <- parcelas_fac %>% 
  group_by(ordem) %>% 
  nest() %>% 
  mutate(wue_order = map_dbl(data, ~mean(.x$wue, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_fac) + 
  geom_jitter(aes(reorder(ordem, -wue_order), wue), alpha = 0.3) +
  geom_boxplot(aes(reorder(ordem, -wue_order), wue), alpha = 0.8) +
  geom_violin(aes(reorder(ordem, -wue_order), wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Ordem do solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_ordem.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = wue,
         x = as.factor(ordem))

ggplot(df_fac_corr, 
       aes(reorder(x, -wue_order), y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Ordem do solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_ordem_corr.png",
       width = 8, height = 6)


# WUE textura ------------------------------------------------------------------------------------

parcelas_fac <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, textura, wue) %>% 
  group_by(textura) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~nrow(.x))) %>%
  unnest(data) %>% 
  ungroup() %>% 
  mutate(textura = case_when(textura == "r" ~ "Arenosa",
                             textura == "m" ~ "Média",
                             textura == "a" ~ "Argilosa",
                             TRUE ~ NA)) %>% 
  mutate(ordena = case_when(textura == "Arenosa" ~ 1,
                            textura == "Média" ~ 2,
                            textura == "Argilosa" ~ 3,
                            TRUE ~ NA))

ggplot(parcelas_fac) +
  geom_histogram(aes(reorder(textura, ordena)), stat = "count") +
  ylab("Observações") +
  xlab("Textura do Solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_textura_hist.png",
       width = 8, height = 6)

df_fac <- parcelas_fac %>% 
  group_by(textura) %>% 
  nest() %>% 
  mutate(wue_order = map_dbl(data, ~mean(.x$wue, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_fac) + 
  geom_jitter(aes(reorder(textura, ordena), wue), alpha = 0.3) +
  geom_boxplot(aes(reorder(textura, ordena), wue), alpha = 0.8) +
  geom_violin(aes(reorder(textura, ordena), wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Textura do solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_textura.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = wue,
         x = as.factor(textura))

ggplot(df_fac_corr, 
       aes(reorder(x, ordena), y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Textura do solo") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_textura_corr.png",
       width = 8, height = 6)


# WUE zona climatica ------------------------------------------------------------------------------------

parcelas_fac <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, zonaclimatica, wue) %>% 
  mutate(zonaclimatica = case_when(zonaclimatica == 1 ~ "TS1",
                                   zonaclimatica == 2 ~ "TS2",
                                   zonaclimatica == 3 ~ "TS3",
                                   zonaclimatica == 4 ~ "TS4",
                                   zonaclimatica == 5 ~ "TS5",
                                   TRUE ~ NA))

ggplot(parcelas_fac) +
  geom_histogram(aes(zonaclimatica), stat = "count") +
  ylab("Observações") +
  xlab("Zona Climática") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_zonaclimatica_hist.png",
       width = 8, height = 6)

df_fac <- parcelas_fac 

ggplot(df_fac) + 
  geom_jitter(aes(zonaclimatica, wue), alpha = 0.3) +
  geom_boxplot(aes(zonaclimatica, wue), alpha = 0.8) +
  geom_violin(aes(zonaclimatica, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Zona Climática") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_zonaclimatica.png",
       width = 8, height = 6)

df_fac_corr <- df_fac %>% 
  mutate(y = wue,
         x = as.factor(zonaclimatica))

ggplot(df_fac_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Zona Climática") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_zonaclimatica_corr.png",
       width = 8, height = 6)


# WUE adensamento --------------------------------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, narvha, wue) 

ggplot(parcelas_num) +
  geom_histogram(aes(narvha)) +
  ylab("Observações") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_narvha_hist.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ "> 1450",
                                   narvha > 1200 ~ "> 1200",
                                   narvha > 950 ~ "> 950",
                                   narvha > 700 ~ "> 700",
                                   TRUE ~ "> 0")) %>% 
  mutate(ordena = case_when(classe_narvha == "> 1450" ~ 5,
                            classe_narvha == "> 1200" ~ 4,
                            classe_narvha == "> 950" ~ 3,
                            classe_narvha == "> 700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num) + 
  geom_jitter(aes(reorder(classe_narvha, ordena), wue), alpha = 0.3) +
  geom_boxplot(aes(reorder(classe_narvha, ordena), wue), alpha = 0.8) +
  geom_violin(aes(reorder(classe_narvha, ordena), wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_narvha.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = wue,
         x = narvha)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_narvha_corr.png",
       width = 8, height = 6)


# WUE grau mato ---------------------------------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, gmato, wue) 

ggplot(parcelas_num) +
  geom_histogram(aes(gmato)) +
  ylab("Observações") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_gmato_hist.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_gmato = case_when(gmato > 75 ~ "> 75",
                                  gmato > 50 ~ "> 50",
                                  gmato > 25 ~ "> 25",
                                  TRUE ~ "> 0")) %>% 
  group_by(classe_gmato) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$wue, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_num) + 
  geom_jitter(aes(classe_gmato, wue), alpha = 0.3) +
  geom_boxplot(aes(classe_gmato, wue), alpha = 0.8) +
  geom_violin(aes(classe_gmato, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_gmato.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = wue,
         x = gmato)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_gmato_corr.png",
       width = 8, height = 6)


# WUE adensamento com efeito grau mato -------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(!classe_idinv == 2) %>%
  select(key_inv, up_c_r, narvha, gmato, wue) %>% 
  mutate(classe_gmato = case_when(gmato >= 75 ~ "Grau Mato >= 75",
                                  gmato <= 25 ~ "Grau Mato <= 25",
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_gmato))

ggplot(parcelas_num) +
  geom_histogram(aes(narvha)) +
  ylab("Observações") +
  xlab("Nº árvores por hectare") +
  facet_wrap(~classe_gmato) + 
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=16),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_hist.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  group_by(classe_narvha) %>% 
  summarise(wue = mean(wue, na.rm = T),
            gmato = mean(gmato, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

coeff <- max(df_num$wue) / 100 #max(df_densidade_media$gmato)

ggplot(df_num) + 
  geom_col(aes(reorder(classe_narvha, ordena), wue), width = 0.5, alpha = 0.8) +
  geom_col(aes(reorder(classe_narvha, ordena), gmato*coeff), width = 0.2, alpha = 1, fill = "#4EA72E") + 
  scale_y_continuous(name = "WUE (Kg C/m³ água)",
                     sec.axis = sec_axis(~./coeff, name = "Grau Mato (%)")) +
  # ylab("ETa Ensemble (mm/ano)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 20, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_eixos.png",
       width = 8, height = 6)

df_num_paralelo <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  group_by(classe_narvha, classe_gmato) %>% 
  summarise(wue = mean(wue, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num_paralelo) + 
  geom_col(aes(reorder(classe_narvha, ordena), wue, 
               fill = classe_gmato), width = 0.5, alpha = 0.8, position = "dodge2") +
  scale_fill_manual(values = c("Grau Mato >= 75" = "#4EA72E",
                               "Grau Mato <= 25" = "#436F97"),
                    labels = c("Grau Mato >= 75" = ">= 75",
                               "Grau Mato <= 25" = "<= 25")) +  
  ylab("WUE (Kg C/m³ água)") +
  xlab("Nº árvores por hectare") +
  labs(fill = "Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        strip.text = element_text(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # legend.position = c(0.8, 0.2),
        # axis.text.x = element_text(angle = 20, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_barras.png",
       width = 8, height = 6)

df_num_jiter <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  mutate(classe_gmato = case_when(gmato > 75 ~ "> 75",
                                  gmato > 50 ~ "> 50",
                                  gmato > 25 ~ "> 25",
                                  TRUE ~ "> 0")) %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num_jiter) + 
  geom_jitter(aes(reorder(classe_narvha, ordena), wue), alpha = 0.3) +
  geom_boxplot(aes(reorder(classe_narvha, ordena), wue), alpha = 0.8) +
  geom_violin(aes(reorder(classe_narvha, ordena), wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Grau Mato") +
  facet_wrap(~classe_gmato) +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=16),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        strip.text = element_text(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_jiter.png",
       width = 8, height = 6)

df_num_corr <- parcelas_num %>% 
  mutate(y = wue,
         x = narvha)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  facet_wrap(~classe_gmato) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 5) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_corr.png",
       width = 8, height = 6)


# WUE grau mato 2 ANOS ---------------------------------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(classe_idinv == 2) %>%
  select(key_inv, up_c_r, gmato, wue) 

ggplot(parcelas_num) +
  geom_histogram(aes(gmato)) +
  ylab("Observações") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        # strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_gmato_hist_2_anos.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_gmato = case_when(gmato > 75 ~ "> 75",
                                  gmato > 50 ~ "> 50",
                                  gmato > 25 ~ "> 25",
                                  TRUE ~ "> 0")) %>% 
  group_by(classe_gmato) %>% 
  nest() %>% 
  mutate(eta_order = map_dbl(data, ~mean(.x$wue, na.rm = T))) %>% 
  unnest(data) %>% 
  ungroup()

ggplot(df_num) + 
  geom_jitter(aes(classe_gmato, wue), alpha = 0.3) +
  geom_boxplot(aes(classe_gmato, wue), alpha = 0.8) +
  geom_violin(aes(classe_gmato, wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_gmato_2_anos.png",
       width = 8, height = 6)

df_num_corr <- df_num %>% 
  mutate(y = wue,
         x = gmato)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 8) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_gmato_corr_2_anos.png",
       width = 8, height = 6)


# WUE adensamento com efeito grau mato 2 ANOS -------------------------------------------------------------

parcelas_num <- wue %>%
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  filter(classe_idinv == 2) %>%
  select(key_inv, up_c_r, narvha, gmato, wue) %>% 
  mutate(classe_gmato = case_when(gmato >= 75 ~ "Grau Mato >= 75",
                                  gmato <= 25 ~ "Grau Mato <= 25",
                                  TRUE ~ NA)) %>% 
  filter(!is.na(classe_gmato))

ggplot(parcelas_num) +
  geom_histogram(aes(narvha)) +
  ylab("Observações") +
  xlab("Nº árvores por hectare") +
  facet_wrap(~classe_gmato) + 
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=16),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_hist_2_anos.png",
       width = 8, height = 6)

df_num <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  group_by(classe_narvha) %>% 
  summarise(wue = mean(wue, na.rm = T),
            gmato = mean(gmato, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

coeff <- max(df_num$wue) / 100 #max(df_densidade_media$gmato)

ggplot(df_num) + 
  geom_col(aes(reorder(classe_narvha, ordena), wue), width = 0.5, alpha = 0.8) +
  geom_col(aes(reorder(classe_narvha, ordena), gmato*coeff), width = 0.2, alpha = 1, fill = "#4EA72E") + 
  scale_y_continuous(name = "WUE (Kg C/m³ água)",
                     sec.axis = sec_axis(~./coeff, name = "Grau Mato (%)")) +
  # ylab("ETa Ensemble (mm/ano)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # axis.text.x = element_text(angle = 20, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_eixos_2_anos.png",
       width = 8, height = 6)

df_num_paralelo <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  group_by(classe_narvha, classe_gmato) %>% 
  summarise(wue = mean(wue, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num_paralelo) + 
  geom_col(aes(reorder(classe_narvha, ordena), wue, 
               fill = classe_gmato), width = 0.5, alpha = 0.8, position = "dodge2") +
  scale_fill_manual(values = c("Grau Mato >= 75" = "#4EA72E",
                               "Grau Mato <= 25" = "#436F97"),
                    labels = c("Grau Mato >= 75" = ">= 75",
                               "Grau Mato <= 25" = "<= 25")) +  
  ylab("WUE (Kg C/m³ água)") +
  xlab("Nº árvores por hectare") +
  labs(fill = "Grau Mato") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        strip.text = element_text(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        # legend.position = c(0.8, 0.2),
        # axis.text.x = element_text(angle = 20, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_barras_2_anos.png",
       width = 8, height = 6)

df_num_jiter <- parcelas_num %>% 
  mutate(classe_narvha = case_when(narvha > 1450 ~ ">1450",
                                   narvha > 1200 ~ ">1200",
                                   narvha > 950 ~ ">950",
                                   narvha > 700 ~ ">700",
                                   TRUE ~ ">0")) %>% 
  mutate(classe_gmato = case_when(gmato > 75 ~ "> 75",
                                  gmato > 50 ~ "> 50",
                                  gmato > 25 ~ "> 25",
                                  TRUE ~ "> 0")) %>% 
  mutate(ordena = case_when(classe_narvha == ">1450" ~ 5,
                            classe_narvha == ">1200" ~ 4,
                            classe_narvha == ">950" ~ 3,
                            classe_narvha == ">700" ~ 2,
                            TRUE ~ 1))

ggplot(df_num_jiter) + 
  geom_jitter(aes(reorder(classe_narvha, ordena), wue), alpha = 0.3) +
  geom_boxplot(aes(reorder(classe_narvha, ordena), wue), alpha = 0.8) +
  geom_violin(aes(reorder(classe_narvha, ordena), wue), alpha = 0.1) +
  ylab("WUE (Kg C/m³ água)") +
  xlab("Grau Mato") +
  facet_wrap(~classe_gmato) +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=16),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        strip.text = element_text(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_jiter_2_anos.png",
       width = 8, height = 6)

df_num_corr <- parcelas_num %>% 
  mutate(y = wue,
         x = narvha)

ggplot(df_num_corr, 
       aes(x, y, group = 1)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  facet_wrap(~classe_gmato) +
  ggpmisc::stat_poly_line(method = "poly_or_mean") + 
  ggpmisc::stat_poly_eq(method = "poly_or_mean", ggpmisc::use_label(c("eq", "R2")), size = 5) +
  ylab("ETa Ensemble (mm/ciclo)") +
  xlab("Nº árvores por hectare") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        strip.text = element_text(color = "black"),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/wue_efeito_narva_gmato_2_corr_2_anos.png",
       width = 8, height = 6)


# REGRESSÃO/PREDICAO PRODUTIVIDADE POR ETA --------------------------------------------------------------------

reg_vtcc <- wue %>% 
  mutate(classe_idinv = case_when(idinv > 7 ~ 7,
                                  idinv > 6 ~ 6,
                                  idinv > 5 ~ 5,
                                  idinv > 4 ~ 4,
                                  idinv > 3 ~ 3,
                                  TRUE ~ 2)) %>% 
  # filter(!classe_idinv == 2) %>% 
  left_join(matmask) %>% 
  select(-matgen) %>% 
  rename(matgen = matgen_mask) %>% 
  distinct(key_inv, .keep_all = T) %>% 
  select(-datamedicao:-dias_corridos, narvha, vtcc, idinv) %>% 
  select(-key, -key_inv, -up_c_r, -numparcela, -propriedade, -data_plantio) %>% 
  select(-ultima_medicao, -umn, -latitude, -longitude, -int, -area_up, -rotacao, -m2_arv) %>% 
  select(-zona_fisiografica, -solo) %>% 
  select(-wue, -classe_idinv) %>% 
  # select(-classe_idinv, -eta, -zona_fisiografica) %>%
  drop_na() %>%
  mutate_if(is.character, as.factor) 

# vtcc_line_corr <- lm(formula = vtcc ~ ., data = reg_vtcc)
# r2 <- summary(vtcc_line_corr)
# r2
# 
# vtcc_pred_line_corr <- data.frame(vtcc_pred = predict(vtcc_line_corr, reg_vtcc)) %>% 
#   as_tibble() %>% 
#   bind_cols(reg_vtcc)
# 
# # skimr::skim(wue_pred_line_corr)
# 
# ggplot(vtcc_pred_line_corr, 
#        aes(vtcc, vtcc_pred)) + 
#   geom_point(alpha = 0.1, size = 4) +
#   geom_abline(alpha = 0.3) +
#   annotate(geom = "text", 
#            x = 50, y = 350, 
#            label = str_glue("italic(R) ^ 2 == {round(r2$adj.r.squared, 2)}"), parse = T,
#            size = 6) +
#   scale_x_continuous(limits = c(0, 400)) +
#   scale_y_continuous(limits = c(0, 400)) +
#   ylab("VTCC Predicted (m³/ha)") +
#   xlab("VTCC (m³/ha)") +
#   theme_light(20) + 
#   theme(axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         axis.text=element_text(size=20),
#         legend.text=element_text(size=20),
#         legend.title = element_text(size=20),
#         axis.ticks.y = element_blank(),
#         axis.ticks.x = element_line(color = "black"),
#         # axis.text.x = element_text(angle = 90, vjust = 0.7),
#         legend.background = element_rect(fill = alpha("white", 0.8)),
#         panel.grid.minor=element_blank()
#   )
# 
# ggsave(filename = "output/defesa/vtcc_predict_corr_linear.png",
#        width = 8, height = 6)

set.seed(1)

# separa dataset de treino e teste
index_sample <- sample(2, nrow(reg_vtcc),
                      replace = T,
                      prob = c(0.7, 0.3))

vtcc_treino <- reg_vtcc[index_sample == 1,]
vtcc_teste <- reg_vtcc[index_sample == 2,]

# com ETa ------------------------------------

reg_vtcc_treino <- lm(formula = vtcc ~ polym(zonaclimatica, macroambiente, gmato, cad, dc, altitude,
                                         ordem, textura, matgen, narvha, eta,
                                         degree = 3, raw = TRUE), 
                  data = vtcc_treino)
r2 <- summary(reg_vtcc_treino)
r2

df_vtcc_pred <- data.frame(vtcc_pred = predict(reg_vtcc_treino, vtcc_teste)) %>% 
  as_tibble() %>% 
  bind_cols(vtcc_teste)

r2_pred <- summary(lm(formula = vtcc ~ vtcc_pred, data = df_vtcc_pred))
r2_pred

ggplot(df_vtcc_pred, 
       aes(vtcc, vtcc_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 50, y = 380, 
           label = str_glue("R² modelo = {round(r2$adj.r.squared, 2)}"), #parse = T,
           size = 6) +
  annotate(geom = "text", 
           x = 50, y = 320, 
           label = str_glue("R² predição = {round(r2_pred$adj.r.squared, 2)}"), #parse = T,
           size = 6) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 400)) +
  ylab("VTCC Predicted (m³/ha)") +
  xlab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_predict_eta.png",
       width = 8, height = 6)


# sem ETa ------------------------------------

reg_vtcc_treino <- lm(formula = vtcc ~ polym(zonaclimatica, macroambiente, gmato, cad, dc, altitude,
                                             ordem, textura, matgen, narvha, 
                                             degree = 3, raw = TRUE), 
                      data = vtcc_treino)
r2 <- summary(reg_vtcc_treino)
r2

df_vtcc_pred <- data.frame(vtcc_pred = predict(reg_vtcc_treino, vtcc_teste)) %>% 
  as_tibble() %>% 
  bind_cols(vtcc_teste)

r2_pred <- summary(lm(formula = vtcc ~ vtcc_pred, data = df_vtcc_pred))
r2_pred

ggplot(df_vtcc_pred, 
       aes(vtcc, vtcc_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 50, y = 380, 
           label = str_glue("R² modelo = {round(r2$adj.r.squared, 2)}"), #parse = T,
           size = 6) +
  annotate(geom = "text", 
           x = 50, y = 320, 
           label = str_glue("R² predição = {round(r2_pred$adj.r.squared, 2)}"), #parse = T,
           size = 6) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 400)) +
  ylab("VTCC Predicted (m³/ha)") +
  xlab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_predict_no_eta.png",
       width = 8, height = 6)


# com idinv ------------------------------------

reg_vtcc_treino <- lm(formula = vtcc ~ polym(zonaclimatica, macroambiente, gmato, cad, dc, altitude,
                                             ordem, textura, matgen, narvha, idinv,
                                             degree = 3, raw = TRUE), 
                      data = vtcc_treino)
r2 <- summary(reg_vtcc_treino)
r2

df_vtcc_pred <- data.frame(vtcc_pred = predict(reg_vtcc_treino, vtcc_teste)) %>% 
  as_tibble() %>% 
  bind_cols(vtcc_teste)

r2_pred <- summary(lm(formula = vtcc ~ vtcc_pred, data = df_vtcc_pred))
r2_pred

ggplot(df_vtcc_pred, 
       aes(vtcc, vtcc_pred)) + 
  geom_point(alpha = 0.1, size = 4) +
  geom_abline(alpha = 0.3) +
  annotate(geom = "text", 
           x = 50, y = 380, 
           label = str_glue("R² modelo = {round(r2$adj.r.squared, 2)}"), #parse = T,
           size = 6) +
  annotate(geom = "text", 
           x = 50, y = 320, 
           label = str_glue("R² predição = {round(r2_pred$adj.r.squared, 2)}"), #parse = T,
           size = 6) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 400)) +
  ylab("VTCC Predicted (m³/ha)") +
  xlab("VTCC (m³/ha)") +
  theme_light(20) + 
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size=20),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = 0.7),
        legend.background = element_rect(fill = alpha("white", 0.8)),
        panel.grid.minor=element_blank()
  )

ggsave(filename = "output/defesa/vtcc_predict_idinv.png",
       width = 8, height = 6)
