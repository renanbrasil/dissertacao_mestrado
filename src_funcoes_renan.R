library(tidyverse)
library(sf)

sf_use_s2(F)

# calcula idade ----------------------------------------------------------------------------------------------------

# data_plantio = as.Date(case_when(rotacao == 1 ~ fim_plantio,  rotacao > 1 ~ inicio_rotacao))


# calcula area ----------------------------------------------------------------------------------------------------

# mutate(area = units::set_units(st_area(geometry), "ha"))


# alguma coluna e NA ----------------------------------------------------------------------------------------------------

# mutate(any_na = pmap_lgl(., ~any(is.na(c(...)))))

# filtra polygons -------------------------------------------------------------------------------------------------

filter_polygon <- function(x, y) {

  z <- x %>%
    filter(lengths(st_intersects(x, st_geometry(y))) > 0)

  return(z)

}
#   
#   filtro <- faz %>% 
#   filter(lengths(st_intersects(., st_geometry(grid_v2))) > 0)


# funcao para limpar strings em uma variaveis ---------------------------------------------------------------------

limpa_nome <- function(x) { #funcao para limpar caracteres especiais e normalizar nomes
  
  x %>%
    stringr::str_to_lower() %>% #remove maiuscula
    stringr::str_squish() %>% #remove espaço duplo
    stringr::str_trim() %>% #remove espaço nas pontas das frases
    stringr::str_replace_all("__", "_") %>% #remove _ duplicado
    stringr::str_replace_all("/", "-") %>% #remove / pois são pathnames
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') #transforma o padrao alfanumerico
  
}


# funcao para extrair o valor que mais se repete nas folhas (moda) ------------------------------------------------
getmode <- function(x) {
  uniqx <- unique(x)
  tab <- tabulate(match(x, uniqx))
  out <- uniqx[tab == max(tab)]
  z <- mean(out)
  z
}

# metodo da distancia entre os quartis - 1.5x distante do range .25 e .75
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#converter radianos
convert_degree <- function(x) { #converte degree do st_buffer para metros
  
  z <- (x / 111.139) / 1000
  
  return(z)
  
}

terra_clip_raster <- function(r, s) {
  
  a <- try(terra::crop(r, s), silent = TRUE)

  if (inherits(a, "try-error")){
    return(NULL)
  }
  
  z <- terra::mask(a, s)

  all_na <- terra::values(z[[1]]) %>%
    is.na() %>%
    all()
  
  if (all_na) {
    
    return(NULL)
    
  } else {
    
    return(z)
    
  }
}

clip_raster <- function(r, s) {
  
  ss <- s %>% as('Spatial')
  
  a <- try(raster::crop(r, ss), silent = TRUE)
  
  if (inherits(a, "try-error")){
    return(NULL)
  }
  
  z <- raster::mask(a, ss)
  
  all_na <- raster::values(z[[1]]) %>%
    is.na() %>%
    all()
  
  if (all_na) {
    
    return(NULL)
    
  } else {
    
    return(z)
    
  }
  
}


ndvi <- function(x) {
  z <- (x[[2]] - x[[1]]) / (x[[2]] + x[[1]]) # red(banda 3) vira banda 1 e infrared(banda 4) vira banda 2
  return(z)
}


mean_top_perc <- function(x, y) {
  # x deve ser um df
  #y deve ser um valor % de 0 ate 1
  
  a <- x %>% 
    filter(!is.na(.[1])) %>% 
    arrange(desc(.[1]))
  
  b <- round(nrow(a) * y)
  
  z <- a %>% head(b) %>% unlist() %>% mean()
  return(z)
}


# estrutura de FOR ------------------------------------------------
# 
# tab_out <- vector("list", nrow(tab_cena))
# 
# pb <- progress::progress_bar$new(total = nrow(tab_cena), format = "[:bar] :elapsedfull :percent :eta")
# 
# i = 1
# for(i in 1:nrow(tab_cena)) {
# 
# 	tab_out[[i]] <- cena
# 	
# 	pb$tick()
# 	
# }
# 
# tab <- bind_rows(tab_out)



# arredondar numeros grandes para K, M, T... ------------------------------------------------


so_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"),
    TRUE ~ "To be implemented..."
  )
}


# formatação de graficos ------------------------------------------------
# 
# theme_light(14) + 
#   theme(axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         axis.text=element_text(size=15),
#         axis.ticks.y = element_blank(),
#         axis.ticks.x = element_line(color = "black"),
#         panel.grid.minor=element_blank()
#   )

# colocar NA em erros ------------------------------------------------
#
# tryCatch(raster::raster(.x), error = function(e){NA})



# regressão polinomial ou linear ---------------------------------------

#para plotar um smooth ou poly line com variavel factor adicionar aes(group = 1) no ggplot()
#The reason group = 1 works is that when you have a discrete axis, ggplot doesn't assume what observations get grouped together to make a line. group = 1 sets a constant group for all observations, giving you one line.

formula <- y ~ poly(x, 3, raw = TRUE)

poly_or_mean <- function(formula, data, ...) {
  
  fm <- lm(formula = formula, data = data, ...)
  
  if (anova(fm)[["Pr(>F)"]][1] > 0.1) {
    
    lm(formula = y ~ x, data = data, ...)
    
  } else {
    
    fm
    
  }
}


# teste-t ----------------------------

resultado_t_test <- function(x, y = 0.05) { #definindo uma função com 2 argumentos
  #teste logico com o resultado p-valor do t.test e o grau de confiança definido
  if_else(x$p.value >= y,
          paste0("P-valor: ", round(x$p.value,4), #arredondamento de casas decimais
                 "(>= ", y, ").", #\n para pular a linha
                 "NÃO HÁ evidencias para rejeitar H0, logo, NÃO HÁ diferença entre os grupos"),
          paste0("P-valor: ", round(x$p.value,4),
                 "(< ", y, ").",
                 "HÁ evidencias para rejeitar H0, logo, HÁ diferença entre os grupos"))
}

