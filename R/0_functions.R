
#' @param x file
#' 
#' @param col_type padrão de leitura para todas as colunas; por default é chr.
#' 
#' @return tbl com colunas chr
read_csv2_chr <- function(file, col_type = "c"){
  
  readr::read_csv2(file = {{file}},
                   col_types = readr::cols(.default = col_type))
}

#' Carrega CSVs de um diretório
#' 
#' @param x diretório onde CSVs estão
#' 
#' @return retorna lista contendo CSVs
#' 
#' @example load_data("data/")
#' 
#' @author Gendson Moreira <https://github.com/moreiragendson>
load_data <- function(x){
  fs::dir_ls(x, regexp = "*.csv") %>% 
  purrr::map(.f = read_csv2_chr)
}

#' @param tbl_list lista de tibbles
#' 
#' @return retorna nome de todas as variáveis em uma lista
#' 
#' @example get_var_names(lista_de_data_frames)
get_var_names <- function(tbl_list){
  tbl_list %>% 
    purrr::map(.f = names)
}
#################################################

#---- tabyl_pct serve para alimentar get_pct ####

#################################################

#' É uma adaptação de \code{\link[janitor]{tabyl}} exibe o total e o percentuais de todas as categorias de uma variável
#' 
#' @param tbl data frames
#' 
#' @param var variável
#' 
#' @return retorna total e percentual de categorias de uma variável
tabyl_pct <- function(tbl, var){
  tbl %>% 
    janitor::tabyl(var1 = var ) %>% 
    janitor::adorn_pct_formatting(digits = 2)
}

#' aplica a função tabyl_pct em todas as colunas de um tbl
#' 
#' @return retorna uma lista com tbl |1 por varíavel| contendo n e percentuais de todas as variáveis.

get_pct <- function(tbl){
  
  vars <- colnames(tbl)
  
  vars %>% 
    purrr::map(.x = tbl,
               .f = tabyl_pct)
}
