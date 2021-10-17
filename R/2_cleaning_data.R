
# ---- read files ####

sismigra <- load_data("data/") %>% 
  bind_rows() %>% 
  clean_names()


# ---- first looking ####

resumo <- get_pct(sismigra)

# ---- transform ####

sismigra %<>% mutate(
  
  estado_civil = str_to_title(estado_civil),
  
  estado_civil = case_when(
    
    estado_civil %in% c("Separado Judicialmente",
                        "Separado consensualmente") ~ "Separado",
    estado_civil %in% c("Desconhecido", "-") ~ "Sem informação",
    estado_civil %in% c("Desquitado", "Outros") ~ "Outros",
    TRUE ~ estado_civil
    
  ),
  
  sexo = case_when(sexo %in% c("Não especificad",
                            "Não especificado",
                            "-") ~ "Sem informação",
                TRUE ~ sexo
                
  ),
  
  classificacao_revisada = str_to_title(classificacao_revisada),
  
  classificacao_revisada = case_when(
    classificacao_revisada %in% c("Não aplicáveis", 
                                  "Não aplicável") ~ "Não se aplica",
    classificacao_revisada %in% c("Provisório",
                                  "Residente") ~ "Residente",
    TRUE ~ classificacao_revisada
    
  ),
  
  unidade_fed_residencia = ifelse(
    unidade_fed_residencia == "-", NA, unidade_fed_residencia
  
  ),
  
  pais_nacionalidade = str_to_title(pais_nacionalidade),
  
  pais_nacionalidade = ifelse(
    pais_nacionalidade == "-", NA, pais_nacionalidade
  
  )
  
)

# ---- problema com municípios ####

# O problema é que a variável municipio_residencia mostra UF_MUNICIPIO
# Gostaria que fosse apenas MUNICIPIO.

# O código abaixo identifica que existe "_" -- underscore -- em municipio_residencia
# Se sim, ele retira as três primeiras strings do chr vector

sismigra %<>% 
  mutate(municipio = 
           case_when(
             
             str_detect(municipio_residencia, pattern = 
                          fixed(pattern = "_")) == TRUE ~ # logo,
               
               str_sub(municipio_residencia, start = 4,
                       end = str_length(municipio_residencia)),
             
             TRUE ~ municipio_residencia),
         
         
         municipio = str_to_title(municipio),
         municipio = txt4cs::remove_accent(municipio),
         
         municipio = case_when(
           municipio %in% c("", "-", "Nao Especificado") ~ "Sem informação",
           TRUE ~ municipio),
         
         pais_nacionalidade = txt4cs::remove_accent(pais_nacionalidade)
         
         )

# ---- filtro ####

filtrado <- sismigra %>% 
  filter(
    !classificacao_revisada %in% c("Não Aplicável",
                                   "Não Aplicáveis",
                                   "-")
        ) 

# fs::dir_create("results")

filtrado %>% saveRDS("results/registro_18_21.rds")
