rm(list = ls())
gc()

library(tidyverse)
library(data.table)
library(scales)
library(purrr)
library(readr)
library(vroom)
library(INLA)
library(doParallel)
library(sp)
library(sn)
library(snow)
library(lme4)
library(nowcaster)

n_cores <- detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)

####NPreparando dados
dados<-read.csv("dado/clean_data_srag_sragnofever_epiweek.csv.gz")
limiar<-read.csv2("dado/limiares_UF_srag_leitos.csv")
uf_auxi<-read.csv("dado/tabela_auxiliar_uf.csv")

uf_auxi <- uf_auxi %>%
  add_row(
    DS_UF_SIGLA = "BR",
    SG_UF_NOT = 0
  )

###lendo funções

source("fct/get.intensity.R")
source("fct/slope.estimate.quant_fx.R")
source("fct/run_nowcasting_leitos.R")

###Parametros

ano<-epiyear(Sys.Date())-4
today.week.ori<-epiweek(Sys.Date())
lyear<-epiyear(Sys.Date())
def_dmax<-15
window<-2.25*def_dmax

###Dados UF

uf_auxi2<- uf_auxi %>%
  select(-CO_MUN_NOT)

df<- dados %>%
  filter(DT_SIN_PRI_epiyear>=ano)%>%
  mutate(fx_etaria= case_when(
    idade_em_anos<=12 ~ "0-12",
    idade_em_anos>=13  ~ "> 12"
  ))%>%
  select(SG_UF_NOT, CO_MUN_NOT, DT_SIN_PRI, DT_DIGITA, DT_DIGITA_epiyear, DT_DIGITA_epiweek,
         SinPri2Digita_DelayWeeks,fx_etaria,idade_em_anos) %>%
  mutate(DT_SIN_PRI=ymd(DT_SIN_PRI),
         DT_DIGITA=ymd(DT_DIGITA)) %>%
  filter(SG_UF_NOT<90, !is.na(SG_UF_NOT))%>%
  left_join(uf_auxi2, by=c("SG_UF_NOT"))
  

rm(dados)
gc()

threshold <- df %>%
  select(SG_UF_NOT, DS_UF_SIGLA, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter(
    (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
      (DT_DIGITA_epiyear == lyear)
  ) %>%
  dplyr::group_by(SG_UF_NOT, DS_UF_SIGLA) %>%
  dplyr::summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=0.975, na.rm=TRUE)) %>%
  dplyr::mutate(dmax = dmax + 2,
                dmax = case_when(
                  dmax > def_dmax ~ as.numeric(def_dmax),
                  dmax < 4 ~ 4,
                  TRUE ~ as.numeric(dmax)),
                wdw = case_when(
                  ceiling(2.25*dmax) > window ~ as.numeric(window),
                  TRUE ~ as.numeric(ceiling(2.25*dmax)))
  )

####Agregação
UF <- unique(df$DS_UF_SIGLA)
UF <- sort(UF)


now_uf<-run_nowcasting_leitos(df=df, threshold = threshold, UF= UF)

source("fct/nowcast_leitos_br.R")

df_inten <- now_uf$intensidade_srag %>%
  mutate(fx_etaria=case_when(
    fx_etaria.num==1~"0-12",
    fx_etaria.num==2~ "> 12"
  ))

df_tend <- now_uf$tendencia_srag %>%
  bind_rows(tendencia_br)%>%
  mutate(fx_etaria=case_when(
    fx_etaria.num==1~"0-12",
    fx_etaria.num==2~ "> 12"
  )) %>%
  pivot_wider(names_from = window, values_from = tendencia,  
              names_glue = "{.value}_{window}_semanas") %>%
  left_join(uf_auxi, by="DS_UF_SIGLA") %>%
  select(-fx_etaria.num, -CO_MUN_NOT)

df_now<-now_uf$for_srag %>%
  bind_rows(dados_f_br)%>%
          bind_rows(tendencia_br)%>%
        left_join(df_tend, by=c("DS_UF_SIGLA", "fx_etaria")) 


write.csv2(df_now, "output/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre")
wirte.csv2(df_tend, "output/estados_intensidade_sem_filtro_febre")

#########################################################################################
######################## CAPITAIS #######################################################
#########################################################################################

######Nowcasting capitais

cod_cap<- c(uf_auxi$CO_MUN_NOT)  

df_cap<- df %>%
  filter(CO_MUN_NOT %in% cod_cap) 

###Nowcastong UF
###Calculando o threshold
threshold_cap <- df_cap %>%
  select(SG_UF_NOT,DS_UF_SIGLA, CO_MUN_NOT, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter(
    (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
      (DT_DIGITA_epiyear == lyear)
  ) %>%
  dplyr::group_by(SG_UF_NOT,DS_UF_SIGLA, CO_MUN_NOT) %>%
  dplyr::summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=0.975, na.rm=TRUE)) %>%
  dplyr::mutate(dmax = dmax + 2,
                dmax = case_when(
                  dmax > def_dmax ~ as.numeric(def_dmax),
                  dmax < 4 ~ 4,
                  TRUE ~ as.numeric(dmax)),
                wdw = case_when(
                  ceiling(2.25*dmax) > window ~ as.numeric(window),
                  TRUE ~ as.numeric(ceiling(2.25*dmax)))
  )

now_cap<-run_nowcasting_leitos(df=df_cap, threshold = threshold_cap, UF= UF)

dfc_inten <- now_cap$intensidade_srag %>%
  mutate(fx_etaria=case_when(
    fx_etaria.num==1~"0-12",
    fx_etaria.num==2~ "> 12"
  ))

dfc_tend <- now_cap$tendencia_srag %>%
  mutate(fx_etaria=case_when(
    fx_etaria.num==1~"0-12",
    fx_etaria.num==2~ "> 12"
  )) %>%
  pivot_wider(names_from = window, values_from = tendencia,  
              names_glue = "{.value}_{window}_semanas") %>%
  select(-fx_etaria.num) %>%
  left_join(uf_auxi, by="DS_UF_SIGLA")

dfc_now<-now_cap$for_srag %>%
  left_join(df_tend, by=c("DS_UF_SIGLA", "fx_etaria")) %>%
  left_join(uf_auxi, by=c("DS_UF_SIGLA", "SG_UF_NOT"))

write.csv2(dfc_now, "output/capitais_serie_estimativas_tendencia_sem_filtro_febre")
wirte.csv2(dfc_tend, "output/capitais_intensidade_sem_filtro_febre")

