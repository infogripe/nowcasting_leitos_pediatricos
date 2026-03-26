##Nowcasting leitos

run_nowcasting_leitos <- function(df, threshold, UF){

comb <- function(x, y) {
  lapply(seq_along(x),
    function(i) rbind(x[[i]], y[[i]]))
}

saida <- foreach(i=1:length(UF),
                  .packages=c("tidyverse", "data.table", "scales", "purrr",
                  "readr", "vroom", "INLA", "snow", "nowcaster", "get.intensity",
                  "slope.estimate.quant_fx"), 
                 .export = c("get.intensity", "slope.estimate.quant_fx", "limiar"),
                  .combine="comb") %dopar% {
                    
                    ## Função de Nowcasting
                    dados2 <- df %>% filter(DS_UF_SIGLA == UF[i])

                    dwdw<- threshold %>% filter (DS_UF_SIGLA==UF[i]) %>%
                      pull(wdw)
                    ddmax<-threshold %>% filter (DS_UF_SIGLA==UF[i]) %>%
                      pull(dmax)
                    
                    now <- nowcasting_inla(
                        dataset=dados2,
                        bins_age = c(0,13,160),
                        age_col = idade_em_anos,
                        date_onset = DT_SIN_PRI,
                        date_report = DT_DIGITA,
                        Dmax = ddmax,
                        K = 0,
                        trajectories = T,
                        wdw=dwdw)
                    
    ### objetos para exportação do foreach: ###
                    
nowcast_age<-now$age %>%
            mutate(epiweek=epiweek(dt_event),
                   epiyear=epiyear(dt_event)) %>%
            mutate(casos2=Median) %>%
            mutate(fx_etaria=case_when(
            fx_etaria.num==1~"0-12",
            fx_etaria.num==2~ "> 12"
            )) %>%
  select(dt_event, epiyear, epiweek, fx_etaria, casos2, Median, LS, LI, LSb, LIb)

###Corte dos dados observados           
cut_week<-epiweek(min(nowcast_age$dt_event))
cut_year<-epiyear(min(nowcast_age$dt_event))

####Dados notificados
dados_noti<- dados2 %>%
  mutate(
    epiweek=epiweek(DT_SIN_PRI),
    epiyear=epiyear(DT_SIN_PRI)) %>%
  group_by(epiweek, epiyear, fx_etaria)%>%
  summarise(casos.notificados=n()) 

###Dados final
                    
    dados_f<- dados2 %>%
      mutate(
      epiweek=epiweek(DT_SIN_PRI),
      epiyear=epiyear(DT_SIN_PRI)) %>%
      group_by(epiweek, epiyear, fx_etaria)%>%
      summarise(casos2=n()) %>%
      filter(
        epiyear < cut_year |
          (epiyear == cut_year & epiweek < cut_week)
      ) %>%
      bind_rows(nowcast_age)%>%
      group_by(fx_etaria)%>%
      arrange(epiyear, epiweek)%>%
      mutate(media.movel=round(zoo::rollmean(casos2, k=3, fill = NA, align = "center"))) %>%
      left_join(dados_noti, by=c("fx_etaria", "epiyear", "epiweek")) %>%
      select(-casos2) 

    dados_f$DS_UF_SIGLA<-UF[i]
    
###Dados de intensidade
    intensidade <- map_dfr(
      c(1, 2),
      ~ get.intensity(
        trajectories = now$trajectories,
        region_code = UF[i],
        fx_etarian = .x
      )
    )
    

###Dados de tendência
    
    grid <- expand_grid(
      window = c(3, 6),
      fx_etarian = c(1, 2)
    )
    
    tendencia <- pmap_dfr(
      grid,
      function(window, fx_etarian) {
        slope.estimate.quant_fx(
          trajectories = now$trajectories,
          window = window,
          fx_etarian = fx_etarian
        ) %>%
          mutate(window = window,
                 fx_etaria.num = fx_etarian)
      }
    )
    
    tendencia$DS_UF_SIGLA<-UF[i]
      
    intensidade$DS_UF_SIGLA<-UF[i]

    rm(trajetorias_total_srag)
    
    list(dados_f,
         intensidade,
         tendencia#,
         )
    ###### final do foreach
                  }
## manejo comum dos outputs
for_srag <- saida[[1]]
intensidade_srag<-saida[[2]]
tendencia_srag<- saida[[3]]


return(list(
  for_srag = for_srag,
  intensidade_srag = intensidade_srag,
  tendencia_srag = tendencia_srag
  
))

}
