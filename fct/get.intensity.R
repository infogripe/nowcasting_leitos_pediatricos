#'@title get.intesity
#'
#'
#' @param trajectories Data.frame with the predicted or nowcasted estimate
#'  @param region_code IBGE area code 
#' @param window [in weeks] Window of how much time will be used to calculate the intensity
#'
#' @return Intensity of SRAG cases according to epidemic thresholds
#' @export


get.intensity<- function(trajectories, region_code, window=2, fx_etarian){
  
  limi<-limiar %>%
    filter(escala=="casos") %>%
    mutate(across(1:5, as.numeric)) %>%
    filter(regiao==region_code) %>%
    mutate(fx_etaria.num=case_when(
      fx_etaria=="0 a 12 anos" ~1,
      TRUE ~ 2
    )) %>%
    filter(fx_etaria.num==fx_etarian)
  
  ##calculando intensidade nas últimas 2 semanas
  inten<-  now$trajectories %>%
    filter(fx_etaria.num==fx_etaria) %>%
    group_by(sample,Time,dt_event, fx_etaria.num) %>%
    summarise(Y=sum(Y, na.rm = TRUE))%>%
    as.data.frame() %>%
    filter(dt_event>(max(dt_event)-window*7)) %>%
    summarise("Baixo risco"=sum(Y<limi$baixo)*100/n(),
              "Segurança"=sum(Y>=limi$baixo & Y<limi$moderado)*100/n(),
              "Alerta"=sum(Y>=limi$moderado & Y<limi$alto)*100/n(),
              "Risco"=sum(Y>=limi$alto & Y<limi$muito_alto)*100/n(),
              "Alto risco"=sum(Y>=limi$muito_alto)*100/n()) %>%
    pivot_longer(
      everything(),
      names_to = "intensidade",
      values_to = "prob"
    ) %>%
    mutate(cum=cumsum(prob))
  
  inten_f<-inten %>% filter(cum>=50) %>%
    filter(cum==min(cum)) %>% 
    filter(prob>0) %>% ###excluir as regiões em que a prob == 0, caso tenha repetição de valor do cumsum
    select(intensidade) 
 
  tibble(
    fx_etaria.num = fx_etarian,
    intensidade = inten_f$intensidade,
    regiao=region_code,
    janela=window
  )
  
}
