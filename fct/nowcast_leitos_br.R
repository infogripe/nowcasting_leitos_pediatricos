

threshold_br <- df %>%
  select(SG_UF_NOT, DS_UF_SIGLA, DT_DIGITA_epiyear, DT_DIGITA_epiweek, SinPri2Digita_DelayWeeks) %>%
  filter(
    (DT_DIGITA_epiyear == lyear-1 & DT_DIGITA_epiweek >= today.week.ori) |
      (DT_DIGITA_epiyear == lyear)
  ) %>%
  dplyr::summarize(dmax = quantile(SinPri2Digita_DelayWeeks, probs=0.975, na.rm=TRUE)) %>%
  dplyr::mutate(dmax = dmax + 2,
                dmax = case_when(
                  dmax > def_dmax ~ as.numeric(def_dmax),
                  dmax < 4 ~ 4,
                  TRUE ~ as.numeric(dmax)),
                wdw = case_when(
                  ceiling(2.25*dmax) > window ~ as.numeric(window),
                  TRUE ~ as.numeric(ceiling(2.25*dmax))))


dwdw<- threshold_br %>% 
  pull(wdw)
ddmax<-threshold_br %>% 
  pull(dmax)

now_br <- nowcasting_inla(
  dataset=df,
  bins_age = c(0,13,160),
  age_col = idade_em_anos,
  date_onset = DT_SIN_PRI,
  date_report = DT_DIGITA,
  Dmax = ddmax,
  K = 0,
  trajectories = T,
  wdw=dwdw)


nowcast_age_br<-now_br$age %>%
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(casos2=Median) %>%
  mutate(fx_etaria=case_when(
    fx_etaria.num==1~"0-12",
    fx_etaria.num==2~ "> 12"
  )) %>%
  select(dt_event, epiyear, epiweek, fx_etaria, casos2, Median, LS, LI, LSb, LIb)

###Corte dos dados observados           
cut_week<-epiweek(min(nowcast_age_br$dt_event))
cut_year<-epiyear(min(nowcast_age_br$dt_event))

####Dados notificados
dados_noti<- df %>%
  mutate(
    epiweek=epiweek(DT_SIN_PRI),
    epiyear=epiyear(DT_SIN_PRI)) %>%
  group_by(epiweek, epiyear, fx_etaria)%>%
  summarise(casos.notificados=n()) 

###Dados final

dados_f_br<- df %>%
  mutate(
    epiweek=epiweek(DT_SIN_PRI),
    epiyear=epiyear(DT_SIN_PRI)) %>%
  group_by(epiweek, epiyear, fx_etaria)%>%
  summarise(casos2=n()) %>%
  filter(
    epiyear < cut_year |
      (epiyear == cut_year & epiweek < cut_week)
  ) %>%
  bind_rows(nowcast_age_br)%>%
  group_by(fx_etaria)%>%
  arrange(epiyear, epiweek)%>%
  mutate(media.movel=round(zoo::rollmean(casos2, k=3, fill = NA, align = "center"))) %>%
  left_join(dados_noti, by=c("fx_etaria", "epiyear", "epiweek")) %>%
  select(-casos2) %>%
  mutate(
    "DS_UF_SIGLA"="BR"
  )


grid <- expand_grid(
      window = c(3, 6),
      fx_etarian = c(1, 2)
    )
    
    tendencia_br <- pmap_dfr(
      grid,
      function(window, fx_etarian) {
        slope.estimate.quant_fx(
          trajectories = now_br$trajectories,
          window = window,
          fx_etarian = fx_etarian
        ) %>%
          mutate(window = window,
                 fx_etaria.num = fx_etarian)
      }
    )
    
    tendencia_br<-tendencia_br %>%
      mutate(
        "DS_UF_SIGLA"="BR"
      )
      
    