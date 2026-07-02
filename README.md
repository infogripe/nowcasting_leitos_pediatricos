# SRAG Nowcasting Estimates for Bed Occupancy Assessment  
# Estimativas de Nowcasting de SRAG para Avaliação de Ocupação de Leitos

---

<details open>
<summary><strong>PT-BR</strong></summary>

## Visão geral

Este repositório contém estimativas de nowcasting dos casos de Síndrome Respiratória Aguda Grave (SRAG) para apoiar a avaliação da ocupação de leitos pediátricos e adultos.

As estimativas são produzidas para duas faixas etárias:
- 0–12 anos
- maiores de 12 anos

As análises são baseadas em dados de notificação de SRAG do sistema SIVEP-Gripe.

---

## Definição de caso e fonte de dados

Os dados utilizados neste repositório são derivados do SIVEP-Gripe aplicando o seguinte critério:

### SRAG sem exigência de febre

Filtro intermediário (Ministério da Saúde):

- Tosse OU dor de garganta E  
- Dispneia OU saturação < 95% OU desconforto respiratório E  
- Internação OU óbito  

---

## Conjuntos de dados gerados

Este repositório gera os seguintes arquivos:

- `estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv` (nível de estado e país)  
- `capitais_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv` (nível de capital)  

---

## Dicionário de dados

| Variável | Descrição |
|----------|-----------|
| epiweek | Semana epidemiológica |
| epiyear | Ano epidemiológico |
| fx_etaria | Faixa etária |
| Median | Mediana da estimativa |
| LS | Limite superior do intervalo de credibilidade de 95% |
| LI | Limite inferior do intervalo de credibilidade de 95% |
| LSb | Limite superior do intervalo de credibilidade de 50% |
| LIb | Limite inferior do intervalo de credibilidade de 50% |
| media.movel | Média móvel |
| casos.notificados | Casos notificados |
| DS_UF_SIGLA | Sigla da UF |
| tendencia_3_semanas | Tendência de 3 semanas |
| tendencia_6_semanas | Tendência de 6 semanas |
| SG_UF_NOT | Código da UF (unidade de notificação) |

---

## Objetivo

Essas estimativas de nowcasting têm como objetivo apoiar o monitoramento em tempo real da carga de doenças respiratórias e auxiliar na avaliação da demanda por leitos hospitalares em populações pediátricas e adultas.

</details>

---

<details>
<summary><strong>EN</strong></summary>

## Overview

This repository contains nowcasting estimates of Severe Acute Respiratory Syndrome (SRAG) cases to support the assessment of pediatric and adult hospital bed occupancy.

Estimates are produced for two age groups:
- 0–12 years
- older than 12 years

The analyses are based on SRAG notification data from the SIVEP-Gripe surveillance system.

---

## Case definition and data source

The data used in this repository are derived from SIVEP-Gripe using the following inclusion criteria:

### SRAG definition without fever requirement

Intermediate filter (Ministry of Health):

- Cough OR sore throat AND  
- Dyspnea OR oxygen saturation < 95% OR respiratory distress AND  
- Hospitalization OR death  

---

## Generated datasets

This repository generates the following files:

- `estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv` (state and country level)  
- `capitais_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv` (capital level)  

---

## Data dictionary

| Variable | Description |
|----------|-------------|
| epiweek | Epidemiological week |
| epiyear | Epidemiological year |
| fx_etaria | Age group |
| Median | Median estimate |
| LS | Upper bound of the 95% credible interval |
| LI | Lower bound of the 95% credible interval |
| LSb | Upper bound of the 50% credible interval |
| LIb | Lower bound of the 50% credible interval |
| media.movel | Moving average |
| casos.notificados | Reported cases |
| DS_UF_SIGLA | State abbreviation |
| tendencia_3_semanas | 3-week trend |
| tendencia_6_semanas | 6-week trend |
| SG_UF_NOT | Reporting unit code (state) |

---

## Objective

These nowcasting estimates aim to support real-time monitoring of respiratory disease burden and assist in evaluating hospital bed demand in pediatric and adult populations.

</details>
