# Análise espaço-temporal da vacinação BCG

* **OBJETIVO:** predizer e analisar existências de agrupamentos espaciais da vacinação de BCG no Brasil entre os anos de 2019 a 2022.

* **DADOS:** os dados utilizados são públicos e estão disponíveis no site do DATASUS do Ministério da Saúde (https://datasus.saude.gov.br/informacoes-de-saude-tabnet/)

| Variável        | Tipo           |
| ------------- |:-------------:|
| Doses aplicadas de vacina BCG | int |
| Cobertura vacinal da vacina BCG      | float |
| Data      | date      |

## A análise está dividida em duas partes:
1. [Analise de séries temporais e forecast das doses aplicadas da vacina BCG, entre janeiro de 2019 a dezembro de 2022](https://github.com/pedrodesa/spatio_temporal_BCGvaccination/blob/main/SCRIPT_time_series_analysis_BCG.R).
2. [Análise de clusters espaciais das coberturas vacinais de BCG entre os anos de 2019 a 2022](https://github.com/pedrodesa/spatio_temporal_BCGvaccination/blob/main/SCRIPT_spatial_analysis_BCG.R).
