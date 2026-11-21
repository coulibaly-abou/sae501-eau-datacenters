# Data centers, IA et eau : tableau de bord R Shiny

SAÉ 5.EMS.01, BUT3 Science des données, IUT de Carcassonne (2026-2027).
Ayi AMAVIGAN, Ada GADJI, Aboubakar COULIBALY.

**Application en ligne : https://datacente.shinyapps.io/eau-datacenters-sae501/**

Application interactive tirée du Rapport 2. Elle reprend les mêmes calculs et retrouve les mêmes chiffres :
6 239 sites dans 135 pays, 336 Mm³ d'eau en 2024 (entre 88 et 999), Luxembourg à 1,2 %, France à 5,7 Mm³.

## Contenu

| Fichier | Rôle |
|---|---|
| `donnees_brutes/` | panel annuel, inventaire des data centers, sites OSM, provinces Aqueduct, AQUASTAT par usage, sites Google, rapport UE, climat mensuel CCKP, fond de carte Natural Earth |
| `prep.R` | Calculs du rapport (répartition AIE, eau, ACP + CAH, prévision France), écrit `app/app_data.rds` |
| `app/app.R` | Application Shiny (bslib + plotly) |
| `deploy.R` | Mise en ligne sur shinyapps.io |

## Onglets (version 2, 20/11/2026, après les remarques du jury)

Chaque graphique a un encadré « Comment lire » et « À retenir », pensé pour des élus et le grand public.

1. **L'essentiel** : 5 messages clés avec leurs dates.
2. **Carte du monde** : carte cliquable (7 indicateurs) et fiche par pays, avec des équivalents du quotidien (piscines, habitants, foyers, km en voiture).
3. **Dans les régions** : les sites OpenStreetMap colorés selon le stress hydrique de leur région (WRI Aqueduct). Le même indicateur donne 10 % des sites en stress élevé à l'échelle du pays, mais 27 % à l'échelle de la région.
4. **Au fil de l'année** : pluie et température mois par mois (normales 1991-2020), mois chauds et secs.
5. **Arbitrages** : l'eau des data centers face à l'eau des habitants, les pays à surveiller, et les mesures réelles de Google par site (2025).
6. **Hier et demain** : 2015-2022, puis les scénarios de l'AIE jusqu'en 2035.
7. **Profils de pays** : ACP et CAH en 4 familles, avec une carte.
8. **Données et méthode** : tableau téléchargeable, méthode, sources et dates.

## Lancer en local

```r
# install.packages(c("shiny","bslib","bsicons","plotly","DT","dplyr","tidyr","leaflet","sf"))
Rscript prep.R                 # seulement si les données changent
shiny::runApp("app")
```

## Sources des données

| Donnée | Source | Période |
|---|---|---|
| Data centers par pays | inventaire data-centres-worldwide | relevé 2024 |
| Électricité des data centers, scénarios | AIE, *Energy and AI* (2025) | 2020-2035 |
| Eau prélevée, stress hydrique, population, PIB | FAO AQUASTAT, Banque mondiale | 2015-2022 |
| Eau par usage (agriculture, industrie, ménages) | FAO AQUASTAT | ≤ 2017 |
| Électricité et CO₂ par kWh | Ember | 2019-2022 |
| Sites géolocalisés | OpenStreetMap (licence ODbL) | relevé du 20/09/2026 |
| Stress hydrique par région | WRI Aqueduct 4.0 | référence 1979-2019 |
| Climat mensuel | Banque mondiale CCKP (CRU TS 4.08) | normales 1991-2020 |
| Eau par site Google | Google, rapport environnemental 2026 | 2025 |
| Fond de carte | Natural Earth (domaine public) | 1:50m |

Les logos de l'IUT et de l'Université de Perpignan Via Domitia appartiennent à leurs établissements.
