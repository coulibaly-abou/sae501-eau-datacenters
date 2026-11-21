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

## Onglets

Tableau de bord tiré des rapports 1 et 2, pensé pour un oral : titres qui donnent le résultat, une phrase par bloc, méthode au survol de l'icône ⓘ.

1. **Accueil** : photo, titre, équipe, 4 chiffres clés.
2. **Carte** : parcours en 4 étapes (où sont-ils, combien d'eau, où l'eau manque, où ça coince) ; zoom pays (régions et sites) ; fiche pays (jauge, « sur 10 000 litres », saisons, comparaison).
3. **Régions** : densité des sites (noyau gaussien), points chauds dense × sec, manque d'eau aujourd'hui / 2030 / 2050 (Aqueduct), disparités par région.
4. **Saisons** : effet ciseaux (pluie et degrés-jours de refroidissement), carte mois par mois, calendrier des 20 pays les plus équipés.
5. **Usages** : sur 10 000 litres, équivalents d'un data center (1 / 10 / 100 MW), eau directe 9 % / indirecte 91 %, usages 1990-2035.
6. **Demain** : scénarios AIE, pays les plus exposés en 2035, France avec la part de l'IA.
7. **Méthode** : estimation en 5 étapes, choix du modèle de prévision, profils de pays (ACP + CAH).
8. **Sources** : données par pays (CSV), sources et dates, limites.

Le curseur d'année (2024-2035, bouton ▶) et le pays choisi s'appliquent à toute l'application.

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

Photos de l'accueil (Wikimedia Commons) : « BalticServers data center » par BalticServers.com (CC BY-SA 3.0) ; « Data centers in Ashburn » par Theodore Christopher (CC0) ; « Google Data Center, Council Bluffs Iowa » par Chad Davis (CC BY 2.0).
