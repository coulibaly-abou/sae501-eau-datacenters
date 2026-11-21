# =============================================================================
# SAÉ 5.EMS.01 - Data centers et eau : tableau de bord interactif (version 2)
# Ayi AMAVIGAN - Ada GADJI - Aboubakar COULIBALY | BUT3 Science des données, IUT de Carcassonne
# Version 2 (20/11/2026) : prise en compte des remarques du jury
#   - lecture tout public (« Comment lire » / « À retenir » sous chaque graphique)
#   - disparités à l'intérieur des pays (régions), saisons, arbitrages entre usages
#   - équivalents du quotidien, CO2 de l'électricité
# Données préparées par ../prep.R
# =============================================================================
library(shiny); library(bslib); library(bsicons); library(plotly); library(DT)
library(dplyr); library(tidyr); library(leaflet); library(sf)

d <- readRDS("app_data.rds")
wue <- d$wue

# ------------------------------------------------------------- Constantes ----
PISCINE <- 2500     # m3, bassin olympique 50 x 25 x 2 m
HAB_M3 <- d$conso_hab  # 65 m3 par habitant et par an (ratio Arcep : 6,5 Mm3 = 100 000 personnes)
FOYER_MWH <- 4.5    # ordre de grandeur : électricité d'un foyer français par an
KM_KG <- 0.2        # ordre de grandeur : kg de CO2 par km en voiture thermique
mois_fr <- c("janv.", "févr.", "mars", "avril", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc.")

cat_lab <- c("Aucun (<25 %)", "Faible (25-50 %)", "Moyen (50-75 %)", "Élevé (75-100 %)", "Critique (>100 %)")
categoriser <- function(s) cut(s, c(-Inf, 25, 50, 75, 100, Inf), labels = cat_lab)
col_aq <- setNames(c("#1a9850", "#a6d96a", "#fee08b", "#f46d43", "#a50026", "#8c6d31"), d$lab_aq)
col_classes <- c("Petits pays riches" = "#1b9e77", "Émergents, revenus plus faibles" = "#d95f02",
                 "Pays très secs" = "#e7298a", "Grands pays équipés" = "#7570b3")

fmt <- function(x, k = 1) ifelse(is.na(x), "n.d.",
  formatC(x, format = "f", digits = k, big.mark = " ", decimal.mark = ","))
pct <- function(x) ifelse(is.na(x), "n.d.", paste0(trimws(formatC(x, format = "fg", digits = 2, decimal.mark = ",")), " %"))
grand <- function(x) ifelse(is.na(x), "n.d.", ifelse(x >= 1e6, paste(fmt(x / 1e6, 1), "millions"),
                     ifelse(x >= 1e3, paste(fmt(round(x, -2), 0)), fmt(x, 0))))
LEG <- list(orientation = "h", x = 0, y = 1.12)

# Calcul pour tous les pays selon les réglages (coefficient d'eau, horizon, scénario)
calculer <- function(w, scenario, horizon) {
  p <- d$pays
  if (horizon == 2024) p$f <- 1 else
    p <- p |> left_join(d$scen_zone |> filter(scenario == !!scenario, horizon == !!horizon) |>
                          select(zone, f = f_zone), by = "zone")
  p |> mutate(
    e_h = e0 * f,                                            # TWh
    e_ia = d$part_ia[["monde"]] * e0 + d$part_ia[["hausse"]] * (e_h - e0),
    eau = e_h * w, eau_ia = e_ia * w,                        # millions de m3
    part = 100 * eau / (prelev0 * 1e3),                      # % des prélèvements du pays
    part_bas = 100 * e_h * wue[["bas"]] / (prelev0 * 1e3),
    part_haut = 100 * e_h * wue[["haut"]] / (prelev0 * 1e3),
    eau_menages = prelev0 * 1e3 * pct_menages / 100,         # millions de m3
    ratio_menages = 100 * eau / eau_menages,
    part_elec = 100 * e_h / elec,
    co2_kt = e_h * gco2_kwh,                                 # TWh x g/kWh = milliers de tonnes
    stress_h = stress0 * (1 + (eau - e0 * w) / (prelev0 * 1e3)),
    cat0 = categoriser(stress0), cat_h = categoriser(stress_h))
}

# Encadré de lecture sous chaque graphique
lecture <- function(comment, retenir) card_footer(
  div(class = "small text-muted mb-1", bs_icon("book"), tags$b(" Comment lire : "), comment),
  div(class = "retenir", bs_icon("lightbulb-fill"), tags$b(" À retenir : "), retenir))
bloc <- function(titre, sortie, comment, retenir, h = "420px", plot = plotlyOutput)
  card(full_screen = TRUE, card_header(titre), plot(sortie, height = h), lecture(comment, retenir))

# ------------------------------------------------------------- Interface -----
theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#1f6f8b", base_font = font_google("Inter")) |>
  bs_add_rules("
    .bslib-value-box .value-box-value{font-size:1.55rem !important;line-height:1.2;}
    .bslib-value-box .value-box-showcase .bi{font-size:2rem !important;width:2rem;height:2rem;}
    .retenir{background:#fff6d6;border-left:4px solid #f0ad4e;padding:6px 10px;border-radius:4px;}
    .message{border-left:5px solid #1f6f8b;padding:10px 14px;margin-bottom:10px;background:#f4f8fa;border-radius:4px;}
    .message .chiffre{font-size:1.6rem;font-weight:700;color:#1f6f8b;}
    .fiche dt{font-weight:500;color:#555;} .fiche dd{font-size:1.05rem;margin-bottom:.45rem;}
    .equiv{font-size:.85rem;color:#666;}
    .bandeau{gap:24px;padding:18px 8px;}
    .bandeau .logo{height:110px;width:auto;}
    .cadre-titre{flex:1;border:3px solid #1f6f8b;border-radius:14px;padding:22px 26px;background:linear-gradient(180deg,#f4f8fa,#ffffff);box-shadow:0 4px 18px rgba(31,111,139,.12);}
    .cadre-titre h1{font-weight:800;color:#1f6f8b;margin:6px 0;font-size:2.4rem;}
    .sur-titre{text-transform:uppercase;letter-spacing:.08em;font-size:.8rem;color:#777;}
    .sous-titre{font-size:1.15rem;color:#333;margin:0;}
    .auteurs{font-size:1.15rem;margin:14px 0 22px;} .auteurs b{color:#1f6f8b;}
    .tuile{text-align:center;padding:18px;border-radius:12px;background:#fff;border:1px solid #e3e9ee;}
    .tuile .gros{font-size:2rem;font-weight:800;color:#1f6f8b;line-height:1.1;} .tuile .bi{font-size:1.8rem;color:#f0ad4e;}
    .etapes{display:flex;gap:6px;align-items:stretch;flex-wrap:wrap;} .etape{flex:1 1 150px;border:2px solid #1f6f8b;border-radius:12px;padding:14px;background:#fff;}
    .etape .num{display:inline-block;width:34px;height:34px;border-radius:50%;background:#1f6f8b;color:#fff;font-weight:700;text-align:center;line-height:34px;margin-right:6px;}
    .etape .val{font-size:1.25rem;font-weight:700;color:#d35400;margin-top:6px;}
    .fleche{display:flex;align-items:center;justify-content:center;font-size:1.8rem;color:#1f6f8b;flex:0 0 28px;}
    @media (max-width:900px){.bandeau{flex-direction:column;} .bandeau .logo{height:80px;} .fleche{transform:rotate(90deg);}}")

ui <- page_navbar(
  id = "nav", title = "Data centers et eau", theme = theme, fillable = FALSE, window_title = "Data centers, IA et eau - SAÉ 5.EMS.01",
  sidebar = sidebar(id = "sb", width = 285, open = "desktop",
    h6(bs_icon("sliders"), " Réglages (facultatif)"),
    radioButtons("coef", "Combien d'eau pour 1 kWh consommé ?",
      choices = c("Peu : 0,21 L (eau prélevée sur place, Arcep 2024)" = "bas",
                  "Moyenne : 0,81 L (notre valeur de référence)" = "central",
                  "Beaucoup : 2,41 L (avec l'eau des centrales électriques)" = "haut",
                  "Valeur libre" = "perso"), selected = "central"),
    conditionalPanel("input.coef == 'perso'", sliderInput("coef_perso", NULL, 0.1, 5, 0.81, 0.05)),
    radioButtons("horizon", "Année", c("2024" = 2024, "2030" = 2030, "2035" = 2035), 2024, inline = TRUE),
    conditionalPanel("input.horizon != '2024'",
      radioButtons("scen", "Croissance des data centers (AIE)",
        c("Lente (x1,7 en 2035)" = "bas", "Centrale (x2,9 en 2035)" = "central",
          "Rapide (x4,1 en 2035)" = "haut"), "central")),
    hr(),
    div(class = "small text-muted",
        "Par défaut : année 2024 et 0,81 L d'eau par kWh. Toutes les cartes et tous les chiffres",
        "se mettent à jour quand vous changez ces réglages.")
  ),

  # ---- 0. Accueil
  nav_panel("Accueil", value = "Accueil", icon = bs_icon("door-open"),
    div(class = "bandeau d-flex justify-content-between align-items-center",
      tags$img(src = "logo_iut.png", class = "logo", alt = "IUT Perpignan Carcassonne Narbonne"),
      div(class = "cadre-titre text-center",
        div(class = "sur-titre", "SAÉ 5.EMS.01 · Mener une étude statistique dans un domaine d'application"),
        h1("L'eau des data centers"),
        p(class = "sous-titre", "Estimations, limites, analyse temporelle et analyse multivariée"),
        hr(),
        p(class = "sous-titre", tags$i("Les ressources en eau suffiront-elles pour les data centers, dédiés à l'IA ou non, en plus des autres usages, aujourd'hui et en 2035 ?"))),
      tags$img(src = "logo_upvd.png", class = "logo", alt = "Université de Perpignan Via Domitia")),
    div(class = "auteurs text-center",
      tags$b("Ayi AMAVIGAN"), " · ", tags$b("Ada GADJI"), " · ", tags$b("Aboubakar COULIBALY"), br(),
      span(class = "small text-muted", "BUT Science des données, 3e année · IUT de Carcassonne · Année 2026-2027 · Professeure : Mme Viguier-Pla")),
    layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
      div(class = "tuile", bs_icon("hdd-rack"), div(class = "gros", "6 239"), "data centers recensés dans 135 pays"),
      div(class = "tuile", bs_icon("droplet-half"), div(class = "gros", "336 millions m³"), "d'eau utilisée par les data centers du monde en 2024"),
      div(class = "tuile", bs_icon("geo-alt"), div(class = "gros", "10 % → 27 %"), "des sites en manque d'eau : vu du pays, puis vu de leur région"),
      div(class = "tuile", bs_icon("graph-up-arrow"), div(class = "gros", "x1,7 à x4,1"), "hausse de l'électricité des data centers d'ici 2035")),
    div(class = "text-center my-4", actionButton("entrer", "Découvrir l'étude", icon = bs_icon("arrow-right-circle"), class = "btn-primary btn-lg px-5")),
    p(class = "text-center small text-muted", "Données : AIE, FAO AQUASTAT, Banque mondiale, Ember, WRI Aqueduct, OpenStreetMap, Arcep, Google.",
      "Les onglets en haut de la page suivent l'ordre de l'étude.")
  ),

  # ---- 1. L'essentiel
  nav_panel("L'essentiel", icon = bs_icon("house"),
    layout_columns(col_widths = c(7, 5),
      card(card_header(h5(class = "m-0", "Les ressources en eau suffisent-elles pour les data centers, en plus des autres usages ?")),
        card_body(
          uiOutput("messages"),
          p(class = "small text-muted mt-2",
            "Data center : bâtiment qui héberge des serveurs informatiques (sites web, messagerie, stockage, IA).",
            "Il consomme de l'électricité et utilise de l'eau surtout pour se refroidir.",
            "Nous estimons cette eau pour 135 pays, puis nous la comparons à l'eau disponible et aux autres usages."))),
      card(card_header("Comment utiliser ce tableau de bord"),
        card_body(tags$ol(
          tags$li(tags$b("Carte du monde"), " : cliquez sur un pays pour obtenir sa fiche."),
          tags$li(tags$b("Dans les régions"), " : le même pays peut avoir des régions humides et des régions sèches."),
          tags$li(tags$b("Au fil de l'année"), " : l'été, il fait plus chaud et il pleut souvent moins."),
          tags$li(tags$b("Arbitrages"), " : l'eau des data centers face à l'eau des habitants."),
          tags$li(tags$b("Hier et demain"), " : de 2015 à 2035."),
          tags$li(tags$b("Profils de pays"), " : quels pays se ressemblent."),
          tags$li(tags$b("Données et méthode"), " : sources, dates, limites.")),
          p(class = "small text-muted", "Les réglages à gauche permettent de tester d'autres hypothèses."))))
  ),

  # ---- 2. Carte du monde + fiche pays
  nav_panel("Carte du monde", icon = bs_icon("globe-europe-africa"),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span("Carte du monde : cliquez sur un pays"),
          div(class = "d-flex gap-3 align-items-center",
            selectInput("var_carte", NULL, width = "320px", c(
              "Part de l'eau du pays utilisée par les data centers" = "part",
              "Eau utilisée par les data centers (millions de m³)" = "eau",
              "Eau des data centers / eau des habitants" = "ratio_menages",
              "Nombre de data centers" = "n_dc",
              "Stress hydrique du pays" = "stress0",
              "Sites situés dans une région en manque d'eau" = "pct_sites_region_stress",
              "CO₂ de l'électricité des data centers" = "co2_kt")),
            checkboxInput("pts", "Afficher les sites", FALSE))),
        leafletOutput("carte", height = "520px"),
        lecture(textOutput("carte_comment", inline = TRUE), textOutput("carte_retenir", inline = TRUE))),
      card(card_header(class = "d-flex justify-content-between align-items-center", "Fiche pays",
                       selectInput("pays_sel", NULL, width = "190px",
                                   setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)], "FRA")),
           uiOutput("fiche"))
    )
  ),

  # ---- 3. Dans les régions
  nav_panel("Dans les régions", icon = bs_icon("geo-alt"),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span("Chaque point est un data center, coloré selon le manque d'eau de SA région"),
          selectInput("pays_reg", NULL, width = "220px",
                      c("Monde entier" = "MONDE", setNames(sort(unique(d$osm$iso3)),
                        d$osm$pays[match(sort(unique(d$osm$iso3)), d$osm$iso3)])), "FRA")),
        leafletOutput("carte_reg", height = "520px"),
        lecture("couleur = stress hydrique de la région (part de l'eau disponible déjà prélevée, WRI Aqueduct 4.0).",
                textOutput("reg_retenir", inline = TRUE))),
      card(card_header("Régions qui accueillent le plus de sites"), DTOutput("tab_reg"),
           card_footer(class = "small text-muted",
             "Sites : OpenStreetMap, relevé du 20/09/2026 (inventaire partiel, collaboratif)."))
    )
  ),

  # ---- 4. Au fil de l'année
  nav_panel("Au fil de l'année", icon = bs_icon("calendar3"),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span("Pluie et température, mois par mois"),
          selectInput("pays_clim", NULL, width = "220px",
                      setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)], "FRA")),
        plotlyOutput("clim", height = "400px"),
        lecture("barres bleues = pluie moyenne du mois ; courbe rouge = température. Les mois surlignés sont à la fois plus chauds et plus secs que la moyenne de l'année. Normales 1991-2020.",
                textOutput("clim_retenir", inline = TRUE))),
      bloc("Combien de mois à la fois chauds et secs ?", "carte_mois",
           "plus la couleur est foncée, plus le pays a de mois où la chaleur et le manque de pluie se cumulent.",
           "c'est à ces périodes que les data centers refroidissent le plus, alors que l'eau est la plus rare. Une moyenne annuelle cache ces pics.",
           "400px", leafletOutput)
    ),
    card(card_body(class = "small",
      bs_icon("info-circle"), " Pourquoi l'été compte : beaucoup de data centers se refroidissent en évaporant de l'eau.",
      "Plus l'air est chaud, plus il faut d'eau. Nos chiffres annuels ne donnent pas la consommation mois par mois :",
      "cette page montre seulement quand le risque est le plus fort."))
  ),

  # ---- 5. Arbitrages
  nav_panel("Arbitrages", icon = bs_icon("signpost-split"),
    layout_columns(col_widths = c(7, 5),
      bloc("Où l'eau des data centers peut-elle entrer en concurrence avec les autres usages ?", "arb",
           "chaque bulle est un pays (taille = nombre de sites). À droite : beaucoup de sites dans des régions qui manquent d'eau. En haut : l'eau des data centers pèse plus face à l'eau des habitants. Pays d'au moins 10 data centers.",
           textOutput("arb_retenir", inline = TRUE), "480px"),
      card(card_header("Pays à surveiller (au moins 2 signaux sur 3)"), DTOutput("tab_arb"),
           card_footer(class = "small text-muted",
             "Signaux : (1) eau des DC ≥ 1 % de l'eau des habitants ; (2) ≥ 25 % des sites dans une région en stress élevé ;",
             "(3) stress hydrique du pays ≥ 50 %. Eau des habitants : AQUASTAT, dernière année connue (≤ 2017)."))
    ),
    layout_columns(col_widths = c(7, 5),
      bloc("Mesures réelles : l'eau consommée par chaque data center de Google en 2025", "carte_google",
           "taille du cercle = eau consommée ; couleur = manque d'eau de la région. Ce sont des mesures publiées par Google (rapport environnemental 2026), pas des estimations.",
           "un seul site peut consommer autant d'eau qu'une ville de plusieurs dizaines de milliers d'habitants. Certains gros consommateurs sont dans des régions déjà en manque d'eau (Nebraska, Wallonie).",
           "430px", leafletOutput),
      bloc("Les 15 sites Google qui consomment le plus d'eau (2025)", "bar_google",
           "barre = eau consommée en 2025, convertie en nombre d'habitants (65 m³ par personne et par an).",
           "les écarts entre sites sont énormes : certains sites utilisent presque pas d'eau (refroidissement à l'air), d'autres des millions de m³.",
           "430px")
    )
  ),

  # ---- 6. Analyse statistique (menu)
  nav_menu("Analyse statistique", icon = bs_icon("bar-chart-line"),
  nav_panel("1. Méthode d'estimation", icon = bs_icon("calculator"),
    card(card_header(h5(class = "m-0", "Aucune base ne donne l'eau des data centers de chaque pays : nous l'estimons en 5 étapes")),
         card_body(uiOutput("etapes")),
         lecture("chaque case est une étape du calcul ; le chiffre orange est le résultat pour la France avec les réglages choisis à gauche.",
                 "on part d'un total mondial fiable (AIE), on le répartit entre pays, puis on le convertit en eau. Chaque étape repose sur une hypothèse, que nous testons ci-dessous.")),
    layout_columns(col_widths = c(6, 6),
      bloc("Étapes 1 et 2 : comment les 415 TWh de 2024 sont répartis", "sunburst",
           "cliquez sur une zone pour l'agrandir. Centre = 4 grandes zones (chiffres AIE) ; extérieur = pays, au prorata de leur nombre de data centers.",
           "les États-Unis et la Chine représentent près de 70 % de l'électricité des data centers du monde.", "420px"),
      bloc("Contrôle : notre répartition retrouve-t-elle les chiffres publiés ?", "validation",
           "part des data centers dans l'électricité du pays : notre modèle (bleu) et la valeur publiée par l'AIE (gris).",
           "les écarts sont faibles (4,3 % contre environ 4 % aux États-Unis, 1,8 % contre 2 % au Japon). Ce contrôle reste partiel : plusieurs pays sont calés sur l'AIE.", "420px")),
    layout_columns(col_widths = c(6, 6),
      bloc("Étape 3 : l'hypothèse qui compte le plus, les litres d'eau par kWh", "coefs",
           "eau totale des data centers du monde en 2024 selon le coefficient retenu. Bas = eau prélevée sur place (Arcep) ; moyen = moyenne par type de refroidissement ; haut = eau des centrales électriques comprise.",
           "le résultat varie de 1 à 11 selon le coefficient : c'est la principale incertitude, d'où une fourchette plutôt qu'un chiffre unique.", "380px"),
      bloc("Vérification sur la France, seul pays où l'on a des mesures", "calage_fr",
           "électricité des data centers français : notre modèle (orange) et les sources françaises (bleu). L'Arcep ne couvre qu'une partie des sites.",
           "notre estimation 2024 (7 TWh) est entre la mesure partielle (2,7) et le parc total (12). En 2030, nous sommes un peu sous l'ADEME et RTE : la France est sans doute sous-estimée.", "380px"))
  ),
  nav_panel("2. Modèle et prévisions", icon = bs_icon("bezier2"),
    layout_columns(col_widths = c(7, 5),
      bloc("Quel modèle pour prévoir l'eau prélevée en France ? Test sur 2020-2022", "backtest",
           "chaque modèle apprend sur 2015-2019 (zone grise), puis prévoit 2020-2022 sans connaître la réponse. La courbe noire est la réalité.",
           "le modèle de dérive (vert), qui prolonge la tendance moyenne, est le plus proche de la réalité.", "400px"),
      bloc("Erreur moyenne de chaque modèle", "mae",
           "erreur absolue moyenne (MAE) sur 2020-2022, en milliards de m³ : plus la barre est courte, meilleur est le modèle.",
           "dérive : 0,75 milliard de m³ d'erreur, deux fois moins que les autres. C'est le modèle retenu.", "400px")),
    layout_columns(col_widths = c(6, 6),
      bloc("Prévision des prélèvements d'eau de la France jusqu'en 2035", "fr_prev",
           "points noirs = observé (2015-2022) ; pointillés = prévision du modèle de dérive ; zone bleue = intervalle de confiance à 95 %.",
           textOutput("prev_retenir", inline = TRUE), "380px"),
      bloc("Eau des data centers français : 2024, 2030, 2035", "fr_scen",
           "barres = eau des data centers selon la croissance (coefficient choisi à gauche) ; rouge = part attribuée à l'IA ; étiquette = part de l'eau prélevée prévue.",
           textOutput("frscen_retenir", inline = TRUE), "380px")),
    bloc("Sensibilité : eau des data centers du monde en 2035 selon les deux hypothèses principales", "sensib",
         "chaque case croise un coefficient d'eau (ligne) et un scénario de croissance (colonne). Valeurs en millions de m³.",
         "même dans le cas le plus fort, l'eau des data centers reste de l'ordre de 0,2 % de l'eau prélevée par les 30 pays les plus équipés.", "300px")
  ),
  nav_panel("3. Hier et demain", icon = bs_icon("graph-up-arrow"),
    layout_columns(col_widths = c(6, 6),
      bloc("Hier (2015-2022) : électricité et eau prélevée dans les 30 pays les plus équipés", "hist",
           "base 100 en 2015 : une valeur de 120 veut dire +20 % par rapport à 2015. Tous usages confondus.",
           "l'électricité augmente (+21 % en 7 ans) alors que l'eau prélevée reste stable : la demande des data centers s'ajoute à celle des autres usages.", "360px"),
      bloc("Demain (2024-2035) : électricité mondiale des data centers selon l'AIE", "traj",
           "trait noir = mesures 2020-2024 ; pointillés = les 4 scénarios de l'Agence internationale de l'énergie.",
           "d'ici 2035, la consommation des data centers serait multipliée par 1,7 à 4,1.", "360px")
    ),
    layout_columns(col_widths = c(7, 5),
      bloc("Les 10 pays les plus exposés : part de leur eau utilisée par les data centers", "top10_fut",
           "points = 2024, 2030 et 2035 (croissance choisie à gauche) ; barre rose = écart entre croissance lente et rapide en 2035.",
           textOutput("fut_retenir", inline = TRUE), "380px"),
      card(card_header("Un pays peut-il basculer dans une catégorie de stress plus grave ?"), tableOutput("tab_cat"),
           lecture("pour chaque scénario, nombre de pays dont la catégorie de stress change à cause de la hausse de l'eau des data centers.",
                   "avec 0,81 L/kWh, aucun pays ne change de catégorie, même en 2035. Avec 2,41 L/kWh, Singapour (déjà à 74 %) peut franchir le seuil de 75 %."))
    )
  ),

  # ---- 7. Profils de pays
  nav_panel("4. Profils de pays (ACP)", icon = bs_icon("diagram-3"),
    layout_columns(col_widths = c(6, 6),
      bloc("Carte des 4 familles de pays", "carte_classes",
           "les 61 pays qui ont au moins 10 data centers sont regroupés en 4 familles qui se ressemblent (taille, eau, richesse, électricité).",
           "les pays où l'eau des data centers pèse le plus (Luxembourg, Irlande, Singapour) sont tous de « petits pays riches ».", "400px", leafletOutput),
      bloc("Les pays placés selon 2 grands critères", "acp_ind",
           "à droite = grands pays (beaucoup d'habitants, d'électricité, d'eau prélevée) ; en haut = pays riches avec beaucoup d'eau par habitant ; en bas = pays secs. Méthode : analyse en composantes principales (ACP).",
           "les pays très secs (Arabie saoudite, Émirats) forment une famille à part, mais ils ont encore peu de data centers.", "400px")
    ),
    card(card_header("Portrait de chaque famille (valeurs médianes)"), DTOutput("tab_classes"))
  )),

  # ---- 8. Données et méthode
  nav_panel("Données et méthode", icon = bs_icon("info-circle"),
    navset_card_underline(
      nav_panel("Tableau par pays",
        div(class = "d-flex justify-content-end mb-2", downloadButton("dl", "Télécharger (CSV)", class = "btn-sm")),
        DTOutput("tab_pays")),
      nav_panel("Méthode",
        h5("Notre question"),
        p("Les ressources en eau permettront-elles de répondre à la demande des data centers (dédiés à l'IA ou non) en plus des autres usages, aujourd'hui et demain ?"),
        h5("Calcul en 4 étapes"),
        tags$ol(
          tags$li("Électricité des data centers en 2024 : total mondial de l'AIE (415 TWh), réparti entre États-Unis, Chine, Europe et reste du monde, puis entre pays selon leur nombre de sites."),
          tags$li("Eau = électricité × litres d'eau par kWh (0,21 / 0,81 / 2,41)."),
          tags$li("Comparaison avec l'eau prélevée par le pays (moyenne 2015-2022), avec l'eau des habitants et avec le stress hydrique, au niveau du pays et de la région."),
          tags$li("Projection 2030 et 2035 avec les scénarios de l'AIE.")),
        h5("Équivalents utilisés (ordres de grandeur)"),
        tags$ul(
          tags$li("1 piscine olympique = 2 500 m³."),
          tags$li("1 habitant = 65 m³ d'eau par an (ratio Arcep : 6,5 millions de m³ = 100 000 personnes)."),
          tags$li("1 foyer français ≈ 4,5 MWh d'électricité par an."),
          tags$li("1 km en voiture thermique ≈ 0,2 kg de CO₂."),
          tags$li("1 MW de puissance utilisée toute l'année = 8 760 MWh.")),
        h5("Limites"),
        tags$ul(
          tags$li("L'électricité est répartie selon le nombre de sites, pas selon leur taille."),
          tags$li("Un seul coefficient d'eau pour tous les pays, d'où la fourchette 0,21 à 2,41 L/kWh."),
          tags$li("Les sites OpenStreetMap ne sont pas tous les data centers : l'inventaire est partiel."),
          tags$li("Les saisons sont décrites par le climat ; nous n'avons pas la consommation d'eau mois par mois."),
          tags$li("La part de l'IA est une hypothèse de l'AIE, pas une mesure."))),
      nav_panel("Sources et dates",
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("Donnée"), tags$th("Source"), tags$th("Période"))),
          tags$tbody(
            tags$tr(tags$td("Nombre de data centers par pays"), tags$td("Inventaire data-centres-worldwide"), tags$td("relevé 2024")),
            tags$tr(tags$td("Électricité des data centers, scénarios"), tags$td("AIE, Energy and AI (2025)"), tags$td("2020-2024, 2030, 2035")),
            tags$tr(tags$td("Eau prélevée, stress hydrique, PIB, population"), tags$td("FAO AQUASTAT, Banque mondiale"), tags$td("moyenne 2015-2022")),
            tags$tr(tags$td("Eau des habitants, de l'agriculture, de l'industrie"), tags$td("FAO AQUASTAT"), tags$td("dernière année connue, ≤ 2017")),
            tags$tr(tags$td("Électricité et CO₂ par kWh"), tags$td("Ember"), tags$td("moyenne 2019-2022")),
            tags$tr(tags$td("Sites géolocalisés"), tags$td("OpenStreetMap (ODbL)"), tags$td("relevé du 20/09/2026")),
            tags$tr(tags$td("Stress hydrique par région"), tags$td("WRI Aqueduct 4.0"), tags$td("référence 1979-2019")),
            tags$tr(tags$td("Pluie et température par mois"), tags$td("Banque mondiale CCKP (CRU TS 4.08)"), tags$td("normales 1991-2020")),
            tags$tr(tags$td("Eau consommée par site Google"), tags$td("Google, rapport environnemental 2026"), tags$td("année 2025")),
            tags$tr(tags$td("Data centers en France"), tags$td("Arcep, ADEME, RTE"), tags$td("2021-2024, projections 2030")))),
        p(class = "text-muted small", "SAÉ 5.EMS.01 - Ayi AMAVIGAN, Ada GADJI, Aboubakar COULIBALY - BUT3 Science des données,",
          "IUT de Carcassonne, 2026-2027. Professeure : Mme Viguier-Pla.")))
  ),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "SAÉ 5.EMS.01 - IUT de Carcassonne"))
)

# --------------------------------------------------------------- Serveur -----
server <- function(input, output, session) {
  observeEvent(input$entrer, nav_select("nav", "L'essentiel"))
  observeEvent(input$nav, toggle_sidebar("sb", open = input$nav != "Accueil"), ignoreInit = FALSE)
  w  <- reactive(if (input$coef == "perso") input$coef_perso else wue[[input$coef]])
  hz <- reactive(as.integer(input$horizon))
  sc <- reactive(if (hz() == 2024) "central" else input$scen)
  res <- reactive(calculer(w(), sc(), hz()))
  pres <- reactive(res() |> filter(!is.na(prelev0)))
  quand <- reactive(if (hz() == 2024) "en 2024" else paste0("en ", hz(), " (croissance ", c(bas = "lente", central = "centrale", haut = "rapide")[[sc()]], ")"))

  # ---------- L'essentiel
  output$messages <- renderUI({
    r <- res(); p <- pres()
    lux <- p |> filter(n_dc >= 5) |> slice_max(part, n = 1)
    fr <- r |> filter(iso3 == "FRA")
    ech <- d$echelles |> filter(classe %in% d$lab_aq[4:5]) |> group_by(echelle) |> summarise(p = sum(part))
    msg <- function(ch, txt) div(class = "message", span(class = "chiffre", ch), br(), txt)
    tagList(
      msg(paste(fmt(sum(r$eau), 0), "millions de m³"),
          paste0("d'eau utilisée par les data centers du monde ", quand(), ", soit environ ",
                 grand(sum(r$eau) * 1e6 / PISCINE), " piscines olympiques. C'est de l'ordre de 0,01 % de l'eau prélevée par les 30 pays les plus équipés.")),
      msg(paste0(lux$country, " : ", pct(lux$part)),
          "le pays où les data centers pèsent le plus dans l'eau prélevée. Dans 9 pays sur 10, c'est moins de 0,1 %."),
      msg(paste0(fmt(ech$p[ech$echelle == "Pays entier"], 0), " % → ", fmt(ech$p[ech$echelle == "Région du site"], 0), " %"),
          "des sites sont en manque d'eau élevé si l'on regarde le pays entier, puis la région du site. La moyenne nationale cache des régions sèches."),
      msg(paste("France :", fmt(fr$eau, 1), "millions de m³"),
          paste0(quand(), ", soit l'eau d'environ ", grand(fr$eau * 1e6 / HAB_M3),
                 " habitants. La moitié des sites français sont en Île-de-France.")),
      msg("En été", "il fait plus chaud et il pleut souvent moins : c'est quand les data centers ont le plus besoin d'eau que la ressource est la plus rare."))
  })

  # ---------- Carte du monde
  bins <- list(part = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1, Inf), eau = c(0, 0.1, 1, 5, 20, 50, Inf),
               ratio_menages = c(0, 0.1, 0.5, 1, 2, 5, Inf), n_dc = c(0, 5, 20, 50, 100, 300, Inf),
               stress0 = c(0, 25, 50, 75, 100, Inf), pct_sites_region_stress = c(0, 10, 25, 50, 75, 100.01),
               co2_kt = c(0, 10, 100, 500, 1000, 5000, Inf))
  unite <- c(part = "% de l'eau prélevée", eau = "millions de m³", ratio_menages = "% de l'eau des habitants",
             n_dc = "data centers", stress0 = "% (stress hydrique)", pct_sites_region_stress = "% des sites",
             co2_kt = "milliers de t de CO₂")
  pal_v <- c(part = "YlGnBu", eau = "Blues", ratio_menages = "PuRd", n_dc = "Purples", stress0 = "YlOrRd",
             pct_sites_region_stress = "OrRd", co2_kt = "Greys")
  output$carte <- renderLeaflet({
    v <- input$var_carte
    m <- d$monde |> left_join(res() |> mutate(val = .data[[v]]) |> select(iso3, country, val), by = "iso3")
    pal <- colorBin(pal_v[[v]], domain = m$val, bins = bins[[v]], na.color = "#e8e8e8")
    lab <- sprintf("<b>%s</b><br>%s : %s", ifelse(is.na(m$country), "Pas de data center recensé", m$country),
                   unite[[v]], ifelse(is.na(m$val), "n.d.", fmt(m$val, ifelse(v %in% c("n_dc"), 0, 2)))) |>
      lapply(htmltools::HTML)
    l <- leaflet(m, options = leafletOptions(worldCopyJump = TRUE, minZoom = 1, zoomSnap = 0.25)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(layerId = m$iso3, fillColor = pal(m$val), fillOpacity = 0.85, color = "white", weight = 0.6,
                  label = lab, highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)) |>
      addLegend("bottomleft", pal = pal, values = m$val, title = unite[[v]], opacity = 0.9, na.label = "pas de donnée",
                labFormat = function(type, cuts, p) { k <- formatC(cuts, format = "fg", digits = 3, big.mark = " ", decimal.mark = ",")
                  paste0(head(k, -1), " – ", ifelse(is.infinite(tail(cuts, -1)), "et plus", tail(k, -1))) }) |>
      setView(5, 25, 1.75)
    if (isTRUE(input$pts)) l <- l |> addCircleMarkers(data = d$osm, lng = ~lon, lat = ~lat, radius = 2, stroke = FALSE,
                                                      fillColor = "#c0392b", fillOpacity = 0.6, label = ~nom)
    l
  })
  observeEvent(input$carte_shape_click, {
    id <- input$carte_shape_click$id
    if (!is.null(id) && id %in% d$pays$iso3) updateSelectInput(session, "pays_sel", selected = id)
  })
  output$carte_comment <- renderText(switch(input$var_carte,
    part = "plus la couleur est foncée, plus les data centers utilisent une grande part de l'eau prélevée dans le pays (tous usages).",
    eau = "volume d'eau utilisé par les data centers du pays, en millions de m³ (1 million de m³ = 400 piscines olympiques).",
    ratio_menages = "eau des data centers comparée à l'eau distribuée aux habitants (eau du robinet). 1 % = les data centers utilisent 1 litre quand les habitants en utilisent 100.",
    n_dc = "nombre de data centers recensés dans le pays (inventaire 2024).",
    stress0 = "part de l'eau disponible que le pays prélève déjà. Au-dessus de 75 % : stress élevé ; au-dessus de 100 % : le pays prélève plus que ce qui se renouvelle.",
    pct_sites_region_stress = "part des data centers du pays situés dans une région où le manque d'eau est élevé ou extrême (Aqueduct > 40 %).",
    co2_kt = "CO₂ émis pour produire l'électricité des data centers ; dépend du mix électrique (charbon, gaz, nucléaire, renouvelables)."))
  output$carte_retenir <- renderText({
    p <- pres() |> filter(n_dc >= 5); v <- input$var_carte
    if (v %in% c("stress0", "pct_sites_region_stress", "n_dc")) {
      t <- p |> filter(!is.na(.data[[v]])) |> slice_max(.data[[v]], n = 3, with_ties = FALSE)
      paste0("exemples de valeurs les plus fortes (pays d'au moins 5 sites) : ", paste0(t$country, " (", fmt(t[[v]], 0), ")", collapse = ", "), ".")
    } else {
      t <- p |> filter(!is.na(.data[[v]])) |> slice_max(.data[[v]], n = 3, with_ties = FALSE)
      paste0(quand(), ", les valeurs les plus fortes sont : ", paste0(t$country, " (", fmt(t[[v]], ifelse(max(t[[v]]) > 100, 0, 2)), ")", collapse = ", "), ".")
    }
  })

  output$fiche <- renderUI({
    r <- res() |> filter(iso3 == input$pays_sel)
    if (nrow(r) == 0) return(p("Pas de données."))
    cl <- d$climat |> filter(iso3 == r$iso3, sensible)
    cls <- d$classes |> filter(iso3 == r$iso3)
    reg <- d$regions |> filter(iso3 == r$iso3) |> slice_max(sites, n = 1, with_ties = FALSE)
    ligne <- function(t, v, e = NULL) tagList(tags$dt(t), tags$dd(v, if (!is.null(e)) div(class = "equiv", e)))
    verdict <- if (is.na(r$part)) "Données d'eau manquantes pour ce pays." else
      if (r$part < 0.1) "L'eau des data centers reste très faible face aux autres usages du pays." else
      if (r$part < 1) "L'eau des data centers est faible, mais visible : à suivre si les sites se multiplient." else
      "L'eau des data centers pèse nettement : point de vigilance."
    card_body(class = "fiche",
      h4(r$country), p(class = "text-muted small", paste("Réglages :", quand(), "-", fmt(w(), 2), "L/kWh")),
      div(class = "retenir mb-3", verdict),
      tags$dl(
        ligne("Data centers recensés", r$n_dc, if (nrow(reg)) paste0("dont ", reg$sites, " sites OSM en ", reg$region)),
        ligne("Électricité des data centers", paste(fmt(r$e_h, 2), "TWh"),
              paste0(pct(r$part_elec), " de l'électricité du pays ≈ ", grand(r$e_h * 1e6 / FOYER_MWH), " foyers")),
        ligne("Eau utilisée", paste(fmt(r$eau, 2), "millions de m³"),
              paste0("≈ ", grand(r$eau * 1e6 / PISCINE), " piscines olympiques ≈ eau de ", grand(r$eau * 1e6 / HAB_M3), " habitants")),
        ligne("Part de l'eau prélevée du pays", pct(r$part), paste0("fourchette ", pct(r$part_bas), " à ", pct(r$part_haut))),
        ligne("Face à l'eau des habitants", pct(r$ratio_menages), if (!is.na(r$annee_secteurs)) paste("eau des habitants : AQUASTAT", r$annee_secteurs)),
        ligne("Stress hydrique du pays", if (is.na(r$stress0)) "n.d." else paste0(fmt(r$stress0, 0), " % (", r$cat0, ")")),
        ligne("Sites en région qui manque d'eau", pct(r$pct_sites_region_stress), if (!is.na(r$sites_osm)) paste(r$sites_osm, "sites géolocalisés")),
        ligne("Mois chauds et secs", if (nrow(cl)) paste(mois_fr[cl$mois], collapse = ", ") else "aucun"),
        ligne("CO₂ de leur électricité", if (is.na(r$co2_kt)) "n.d." else paste(fmt(r$co2_kt, 0), "milliers de tonnes"),
              if (!is.na(r$co2_kt)) paste0("≈ ", grand(r$co2_kt * 1e6 / KM_KG), " km en voiture (", fmt(r$gco2_kwh, 0), " g de CO₂ par kWh)")),
        if (nrow(cls)) ligne("Famille de pays", cls$nom_classe)))
  })

  # ---------- Dans les régions
  osm_sel <- reactive(if (input$pays_reg == "MONDE") d$osm else d$osm |> filter(iso3 == input$pays_reg))
  output$carte_reg <- renderLeaflet({
    o <- osm_sel() |> filter(!is.na(stress_region))
    pal <- colorFactor(col_aq, levels = d$lab_aq)
    leaflet(o) |> addTiles() |>
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = if (input$pays_reg == "MONDE") 3 else 5,
                       stroke = TRUE, weight = 0.5, color = "#333", fillColor = ~pal(stress_region), fillOpacity = 0.85,
                       label = ~lapply(paste0("<b>", nom, "</b><br>", region, " (", pays, ")<br>Manque d'eau de la région : ", stress_region), htmltools::HTML)) |>
      addLegend("bottomleft", colors = col_aq, labels = d$lab_aq, title = "Stress de la région", opacity = 0.9) |>
      fitBounds(min(o$lon), min(o$lat), max(o$lon), max(o$lat))
  })
  output$tab_reg <- renderDT({
    t <- (if (input$pays_reg == "MONDE") d$regions else d$regions |> filter(iso3 == input$pays_reg)) |>
      arrange(-sites) |> transmute(Région = region, Pays = pays, Sites = sites, `Manque d'eau` = stress_region)
    datatable(t, rownames = FALSE, options = list(pageLength = 12, dom = "tp", scrollX = TRUE)) |>
      formatStyle("Manque d'eau", backgroundColor = styleEqual(d$lab_aq, paste0(col_aq, "66")))
  })
  output$reg_retenir <- renderText({
    o <- osm_sel() |> filter(!is.na(stress_region))
    if (nrow(o) == 0) return("pas de site géolocalisé pour ce pays.")
    p <- 100 * mean(o$stress_region %in% d$lab_aq[4:5])
    top <- o |> count(region, stress_region, sort = TRUE) |> slice(1)
    paste0(fmt(p, 0), " % des ", nrow(o), " sites sont dans une région où le manque d'eau est élevé ou extrême. ",
           "La région qui en compte le plus est ", top$region, " (", top$n, " sites, stress ", tolower(top$stress_region), ").")
  })
  # ---------- Au fil de l'année
  output$clim <- renderPlotly({
    c <- d$climat |> filter(iso3 == input$pays_clim) |> arrange(mois) |> mutate(m = mois_fr[mois])
    if (nrow(c) == 0) return(plotly_empty())
    sh <- lapply(c$mois[c$sensible], function(i) list(type = "rect", xref = "x", yref = "paper", x0 = i - 0.5, x1 = i + 0.5,
                                                       y0 = 0, y1 = 1, fillcolor = "#f0ad4e", opacity = 0.25, line = list(width = 0), layer = "below"))
    plot_ly(c, x = ~mois) |>
      add_bars(y = ~pr, name = "Pluie (mm)", marker = list(color = "#6baed6"),
               hovertext = ~paste0(m, " : ", fmt(pr, 0), " mm"), hoverinfo = "text") |>
      add_lines(y = ~tas, name = "Température (°C)", yaxis = "y2", line = list(color = "#d62728", width = 3),
                hovertext = ~paste0(m, " : ", fmt(tas, 1), " °C"), hoverinfo = "text") |>
      layout(shapes = sh, legend = LEG, margin = list(r = 70),
             xaxis = list(title = "", tickvals = 1:12, ticktext = mois_fr),
             yaxis = list(title = "Pluie (mm par mois)", rangemode = "tozero"),
             yaxis2 = list(title = "Température (°C)", overlaying = "y", side = "right", showgrid = FALSE))
  })
  output$clim_retenir <- renderText({
    c <- d$climat |> filter(iso3 == input$pays_clim)
    n <- d$pays$country[d$pays$iso3 == input$pays_clim]
    if (!any(c$sensible)) return(paste0(n, " : aucun mois n'est à la fois plus chaud et plus sec que la moyenne."))
    paste0(n, " : mois chauds et secs = ", paste(mois_fr[c$mois[c$sensible]], collapse = ", "),
           ". Écart de pluie entre le mois le plus humide et le plus sec : ", fmt(max(c$pr), 0), " mm contre ", fmt(min(c$pr), 0), " mm.")
  })
  output$carte_mois <- renderLeaflet({
    nm <- d$climat |> group_by(iso3) |> summarise(n = sum(sensible))
    m <- d$monde |> left_join(nm, by = "iso3") |> left_join(d$pays |> select(iso3, country, n_dc), by = "iso3")
    pal <- colorBin("YlOrBr", domain = 0:6, bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 6.5), na.color = "#e8e8e8")
    leaflet(m) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(fillColor = pal(m$n), fillOpacity = 0.85, color = "white", weight = 0.5,
                  label = paste0(coalesce(m$country, m$iso3), " : ", m$n, " mois chauds et secs", ifelse(is.na(m$n_dc), "", paste0(" - ", m$n_dc, " data centers")))) |>
      addLegend("bottomleft", pal = pal, values = m$n, title = "Mois chauds et secs", labFormat = function(type, cuts, p) c("0", "1", "2", "3", "4", "5 ou +")) |>
      setView(10, 25, 1)
  })

  # ---------- Arbitrages
  arb <- reactive(pres() |> filter(n_dc >= 10) |>
    mutate(s1 = !is.na(ratio_menages) & ratio_menages >= 1, s2 = !is.na(pct_sites_region_stress) & pct_sites_region_stress >= 25,
           s3 = !is.na(stress0) & stress0 >= 50, signaux = s1 + s2 + s3))
  output$arb <- renderPlotly({
    a <- arb() |> filter(!is.na(ratio_menages), !is.na(pct_sites_region_stress), ratio_menages > 0)
    plot_ly(a, x = ~pct_sites_region_stress, y = ~ratio_menages,
            color = ~factor(signaux, 0:3, c("0 signal", "1 signal", "2 signaux", "3 signaux")),
            colors = c("#9ecae1", "#fdd49e", "#fc8d59", "#b30000"), type = "scatter", mode = "markers+text",
            text = ~ifelse(signaux >= 2 | n_dc >= 150, country, ""), textposition = "top center", textfont = list(size = 10),
            marker = list(size = ~pmin(55, 8 + 1.6 * sqrt(n_dc)), opacity = 0.75, line = list(color = "#555", width = 0.5)),
            hovertext = ~paste0("<b>", country, "</b><br>Sites en région stressée : ", fmt(pct_sites_region_stress, 0),
                                " %<br>Eau des DC / eau des habitants : ", pct(ratio_menages), "<br>Data centers : ", n_dc),
            hoverinfo = "text") |>
      layout(xaxis = list(title = "% des sites dans une région en manque d'eau élevé", range = c(-5, 105)),
             yaxis = list(title = "Eau des DC / eau des habitants (%, échelle log)", type = "log", range = c(-3.3, 1)),
             shapes = list(list(type = "line", x0 = 25, x1 = 25, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "grey")),
                           list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 1, y1 = 1, line = list(dash = "dot", color = "grey")),
                           list(type = "rect", x0 = 25, x1 = 105, y0 = 1, y1 = 10, fillcolor = "#b30000", opacity = 0.05, line = list(width = 0))),
             annotations = list(list(x = 100, y = 0.55, xanchor = "right", text = "zone d'arbitrage", showarrow = FALSE,
                                     font = list(color = "#b30000", size = 12), yref = "y", yanchor = "bottom")),
             legend = LEG)
  })
  output$tab_arb <- renderDT({
    t <- arb() |> filter(signaux >= 2) |> arrange(-signaux, -ratio_menages) |>
      transmute(Pays = country, Signaux = signaux, `DC / eau habitants` = pct(ratio_menages),
                `Sites en région stressée` = paste0(fmt(pct_sites_region_stress, 0), " %"), `Stress pays` = paste0(fmt(stress0, 0), " %"))
    datatable(t, rownames = FALSE, options = list(dom = "tp", pageLength = 10, scrollX = TRUE))
  })
  output$arb_retenir <- renderText({
    t <- arb() |> filter(signaux >= 2) |> arrange(-signaux, -ratio_menages)
    paste0(nrow(t), " pays cumulent au moins 2 signaux ", quand(), " : ", paste(head(t$country, 6), collapse = ", "),
           ". Ailleurs, l'eau des data centers reste petite face à l'eau des habitants ; mais localement, un site peut entrer en concurrence avec l'eau potable ou l'agriculture.")
  })
  output$carte_google <- renderLeaflet({
    g <- d$google |> mutate(stress_region = coalesce(as.character(stress_region), "Non renseigné"))
    pal <- colorFactor(c(col_aq, "Non renseigné" = "#999999"), levels = c(d$lab_aq, "Non renseigné"))
    leaflet(g) |> addTiles() |>
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~pmax(4, sqrt(eau_m3 / 1e4) * 1.6), fillColor = ~pal(stress_region),
                       fillOpacity = 0.8, color = "#333", weight = 0.8,
                       label = ~lapply(paste0("<b>", site, "</b><br>Eau consommée en 2025 : ", fmt(eau_m3 / 1e6, 2),
                                              " million de m³<br>≈ ", grand(eau_m3 / HAB_M3), " habitants<br>Région : ",
                                              coalesce(region, "?"), " - ", stress_region), htmltools::HTML)) |>
      addLegend("bottomleft", colors = c(col_aq[1:5], "#999999"), labels = c(d$lab_aq[1:5], "Non renseigné"),
                title = "Stress de la région", opacity = 0.9) |>
      setView(-40, 40, 2)
  })
  output$bar_google <- renderPlotly({
    g <- d$google |> slice_max(eau_m3, n = 15) |> arrange(eau_m3) |> mutate(site = factor(site, site),
         stress_region = coalesce(as.character(stress_region), "Non renseigné"))
    plot_ly(g, y = ~site, x = ~eau_m3 / HAB_M3, type = "bar", orientation = "h", color = ~stress_region,
            colors = c(col_aq, "Non renseigné" = "#999999"),
            hovertext = ~paste0(site, " : ", fmt(eau_m3 / 1e6, 2), " million de m³ ≈ ", grand(eau_m3 / HAB_M3), " habitants"),
            hoverinfo = "text") |>
      layout(xaxis = list(title = "Équivalent en nombre d'habitants"), yaxis = list(title = ""), legend = LEG)
  })

  # ---------- Hier et demain
  output$hist <- renderPlotly(plot_ly(d$hist, x = ~year) |>
    add_lines(y = ~elec_ind, name = "Électricité produite", line = list(color = "darkorange", width = 3)) |>
    add_lines(y = ~eau_ind, name = "Eau prélevée", line = list(color = "steelblue", width = 3)) |>
    layout(xaxis = list(title = "", dtick = 1), yaxis = list(title = "Base 100 en 2015"), legend = LEG))
  output$traj <- renderPlotly({
    h <- d$aie |> filter(cas == "Historique")
    p <- plot_ly() |> add_trace(data = h, x = ~annee, y = ~twh, type = "scatter", mode = "lines+markers", name = "Mesuré",
                                line = list(color = "#333", width = 3), marker = list(color = "#333"))
    cols <- c(Headwinds = "#1b9e77", `High Efficiency` = "#377eb8", Base = "#ff7f00", `Lift-Off` = "#e41a1c")
    noms <- c(Headwinds = "Lente", `High Efficiency` = "Efficace", Base = "Centrale", `Lift-Off` = "Rapide")
    for (cs in names(cols)) {
      t <- bind_rows(tibble(annee = 2024, twh = 416), d$aie |> filter(cas == cs) |> select(annee, twh))
      p <- p |> add_trace(data = t, x = ~annee, y = ~twh, type = "scatter", mode = "lines+markers", name = noms[[cs]],
                          line = list(color = cols[[cs]], dash = "dash"), marker = list(color = cols[[cs]]))
    }
    p |> layout(xaxis = list(title = ""), yaxis = list(title = "Électricité des data centers (TWh)"), legend = LEG)
  })
  top10 <- reactive({
    s <- sc()
    top <- calculer(w(), s, 2035) |> filter(!is.na(prelev0), n_dc >= 20) |> slice_max(part, n = 10) |> pull(iso3)
    pts <- bind_rows(lapply(c(2024, 2030, 2035), function(h) calculer(w(), s, h) |> filter(iso3 %in% top) |> mutate(h = factor(h))))
    band <- bind_rows(lapply(c("bas", "haut"), function(x) calculer(w(), x, 2035) |> filter(iso3 %in% top) |> transmute(country, x, part))) |>
      pivot_wider(names_from = x, values_from = part)
    list(pts = pts, band = band)
  })
  output$top10_fut <- renderPlotly({
    pts <- top10()$pts; band <- top10()$band
    ord <- pts |> filter(h == "2035") |> arrange(part) |> pull(country)
    pts$country <- factor(pts$country, ord); band$country <- factor(band$country, ord)
    plot_ly() |>
      add_segments(data = band, x = ~bas, xend = ~haut, y = ~country, yend = ~country,
                   line = list(color = "rgba(178,34,34,0.25)", width = 10), name = "Écart lente / rapide (2035)", hoverinfo = "none") |>
      add_markers(data = pts, x = ~part, y = ~country, color = ~h, colors = c("#9ecae1", "#3182bd", "#08519c"),
                  marker = list(size = 10), text = ~paste0(country, " ", h, " : ", pct(part)), hoverinfo = "text") |>
      layout(xaxis = list(title = "% de l'eau prélevée par le pays"), yaxis = list(title = ""), legend = LEG)
  })
  output$fut_retenir <- renderText({
    p <- top10()$pts |> filter(h == "2035") |> slice_max(part, n = 1)
    paste0("en 2035, le pays le plus exposé reste ", p$country, " (", pct(p$part), "). Partout ailleurs, la part reste faible à l'échelle du pays.")
  })
  output$tab_cat <- renderTable({
    nm <- c(bas = "Lente", central = "Centrale", haut = "Rapide")
    bind_rows(lapply(c("bas", "central", "haut"), function(s) bind_rows(lapply(c(2030, 2035), function(h)
      calculer(w(), s, h) |> filter(!is.na(stress0)) |>
        summarise(Croissance = nm[[s]], Année = as.character(h), `Pays étudiés` = n(),
                  `Changent de catégorie` = sum(cat_h != cat0, na.rm = TRUE),
                  `Hausse max (points)` = fmt(max(stress_h - stress0, na.rm = TRUE), 2),
                  `Pays le plus touché` = country[which.max(stress_h - stress0)])))))
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---------- Profils de pays
  output$carte_classes <- renderLeaflet({
    m <- d$monde |> left_join(d$classes |> select(iso3, country, nom_classe), by = "iso3")
    pal <- colorFactor(col_classes, levels = names(col_classes), na.color = "#e8e8e8")
    leaflet(m) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(fillColor = pal(m$nom_classe), fillOpacity = 0.85, color = "white", weight = 0.5,
                  label = ifelse(is.na(m$nom_classe), "Non classé (moins de 10 data centers)", paste0(m$country, " : ", m$nom_classe))) |>
      addLegend("bottomleft", colors = col_classes, labels = names(col_classes), title = "Famille", opacity = 0.9) |>
      setView(10, 25, 1)
  })
  output$acp_ind <- renderPlotly({
    plot_ly(d$classes, x = ~dim1, y = ~dim2, color = ~nom_classe, colors = col_classes, type = "scatter", mode = "markers+text",
            text = ~country, textposition = "top center", textfont = list(size = 9), marker = list(size = 10, opacity = 0.8),
            hovertext = ~paste0("<b>", country, "</b><br>", nom_classe, "<br>Data centers : ", n_dc,
                                "<br>Part de l'eau : ", pct(part_central)), hoverinfo = "text") |>
      layout(xaxis = list(title = "← petits pays          grands pays →", zeroline = TRUE),
             yaxis = list(title = "← pays secs          eau et richesse →", zeroline = TRUE),
             legend = list(orientation = "h", x = 0, y = -0.22))
  })
  output$tab_classes <- renderDT({
    t <- d$classes |> group_by(Famille = nom_classe) |>
      summarise(Pays = n(), `Data centers` = median(n_dc), `Habitants (M)` = round(median(population) / 1e6, 1),
                `PIB/hab ($)` = round(median(pib_hab)), `Stress (%)` = round(median(stress_hydrique)),
                `Part de l'eau (%)` = round(median(part_central), 3),
                Exemples = paste(head(country[order(-n_dc)], 4), collapse = ", "), .groups = "drop")
    datatable(t, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })


  # ---------- Analyse : méthode d'estimation
  output$etapes <- renderUI({
    fr <- d$pays |> filter(iso3 == "FRA")
    et <- function(n, titre, txt, val) div(class = "etape", span(class = "num", n), tags$b(titre),
                                           p(class = "small mb-0 mt-2", txt), div(class = "val", val))
    fl <- div(class = "fleche", bs_icon("arrow-right"))
    div(class = "etapes",
      et(1, "Total mondial", "Électricité des data centers en 2024 selon l'AIE, répartie entre 4 zones.", "415 TWh"),
      fl,
      et(2, "Par pays", "Dans chaque zone, partage selon le nombre de data centers (Irlande, Japon, Inde fixés sur l'AIE).",
         paste("France :", fmt(fr$e0, 1), "TWh")),
      fl,
      et(3, "En eau", "Eau = électricité × litres d'eau par kWh.", paste0(fmt(fr$e0, 1), " × ", fmt(w(), 2), " = ", fmt(fr$e0 * w(), 1), " Mm³")),
      fl,
      et(4, "Comparaison", "Face à l'eau prélevée par le pays et à son stress hydrique.",
         paste0(pct(100 * fr$e0 * w() / (fr$prelev0 * 1e3)), " de l'eau prélevée")),
      fl,
      et(5, "Projection", "Scénarios de l'AIE pour 2030 et 2035, appliqués à chaque zone.", "x1,7 à x4,1"))
  })
  output$sunburst <- renderPlotly({
    zn <- c(USA = "États-Unis", CHN = "Chine", EUR = "Europe", RDM = "Reste du monde")
    p <- d$pays |> mutate(zone_n = zn[zone])
    top <- p |> filter(zone %in% c("EUR", "RDM")) |> group_by(zone) |> slice_max(e0, n = 8, with_ties = FALSE) |> ungroup()
    autres <- p |> filter(zone %in% c("EUR", "RDM"), !iso3 %in% top$iso3) |> group_by(zone, zone_n) |>
      summarise(e0 = sum(e0), n = n(), .groups = "drop") |> mutate(country = paste0("Autres (", n, " pays)"))
    feuilles <- bind_rows(top |> select(zone, zone_n, country, e0), autres |> select(zone, zone_n, country, e0)) |>
      mutate(id = paste(zone, country), parent = zone)
    racines <- p |> group_by(zone, zone_n) |> summarise(e0 = sum(e0), .groups = "drop") |>
      mutate(id = zone, parent = "", country = zone_n)
    s <- bind_rows(racines, feuilles)
    plot_ly(s, ids = ~id, labels = ~country, parents = ~parent, values = ~e0, type = "sunburst", branchvalues = "total",
            hovertext = ~paste0(country, " : ", fmt(e0, 1), " TWh (", fmt(100 * e0 / 415, 1), " % du monde)"),
            hoverinfo = "text", insidetextorientation = "radial") |>
      layout(sunburstcolorway = c("#1f6f8b", "#d62728", "#3182bd", "#f0ad4e"), margin = list(t = 10, b = 10))
  })
  output$validation <- renderPlotly({
    v <- d$validation |> arrange(modele_pct) |> mutate(country = factor(country, country))
    plot_ly(v, y = ~country) |>
      add_bars(x = ~modele_pct, name = "Notre modèle", marker = list(color = "#1f6f8b"), orientation = "h",
               text = ~paste0(fmt(modele_pct, 1), " %"), textposition = "outside") |>
      add_bars(x = ~ref_pct, name = "Valeur publiée (AIE)", marker = list(color = "#bdbdbd"), orientation = "h",
               text = ~paste0(fmt(ref_pct, 1), " %"), textposition = "outside") |>
      layout(barmode = "group", xaxis = list(title = "% de l'électricité du pays (échelle log)", type = "log"),
             yaxis = list(title = ""), legend = LEG)
  })
  output$coefs <- renderPlotly({
    k <- tibble(nom = c("Bas : 0,21 L/kWh", "Moyen : 0,81 L/kWh", "Haut : 2,41 L/kWh"), w = unname(wue)) |>
      mutate(eau = 415 * w, nom = factor(nom, nom))
    plot_ly(k, x = ~nom, y = ~eau, type = "bar", marker = list(color = c("#9ecae1", "#1f6f8b", "#08306b")),
            text = ~paste0(fmt(eau, 0), " Mm³<br>≈ ", grand(eau * 1e6 / PISCINE), " piscines"), textposition = "outside",
            hoverinfo = "text", hovertext = ~paste0(nom, " : ", fmt(eau, 0), " millions de m³")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Millions de m³ (monde, 2024)", range = c(0, 1250)))
  })
  output$calage_fr <- renderPlotly({
    nm <- c(bas = "lente", central = "centrale", haut = "rapide")
    mod <- bind_rows(tibble(periode = "2024", source = "Notre modèle", twh = d$pays$e0[d$pays$iso3 == "FRA"]),
      bind_rows(lapply(names(nm), function(s) tibble(periode = "2030", source = paste("Notre modèle, croissance", nm[[s]]),
        twh = calculer(w(), s, 2030) |> filter(iso3 == "FRA") |> pull(e_h))))) |>
      mutate(type = "Notre modèle", mini = NA_real_, maxi = NA_real_)
    r <- bind_rows(d$refs_fr |> mutate(type = "Source française"), mod) |> arrange(desc(periode), twh) |>
      mutate(lab = paste0(periode, " · ", source), lab = factor(lab, lab),
             e_plus = ifelse(is.na(maxi), 0, maxi - twh), e_moins = ifelse(is.na(mini), 0, twh - mini))
    plot_ly(r, y = ~lab, x = ~twh, color = ~type, colors = c("Notre modèle" = "#e67e22", "Source française" = "#1f6f8b"),
            type = "bar", orientation = "h", cliponaxis = FALSE,
            text = ~ifelse(is.na(maxi), paste(fmt(twh, 1), "TWh"), paste0(fmt(twh, 1), " TWh (", fmt(mini, 0), " à ", fmt(maxi, 0), ")")),
            textposition = "outside") |>
      layout(xaxis = list(title = "TWh par an", range = c(0, 30)), yaxis = list(title = ""), legend = LEG)
  })

  # ---------- Analyse : modèle et prévisions
  output$backtest <- renderPlotly({
    o <- d$prev_fr$obs; b <- d$backtest
    cols <- c("Naïf" = "#7f7f7f", "Dérive" = "#2ca02c", "ETS" = "#9467bd", "ARIMA" = "#ff7f0e")
    p <- plot_ly() |>
      add_trace(data = o, x = ~annee, y = ~valeur, type = "scatter", mode = "lines+markers", name = "Réalité",
                line = list(color = "#111", width = 3), marker = list(size = 8, color = "#111"))
    for (m in names(cols)) {
      bm <- bind_rows(o |> filter(annee == 2019) |> transmute(annee, pred = valeur), b |> filter(modele == m) |> select(annee, pred))
      p <- p |> add_trace(data = bm, x = ~annee, y = ~pred, type = "scatter", mode = "lines+markers", name = m,
                          line = list(color = cols[[m]], dash = "dash", width = if (m == "Dérive") 3.5 else 2),
                          marker = list(color = cols[[m]]))
    }
    p |> layout(shapes = list(list(type = "rect", x0 = 2014.6, x1 = 2019.5, y0 = 0, y1 = 1, yref = "paper",
                                   fillcolor = "#999", opacity = 0.12, line = list(width = 0))),
                annotations = list(list(x = 2017, y = 1.04, yref = "paper", text = "apprentissage", showarrow = FALSE),
                                   list(x = 2021, y = 1.04, yref = "paper", text = "test", showarrow = FALSE)),
                xaxis = list(title = "", dtick = 1), yaxis = list(title = "Eau prélevée en France (milliards de m³)"),
                legend = list(orientation = "h", x = 0, y = -0.12))
  })
  output$mae <- renderPlotly({
    s <- d$prev_fr$scores |> arrange(desc(MAE)) |> mutate(modele = factor(modele, modele))
    plot_ly(s, y = ~modele, x = ~MAE, type = "bar", orientation = "h",
            marker = list(color = ifelse(as.character(s$modele) == d$prev_fr$choix, "#2ca02c", "#c7c7c7")),
            text = ~paste(fmt(MAE, 2), "Mds m³"), textposition = "outside", hoverinfo = "text",
            hovertext = ~paste0(modele, " : erreur moyenne ", fmt(MAE, 2), " milliard de m³")) |>
      layout(xaxis = list(title = "Erreur absolue moyenne (milliards de m³)", range = c(0, 3)), yaxis = list(title = ""))
  })
  output$fr_prev <- renderPlotly({
    o <- d$prev_fr$obs; pv <- d$prev_fr$prev
    plot_ly() |>
      add_ribbons(data = pv, x = ~annee, ymin = ~basse, ymax = ~haute, name = "Intervalle à 95 %",
                  fillcolor = "rgba(31,111,139,0.15)", line = list(width = 0)) |>
      add_lines(data = pv, x = ~annee, y = ~prevision, name = "Prévision (dérive)",
                line = list(dash = "dash", color = "#1f6f8b", width = 3)) |>
      add_trace(data = o, x = ~annee, y = ~valeur, type = "scatter", mode = "lines+markers", name = "Observé",
                line = list(color = "#111"), marker = list(color = "#111")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Milliards de m³ par an", rangemode = "tozero"), legend = LEG)
  })
  output$prev_retenir <- renderText({
    pv <- d$prev_fr$prev |> filter(annee %in% c(2030, 2035))
    paste0("le modèle prolonge la baisse : ", fmt(pv$prevision[1], 1), " milliards de m³ en 2030 et ", fmt(pv$prevision[2], 1),
           " en 2035. L'incertitude est forte (de ", fmt(pv$basse[2], 1), " à ", fmt(pv$haute[2], 1),
           " en 2035) : 8 années d'observation ne suffisent pas pour prévoir 13 ans plus loin.")
  })
  fr_scen <- reactive({
    e0f <- d$pays$e0[d$pays$iso3 == "FRA"]
    prel <- c(`2024` = d$pays$prelev0[d$pays$iso3 == "FRA"],
              `2030` = d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2030],
              `2035` = d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2035])
    nm <- c(bas = "lente", central = "centrale", haut = "rapide")
    bind_rows(tibble(lab = "2024", e_h = e0f, h = "2024"),
      bind_rows(lapply(c(2030, 2035), function(h) bind_rows(lapply(names(nm), function(s)
        tibble(lab = paste0(h, " ", nm[[s]]), e_h = calculer(w(), s, h) |> filter(iso3 == "FRA") |> pull(e_h),
               h = as.character(h))))))) |>
      mutate(e_ia = d$part_ia[["fr"]] * e0f + d$part_ia[["hausse"]] * (e_h - e0f),
             eau = e_h * w(), eau_ia = e_ia * w(), part = 100 * eau / (prel[h] * 1e3), lab = factor(lab, unique(lab)))
  })
  output$fr_scen <- renderPlotly({
    f <- fr_scen()
    plot_ly(f, x = ~lab) |>
      add_bars(y = ~eau - eau_ia, name = "Data centers hors IA", marker = list(color = "#9ecae1")) |>
      add_bars(y = ~eau_ia, name = "Data centers IA", marker = list(color = "#d62728"),
               text = ~pct(part), textposition = "outside") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Millions de m³ d'eau"), legend = LEG)
  })
  output$frscen_retenir <- renderText({
    f <- fr_scen()
    paste0("l'eau des data centers français passe de ", fmt(f$eau[1], 1), " millions de m³ en 2024 à ",
           fmt(min(f$eau[f$h == "2035"]), 1), " - ", fmt(max(f$eau[f$h == "2035"]), 1), " en 2035, soit au plus ",
           pct(max(f$part)), " de l'eau prélevée en France. L'enjeu est local (Île-de-France), pas national.")
  })
  output$sensib <- renderPlotly({
    k <- c("Bas 0,21" = wue[["bas"]], "Moyen 0,81" = wue[["central"]], "Haut 2,41" = wue[["haut"]])
    sc_n <- c(bas = "Lente", central = "Centrale", haut = "Rapide")
    g <- expand.grid(coef = names(k), scen = names(sc_n), stringsAsFactors = FALSE) |> as_tibble()
    g$eau <- mapply(function(cf, sn) sum(calculer(k[[cf]], sn, 2035)$eau), g$coef, g$scen)
    g <- g |> mutate(scen = factor(sc_n[scen], sc_n), coef = factor(coef, names(k)))
    plot_ly(g, x = ~scen, y = ~coef, z = ~eau, type = "heatmap", colorscale = list(c(0, "#deebf7"), c(1, "#08519c")), showscale = FALSE,
            text = ~paste(fmt(eau, 0), "Mm³"), texttemplate = "%{text}", textfont = list(size = 15),
            hoverinfo = "text", hovertext = ~paste0(coef, " L/kWh · croissance ", scen, " : ", fmt(eau, 0), " millions de m³")) |>
      layout(xaxis = list(title = "Croissance des data centers d'ici 2035 (AIE)"), yaxis = list(title = "Litres d'eau par kWh"))
  })

  # ---------- Données
  tab <- reactive(res() |> transmute(Pays = country, Zone = zone, `Data centers` = n_dc,
    `Électricité DC (TWh)` = round(e_h, 3), `Eau DC (Mm³)` = round(eau, 3), `Part eau du pays (%)` = signif(part, 3),
    `DC / eau habitants (%)` = signif(ratio_menages, 3), `Stress pays (%)` = round(stress0, 1),
    `Sites en région stressée (%)` = round(pct_sites_region_stress, 1), `CO₂ (kt)` = round(co2_kt, 1)) |>
    arrange(desc(`Part eau du pays (%)`)))
  output$tab_pays <- renderDT(datatable(tab(), rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)))
  output$dl <- downloadHandler(filename = function() paste0("eau_datacenters_", hz(), "_", sc(), ".csv"),
                               content = function(f) write.csv(tab(), f, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
