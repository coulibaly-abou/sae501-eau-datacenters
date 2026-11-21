# =============================================================================
# SAÉ 5.EMS.01 - L'eau des data centers : tableau de bord interactif
# Ayi AMAVIGAN - Ada GADJI - Aboubakar COULIBALY | BUT3 Science des données, IUT de Carcassonne
# Tiré des rapports 1 et 2 du groupe. Données préparées par ../prep.R
# Principe : peu de texte, une idée par graphique, l'aide de lecture au survol de l'icône (i).
# =============================================================================
library(shiny); library(bslib); library(bsicons); library(plotly); library(DT)
library(dplyr); library(tidyr); library(leaflet); library(sf)

options(DT.options = list(language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json")))
# Graphiques sans barre d'outils
renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  force(env)
  if (!quoted) expr <- substitute(expr)
  plotly::renderPlotly(plotly::config(eval(expr, env), displayModeBar = FALSE, locale = "fr"), quoted = FALSE)
}

d <- readRDS("app_data.rds")
wue <- d$wue
CHOIX_PAYS <- c("Monde entier" = "MONDE", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)])

# ------------------------------------------------------------- Constantes ----
PISCINE <- 2500          # m3, bassin olympique
HAB_M3 <- d$conso_hab    # 65 m3 par habitant et par an (ratio Arcep)
mois_fr <- c("janv.", "févr.", "mars", "avril", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc.")
cat_lab <- c("aucun", "faible", "moyen", "élevé", "critique")
categoriser <- function(s) cut(s, c(-Inf, 25, 50, 75, 100, Inf), labels = cat_lab)
col_aq <- setNames(c("#1a9850", "#a6d96a", "#fee08b", "#f46d43", "#a50026", "#8c6d31"), d$lab_aq)
col_classes <- c("Petits pays riches" = "#1b9e77", "Émergents, revenus plus faibles" = "#d95f02",
                 "Pays très secs" = "#e7298a", "Grands pays équipés" = "#7570b3")

fmt <- function(x, k = 1) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = k, big.mark = " ", decimal.mark = ","))
pct <- function(x) ifelse(is.na(x), "n.d.", paste0(trimws(formatC(x, format = "fg", digits = 2, decimal.mark = ",")), " %"))
grand <- function(x) ifelse(is.na(x), "n.d.", ifelse(x >= 1e6, paste(fmt(x / 1e6, 1), "millions"),
                     ifelse(x >= 1e3, fmt(round(x, -2), 0), fmt(x, 0))))
LEG <- list(orientation = "h", x = 0, y = 1.12)

calculer <- function(w, scenario, horizon) {
  p <- d$pays
  if (horizon == 2024) p$f <- 1 else
    p <- p |> left_join(d$scen_zone |> filter(scenario == !!scenario, horizon == !!horizon) |> select(zone, f = f_zone), by = "zone")
  p |> mutate(e_h = e0 * f,
              e_ia = d$part_ia[["monde"]] * e0 + d$part_ia[["hausse"]] * (e_h - e0),
              eau = e_h * w, eau_ia = e_ia * w,
              part = 100 * eau / (prelev0 * 1e3),
              eau_menages = prelev0 * 1e3 * pct_menages / 100,
              ratio_menages = 100 * eau / eau_menages,
              stress_h = stress0 * (1 + (eau - e0 * w) / (prelev0 * 1e3)),
              cat0 = categoriser(stress0), cat_h = categoriser(stress_h))
}

# Un bloc = un titre (question), le graphique, une phrase à retenir. L'aide est dans l'icône (i).
bloc <- function(titre, sortie, aide, retenir, h = "380px", plot = plotlyOutput)
  card(full_screen = TRUE,
       card_header(class = "d-flex justify-content-between align-items-center gap-2",
                   span(class = "titre-bloc", titre), tooltip(bs_icon("info-circle", class = "aide"), aide)),
       plot(sortie, height = h),
       card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", retenir))

# ------------------------------------------------------------- Interface -----
theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#1f6f8b", base_font = font_google("Inter")) |>
  bs_add_rules("
    body{font-size:15px;}
    /* Barre de navigation */
    .navbar{padding:.55rem 1.4rem !important;background:linear-gradient(90deg,#123f52 0%,#1f6f8b 100%) !important;
            box-shadow:0 3px 14px rgba(0,0,0,.18);}
    .navbar .container-fluid{flex-wrap:nowrap;}
    .navbar-brand{display:flex;align-items:center;padding:0;margin-right:2rem;}
    @media (min-width:992px){
      .navbar > .container-fluid{display:grid !important;grid-template-columns:1fr auto 1fr;align-items:center;}
      .navbar > .container-fluid > .navbar-brand{grid-column:1;justify-self:start;}
      .navbar > .container-fluid > .navbar-collapse{grid-column:2;justify-content:center;}
      .navbar > .container-fluid > .navbar-toggler{display:none;}}
    .marque{display:flex;align-items:center;gap:.6rem;line-height:1.1;}
    .marque .goutte{font-size:1.7rem;color:#7fd3f7;}
    .marque-titre{font-weight:800;font-size:1.25rem;color:#fff;letter-spacing:.01em;}
    .marque-sous{font-size:.72rem;color:rgba(255,255,255,.72);letter-spacing:.06em;text-transform:uppercase;margin-top:2px;}
    .navbar .navbar-nav{margin-left:auto !important;margin-right:auto !important;gap:.4rem;}
    .navbar .nav-link{color:rgba(255,255,255,.88) !important;font-weight:500;font-size:1rem;padding:.5rem 1.05rem !important;
                      border-radius:999px;border:0 !important;transition:background .15s,color .15s;}
    .navbar .nav-link .bi{margin-right:.15rem;}
    .navbar .nav-link:hover{background:rgba(255,255,255,.14);color:#fff !important;}
    .navbar .nav-link.active{background:#fff !important;color:#1f6f8b !important;font-weight:700;box-shadow:0 2px 8px rgba(0,0,0,.15);}
    .navbar .nav-link.active::after,.navbar .nav-link::after{display:none !important;}
    .navbar .navbar-nav .nav-item{border:0 !important;}
    .navbar-nav .nav-link.active, .navbar-underline .nav-link.active{border-bottom:0 !important;}
    @media (max-width:1450px){.navbar .nav-link{padding:.45rem .75rem !important;font-size:.95rem;} .navbar .navbar-nav{gap:.2rem;}}
    @media (max-width:1250px){.marque-sous{display:none;}}
    .titre-bloc{font-weight:600;}
    .aide{color:#1f6f8b;cursor:help;font-size:1.05rem;}
    .card-footer.retenir{background:#fff8e1;border-top:2px solid #f0ad4e;font-size:.95rem;}
    .card-footer.retenir .bi{color:#f0ad4e;}
    .bandeau{gap:24px;padding:18px 8px;} .bandeau .logo{height:110px;width:auto;}
    .cadre-titre{flex:1;border:3px solid #1f6f8b;border-radius:14px;padding:22px 26px;background:linear-gradient(180deg,#f4f8fa,#fff);box-shadow:0 4px 18px rgba(31,111,139,.12);}
    .cadre-titre h1{font-weight:800;color:#1f6f8b;margin:6px 0;font-size:2.4rem;}
    .sur-titre{text-transform:uppercase;letter-spacing:.08em;font-size:.8rem;color:#777;}
    .sous-titre{font-size:1.15rem;color:#333;margin:0;}
    .auteurs{font-size:1.15rem;margin:14px 0 22px;} .auteurs b{color:#1f6f8b;}
    .tuile{text-align:center;padding:18px 12px;border-radius:12px;background:#fff;border:1px solid #e3e9ee;height:100%;}
    .tuile .gros{font-size:1.9rem;font-weight:800;color:#1f6f8b;line-height:1.15;} .tuile .bi{font-size:1.7rem;color:#f0ad4e;}
    .tuile.dc{border:2px solid #d35400;} .tuile.dc .gros{color:#d35400;}
    .etapes{display:flex;gap:6px;align-items:stretch;flex-wrap:wrap;}
    .etape{flex:1 1 140px;border:2px solid #1f6f8b;border-radius:12px;padding:12px;background:#fff;text-align:center;}
    .etape .num{display:inline-block;width:30px;height:30px;border-radius:50%;background:#1f6f8b;color:#fff;font-weight:700;line-height:30px;}
    .etape .val{font-size:1.2rem;font-weight:700;color:#d35400;margin-top:4px;}
    .fleche{display:flex;align-items:center;justify-content:center;font-size:1.6rem;color:#1f6f8b;flex:0 0 24px;}
    .fiche dt{font-weight:500;color:#666;font-size:.9rem;} .fiche dd{font-size:1.1rem;margin-bottom:.5rem;}
    .fiche .equiv{font-size:.85rem;color:#777;}
    .barre-carte .shiny-input-container{margin-bottom:0 !important;}
    /* Accueil : pas de panneau « Hypothèses » ni de flèche */
    body.sur-accueil .bslib-sidebar-layout > .sidebar,
    body.sur-accueil .bslib-sidebar-layout > .collapse-toggle{display:none !important;}
    body.sur-accueil .bslib-sidebar-layout{grid-template-columns:0 1fr !important;}
    /* Accueil */
    .accueil{margin:-1.5rem -1.5rem 0 -1.5rem;}
    .bande-logos{display:flex;justify-content:space-between;align-items:center;padding:14px 48px;background:#fff;}
    .bande-logos img{height:62px;width:auto;}
    .bande-logos span{font-size:.8rem;letter-spacing:.12em;text-transform:uppercase;color:#7a8a94;}
    .hero{position:relative;min-height:62vh;display:flex;align-items:center;justify-content:center;text-align:center;
          background-color:#123f52;}
    .hero-contenu{max-width:900px;padding:70px 24px;color:#fff;}
    .hero-sur{font-size:.85rem;letter-spacing:.3em;text-transform:uppercase;color:#7fd3f7;margin-bottom:14px;}
    .hero h1{font-size:4rem;font-weight:800;margin:0 0 18px;letter-spacing:-.01em;text-shadow:0 2px 20px rgba(0,0,0,.4);}
    .hero-question{font-size:1.45rem;font-weight:300;margin:0 0 34px;color:rgba(255,255,255,.92);}
    .btn-hero{background:#fff !important;color:#123f52 !important;border:0;border-radius:999px;padding:.85rem 2.4rem;
              font-size:1.1rem;font-weight:700;box-shadow:0 6px 24px rgba(0,0,0,.3);}
    .btn-hero:hover{background:#7fd3f7 !important;}
    .hero-noms{margin-top:44px;font-size:1rem;color:rgba(255,255,255,.9);letter-spacing:.04em;}
    .hero-noms span{font-size:.85rem;color:rgba(255,255,255,.65);}
    .chiffres{display:flex;justify-content:center;flex-wrap:wrap;padding:56px 24px;background:#fff;}
    .chiffre{flex:1 1 200px;max-width:300px;text-align:center;padding:0 24px;border-left:1px solid #e3e9ee;}
    .chiffre:first-child{border-left:0;}
    .chiffre .nb{font-size:2.6rem;font-weight:800;color:#1f6f8b;line-height:1;}
    .chiffre .lib{margin-top:10px;color:#5b6b75;font-size:.95rem;}
    .photos{display:grid;grid-template-columns:1fr 1fr;gap:24px;padding:0 48px 40px;background:#fff;}
    .photo{height:300px;border-radius:16px;background-size:cover;background-position:center;position:relative;overflow:hidden;
           box-shadow:0 8px 30px rgba(0,0,0,.12);}
    .photo .legende{position:absolute;left:0;right:0;bottom:0;padding:22px 24px;color:#fff;font-size:.95rem;
                    background:linear-gradient(0deg,rgba(0,0,0,.75),rgba(0,0,0,0));}
    .photo .legende b{font-size:1.15rem;}
    .credits{text-align:center;font-size:.72rem;color:#9aa6ad;padding:0 0 24px;background:#fff;}
    @media (max-width:900px){.hero h1{font-size:2.6rem;} .photos{grid-template-columns:1fr;padding:0 16px 30px;}
      .bande-logos{padding:10px 16px;} .bande-logos span{display:none;} .chiffre{border-left:0;margin-bottom:24px;} .fleche{transform:rotate(90deg);}}")

ui <- page_navbar(
  id = "nav", fillable = FALSE, theme = theme,
  title = div(class = "marque", bs_icon("droplet-fill", class = "goutte"),
              div(div(class = "marque-titre", "L'eau des data centers"),
                  div(class = "marque-sous", "SAÉ 5.EMS.01 · IUT de Carcassonne"))),
  window_title = "L'eau des data centers - SAÉ 5.EMS.01",
  sidebar = sidebar(id = "sb", width = 260, open = "closed",
    h6(bs_icon("sliders"), " Hypothèses"),
    radioButtons("horizon", "Année", c("2024" = 2024, "2030" = 2030, "2035" = 2035), 2024, inline = TRUE),
    conditionalPanel("input.horizon != '2024'",
      radioButtons("scen", "Croissance des data centers", c("Lente" = "bas", "Centrale" = "central", "Rapide" = "haut"), "central")),
    radioButtons("coef", "Eau utilisée par kWh", c("Basse : 0,21 L" = "bas", "Moyenne : 0,81 L" = "central", "Haute : 2,41 L" = "haut"), "central"),
    div(class = "small text-muted", "La valeur moyenne sert de référence. Tous les chiffres se mettent à jour.")),

  # ---- Accueil
  nav_panel("Accueil", value = "Accueil", icon = bs_icon("house"),
    tags$script(HTML("
      document.body.classList.add('sur-accueil');
      $(document).on('shown.bs.tab', 'a[data-bs-toggle]', function(e){
        document.body.classList.toggle('sur-accueil', $(e.target).attr('data-value') === 'Accueil');
      });")),
    div(class = "accueil",
      div(class = "bande-logos",
        tags$img(src = "logo_iut.png", alt = "IUT Perpignan Carcassonne Narbonne"),
        span("BUT Science des données · SAÉ 5.EMS.01 · 2026-2027"),
        tags$img(src = "logo_upvd.png", alt = "Université de Perpignan Via Domitia")),
      div(class = "hero", style = "background:linear-gradient(180deg,rgba(8,30,44,.50) 0%,rgba(8,30,44,.80) 100%),url('photo_serveurs.jpg') center/cover no-repeat;",
        div(class = "hero-contenu",
          div(class = "hero-sur", "Étude statistique"),
          h1("L'eau des data centers"),
          p(class = "hero-question", "Les ressources en eau suffiront-elles, aujourd'hui et en 2035 ?"),
          actionButton("entrer", "Découvrir l'étude", icon = bs_icon("arrow-right"), class = "btn-hero"),
          div(class = "hero-noms", "Ayi AMAVIGAN · Ada GADJI · Aboubakar COULIBALY", br(),
              span("Professeure : Mme Viguier-Pla · IUT de Carcassonne")))),
      div(class = "chiffres",
        div(class = "chiffre", div(class = "nb", "6 239"), div(class = "lib", "data centers dans 135 pays")),
        div(class = "chiffre", div(class = "nb", "336 M m³"), div(class = "lib", "d'eau utilisée en 2024")),
        div(class = "chiffre", div(class = "nb", "< 0,1 %"), div(class = "lib", "de l'eau d'un pays, 9 fois sur 10")),
        div(class = "chiffre", div(class = "nb", "27 %"), div(class = "lib", "des sites en région qui manque d'eau"))),
      div(class = "photos",
        div(class = "photo", style = "background-image:url('photo_ashburn.jpg');",
            div(class = "legende", tags$b("Ashburn, Virginie"), br(), "La plus forte concentration de data centers au monde")),
        div(class = "photo", style = "background-image:url('photo_council_bluffs.jpg');",
            div(class = "legende", tags$b("Council Bluffs, Iowa"), br(), "Le site Google qui consomme le plus d'eau : 5 millions de m³ en 2025"))),
    )
  ),

  # ---- Carte du monde
  nav_panel("Carte", icon = bs_icon("globe-europe-africa"),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "Où sont les data centers, et combien d'eau utilisent-ils ?"),
          selectInput("pays_sel", NULL, width = "210px", CHOIX_PAYS, "MONDE")),
        div(class = "barre-carte px-3 pt-3 pb-2",
          selectInput("var_carte", NULL, width = "100%", c(
            "Couleur : part de l'eau du pays utilisée par les data centers" = "part",
            "Couleur : eau utilisée par les data centers" = "eau",
            "Couleur : nombre de data centers" = "n_dc",
            "Couleur : manque d'eau du pays (stress hydrique)" = "stress0"))),
        leafletOutput("carte", height = "540px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("carte_retenir", inline = TRUE))),
      card(card_header(span(class = "titre-bloc", bs_icon("card-text"), " Fiche")), uiOutput("fiche"))
    )
  ),

  # ---- Régions
  nav_panel("Régions", icon = bs_icon("geo-alt"),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "Un pays peut avoir des régions sèches"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"), "Chaque point est un data center (OpenStreetMap, 20/09/2026). Sa couleur indique le manque d'eau de SA région (WRI Aqueduct)."),
            selectInput("pays_reg", NULL, width = "210px",
                        c("Monde entier" = "MONDE", setNames(sort(unique(d$osm$iso3)), d$osm$pays[match(sort(unique(d$osm$iso3)), d$osm$iso3)])), "MONDE"))),
        leafletOutput("carte_reg", height = "540px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("reg_retenir", inline = TRUE))),
      card(card_header(span(class = "titre-bloc", "Régions avec le plus de sites")), DTOutput("tab_reg"))
    )
  ),

  # ---- Saisons
  nav_panel("Saisons", icon = bs_icon("sun"),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "L'été : plus chaud, souvent plus sec"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"), "Barres = pluie du mois, courbe = température (normales 1991-2020). En orange : les mois plus chauds ET plus secs que la moyenne, quand les data centers refroidissent le plus."),
            selectInput("pays_clim", NULL, width = "210px", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)], "FRA"))),
        plotlyOutput("clim", height = "380px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("clim_retenir", inline = TRUE))),
      bloc("Combien de mois chauds et secs ?", "carte_mois",
           "Plus la couleur est foncée, plus le pays a de mois où chaleur et manque de pluie se cumulent.",
           "Une moyenne annuelle cache ces pics d'été.", "380px", leafletOutput))
  ),

  # ---- Usages de l'eau (arbitrages)
  nav_panel("Usages", icon = bs_icon("signpost-split"),
    card(card_header(class = "d-flex justify-content-between align-items-center",
           span(class = "titre-bloc", "Sur 10 000 litres d'eau prélevés, qui les utilise ?"),
           tooltip(bs_icon("info-circle", class = "aide"), "Prélèvements d'eau des 132 pays étudiés (FAO AQUASTAT, Rapport 1) et eau des data centers estimée (Rapport 2), selon les hypothèses choisies à gauche.")),
         card_body(uiOutput("litres")),
         card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("litres_retenir", inline = TRUE))),
    layout_columns(col_widths = c(7, 5),
      bloc("Où les data centers peuvent-ils concurrencer les habitants ?", "arb",
           "Chaque bulle est un pays (taille = nombre de sites). À droite : sites dans des régions qui manquent d'eau. En haut : l'eau des data centers pèse face à l'eau des habitants.",
           textOutput("arb_retenir", inline = TRUE), "420px"),
      bloc("Mesures réelles : chaque site Google en 2025", "carte_google",
           "Eau consommée par site, publiée par Google. Taille = volume ; couleur = manque d'eau de la région.",
           "Un seul site peut utiliser l'eau d'une ville de 50 000 habitants.", "420px", leafletOutput))
  ),

  # ---- Demain
  nav_panel("Demain", icon = bs_icon("graph-up-arrow"),
    layout_columns(col_widths = c(6, 6),
      bloc("L'électricité des data centers va fortement augmenter", "traj",
           "Mesures 2020-2024 (trait plein) puis les 4 scénarios de l'Agence internationale de l'énergie.",
           "x1,7 à x4,1 d'ici 2035.", "340px"),
      bloc("Les pays les plus exposés en 2035", "top10_fut",
           "Part de l'eau du pays utilisée par les data centers en 2024, 2030 et 2035. Barre rose : écart entre croissance lente et rapide.",
           textOutput("fut_retenir", inline = TRUE), "340px")),
    layout_columns(col_widths = c(6, 6),
      bloc("France : l'eau prélevée continue de baisser", "fr_prev",
           "Prélèvements d'eau de la France (tous usages), observés 2015-2022 puis prévus (modèle de dérive, intervalle à 95 %).",
           textOutput("prev_retenir", inline = TRUE), "320px"),
      bloc("France : l'eau des data centers d'ici 2035", "fr_scen",
           "Eau des data centers français selon la croissance. En rouge : la part due à l'IA. Étiquette : part de l'eau prélevée en France.",
           textOutput("frscen_retenir", inline = TRUE), "320px"))
  ),

  # ---- Méthode
  nav_panel("Méthode", icon = bs_icon("calculator"),
    navset_card_pill(
      nav_panel("Estimation",
        p(class = "text-muted", "Aucune base ne donne l'eau des data centers de chaque pays : nous l'estimons en 5 étapes (exemple : la France)."),
        uiOutput("etapes"),
        layout_columns(col_widths = c(6, 6), class = "mt-3",
          bloc("Le choix des litres par kWh change tout", "coefs",
               "Eau de tous les data centers du monde en 2024 avec chacun des 3 coefficients d'eau.",
               "Résultat de 88 à 999 millions de m³ : nous donnons donc une fourchette.", "300px"),
          bloc("Notre répartition retrouve les chiffres publiés", "validation",
               "Part des data centers dans l'électricité du pays : notre modèle (bleu) et l'AIE (gris).",
               "Écarts faibles : 4,3 % contre 4 % aux États-Unis.", "300px"))),
      nav_panel("Prévision",
        layout_columns(col_widths = c(7, 5),
          bloc("Quel modèle prévoit le mieux ?", "backtest",
               "Chaque modèle apprend sur 2015-2019 (zone grise) puis prévoit 2020-2022. La courbe noire est la réalité.",
               "Le modèle de dérive (vert) suit le mieux la réalité.", "360px"),
          bloc("Erreur de chaque modèle", "mae",
               "Erreur absolue moyenne sur 2020-2022 (milliards de m³). Plus c'est court, mieux c'est.",
               "Dérive : 0,75, deux fois moins que les autres.", "360px"))),
      nav_panel("Profils de pays (ACP)",
        layout_columns(col_widths = c(6, 6),
          bloc("4 familles de pays", "carte_classes",
               "Les 61 pays ayant au moins 10 data centers, regroupés par ressemblance (ACP puis classification).",
               "Là où l'eau des data centers pèse le plus : les petits pays riches.", "380px", leafletOutput),
          bloc("Les pays selon 2 critères", "acp_ind",
               "Analyse en composantes principales : à droite les grands pays, en haut les pays riches en eau, en bas les pays secs.",
               "Les pays très secs ont encore peu de data centers.", "380px"))))
  ),

  # ---- Sources
  nav_panel("Sources", icon = bs_icon("journal-text"),
    navset_card_pill(
      nav_panel("Données par pays",
        div(class = "d-flex justify-content-end mb-2", downloadButton("dl", "Télécharger (CSV)", class = "btn-sm")),
        DTOutput("tab_pays")),
      nav_panel("Sources et dates",
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("Donnée"), tags$th("Source"), tags$th("Période"))),
          tags$tbody(
            tags$tr(tags$td("Data centers par pays"), tags$td("Inventaire data-centres-worldwide"), tags$td("2024")),
            tags$tr(tags$td("Électricité des data centers, scénarios"), tags$td("AIE, Energy and AI (2025)"), tags$td("2020-2035")),
            tags$tr(tags$td("Eau prélevée, stress hydrique"), tags$td("FAO AQUASTAT, Banque mondiale"), tags$td("2015-2022")),
            tags$tr(tags$td("Eau par usage"), tags$td("FAO AQUASTAT (Rapport 1)"), tags$td("≤ 2017")),
            tags$tr(tags$td("Sites géolocalisés"), tags$td("OpenStreetMap"), tags$td("20/09/2026")),
            tags$tr(tags$td("Stress hydrique par région"), tags$td("WRI Aqueduct 4.0"), tags$td("1979-2019")),
            tags$tr(tags$td("Pluie et température"), tags$td("Banque mondiale CCKP"), tags$td("1991-2020")),
            tags$tr(tags$td("Eau par site Google"), tags$td("Google, rapport 2026"), tags$td("2025")),
            tags$tr(tags$td("Data centers en France"), tags$td("Arcep, ADEME, RTE"), tags$td("2021-2030")),
            tags$tr(tags$td("Photos de l'accueil"), tags$td("Wikimedia Commons : BalticServers.com (CC BY-SA 3.0), Theodore Christopher (CC0), Chad Davis (CC BY 2.0)"), tags$td("")))),
        h6("Limites"),
        tags$ul(class = "small",
          tags$li("L'électricité est répartie selon le nombre de sites, pas leur taille."),
          tags$li("Un seul coefficient d'eau pour tous les pays, d'où une fourchette."),
          tags$li("Les sites OpenStreetMap ne couvrent pas tous les data centers."),
          tags$li("Pas de consommation d'eau mois par mois : la page Saisons montre le risque, pas la consommation.")),
        p(class = "small text-muted", "Ayi AMAVIGAN, Ada GADJI, Aboubakar COULIBALY · BUT3 Science des données · IUT de Carcassonne · 2026-2027"))))
)

# --------------------------------------------------------------- Serveur -----
server <- function(input, output, session) {
  observeEvent(input$entrer, nav_select("nav", "Carte"))
  observeEvent(input$nav, toggle_sidebar("sb", open = input$nav != "Accueil"), ignoreInit = FALSE)

  w  <- reactive(wue[[input$coef]])
  hz <- reactive(as.integer(input$horizon))
  sc <- reactive(if (hz() == 2024) "central" else input$scen)
  res <- reactive(calculer(w(), sc(), hz()))
  pres <- reactive(res() |> filter(!is.na(prelev0)))
  quand <- reactive(if (hz() == 2024) "en 2024" else paste0("en ", hz(), " (croissance ", c(bas = "lente", central = "centrale", haut = "rapide")[[sc()]], ")"))

  # ---------- Pays choisi : le même dans Carte, Régions et Saisons
  synchro <- function(v) {
    if (is.null(v) || v == "") return(invisible())
    if (v == "MONDE") {
      if (input$pays_sel != "MONDE") updateSelectInput(session, "pays_sel", selected = "MONDE")
      if (input$pays_reg != "MONDE") updateSelectInput(session, "pays_reg", selected = "MONDE")
      return(invisible())
    }
    if (v %in% d$pays$iso3 && input$pays_sel != v) updateSelectInput(session, "pays_sel", selected = v)
    cible <- if (v %in% d$osm$iso3) v else "MONDE"
    if (input$pays_reg != cible) updateSelectInput(session, "pays_reg", selected = cible)
    if (v %in% d$climat$iso3 && input$pays_clim != v) updateSelectInput(session, "pays_clim", selected = v)
  }
  observeEvent(input$pays_sel, synchro(input$pays_sel), ignoreInit = TRUE)
  observeEvent(input$pays_reg, if (input$pays_reg != "MONDE") synchro(input$pays_reg), ignoreInit = TRUE)
  observeEvent(input$pays_clim, synchro(input$pays_clim), ignoreInit = TRUE)
  observeEvent(input$carte_shape_click, {
    id <- input$carte_shape_click$id
    if (!is.null(id) && id %in% d$pays$iso3) updateSelectInput(session, "pays_sel", selected = id)
  })

  # ---------- Carte du monde
  bins <- list(part = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1, Inf), eau = c(0, 0.1, 1, 5, 20, 50, Inf),
               n_dc = c(0, 5, 20, 50, 100, 300, Inf), stress0 = c(0, 25, 50, 75, 100, Inf))
  unite <- c(part = "% de l'eau du pays", eau = "millions de m³", n_dc = "data centers", stress0 = "stress hydrique (%)")
  pal_v <- c(part = "YlGnBu", eau = "Blues", n_dc = "Purples", stress0 = "YlOrRd")
  output$carte <- renderLeaflet({
    v <- input$var_carte; iso <- input$pays_sel
    m <- d$monde |> left_join(res() |> mutate(val = .data[[v]]) |> select(iso3, country, val), by = "iso3")
    pal <- colorBin(pal_v[[v]], domain = m$val, bins = bins[[v]], na.color = "#e8e8e8")
    lab <- sprintf("<b>%s</b><br>%s : %s", ifelse(is.na(m$country), "Pas de data center recensé", m$country), unite[[v]],
                   ifelse(is.na(m$val), "n.d.", fmt(m$val, ifelse(v %in% c("n_dc", "stress0"), 0, 2)))) |> lapply(htmltools::HTML)
    l <- leaflet(m, options = leafletOptions(worldCopyJump = TRUE, minZoom = 1, zoomSnap = 0.25)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(layerId = m$iso3, fillColor = pal(m$val), fillOpacity = if (iso == "MONDE") 0.85 else 0.5,
                  color = "white", weight = 0.6, label = lab,
                  highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)) |>
      addLegend("bottomleft", pal = pal, values = m$val, title = unite[[v]], opacity = 0.9, na.label = "pas de donnée",
                labFormat = function(type, cuts, p) { k <- formatC(cuts, format = "fg", digits = 3, big.mark = " ", decimal.mark = ",")
                  paste0(head(k, -1), " – ", ifelse(is.infinite(tail(cuts, -1)), "et plus", tail(k, -1))) })
    if (iso == "MONDE") return(l |> setView(5, 25, 1.75))
    g <- d$monde[d$monde$iso3 == iso, ]; o <- d$osm |> filter(iso3 == iso)
    if (nrow(g)) l <- l |> addPolygons(data = g, fill = FALSE, color = "#111", weight = 3, opacity = 1)
    if (nrow(o)) {
      palr <- colorFactor(col_aq, levels = d$lab_aq, na.color = "#777777")
      l <- l |>
        addCircleMarkers(lng = o$lon, lat = o$lat, radius = 6, color = "#222", weight = 1, fillOpacity = 0.9, fillColor = palr(o$stress_region),
                         label = lapply(paste0("<b>", o$nom, "</b><br>", coalesce(o$region, ""), "<br>Manque d'eau de la région : ",
                                               coalesce(as.character(o$stress_region), "n.d.")), htmltools::HTML)) |>
        addLegend("bottomright", colors = col_aq[1:5], labels = d$lab_aq[1:5], opacity = 0.9,
                  title = htmltools::HTML(paste0(nrow(o), " sites · manque d'eau<br>de leur région")))
    }
    if (nrow(o) >= 2) {
      lo <- quantile(o$lon, c(0.02, 0.98)); la <- quantile(o$lat, c(0.02, 0.98))
      dx <- max(1, diff(lo) * 0.15); dy <- max(1, diff(la) * 0.15)
      l |> fitBounds(lo[[1]] - dx, la[[1]] - dy, lo[[2]] + dx, la[[2]] + dy)
    } else if (nrow(g)) {
      bb <- sf::st_bbox(g)
      if (bb[["xmax"]] - bb[["xmin"]] < 120) l |> fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      else { ct <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(g)))); l |> setView(ct[1, 1], ct[1, 2], 3) }
    } else l |> setView(5, 25, 1.75)
  })
  output$carte_retenir <- renderText({
    if (input$pays_sel != "MONDE") return("Points = data centers du pays, colorés selon le manque d'eau de leur région. Survolez-les.")
    t <- pres() |> filter(n_dc >= 5, !is.na(.data[[input$var_carte]])) |> slice_max(.data[[input$var_carte]], n = 3, with_ties = FALSE)
    paste0("Cliquez sur un pays pour zoomer. Valeurs les plus fortes : ", paste(t$country, collapse = ", "), ".")
  })

  output$fiche <- renderUI({
    ligne <- function(t, v, e = NULL) tagList(tags$dt(t), tags$dd(v, if (!is.null(e)) div(class = "equiv", e)))
    if (input$pays_sel == "MONDE") {
      r <- res(); p <- pres() |> filter(n_dc >= 5) |> slice_max(part, n = 1)
      return(card_body(class = "fiche", h4("Monde"), p(class = "text-muted small", quand()),
        tags$dl(
          ligne("Data centers", fmt(sum(r$n_dc), 0), paste(nrow(r), "pays")),
          ligne("Eau utilisée", paste(fmt(sum(r$eau), 0), "millions de m³"), paste0("≈ ", grand(sum(r$eau) * 1e6 / PISCINE), " piscines olympiques")),
          ligne("Pays le plus exposé", paste0(p$country, " : ", pct(p$part)), "de l'eau du pays")),
        p(class = "small text-muted mt-2", bs_icon("hand-index"), " Cliquez sur un pays de la carte.")))
    }
    r <- res() |> filter(iso3 == input$pays_sel)
    cl <- d$climat |> filter(iso3 == r$iso3, sensible)
    verdict <- if (is.na(r$part)) "Données d'eau manquantes." else if (r$part < 0.1) "Très faible face aux autres usages." else
      if (r$part < 1) "Faible, mais à suivre." else "Point de vigilance."
    card_body(class = "fiche", h4(r$country), p(class = "text-muted small", quand()),
      tags$dl(
        ligne("Data centers", r$n_dc, if (!is.na(r$sites_osm)) paste(r$sites_osm, "géolocalisés sur la carte")),
        ligne("Eau utilisée", paste(fmt(r$eau, 2), "millions de m³"), paste0("≈ eau de ", grand(r$eau * 1e6 / HAB_M3), " habitants")),
        ligne("Part de l'eau du pays", pct(r$part), verdict),
        ligne("Manque d'eau du pays", if (is.na(r$stress0)) "n.d." else paste0(fmt(r$stress0, 0), " % (", r$cat0, ")")),
        ligne("Sites dans une région qui manque d'eau", pct(r$pct_sites_region_stress)),
        ligne("Mois chauds et secs", if (nrow(cl)) paste(mois_fr[cl$mois], collapse = ", ") else "aucun")))
  })

  # ---------- Régions
  osm_sel <- reactive(if (input$pays_reg == "MONDE") d$osm else d$osm |> filter(iso3 == input$pays_reg))
  output$carte_reg <- renderLeaflet({
    o <- osm_sel() |> filter(!is.na(stress_region))
    pal <- colorFactor(col_aq, levels = d$lab_aq)
    leaflet() |> addTiles() |>
      addCircleMarkers(lng = o$lon, lat = o$lat, radius = if (input$pays_reg == "MONDE") 3 else 5,
                       stroke = TRUE, weight = 0.5, color = "#333", fillColor = pal(o$stress_region), fillOpacity = 0.85,
                       label = lapply(paste0("<b>", o$nom, "</b><br>", o$region, " (", o$pays, ")<br>Manque d'eau : ", o$stress_region), htmltools::HTML)) |>
      addLegend("bottomleft", colors = col_aq, labels = d$lab_aq, title = "Manque d'eau de la région", opacity = 0.9) |>
      fitBounds(min(o$lon), min(o$lat), max(o$lon), max(o$lat))
  })
  output$tab_reg <- renderDT({
    t <- (if (input$pays_reg == "MONDE") d$regions else d$regions |> filter(iso3 == input$pays_reg)) |>
      arrange(-sites) |> transmute(Région = region, Pays = pays, Sites = sites, `Manque d'eau` = sub(" \\(.*", "", stress_region))
    if (input$pays_reg != "MONDE") t <- t |> select(-Pays)
    datatable(t, rownames = FALSE, options = list(pageLength = 10, dom = "tp", scrollX = TRUE))
  })
  output$reg_retenir <- renderText({
    o <- osm_sel() |> filter(!is.na(stress_region))
    if (nrow(o) == 0) return("Pas de site géolocalisé pour ce pays.")
    top <- o |> count(region, sort = TRUE) |> slice(1)
    paste0(fmt(100 * mean(o$stress_region %in% d$lab_aq[4:5]), 0), " % des ", nrow(o), " sites sont dans une région qui manque d'eau. ",
           "Région la plus équipée : ", top$region, " (", top$n, " sites).")
  })

  # ---------- Saisons
  output$clim <- renderPlotly({
    c <- d$climat |> filter(iso3 == input$pays_clim) |> arrange(mois) |> mutate(m = mois_fr[mois])
    if (nrow(c) == 0) return(plotly_empty())
    sh <- lapply(c$mois[c$sensible], function(i) list(type = "rect", xref = "x", yref = "paper", x0 = i - 0.5, x1 = i + 0.5,
                                                       y0 = 0, y1 = 1, fillcolor = "#f0ad4e", opacity = 0.25, line = list(width = 0), layer = "below"))
    plot_ly(c, x = ~mois) |>
      add_bars(y = ~pr, name = "Pluie (mm)", marker = list(color = "#6baed6"), hovertext = ~paste0(m, " : ", fmt(pr, 0), " mm"), hoverinfo = "text") |>
      add_lines(y = ~tas, name = "Température (°C)", yaxis = "y2", line = list(color = "#d62728", width = 3),
                hovertext = ~paste0(m, " : ", fmt(tas, 1), " °C"), hoverinfo = "text") |>
      layout(shapes = sh, legend = LEG, margin = list(r = 60), xaxis = list(title = "", tickvals = 1:12, ticktext = mois_fr),
             yaxis = list(title = "Pluie (mm)", rangemode = "tozero"),
             yaxis2 = list(title = "°C", overlaying = "y", side = "right", showgrid = FALSE))
  })
  output$clim_retenir <- renderText({
    c <- d$climat |> filter(iso3 == input$pays_clim); n <- d$pays$country[d$pays$iso3 == input$pays_clim]
    if (!any(c$sensible)) return(paste0(n, " : aucun mois à la fois plus chaud et plus sec."))
    paste0(n, " : mois chauds et secs = ", paste(mois_fr[c$mois[c$sensible]], collapse = ", "), ".")
  })
  output$carte_mois <- renderLeaflet({
    nm <- d$climat |> group_by(iso3) |> summarise(n = sum(sensible))
    m <- d$monde |> left_join(nm, by = "iso3") |> left_join(d$pays |> select(iso3, country), by = "iso3")
    pal <- colorBin("YlOrBr", domain = 0:6, bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 6.5), na.color = "#e8e8e8")
    leaflet(m) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(fillColor = pal(m$n), fillOpacity = 0.85, color = "white", weight = 0.5,
                  label = paste0(coalesce(m$country, m$iso3), " : ", m$n, " mois chauds et secs")) |>
      addLegend("bottomleft", pal = pal, values = m$n, title = "Mois chauds et secs", na.label = "pas de donnée",
                labFormat = function(type, cuts, p) c("0", "1", "2", "3", "4", "5 ou +")) |>
      setView(10, 25, 1)
  })

  # ---------- Usages : sur 10 000 litres
  litres <- reactive({
    p <- pres() |> filter(!is.na(pct_agri), !is.na(pct_indus), !is.na(pct_menages))
    tot <- sum(p$prelev0) * 1e3                                    # millions de m3
    s <- c(agri = sum(p$prelev0 * p$pct_agri), indus = sum(p$prelev0 * p$pct_indus), men = sum(p$prelev0 * p$pct_menages))
    s <- 1e4 * s / sum(s)
    dc <- 1e4 * sum(p$eau) / tot
    list(agri = s[["agri"]], indus = s[["indus"]], men = s[["men"]] , dc = dc)
  })
  output$litres <- renderUI({
    l <- litres()
    tuile <- function(ic, val, txt, cls = "") div(class = paste("tuile", cls), bs_icon(ic), div(class = "gros", val), txt)
    layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
      tuile("flower1", paste(fmt(l$agri, 0), "L"), "agriculture (irrigation)"),
      tuile("building", paste(fmt(l$indus, 0), "L"), "industrie et énergie"),
      tuile("house-door", paste(fmt(l$men, 0), "L"), "habitants (eau du robinet)"),
      tuile("hdd-rack", paste(fmt(l$dc, 1), "L"), paste("data centers,", quand()), "dc"))
  })
  output$litres_retenir <- renderText(paste0("Les data centers utilisent environ ", fmt(litres()$dc, 1),
    ifelse(litres()$dc >= 2, " litres", " litre"), " sur 10 000 : à l'échelle des pays, c'est très peu. Le risque est local (une région, un été)."))

  arb <- reactive(pres() |> filter(n_dc >= 10) |>
    mutate(signaux = (!is.na(ratio_menages) & ratio_menages >= 1) + (!is.na(pct_sites_region_stress) & pct_sites_region_stress >= 25) +
                     (!is.na(stress0) & stress0 >= 50)))
  output$arb <- renderPlotly({
    a <- arb() |> filter(!is.na(ratio_menages), !is.na(pct_sites_region_stress), ratio_menages > 0)
    plot_ly(a, x = ~pct_sites_region_stress, y = ~ratio_menages,
            color = ~factor(signaux, 0:3, c("0 signal", "1 signal", "2 signaux", "3 signaux")),
            colors = c("#9ecae1", "#fdd49e", "#fc8d59", "#b30000"), type = "scatter", mode = "markers+text",
            text = ~ifelse(signaux >= 2 | n_dc >= 1000, country, ""), textposition = "top center", textfont = list(size = 11, color = "#333"),
            marker = list(size = ~pmin(55, 8 + 1.6 * sqrt(n_dc)), opacity = 0.75, line = list(color = "#555", width = 0.5)),
            hovertext = ~paste0("<b>", country, "</b><br>Sites en région sèche : ", fmt(pct_sites_region_stress, 0),
                                " %<br>Eau des DC / eau des habitants : ", pct(ratio_menages)), hoverinfo = "text") |>
      layout(xaxis = list(title = "% des sites dans une région qui manque d'eau", range = c(-5, 115), tickvals = c(0, 25, 50, 75, 100)),
             yaxis = list(title = "Eau des DC / eau des habitants", type = "log", range = c(-3.3, 1),
                          tickvals = c(0.001, 0.01, 0.1, 1, 10), ticktext = c("0,001 %", "0,01 %", "0,1 %", "1 %", "10 %")),
             shapes = list(list(type = "line", x0 = 25, x1 = 25, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "grey")),
                           list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 1, y1 = 1, line = list(dash = "dot", color = "grey")),
                           list(type = "rect", x0 = 25, x1 = 115, y0 = 1, y1 = 10, fillcolor = "#b30000", opacity = 0.05, line = list(width = 0))),
             annotations = list(list(x = 110, y = 0.55, xanchor = "right", text = "zone de concurrence", showarrow = FALSE,
                                     font = list(color = "#b30000", size = 12), yref = "y", yanchor = "bottom")),
             legend = LEG)
  })
  output$arb_retenir <- renderText({
    t <- arb() |> filter(signaux >= 2) |> arrange(-signaux, -ratio_menages)
    paste0("À surveiller : ", paste(head(t$country, 5), collapse = ", "), ".")
  })
  output$carte_google <- renderLeaflet({
    g <- d$google |> mutate(stress_region = coalesce(as.character(stress_region), "Non renseigné"))
    pal <- colorFactor(c(col_aq, "Non renseigné" = "#999999"), levels = c(d$lab_aq, "Non renseigné"))
    leaflet() |> addTiles() |>
      addCircleMarkers(lng = g$lon, lat = g$lat, radius = pmax(4, sqrt(g$eau_m3 / 1e4) * 1.6), fillColor = pal(g$stress_region),
                       fillOpacity = 0.8, color = "#333", weight = 0.8,
                       label = lapply(paste0("<b>", g$site, "</b><br>", fmt(g$eau_m3 / 1e6, 2), " million de m³ en 2025<br>≈ eau de ",
                                             grand(g$eau_m3 / HAB_M3), " habitants<br>Manque d'eau de la région : ", g$stress_region), htmltools::HTML)) |>
      addLegend("bottomleft", colors = c(col_aq[1:5], "#999999"), labels = c(d$lab_aq[1:5], "Non renseigné"), title = "Manque d'eau de la région", opacity = 0.9) |>
      setView(-40, 40, 2)
  })

  # ---------- Demain
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
    p |> layout(xaxis = list(title = ""), yaxis = list(title = "TWh par an"), legend = LEG)
  })
  top10 <- reactive({
    s <- sc()
    top <- calculer(w(), s, 2035) |> filter(!is.na(prelev0), n_dc >= 20) |> slice_max(part, n = 8) |> pull(iso3)
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
                   line = list(color = "rgba(178,34,34,0.25)", width = 10), name = "Écart 2035", hoverinfo = "none") |>
      add_markers(data = pts, x = ~part, y = ~country, color = ~h, colors = c("#9ecae1", "#3182bd", "#08519c"),
                  marker = list(size = 10), text = ~paste0(country, " ", h, " : ", pct(part)), hoverinfo = "text") |>
      layout(xaxis = list(title = "% de l'eau du pays"), yaxis = list(title = ""), legend = LEG)
  })
  output$fut_retenir <- renderText({
    p <- top10()$pts |> filter(h == "2035") |> slice_max(part, n = 1)
    paste0(p$country, " reste en tête (", pct(p$part), " en 2035). Ailleurs, la part reste faible.")
  })
  output$fr_prev <- renderPlotly({
    o <- d$prev_fr$obs; pv <- d$prev_fr$prev
    plot_ly() |>
      add_ribbons(data = pv, x = ~annee, ymin = ~basse, ymax = ~haute, name = "Intervalle à 95 %", fillcolor = "rgba(31,111,139,0.15)", line = list(width = 0)) |>
      add_lines(data = pv, x = ~annee, y = ~prevision, name = "Prévision", line = list(dash = "dash", color = "#1f6f8b", width = 3)) |>
      add_trace(data = o, x = ~annee, y = ~valeur, type = "scatter", mode = "lines+markers", name = "Observé", line = list(color = "#111"), marker = list(color = "#111")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Milliards de m³", rangemode = "tozero"), legend = LEG)
  })
  output$prev_retenir <- renderText({
    pv <- d$prev_fr$prev |> filter(annee == 2035)
    paste0("Environ ", fmt(pv$prevision, 1), " milliards de m³ en 2035, avec une forte incertitude.")
  })
  fr_scen <- reactive({
    e0f <- d$pays$e0[d$pays$iso3 == "FRA"]
    prel <- c(`2024` = d$pays$prelev0[d$pays$iso3 == "FRA"], `2030` = d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2030],
              `2035` = d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2035])
    nm <- c(bas = "lente", central = "centrale", haut = "rapide")
    bind_rows(tibble(lab = "2024", e_h = e0f, h = "2024"),
      bind_rows(lapply(c(2030, 2035), function(h) bind_rows(lapply(names(nm), function(s)
        tibble(lab = paste0(h, " ", nm[[s]]), e_h = calculer(w(), s, h) |> filter(iso3 == "FRA") |> pull(e_h), h = as.character(h))))))) |>
      mutate(e_ia = d$part_ia[["fr"]] * e0f + d$part_ia[["hausse"]] * (e_h - e0f), eau = e_h * w(), eau_ia = e_ia * w(),
             part = 100 * eau / (prel[h] * 1e3), lab = factor(lab, unique(lab)))
  })
  output$fr_scen <- renderPlotly({
    f <- fr_scen()
    plot_ly(f, x = ~lab) |>
      add_bars(y = ~eau - eau_ia, name = "Hors IA", marker = list(color = "#9ecae1")) |>
      add_bars(y = ~eau_ia, name = "IA", marker = list(color = "#d62728"), text = ~pct(part), textposition = "outside") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Millions de m³"), legend = LEG)
  })
  output$frscen_retenir <- renderText({
    f <- fr_scen()
    paste0("De ", fmt(f$eau[1], 1), " à ", fmt(max(f$eau[f$h == "2035"]), 1), " millions de m³ en 2035, soit au plus ", pct(max(f$part)), " de l'eau de la France.")
  })

  # ---------- Méthode
  output$etapes <- renderUI({
    fr <- d$pays |> filter(iso3 == "FRA")
    et <- function(n, titre, val) div(class = "etape", span(class = "num", n), div(tags$b(titre)), div(class = "val", val))
    fl <- div(class = "fleche", bs_icon("arrow-right"))
    div(class = "etapes",
      et(1, "Électricité mondiale (AIE)", "415 TWh"), fl,
      et(2, "Part de la France (selon ses sites)", paste(fmt(fr$e0, 1), "TWh")), fl,
      et(3, "× litres d'eau par kWh", paste(fmt(fr$e0 * w(), 1), "Mm³")), fl,
      et(4, "÷ eau prélevée en France", pct(100 * fr$e0 * w() / (fr$prelev0 * 1e3))), fl,
      et(5, "Projection 2035 (AIE)", "x1,7 à x4,1"))
  })
  output$coefs <- renderPlotly({
    k <- tibble(nom = c("0,21 L/kWh", "0,81 L/kWh", "2,41 L/kWh"), w = unname(wue)) |> mutate(eau = 415 * w, nom = factor(nom, nom))
    plot_ly(k, x = ~nom, y = ~eau, type = "bar", marker = list(color = c("#9ecae1", "#1f6f8b", "#08306b")),
            text = ~paste(fmt(eau, 0), "Mm³"), textposition = "outside", hoverinfo = "none") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Millions de m³ (monde, 2024)", range = c(0, 1200)))
  })
  output$validation <- renderPlotly({
    v <- d$validation |> arrange(modele_pct) |> mutate(country = factor(country, country))
    plot_ly(v, y = ~country) |>
      add_bars(x = ~modele_pct, name = "Notre modèle", marker = list(color = "#1f6f8b"), orientation = "h") |>
      add_bars(x = ~ref_pct, name = "AIE", marker = list(color = "#bdbdbd"), orientation = "h") |>
      layout(barmode = "group", xaxis = list(title = "% de l'électricité du pays", type = "log",
                                            tickvals = c(0.5, 1, 2, 5, 10, 20), ticktext = c("0,5", "1", "2", "5", "10", "20")),
             yaxis = list(title = ""), legend = LEG)
  })
  output$backtest <- renderPlotly({
    o <- d$prev_fr$obs; b <- d$backtest
    cols <- c("Naïf" = "#7f7f7f", "Dérive" = "#2ca02c", "ETS" = "#9467bd", "ARIMA" = "#ff7f0e")
    p <- plot_ly() |> add_trace(data = o, x = ~annee, y = ~valeur, type = "scatter", mode = "lines+markers", name = "Réalité",
                                line = list(color = "#111", width = 3), marker = list(size = 8, color = "#111"))
    for (m in names(cols)) {
      bm <- bind_rows(o |> filter(annee == 2019) |> transmute(annee, pred = valeur), b |> filter(modele == m) |> select(annee, pred))
      p <- p |> add_trace(data = bm, x = ~annee, y = ~pred, type = "scatter", mode = "lines+markers", name = m,
                          line = list(color = cols[[m]], dash = "dash", width = if (m == "Dérive") 3.5 else 2), marker = list(color = cols[[m]]))
    }
    p |> layout(shapes = list(list(type = "rect", x0 = 2014.6, x1 = 2019.5, y0 = 0, y1 = 1, yref = "paper", fillcolor = "#999", opacity = 0.12, line = list(width = 0))),
                xaxis = list(title = "", dtick = 1), yaxis = list(title = "Eau prélevée en France (Mds m³)"), legend = list(orientation = "h", x = 0, y = -0.12))
  })
  output$mae <- renderPlotly({
    s <- d$prev_fr$scores |> arrange(desc(MAE)) |> mutate(modele = factor(modele, modele))
    plot_ly(s, y = ~modele, x = ~MAE, type = "bar", orientation = "h",
            marker = list(color = ifelse(as.character(s$modele) == d$prev_fr$choix, "#2ca02c", "#c7c7c7")),
            text = ~fmt(MAE, 2), textposition = "outside", hoverinfo = "none") |>
      layout(xaxis = list(title = "Erreur moyenne (Mds m³)", range = c(0, 3)), yaxis = list(title = ""))
  })
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
    cl <- d$classes |> mutate(etiq = ifelse(n_dc >= 60 | nom_classe == "Pays très secs" | part_central >= 0.25 |
                                            country %in% c("Inde", "Chine", "Indonésie", "Maroc", "Pakistan"), country, ""))
    plot_ly(cl, x = ~dim1, y = ~dim2, color = ~nom_classe, colors = col_classes, type = "scatter", mode = "markers+text",
            text = ~etiq, textposition = "top center", textfont = list(size = 10, color = "#333"), marker = list(size = 11, opacity = 0.8),
            hovertext = ~paste0("<b>", country, "</b><br>", nom_classe), hoverinfo = "text") |>
      layout(xaxis = list(title = "← petits pays          grands pays →", zeroline = TRUE),
             yaxis = list(title = "← pays secs          eau et richesse →", zeroline = TRUE),
             legend = list(orientation = "h", x = 0, y = -0.22))
  })

  # ---------- Sources
  tab <- reactive(res() |> transmute(Pays = country, `Data centers` = n_dc, `Électricité (TWh)` = round(e_h, 2),
    `Eau (Mm³)` = round(eau, 2), `Part de l'eau du pays (%)` = signif(part, 3), `Stress hydrique (%)` = round(stress0, 1),
    `Sites en région sèche (%)` = round(pct_sites_region_stress, 1)) |> arrange(desc(`Part de l'eau du pays (%)`)))
  output$tab_pays <- renderDT(datatable(tab(), rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)))
  output$dl <- downloadHandler(filename = function() paste0("eau_datacenters_", hz(), ".csv"),
                               content = function(f) write.csv(tab(), f, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
