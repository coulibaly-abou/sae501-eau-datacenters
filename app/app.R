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
sf::sf_use_s2(FALSE)
CENTRES <- {
  pt <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(d$monde)))
  base <- tibble(iso3 = d$monde$iso3, lon = pt[, 1], lat = pt[, 2]) |> distinct(iso3, .keep_all = TRUE)
  med <- d$osm |> group_by(iso3) |> summarise(lon_s = median(lon), lat_s = median(lat), n = n(), .groups = "drop")
  base |> left_join(med, by = "iso3") |> transmute(iso3, lon = ifelse(!is.na(n) & n >= 3, lon_s, lon), lat = ifelse(!is.na(n) & n >= 3, lat_s, lat))
}
sf::sf_use_s2(TRUE)
CHOIX_PAYS <- c("Monde entier" = "MONDE", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)])

# ------------------------------------------------------------- Constantes ----
PISCINE <- 2500          # m3, bassin olympique
KM_KG <- 0.2             # ordre de grandeur : kg de CO2 par km en voiture thermique
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
avec_unite <- function(x, u) paste0(grand(x), ifelse(!is.na(x) & x >= 1e6, " de ", " "), u)

calculer <- function(w, scenario, horizon) {
  p <- d$pays
  if (horizon <= 2024) p$f <- 1 else {
    fz <- d$scen_zone |> filter(scenario == !!scenario) |> select(zone, horizon, f_zone) |>
      pivot_wider(names_from = horizon, values_from = f_zone, names_prefix = "f")
    fz$f <- if (horizon <= 2030) 1 + (fz$f2030 - 1) * (horizon - 2024) / 6 else fz$f2030 + (fz$f2035 - fz$f2030) * (horizon - 2030) / 5
    p <- p |> left_join(fz |> select(zone, f), by = "zone")
  }
  p |> mutate(e_h = e0 * f,
              e_ia = d$part_ia[["monde"]] * e0 + d$part_ia[["hausse"]] * (e_h - e0),
              eau = e_h * w, eau_ia = e_ia * w,
              part = 100 * eau / (prelev0 * 1e3),
              part_bas = 100 * e_h * wue[["bas"]] / (prelev0 * 1e3),
              part_haut = 100 * e_h * wue[["haut"]] / (prelev0 * 1e3),
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
    /* Carte : parcours en 4 étapes */
    .parcours .shiny-options-group{display:flex;flex-wrap:wrap;gap:6px;}
    .parcours .radio-inline, .parcours .form-check-inline{margin:0 !important;padding:0 !important;}
    .parcours input[type=radio]{position:absolute;opacity:0;pointer-events:none;}
    .parcours label span, .parcours .form-check-label{display:inline-block;padding:.42rem .85rem;border-radius:10px;border:2px solid #1f6f8b;font-size:.92rem;white-space:nowrap;
      color:#1f6f8b;font-weight:600;cursor:pointer;background:#fff;transition:all .15s;}
    .parcours input:checked + span, .parcours input:checked + .form-check-label{background:#1f6f8b;color:#fff;box-shadow:0 3px 10px rgba(31,111,139,.3);}
    .etape-texte{display:flex;gap:12px;align-items:flex-start;background:#f4f8fa;border-left:4px solid #1f6f8b;border-radius:8px;
      padding:10px 14px;margin:10px 0 12px;font-size:.98rem;}
    .etape-num{flex:0 0 32px;height:32px;border-radius:50%;background:#1f6f8b;color:#fff;font-weight:800;display:flex;align-items:center;justify-content:center;}
    .leg-bulle{background:rgba(255,255,255,.9);padding:6px 10px;border-radius:6px;font-size:.8rem;line-height:1.2;}
    .leg-bulle .rond{display:inline-block;width:14px;height:14px;border-radius:50%;background:#1f4e8b;opacity:.7;vertical-align:middle;}
    .etiq-surv{font-weight:800;color:#c0392b;text-shadow:0 0 3px #fff,0 0 3px #fff;font-size:12px;}
    /* Fiche */
    .chiffres-fiche{display:grid;grid-template-columns:repeat(3,1fr);gap:8px;margin-bottom:14px;text-align:center;font-size:.8rem;color:#666;}
    .chiffres-fiche .n{font-size:1.25rem;font-weight:800;color:#1f6f8b;}
    .fiche-sous{font-weight:600;font-size:.85rem;color:#444;margin:10px 0 6px;}
    .jauge-titre{font-weight:600;font-size:.85rem;color:#444;margin-bottom:6px;}
    .jauge-barre{position:relative;display:flex;height:12px;border-radius:6px;overflow:visible;margin:22px 0 4px;}
    .jauge-barre .z1{flex:0 0 50%;background:#a6d96a;border-radius:6px 0 0 6px;} .jauge-barre .z2{flex:0 0 25%;background:#fee08b;}
    .jauge-barre .z3{flex:0 0 25%;background:#f46d43;border-radius:0 6px 6px 0;}
    .jauge-curseur{position:absolute;top:-6px;width:4px;height:24px;background:#123f52;border-radius:2px;transform:translateX(-2px);}
    .jauge-curseur span{position:absolute;top:-20px;left:50%;transform:translateX(-50%);font-weight:800;font-size:.85rem;color:#123f52;white-space:nowrap;}
    .jauge-lib{display:flex;justify-content:space-between;font-size:.72rem;color:#888;}
    .litre{display:grid;grid-template-columns:90px 1fr 62px;align-items:center;gap:6px;font-size:.82rem;margin:3px 0;}
    .litre .barre{background:#eef2f4;border-radius:4px;height:10px;} .litre .barre div{height:10px;border-radius:4px;background:#1f6f8b;}
    .litre .val{text-align:right;font-weight:600;} .litre.dc .barre div{background:#d35400;} .litre.dc{color:#d35400;font-weight:700;}
    .mode-emploi div{margin:6px 0;font-size:.9rem;} .mode-emploi .bi{color:#1f6f8b;}
    .comparaison table{font-size:.82rem;}
    .sep-vert{width:1px;height:28px;background:#cfd8dc;}
    .parcours .shiny-input-container{margin-bottom:0 !important;width:auto !important;}
    .parcours .selectize-control{min-width:210px;}
    .curseur-mois .irs-grid, .curseur-mois .irs-min, .curseur-mois .irs-max, .curseur-mois .irs-single{display:none;}
    .tuile.petite{padding:10px 8px;} .tuile.petite .gros{font-size:1.35rem;} .tuile.petite .bi{font-size:1.3rem;}
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
    sliderInput("horizon", "Année", min = 2024, max = 2035, value = 2024, step = 1, sep = "", ticks = FALSE,
                animate = animationOptions(interval = 2200, loop = FALSE)),
    conditionalPanel("input.horizon > 2024",
      radioButtons("scen", "Croissance des data centers", c("Lente" = "bas", "Centrale" = "central", "Rapide" = "haut"), "central")),
    radioButtons("coef", "Eau utilisée par kWh", c("Basse : 0,21 L" = "bas", "Moyenne : 0,81 L" = "central", "Haute : 2,41 L" = "haut"), "central"),
    div(class = "small text-muted", "▶ fait défiler les années. Entre 2024, 2030 et 2035 (scénarios AIE), les valeurs sont interpolées.")),

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
          span(class = "titre-bloc", "La carte de l'étude en 4 étapes"),
          selectInput("pays_sel", NULL, width = "210px", CHOIX_PAYS, "MONDE")),
        div(class = "parcours px-3 pt-3",
          radioButtons("etape", NULL, inline = TRUE, selected = "1",
            c("① Où sont-ils ?" = "1", "② Combien d'eau ?" = "2", "③ Où l'eau manque ?" = "3", "④ Où ça coince ?" = "4"))),
        div(class = "px-3", uiOutput("etape_texte")),
        leafletOutput("carte", height = "520px")),
      card(card_header(class = "d-flex justify-content-between align-items-center",
                       span(class = "titre-bloc", bs_icon("card-text"), " Fiche"),
                       selectInput("pays_comp", NULL, width = "170px", c("Comparer avec…" = "", CHOIX_PAYS[-1]))),
           uiOutput("fiche"),
           conditionalPanel("input.pays_sel != 'MONDE'",
             div(class = "px-3 pb-2", div(class = "fiche-sous", "Pluie et température, mois par mois"),
                 plotlyOutput("fiche_clim", height = "150px"))))
    )
  ),

  # ---- Régions
  nav_panel("Régions", icon = bs_icon("geo-alt"),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "Où se concentrent les data centers, et où l'eau manque-t-elle ?"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              "Densité : estimation par noyau gaussien (méthode non paramétrique) sur les sites OpenStreetMap (20/09/2026). Les contours entourent les zones qui regroupent les 50 %, 25 % et 10 % de sites les plus concentrés. Couleur des points : manque d'eau de la région (WRI Aqueduct ; 2030 et 2050 : scénario tendanciel)."),
            selectInput("pays_reg", NULL, width = "210px",
                        c("Monde entier" = "MONDE", setNames(sort(unique(d$osm$iso3)), d$osm$pays[match(sort(unique(d$osm$iso3)), d$osm$iso3)])), "MONDE"))),
        div(class = "parcours d-flex flex-wrap align-items-center column-gap-4 px-3 pt-3",
          radioButtons("vue_reg", NULL, inline = TRUE, c("Densité" = "densite", "Sites" = "points"), "densite"),
          span(class = "sep-vert"),
          radioButtons("horiz_reg", NULL, inline = TRUE, c("Aujourd'hui" = "stress_region", "2030" = "stress_2030", "2050" = "stress_2050"), "stress_region")),
        div(class = "px-3", uiOutput("reg_texte")),
        leafletOutput("carte_reg", height = "520px")),
      card(card_header(span(class = "titre-bloc", textOutput("reg_titre", inline = TRUE))),
           uiOutput("reg_chiffres"),
           plotlyOutput("reg_barres", height = "430px"),
           card_footer(class = "small text-muted", "Survolez une barre : nombre de sites, manque d'eau, risque de sécheresse (0 à 5)."))
    )
  ),

  # ---- Saisons
  nav_panel("Saisons", icon = bs_icon("sun"),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "L'effet ciseaux : besoin de froid en hausse, pluie en baisse"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              "Barres bleues : pluie moyenne du mois (normales 1991-2020, Banque mondiale CCKP). Courbe rouge : besoin de refroidissement, mesuré en degrés-jours au-dessus de 18 °C (indicateur standard de climatisation), calculé sur la température moyenne du mois. En orange : mois à la fois plus chauds et plus secs que la moyenne."),
            selectInput("pays_clim", NULL, width = "210px", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)], "FRA"))),
        uiOutput("clim_chiffres"),
        plotlyOutput("clim", height = "330px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("clim_retenir", inline = TRUE))),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span(class = "titre-bloc", "La tension voyage avec les saisons"),
          tooltip(bs_icon("info-circle", class = "aide"),
            "Pour le mois choisi, chaque pays est comparé à sa propre moyenne de l'année : plus chaud ? plus sec ? Appuyez sur ▶ pour faire défiler les mois.")),
        div(class = "px-3 pt-2 curseur-mois",
          sliderInput("mois_carte", NULL, min = 1, max = 12, value = 7, step = 1, ticks = FALSE, width = "100%",
                      animate = animationOptions(interval = 1600, loop = TRUE))),
        div(class = "px-3 text-center fw-bold", textOutput("mois_nom", inline = TRUE)),
        leafletOutput("carte_mois", height = "380px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("mois_retenir", inline = TRUE)))
    ),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        span(class = "titre-bloc", "Calendrier de la tension : les 20 pays qui ont le plus de data centers"),
        tooltip(bs_icon("info-circle", class = "aide"),
          "Pays classés du nord au sud. Rouge : mois chaud et sec ; orange clair : chaud ; jaune pâle : sec ; bleu : ni l'un ni l'autre (par rapport à la moyenne du pays).")),
      plotlyOutput("calendrier", height = "520px"),
      card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ",
        "Au nord, la tension tombe surtout de mai à septembre ; au sud (Brésil, Afrique du Sud, Australie), plutôt de septembre à novembre, avant la saison des pluies. Une moyenne annuelle cache ces pics."))
  ),

  # ---- Usages de l'eau (arbitrages)
  nav_panel("Usages", icon = bs_icon("signpost-split"),
    card(card_header(class = "d-flex justify-content-between align-items-center",
           span(class = "titre-bloc", "Sur 10 000 litres d'eau prélevés, qui les utilise ?"),
           tooltip(bs_icon("info-circle", class = "aide"), "Prélèvements d'eau des 132 pays étudiés (FAO AQUASTAT, Rapport 1) et eau des data centers estimée (Rapport 2), selon les hypothèses choisies à gauche.")),
         card_body(uiOutput("litres")),
         card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("litres_retenir", inline = TRUE))),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span(class = "titre-bloc", "À quoi correspond un data center dans la vie de tous les jours ?"),
          tooltip(bs_icon("info-circle", class = "aide"),
            "Un an de fonctionnement. Électricité : 3,77 GWh par MW de puissance informatique installée (moyenne observée dans le rapport de la Commission européenne, données 2024). Eau : litres par kWh choisis à gauche. Équivalents (ordres de grandeur) : foyer français ≈ 4,5 MWh/an ; habitant ≈ 65 m³/an (Arcep) ; douche ≈ 60 L ; piscine olympique = 2 500 m³ ; 1 kg de bœuf ≈ 15 400 L d'eau (empreinte eau, Water Footprint Network, pluie comprise) ; voiture ≈ 0,2 kg de CO₂ par km.")),
        div(class = "parcours d-flex flex-wrap align-items-center column-gap-4 px-3 pt-3",
          radioButtons("mw", NULL, inline = TRUE, c("1 MW (petit)" = 1, "10 MW (moyen)" = 10, "100 MW (grand campus IA)" = 100), 10),
          div(class = "d-flex align-items-center gap-2", span(class = "small text-muted", "Électricité de :"),
              selectInput("mix", NULL, width = "220px", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)][!is.na(d$pays$gco2_kwh[order(d$pays$country)])], "FRA"))),
        card_body(uiOutput("conv")),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("conv_retenir", inline = TRUE))),
      bloc("D'où vient vraiment l'eau des data centers ?", "directe",
           "Data centers suivis par l'Arcep en France (2024) : 0,575 million de m³ prélevés sur place pour le refroidissement, 6,5 millions de m³ en comptant l'eau utilisée pour produire leur électricité.",
           "9 % sur place, 91 % dans les centrales : le levier principal, c'est l'électricité (son mix et son efficacité).", "330px")),
    layout_columns(col_widths = c(6, 6),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "Les autres usages évoluent aussi"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              "Prélèvements d'eau par usage (FAO AQUASTAT, années disponibles) et eau des data centers estimée de 2024 à 2035 (bande : croissance lente à rapide). Échelle logarithmique pour voir les deux ordres de grandeur."),
            selectInput("pays_us", NULL, width = "190px", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)], "FRA"))),
        plotlyOutput("tendance", height = "380px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("tendance_retenir", inline = TRUE))),
      bloc("Où les data centers peuvent-ils concurrencer les habitants ?", "arb",
           "Chaque bulle est un pays (taille = nombre de sites). À droite : beaucoup de sites dans des régions qui manquent d'eau. En haut : l'eau des data centers pèse face à l'eau des habitants. Zone rouge : les deux à la fois.",
           textOutput("arb_retenir", inline = TRUE), "380px")),
    bloc("Mesures réelles : l'eau consommée par chaque site Google en 2025", "carte_google",
         "Eau consommée par site, publiée par Google (rapport environnemental 2026). Taille = volume ; couleur = manque d'eau de la région.",
         "Un seul site peut utiliser l'eau d'une ville de 50 000 habitants : à l'échelle locale, l'arbitrage est réel.", "380px", leafletOutput)
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
    if (!is.null(input$pays_us) && v %in% d$pays$iso3 && input$pays_us != v) updateSelectInput(session, "pays_us", selected = v)
  }
  observeEvent(input$pays_sel, synchro(input$pays_sel), ignoreInit = TRUE)
  observeEvent(input$pays_reg, if (input$pays_reg != "MONDE") synchro(input$pays_reg), ignoreInit = TRUE)
  observeEvent(input$pays_clim, synchro(input$pays_clim), ignoreInit = TRUE)
  observeEvent(input$pays_us, synchro(input$pays_us), ignoreInit = TRUE)
  observeEvent(input$carte_shape_click, {
    id <- input$carte_shape_click$id
    if (!is.null(id) && id %in% d$pays$iso3) updateSelectInput(session, "pays_sel", selected = id)
  })

  # ---------- Carte du monde : parcours en 4 étapes
  bins <- list(part = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1, Inf), n_dc = c(0, 5, 20, 50, 100, 300, Inf),
               stress0 = c(0, 25, 50, 75, 100, Inf))
  titre_leg <- c(n_dc = "Nombre de data centers", part = "Part de l'eau du pays<br>utilisée par les data centers",
                 stress0 = "Manque d'eau du pays<br>(stress hydrique)")
  lab_bins <- list(n_dc = c("1 – 5", "5 – 20", "20 – 50", "50 – 100", "100 – 300", "300 et plus"),
                   part = c("moins de 0,001 %", "0,001 – 0,01 %", "0,01 – 0,05 %", "0,05 – 0,1 %", "0,1 – 0,5 %", "0,5 – 1 %", "plus de 1 %"),
                   stress0 = c("aucun (< 25 %)", "faible (25-50 %)", "moyen (50-75 %)", "élevé (75-100 %)", "critique (> 100 %)"))
  pal_v <- c(n_dc = "Purples", part = "YlGnBu", stress0 = "YlOrRd")
  var_etape <- reactive(switch(input$etape, "1" = "n_dc", "2" = "part", "stress0"))

  output$carte <- renderLeaflet({
    v <- var_etape(); iso <- input$pays_sel; et <- input$etape
    r <- res()
    m <- d$monde |> left_join(r |> mutate(val = .data[[v]]) |> select(iso3, country, val, n_dc, eau, part, part_bas, part_haut, stress0),
                              by = "iso3")
    pal <- colorBin(pal_v[[v]], domain = NULL, bins = bins[[v]], na.color = "#c9c9c9")
    dans <- m$iso3 %in% d$pays$iso3
    remplissage <- ifelse(!dans, "#f4f4f4", ifelse(is.na(m$val), "#c9c9c9", pal(m$val)))
    surv <- arb() |> filter(signaux >= 2) |> pull(iso3)
    opa <- if (iso != "MONDE") 0.45 else if (et == "4") ifelse(m$iso3 %in% surv, 0.9, 0.35) else 0.85
    lab <- ifelse(!dans, paste0("<b>", coalesce(m$country, ""), "</b>Aucun data center recensé"),
      paste0("<b>", m$country, "</b><br>", m$n_dc, " data centers",
             "<br>Eau utilisée : ", fmt(m$eau, 2), " millions de m³",
             "<br>Part de l'eau du pays : ", pct(m$part), " <span style='color:#888'>(entre ", pct(m$part_bas), " et ", pct(m$part_haut), ")</span>",
             "<br>Manque d'eau du pays : ", ifelse(is.na(m$stress0), "n.d.", paste0(fmt(m$stress0, 0), " %")),
             "<br><i>Cliquez pour zoomer</i>")) |> lapply(htmltools::HTML)
    l <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1, zoomSnap = 0.25)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(data = m, layerId = m$iso3, fillColor = remplissage, fillOpacity = opa, color = "white", weight = 0.6, label = lab,
                  highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)) |>
      addLegend("bottomleft", colors = c(pal(head(bins[[v]], -1) + 1e-9), "#c9c9c9", "#f4f4f4"),
                labels = c(lab_bins[[v]], "donnée manquante", "aucun data center"), title = htmltools::HTML(titre_leg[[v]]), opacity = 0.9)

    # Étapes 3 et 4 : bulles = eau utilisée par les data centers (la demande) sur la couleur du manque d'eau (la ressource)
    if (et %in% c("3", "4") && iso == "MONDE") {
      b <- r |> filter(eau > 0) |> inner_join(CENTRES, by = "iso3") |> arrange(desc(eau))
      l <- l |>
        addCircleMarkers(lng = b$lon, lat = b$lat, radius = pmin(42, 3 + 2.4 * sqrt(b$eau)), stroke = TRUE, color = "#fff", weight = 1,
                         fillColor = "#1f4e8b", fillOpacity = 0.6, layerId = paste0("b_", b$iso3),
                         label = lapply(paste0("<b>", b$country, "</b><br>Eau des data centers : ", fmt(b$eau, 1), " millions de m³<br>",
                                               "Manque d'eau du pays : ", ifelse(is.na(b$stress0), "n.d.", paste0(fmt(b$stress0, 0), " %"))), htmltools::HTML)) |>
        addControl(htmltools::HTML("<div class='leg-bulle'><span class='rond'></span> Taille de la bulle =<br>eau utilisée par les data centers</div>"),
                   position = "topright")
    }
    # Étape 4 : pays à surveiller entourés en rouge et nommés
    if (et == "4" && iso == "MONDE" && length(surv)) {
      g <- d$monde[d$monde$iso3 %in% surv, ]
      ce <- CENTRES |> filter(iso3 %in% surv) |> left_join(r |> select(iso3, country), by = "iso3")
      l <- l |> addPolygons(data = g, fill = FALSE, color = "#c0392b", weight = 3, opacity = 1) |>
        addLabelOnlyMarkers(lng = ce$lon, lat = ce$lat, label = ce$country,
                            labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, className = "etiq-surv"))
    }
    if (iso == "MONDE") return(l |> setView(5, 25, 1.75))

    # Zoom sur un pays : régions (bulles) + chaque data center (points)
    g <- d$monde[d$monde$iso3 == iso, ]; o <- d$osm |> filter(iso3 == iso); rg <- d$regions |> filter(iso3 == iso)
    palr <- colorFactor(col_aq, levels = d$lab_aq, na.color = "#777777")
    if (nrow(g)) l <- l |> addPolygons(data = g, fill = FALSE, color = "#111", weight = 3, opacity = 1)
    if (nrow(rg)) l <- l |>
      addCircleMarkers(lng = rg$lon, lat = rg$lat, radius = pmin(45, 8 + 3 * sqrt(rg$sites)), stroke = TRUE, weight = 2,
                       color = palr(rg$stress_region), fillColor = palr(rg$stress_region), fillOpacity = 0.25,
                       label = lapply(paste0("<b>", rg$region, "</b><br>", rg$sites, " data centers<br>Manque d'eau : ", rg$stress_region), htmltools::HTML))
    if (nrow(o)) l <- l |>
      addCircleMarkers(lng = o$lon, lat = o$lat, radius = 4, color = "#222", weight = 1, fillOpacity = 0.95, fillColor = palr(o$stress_region),
                       label = lapply(paste0("<b>", o$nom, "</b><br>", coalesce(o$region, ""), "<br>Manque d'eau de la région : ",
                                             coalesce(as.character(o$stress_region), "n.d.")), htmltools::HTML)) |>
      addLegend("bottomright", colors = col_aq[1:5], labels = d$lab_aq[1:5], opacity = 0.9,
                title = htmltools::HTML("Manque d'eau de la région<br><small>grand cercle = région · point = site</small>"))
    if (nrow(o) >= 2) {
      lo <- quantile(o$lon, c(0.02, 0.98)); la <- quantile(o$lat, c(0.02, 0.98))
      dx <- max(1, diff(lo) * 0.15); dy <- max(1, diff(la) * 0.15)
      l |> fitBounds(lo[[1]] - dx, la[[1]] - dy, lo[[2]] + dx, la[[2]] + dy)
    } else if (nrow(g)) {
      bb <- sf::st_bbox(g)
      if (bb[["xmax"]] - bb[["xmin"]] < 120) l |> fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      else { ce <- CENTRES |> filter(iso3 == iso); l |> setView(ce$lon[1], ce$lat[1], 3) }
    } else l |> setView(5, 25, 1.75)
  })
  observeEvent(input$carte_marker_click, {
    id <- input$carte_marker_click$id
    if (!is.null(id) && startsWith(id, "b_")) updateSelectInput(session, "pays_sel", selected = sub("^b_", "", id))
  })

  # Texte de l'étape (une ou deux phrases, chiffres à jour)
  output$etape_texte <- renderUI({
    r <- res(); p <- pres(); et <- input$etape
    boite <- function(...) div(class = "etape-texte", span(class = "etape-num", et), div(...))
    if (input$pays_sel != "MONDE") {
      x <- r |> filter(iso3 == input$pays_sel); rg <- d$regions |> filter(iso3 == input$pays_sel) |> arrange(-sites)
      return(div(class = "etape-texte", span(class = "etape-num", bs_icon("zoom-in")), div(
        tags$b(paste0(x$country, " : ")), if (nrow(rg)) paste0(x$n_dc, " data centers recensés ; sur la carte, ", sum(rg$sites), " bâtiments géolocalisés (OpenStreetMap) dans ", nrow(rg), " régions. ",
          "La plus équipée : ", rg$region[1], " (", rg$sites[1], " sites, manque d'eau ", tolower(sub(" \\(.*", "", rg$stress_region[1])), ").")
          else "pas de site géolocalisé.", " ", actionLink("retour_monde", "Revenir au monde"))))
    }
    top5 <- r |> slice_max(n_dc, n = 5)
    switch(et,
      "1" = boite(tags$b(paste0(fmt(sum(r$n_dc), 0), " data centers dans ", nrow(r), " pays. ")),
                  paste0("Plus de la moitié (", fmt(100 * sum(top5$n_dc) / sum(r$n_dc), 0), " %) sont dans 5 pays : ", paste(top5$country, collapse = ", "), ".")),
      "2" = { mx <- p |> filter(n_dc >= 5) |> slice_max(part, n = 1)
              boite(tags$b(paste0(fmt(sum(r$eau), 0), " millions de m³ d'eau ", quand(), ". ")),
                    paste0("Dans ", fmt(100 * mean(p$part < 0.1, na.rm = TRUE), 0), " % des pays, c'est moins de 0,1 % de l'eau du pays. Le plus exposé : ",
                    mx$country, " (", pct(mx$part), ").")) },
      "3" = boite(tags$b("Bulles = demande (eau des data centers), couleur = ressource (manque d'eau). "),
                  paste0("Les grosses bulles sont surtout sur des pays jaunes : ",
                  fmt(100 * sum(r$n_dc[!is.na(r$stress0) & r$stress0 < 50]) / sum(r$n_dc), 0),
                  " % des sites sont dans des pays où l'eau ne manque pas.")),
      "4" = { s <- arb() |> filter(signaux >= 2) |> arrange(-signaux)
              boite(tags$b(paste0(nrow(s), " pays à surveiller, entourés en rouge : ")), paste0(paste(s$country, collapse = ", "), ". "),
                    span(class = "text-muted", "Ils cumulent au moins 2 signaux : eau des data centers ≥ 1 % de celle des habitants, ",
                         "≥ 25 % des sites en région sèche, pays déjà en stress (≥ 50 %).")) })
  })
  observeEvent(input$retour_monde, updateSelectInput(session, "pays_sel", selected = "MONDE"))

  # ---------- Fiche (jauge, « sur 10 000 litres », comparaison)
  jauge <- function(v) {
    if (is.na(v)) return(div(class = "text-muted small", "Part de l'eau : donnée manquante"))
    pos <- max(0, min(100, (log10(max(v, 1e-4)) + 3) / 4 * 100))
    div(class = "jauge",
      div(class = "jauge-titre", "Part de l'eau du pays utilisée par les data centers"),
      div(class = "jauge-barre", div(class = "z1"), div(class = "z2"), div(class = "z3"),
          div(class = "jauge-curseur", style = paste0("left:", pos, "%;"), span(pct(v)))),
      div(class = "jauge-lib", span("faible"), span("à suivre (0,1 %)"), span("vigilance (1 %)")))
  }
  litres_pays <- function(x) {
    if (any(is.na(c(x$pct_agri, x$pct_indus, x$pct_menages, x$prelev0)))) return(NULL)
    tot <- x$pct_agri + x$pct_indus + x$pct_menages
    v <- c(Agriculture = x$pct_agri, Industrie = x$pct_indus, Habitants = x$pct_menages) / tot * 1e4
    dc <- 1e4 * x$eau / (x$prelev0 * 1e3)
    ligne <- function(nom, val, cls = "") div(class = paste("litre", cls), span(class = "nom", nom),
      div(class = "barre", div(style = paste0("width:", max(0.8, val / 100), "%;"))), span(class = "val", paste(fmt(val, ifelse(val < 10, 1, 0)), "L")))
    div(class = "litres", div(class = "fiche-sous", "Sur 10 000 litres prélevés dans ce pays"),
        ligne("Agriculture", v[["Agriculture"]]), ligne("Industrie", v[["Industrie"]]), ligne("Habitants", v[["Habitants"]]),
        ligne("Data centers", dc, "dc"))
  }
  output$fiche <- renderUI({
    r <- res()
    if (input$pays_sel == "MONDE") {
      p <- pres() |> filter(n_dc >= 5) |> slice_max(part, n = 1)
      return(card_body(class = "fiche", h4("Monde"), p(class = "text-muted small mb-2", quand()),
        div(class = "chiffres-fiche",
          div(div(class = "n", fmt(sum(r$n_dc), 0)), "data centers"),
          div(div(class = "n", fmt(sum(r$eau), 0)), "millions de m³"),
          div(div(class = "n", pct(p$part)), p$country)),
        div(class = "mode-emploi",
          div(bs_icon("1-circle"), " Suivez les 4 étapes au-dessus de la carte."),
          div(bs_icon("2-circle"), " Cliquez sur un pays (ou une bulle) pour zoomer."),
          div(bs_icon("3-circle"), " Faites défiler les années avec ▶ dans le panneau de gauche."))))
    }
    x <- r |> filter(iso3 == input$pays_sel)
    comp <- NULL
    if (!is.null(input$pays_comp) && input$pays_comp != "" && input$pays_comp != input$pays_sel) {
      y <- r |> filter(iso3 == input$pays_comp)
      lig <- function(t, a, b) tags$tr(tags$td(t), tags$td(a), tags$td(b))
      comp <- div(class = "comparaison", div(class = "fiche-sous", "Comparaison"),
        tags$table(class = "table table-sm mb-0",
          tags$thead(tags$tr(tags$th(""), tags$th(x$country), tags$th(y$country))),
          tags$tbody(
            lig("Data centers", x$n_dc, y$n_dc),
            lig("Eau (Mm³)", fmt(x$eau, 1), fmt(y$eau, 1)),
            lig("Part de l'eau", pct(x$part), pct(y$part)),
            lig("Manque d'eau", paste0(fmt(x$stress0, 0), " %"), paste0(fmt(y$stress0, 0), " %")),
            lig("Sites en région sèche", pct(x$pct_sites_region_stress), pct(y$pct_sites_region_stress)))))
    }
    card_body(class = "fiche", h4(x$country), p(class = "text-muted small mb-2", quand()),
      div(class = "chiffres-fiche",
        div(div(class = "n", x$n_dc), "data centers"),
        div(div(class = "n", fmt(x$eau, 1)), "millions de m³"),
        div(div(class = "n", grand(x$eau * 1e6 / HAB_M3)), "habitants (équivalent)")),
      jauge(x$part), litres_pays(x), comp)
  })
  output$fiche_clim <- renderPlotly({
    req(input$pays_sel != "MONDE")
    c <- d$climat |> filter(iso3 == input$pays_sel) |> arrange(mois)
    if (nrow(c) == 0) return(plotly_empty())
    sh <- lapply(c$mois[c$sensible], function(i) list(type = "rect", xref = "x", yref = "paper", x0 = i - 0.5, x1 = i + 0.5,
                                                       y0 = 0, y1 = 1, fillcolor = "#f0ad4e", opacity = 0.3, line = list(width = 0), layer = "below"))
    plot_ly(c, x = ~mois) |>
      add_bars(y = ~pr, marker = list(color = "#9ecae1"), hovertext = ~paste0(mois_fr[mois], " : ", fmt(pr, 0), " mm"), hoverinfo = "text", showlegend = FALSE) |>
      add_lines(y = ~tas, yaxis = "y2", line = list(color = "#d62728", width = 2.5), hovertext = ~paste0(mois_fr[mois], " : ", fmt(tas, 1), " °C"),
                hoverinfo = "text", showlegend = FALSE) |>
      layout(shapes = sh, margin = list(l = 30, r = 30, t = 5, b = 25),
             xaxis = list(title = "", tickvals = 1:12, ticktext = substr(mois_fr, 1, 1), tickfont = list(size = 10)),
             yaxis = list(title = "", tickfont = list(size = 9)), yaxis2 = list(title = "", overlaying = "y", side = "right", showgrid = FALSE, tickfont = list(size = 9)))
  })

  # ---------- Régions : densité, points chauds, futur, disparités
  osm_sel <- reactive(if (input$pays_reg == "MONDE") d$osm else d$osm |> filter(iso3 == input$pays_reg))
  reg_sel <- reactive(if (input$pays_reg == "MONDE") d$regions else d$regions |> filter(iso3 == input$pays_reg))
  nom_hor <- reactive(c(stress_region = "aujourd'hui", stress_2030 = "en 2030", stress_2050 = "en 2050")[[input$horiz_reg]])
  sec <- function(x) as.character(x) %in% d$lab_aq[4:5]

  # Estimation de densité par noyau (MASS::kde2d) et zones de plus forte densité
  dens <- reactive({
    o <- osm_sel() |> filter(!is.na(lat), !is.na(lon))
    if (nrow(o) < 15) return(NULL)
    monde <- input$pays_reg == "MONDE"
    # kde2d utilise h/4 comme écart-type : monde = 0,75° (~80 km) ; pays = règle de référence normale / 2
    h <- if (monde) c(3, 3) else pmin(3, pmax(0.4, c(MASS::bandwidth.nrd(o$lon), MASS::bandwidth.nrd(o$lat)) / 2))
    lims <- c(range(o$lon) + c(-1, 1) * h[1], range(o$lat) + c(-1, 1) * h[2])
    k <- MASS::kde2d(o$lon, o$lat, h = h, n = if (monde) c(500, 250) else c(160, 160), lims = lims)
    zp <- k$z[cbind(findInterval(o$lon, k$x, all.inside = TRUE), findInterval(o$lat, k$y, all.inside = TRUE))]
    niv <- quantile(zp, c(0.5, 0.75, 0.9))
    list(k = k, niv = niv, o = o |> mutate(densite = zp, dense = zp >= niv[[2]]), sd_km = round(h[1] / 4 * 111))
  })

  output$carte_reg <- renderLeaflet({
    o <- osm_sel() |> filter(!is.na(.data[[input$horiz_reg]]))
    req(nrow(o) > 0)
    st <- o[[input$horiz_reg]]
    pal <- colorFactor(col_aq, levels = d$lab_aq)
    l <- leaflet() |> addProviderTiles(providers$Esri.WorldGrayCanvas)
    ds <- dens()
    if (input$vue_reg == "densite" && !is.null(ds)) {
      cols <- c("#fdd0a2", "#fd8d3c", "#a63603")
      for (i in 1:3) {
        cl <- contourLines(ds$k$x, ds$k$y, ds$k$z, levels = ds$niv[[i]])
        for (c1 in cl) l <- l |> addPolygons(lng = c1$x, lat = c1$y, fillColor = cols[i], fillOpacity = 0.35, color = cols[i], weight = 1.2, opacity = 0.9)
      }
      l <- l |> addLegend("topright", colors = cols, opacity = 0.8, title = "Concentration des sites",
                          labels = c("zone des 50 % les plus concentrés", "zone des 25 %", "zone des 10 %"))
    }
    l <- l |> addCircleMarkers(lng = o$lon, lat = o$lat, radius = if (input$pays_reg == "MONDE") 2.5 else 4.5, stroke = TRUE, weight = 0.5, color = "#333",
                               fillColor = pal(st), fillOpacity = if (input$vue_reg == "densite") 0.75 else 0.9,
                               label = lapply(paste0("<b>", o$nom, "</b><br>", o$region, " (", o$pays, ")<br>Manque d'eau ", nom_hor(), " : ", st), htmltools::HTML))
    # Points chauds : régions avec beaucoup de sites dans une zone dense ET une région qui manque d'eau
    hs <- chauds()
    if (nrow(hs)) l <- l |>
      addCircleMarkers(lng = hs$lon, lat = hs$lat, radius = 16, fill = FALSE, color = "#c0392b", weight = 3, opacity = 1) |>
      addLabelOnlyMarkers(lng = hs$lon, lat = hs$lat, label = hs$region,
                          labelOptions = labelOptions(noHide = TRUE, direction = "right", offset = c(14, 0), textOnly = TRUE, className = "etiq-surv"))
    l |> addLegend("bottomleft", colors = col_aq[1:5], labels = d$lab_aq[1:5], opacity = 0.9,
                   title = htmltools::HTML(paste0("Manque d'eau de la région<br>", nom_hor()))) |>
      fitBounds(unname(quantile(o$lon, 0.01)), unname(quantile(o$lat, 0.01)), unname(quantile(o$lon, 0.99)), unname(quantile(o$lat, 0.99)))
  })

  chauds <- reactive({
    ds <- dens(); if (is.null(ds)) return(tibble(region = character(), lon = numeric(), lat = numeric(), n = integer()))
    ds$o |> filter(dense, sec(.data[[input$horiz_reg]]), !is.na(region)) |>
      group_by(region, pays) |> summarise(n = n(), lon = mean(lon), lat = mean(lat), .groups = "drop") |>
      filter(n >= 3) |> slice_max(n, n = if (input$pays_reg == "MONDE") 6 else 3, with_ties = FALSE)
  })

  output$reg_texte <- renderUI({
    ds <- dens(); o <- osm_sel() |> filter(!is.na(.data[[input$horiz_reg]]))
    if (is.null(ds) || nrow(o) == 0) return(div(class = "etape-texte", span(class = "etape-num", bs_icon("info")), div("Trop peu de sites géolocalisés pour estimer une densité.")))
    dn <- ds$o |> filter(dense)
    p_sec <- 100 * mean(sec(dn[[input$horiz_reg]]), na.rm = TRUE)
    hs <- chauds()
    rg <- reg_sel()
    plus_sec <- if (input$horiz_reg != "stress_region") {
      niv <- function(x) match(as.character(x), d$lab_aq[1:5])
      n <- sum(niv(rg[[input$horiz_reg]]) > niv(rg$stress_region), na.rm = TRUE)
      paste0(" ", n, " région", ifelse(n > 1, "s", ""), " équipée", ifelse(n > 1, "s", ""), " passe", ifelse(n > 1, "nt", ""),
             " dans une catégorie plus sèche ", nom_hor(), ".")
    } else ""
    div(class = "etape-texte", span(class = "etape-num", bs_icon("fire")), div(
      tags$b(paste0("Les zones les plus denses regroupent ", nrow(dn), " sites : ", fmt(p_sec, 0), " % sont dans une région qui manque d'eau ", nom_hor(), ".")),
      plus_sec,
      if (nrow(hs)) span(paste0(" Points chauds (cercles rouges) : ", paste0(hs$region, if (input$pays_reg == "MONDE") paste0(" (", hs$pays, ")") else "", collapse = ", "), ".")),
      span(class = "text-muted small", paste0(" Noyau gaussien, écart-type ≈ ", ds$sd_km, " km."))))
  })

  output$reg_titre <- renderText(if (input$pays_reg == "MONDE") "Régions les plus équipées du monde" else
    paste("Disparités :", d$osm$pays[match(input$pays_reg, d$osm$iso3)]))
  output$reg_chiffres <- renderUI({
    rg <- reg_sel(); o <- osm_sel() |> filter(!is.na(.data[[input$horiz_reg]]))
    if (nrow(rg) == 0) return(NULL)
    top <- rg |> slice_max(sites, n = 1, with_ties = FALSE)
    card_body(class = "pb-0", div(class = "chiffres-fiche",
      div(div(class = "n", nrow(rg)), "régions équipées"),
      div(div(class = "n", paste0(fmt(100 * top$sites / sum(rg$sites), 0), " %")), paste("des sites en", top$region)),
      div(div(class = "n", paste0(fmt(100 * mean(sec(o[[input$horiz_reg]])), 0), " %")), paste("en région sèche", nom_hor()))))
  })
  output$reg_barres <- renderPlotly({
    rg <- reg_sel() |> slice_max(sites, n = 15, with_ties = FALSE) |>
      mutate(st = as.character(.data[[input$horiz_reg]]), st = coalesce(st, "Non renseigné"),
             lab = if (input$pays_reg == "MONDE") paste0(region, " (", pays, ")") else region) |>
      arrange(sites) |> mutate(lab = factor(lab, unique(lab)))
    plot_ly(rg, y = ~lab, x = ~sites, type = "bar", orientation = "h", color = ~st, colors = c(col_aq, "Non renseigné" = "#bbbbbb"),
            hovertext = ~paste0("<b>", region, "</b><br>", sites, " sites<br>Manque d'eau ", nom_hor(), " : ", st,
                                "<br>Risque de sécheresse : ", ifelse(is.na(secheresse), "n.d.", fmt(secheresse, 1)), " / 5"),
            hoverinfo = "text") |>
      layout(xaxis = list(title = "Sites géolocalisés"), yaxis = list(title = ""), showlegend = FALSE,
             margin = list(l = 10, r = 10, t = 10))
  })

  # ---------- Saisons : effet ciseaux, carte mois par mois, calendrier
  etat_lab <- c("Chaud et sec", "Chaud", "Sec", "Ni chaud ni sec")
  etat_col <- c("Chaud et sec" = "#d7301f", "Chaud" = "#fdae6b", "Sec" = "#fee8a8", "Ni chaud ni sec" = "#c6dbef")
  CLIM <- d$climat |> group_by(iso3) |>
    mutate(chaud = tas > mean(tas), sec = pr < mean(pr),
           etat = factor(ifelse(chaud & sec, etat_lab[1], ifelse(chaud, etat_lab[2], ifelse(sec, etat_lab[3], etat_lab[4]))), etat_lab),
           dj = pmax(0, tas - 18) * c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[mois],      # degrés-jours de refroidissement
           tension = as.numeric(scale(tas)) - as.numeric(scale(pr))) |> ungroup()
  mois_long <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")

  output$clim_chiffres <- renderUI({
    c <- CLIM |> filter(iso3 == input$pays_clim)
    if (nrow(c) == 0) return(NULL)
    pire <- c |> slice_max(tension, n = 1, with_ties = FALSE)
    card_body(class = "pb-0", div(class = "chiffres-fiche",
      div(div(class = "n", sum(c$sensible)), "mois chauds et secs"),
      div(div(class = "n", mois_long[pire$mois]), "mois le plus tendu"),
      div(div(class = "n", paste0("×", fmt(max(c$pr) / max(min(c$pr), 1), 1))), "pluie : mois humide / mois sec")))
  })
  output$clim <- renderPlotly({
    c <- CLIM |> filter(iso3 == input$pays_clim) |> arrange(mois)
    if (nrow(c) == 0) return(plotly_empty())
    sh <- lapply(c$mois[c$sensible], function(i) list(type = "rect", xref = "x", yref = "paper", x0 = i - 0.5, x1 = i + 0.5,
                                                       y0 = 0, y1 = 1, fillcolor = "#f0ad4e", opacity = 0.22, line = list(width = 0), layer = "below"))
    plot_ly(c, x = ~mois) |>
      add_bars(y = ~pr, name = "Pluie (mm)", marker = list(color = "#6baed6"),
               hovertext = ~paste0(mois_long[mois], " : ", fmt(pr, 0), " mm de pluie"), hoverinfo = "text") |>
      add_lines(y = ~dj, name = "Besoin de refroidissement (degrés-jours > 18 °C)", yaxis = "y2", line = list(color = "#d62728", width = 3.5),
                hovertext = ~paste0(mois_long[mois], " : ", fmt(dj, 0), " degrés-jours (", fmt(tas, 1), " °C en moyenne)"), hoverinfo = "text") |>
      layout(shapes = sh, legend = LEG, margin = list(r = 60), xaxis = list(title = "", tickvals = 1:12, ticktext = mois_fr),
             yaxis = list(title = "Pluie (mm)", rangemode = "tozero"),
             yaxis2 = list(title = "Degrés-jours", overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero"))
  })
  output$clim_retenir <- renderText({
    c <- CLIM |> filter(iso3 == input$pays_clim); n <- d$pays$country[d$pays$iso3 == input$pays_clim]
    if (nrow(c) == 0) return("Pas de données climatiques pour ce pays.")
    if (max(c$dj) < 1) return(paste0(n, " : il ne fait jamais plus de 18 °C en moyenne mensuelle ; les data centers peuvent se refroidir surtout avec l'air extérieur."))
    m <- c |> slice_max(dj, n = 1, with_ties = FALSE)
    paste0(n, " : en ", tolower(mois_long[m$mois]), ", le besoin de refroidissement est au plus haut (", fmt(m$dj, 0), " degrés-jours)",
           if (m$pr < mean(c$pr)) paste0(" alors qu'il ne tombe que ", fmt(m$pr, 0), " mm de pluie, contre ", fmt(mean(c$pr), 0), " mm en moyenne : c'est l'effet ciseaux.")
           else paste0(" mais il pleut plus que la moyenne (", fmt(m$pr, 0), " mm contre ", fmt(mean(c$pr), 0), " mm) : pas d'effet ciseaux ce mois-là."))
  })

  output$mois_nom <- renderText(mois_long[input$mois_carte])
  output$carte_mois <- renderLeaflet({
    cm <- CLIM |> filter(mois == input$mois_carte)
    m <- d$monde |> left_join(cm |> select(iso3, etat, tas, pr), by = "iso3") |> left_join(d$pays |> select(iso3, country, n_dc), by = "iso3")
    remp <- ifelse(is.na(m$etat), "#eeeeee", etat_col[as.character(m$etat)])
    lab <- paste0("<b>", coalesce(m$country, m$iso3), "</b><br>", ifelse(is.na(m$etat), "pas de donnée", as.character(m$etat)),
                  ifelse(is.na(m$tas), "", paste0("<br>", fmt(m$tas, 1), " °C · ", fmt(m$pr, 0), " mm")),
                  ifelse(is.na(m$n_dc), "", paste0("<br>", m$n_dc, " data centers"))) |> lapply(htmltools::HTML)
    leaflet(options = leafletOptions(minZoom = 1, zoomSnap = 0.25)) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(data = m, fillColor = remp, fillOpacity = 0.9, color = "white", weight = 0.5, label = lab) |>
      addLegend("bottomleft", colors = unname(etat_col), labels = names(etat_col), opacity = 0.9, title = "Ce mois-ci, le pays est…") |>
      setView(10, 25, 1)
  })
  output$mois_retenir <- renderText({
    cm <- CLIM |> filter(mois == input$mois_carte) |> inner_join(d$pays |> select(iso3, n_dc), by = "iso3")
    p <- 100 * sum(cm$n_dc[cm$etat == etat_lab[1]]) / sum(cm$n_dc)
    paste0(mois_long[input$mois_carte], " : ", fmt(p, 0), " % des data centers sont dans un pays chaud et sec ce mois-ci.")
  })

  output$calendrier <- renderPlotly({
    top <- d$pays |> slice_max(n_dc, n = 20, with_ties = FALSE) |> select(iso3, country, n_dc) |>
      inner_join(CENTRES |> select(iso3, lat), by = "iso3") |> arrange(lat)
    cal <- CLIM |> filter(iso3 %in% top$iso3) |> left_join(top, by = "iso3") |>
      mutate(code = as.integer(etat), pays = factor(paste0(country, " (", n_dc, ")"), paste0(top$country, " (", top$n_dc, ")")))
    z <- tapply(cal$code, list(cal$pays, cal$mois), function(x) x[1])
    txt <- tapply(paste0("<b>", cal$country, " — ", mois_long[cal$mois], "</b><br>", cal$etat, "<br>", fmt(cal$tas, 1), " °C · ", fmt(cal$pr, 0), " mm"),
                  list(cal$pays, cal$mois), function(x) x[1])
    cs <- list(c(0, etat_col[[1]]), c(0.25, etat_col[[1]]), c(0.25, etat_col[[2]]), c(0.5, etat_col[[2]]),
               c(0.5, etat_col[[3]]), c(0.75, etat_col[[3]]), c(0.75, etat_col[[4]]), c(1, etat_col[[4]]))
    plot_ly(x = mois_fr, y = rownames(z), z = z, type = "heatmap", colorscale = cs, zmin = 0.5, zmax = 4.5, showscale = FALSE,
            text = txt, hoverinfo = "text", xgap = 2, ygap = 2) |>
      layout(xaxis = list(title = "", side = "top"), yaxis = list(title = "", tickfont = list(size = 11)),
             margin = list(l = 10, r = 10, t = 30, b = 40),
             annotations = list(list(x = 1.0, y = -0.08, xref = "paper", yref = "paper", xanchor = "right", showarrow = FALSE,
                                     text = "<span style='color:#d7301f'>■</span> chaud et sec   <span style='color:#fdae6b'>■</span> chaud   <span style='color:#e6c86e'>■</span> sec   <span style='color:#9ecae1'>■</span> ni l'un ni l'autre",
                                     font = list(size = 12))))
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

  # ---------- Usages : convertisseur du quotidien, eau directe / indirecte, tendances
  GWH_PAR_MW <- sum(d$eu$elec_gwh) / sum(d$eu$puissance_mw)   # 3,77 GWh par MW (Commission européenne, 2024)
  conv <- reactive({
    mw <- as.numeric(input$mw); g <- d$pays$gco2_kwh[d$pays$iso3 == input$mix]
    mwh <- mw * GWH_PAR_MW * 1e3; eau <- mwh * 1e3 * w() / 1e3      # m3
    list(mw = mw, mwh = mwh, eau = eau, co2_t = mwh * g / 1e3, g = g, pays = d$pays$country[d$pays$iso3 == input$mix])
  })
  output$conv <- renderUI({
    x <- conv()
    tuile <- function(ic, val, txt, cls = "") div(class = paste("tuile", cls), bs_icon(ic), div(class = "gros", val), txt)
    tagList(
      layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
        tuile("lightning-charge", paste(fmt(x$mwh / 1e3, 1), "GWh"), paste0("d'électricité par an ≈ ", grand(x$mwh / 4.5), " foyers")),
        tuile("droplet", paste(grand(x$eau), "m³"), paste0("d'eau par an ≈ ", grand(x$eau / HAB_M3), " habitants")),
        tuile("cloud", paste(grand(x$co2_t), "t"), paste0("de CO₂ ≈ ", avec_unite(x$co2_t * 1e3 / KM_KG, "km"), " en voiture"))),
      layout_columns(col_widths = c(4, 4, 4), fill = FALSE, class = "mt-2",
        tuile("water", grand(x$eau * 1e3 / 60), "douches de 5 minutes", "petite"),
        tuile("tsunami", grand(x$eau / PISCINE), "piscines olympiques", "petite"),
        tuile("egg-fried", paste(grand(x$eau * 1e3 / 15400), "kg"), "de bœuf (empreinte eau)", "petite")))
  })
  output$conv_retenir <- renderText({
    x <- conv()
    paste0("Un data center de ", x$mw, " MW utilise autant d'électricité que ", grand(x$mwh / 4.5), " foyers et autant d'eau que ",
           grand(x$eau / HAB_M3), " habitants. Mix électrique choisi : ", x$pays, " (", fmt(x$g, 0), " g de CO₂ par kWh), ",
           "soit le CO₂ de ", avec_unite(x$co2_t * 1e3 / KM_KG, "km"), " en voiture.")
  })

  output$directe <- renderPlotly({
    plot_ly(labels = c("Refroidissement sur place (eau directe)", "Production de l'électricité (eau indirecte)"),
            values = c(0.575, 6.5 - 0.575), type = "pie", hole = 0.55, sort = FALSE,
            marker = list(colors = c("#1f6f8b", "#f0ad4e"), line = list(color = "#fff", width = 2)),
            textinfo = "percent", textfont = list(size = 16, color = "#fff"),
            hovertext = c("0,575 million de m³ (0,21 L/kWh)", "5,9 millions de m³ (2,2 L/kWh)"), hoverinfo = "text+label") |>
      layout(showlegend = TRUE, legend = list(orientation = "h", x = 0, y = -0.1),
             annotations = list(list(text = "<b>6,5</b><br>millions m³", showarrow = FALSE, font = list(size = 15))),
             margin = list(t = 10, b = 10))
  })

  output$tendance <- renderPlotly({
    iso <- input$pays_us
    s <- d$secteurs_ts |> filter(iso3 == iso, annee >= 1990)
    dc <- bind_rows(lapply(2024:2035, function(y) tibble(annee = y,
            bas = sum(calculer(w(), "bas", y)$eau[d$pays$iso3 == iso]) / 1e3,
            central = sum(calculer(w(), "central", y)$eau[d$pays$iso3 == iso]) / 1e3,
            haut = sum(calculer(w(), "haut", y)$eau[d$pays$iso3 == iso]) / 1e3)))
    cols <- c(Agriculture = "#74a52f", Industrie = "#6a6a8a", Habitants = "#3182bd")
    p <- plot_ly()
    for (u in names(cols)) {
      su <- s |> filter(usage == u) |> arrange(annee)
      if (nrow(su)) p <- p |> add_trace(data = su, x = ~annee, y = ~mds_m3, type = "scatter", mode = "lines+markers", name = u,
                                         line = list(color = cols[[u]], width = 2.5), marker = list(color = cols[[u]]),
                                         hovertext = ~paste0(u, " ", annee, " : ", fmt(mds_m3, 2), " milliards de m³"), hoverinfo = "text")
    }
    p |>
      add_ribbons(data = dc, x = ~annee, ymin = ~bas, ymax = ~haut, name = "Data centers (lente à rapide)",
                  fillcolor = "rgba(211,84,0,0.2)", line = list(width = 0), hoverinfo = "none") |>
      add_trace(data = dc, x = ~annee, y = ~central, type = "scatter", mode = "lines", name = "Data centers (centrale)",
                line = list(color = "#d35400", width = 3),
                hovertext = ~paste0("Data centers ", annee, " : ", fmt(central * 1e3, 1), " millions de m³"), hoverinfo = "text") |>
      layout(xaxis = list(title = ""), legend = list(orientation = "h", x = 0, y = -0.15),
             yaxis = list(title = "Milliards de m³ par an (échelle log)", type = "log",
                          tickvals = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), ticktext = c("0,001", "0,01", "0,1", "1", "10", "100", "1 000")))
  })
  output$tendance_retenir <- renderText({
    iso <- input$pays_us; n <- d$pays$country[d$pays$iso3 == iso]
    s <- d$secteurs_ts |> filter(iso3 == iso, annee >= 1990) |> group_by(usage) |> arrange(annee) |>
      summarise(a0 = first(annee), a1 = last(annee), v = 100 * (last(mds_m3) / first(mds_m3) - 1), .groups = "drop") |> filter(a1 > a0)
    e24 <- sum(calculer(w(), "central", 2024)$eau[d$pays$iso3 == iso]); e35 <- sum(calculer(w(), "central", 2035)$eau[d$pays$iso3 == iso])
    txt <- if (nrow(s)) paste0(n, " : ", paste0(tolower(s$usage), " ", ifelse(s$v >= 0, "+", ""), fmt(s$v, 0), " %", collapse = ", "),
                               " (", min(s$a0), "-", max(s$a1), "). ") else paste0(n, " : pas d'historique par usage. ")
    paste0(txt, "Data centers : ×", fmt(e35 / max(e24, 1e-9), 1), " entre 2024 et 2035 (croissance centrale), mais toujours très loin des autres usages.")
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
