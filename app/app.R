# =============================================================================
# SAÉ 5.EMS.01 - L'eau des data centers : tableau de bord interactif
# Ayi AMAVIGAN - Ada GADJI - Aboubakar COULIBALY | BUT3 Science des données, IUT de Carcassonne
# Tiré des rapports 1 et 2 du groupe. Données préparées par ../prep.R
# Principe : peu de texte, une idée par graphique, l'aide de lecture au survol de l'icône (i).
# =============================================================================
library(shiny); library(bslib); library(bsicons); library(plotly); library(DT)
library(dplyr); library(tidyr); library(leaflet); library(sf); library(FactoMineR); library(forecast)

options(DT.options = list(language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json")))
# Grands écrans : la variable CSS --k (1 ; 1,3 ; 1,6 selon la largeur de la fenêtre) agrandit
# la hauteur des graphiques et des cartes, et le texte des graphiques plotly (voir le CSS plus bas).
hauteur_k <- function(h) if (is.character(h) && grepl("^[0-9.]+px$", h)) sprintf("calc(%s * var(--k, 1))", h) else h
plotlyOutput <- function(outputId, width = "100%", height = "400px", ...) plotly::plotlyOutput(outputId, width, hauteur_k(height), ...)
leafletOutput <- function(outputId, width = "100%", height = 400, ...)
  leaflet::leafletOutput(outputId, width, hauteur_k(if (is.numeric(height)) paste0(height, "px") else height), ...)
JS_TEXTE_K <- "function(el){
  var k = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--k')) || 1;
  if (k <= 1) return;
  var lay = JSON.parse(JSON.stringify(el.layout || {}));
  (function agrandir(o){ for (var c in o){ var v = o[c]; if (!v || typeof v !== 'object') continue;
      if (/font$/i.test(c) && !Array.isArray(v)) v.size = (v.size || 12) * k; else agrandir(v); } })(lay);
  lay.font = lay.font || {}; if (!(el.layout.font && el.layout.font.size)) lay.font.size = 12 * k;
  Plotly.relayout(el, lay);
}"
# Graphiques sans barre d'outils
renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
  force(env)
  if (!quoted) expr <- substitute(expr)
  plotly::renderPlotly(htmlwidgets::onRender(plotly::config(eval(expr, env), displayModeBar = FALSE, locale = "fr"), JS_TEXTE_K), quoted = FALSE)
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
FOYER_MWH <- 4.5         # MWh d'électricité par foyer et par an (ordre de grandeur, France)
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

# Test de McNemar : les MÊMES sites classés « stress élevé » selon l'échelle du pays puis celle de la région (WRI Aqueduct)
MCN <- local({
  o <- d$osm |> left_join(d$pays |> select(iso3, stress_pays_aq), by = "iso3") |> filter(!is.na(stress_region), !is.na(stress_pays_aq))
  s <- function(x) factor(as.character(x) %in% d$lab_aq[4:5], c(FALSE, TRUE))
  t <- table(pays = s(o$stress_pays_aq), region = s(o$stress_region))
  test <- mcnemar.test(t)
  top <- o |> count(region, sort = TRUE) |> slice(1)
  list(n = nrow(o), top_region = top$region, top_pct = 100 * top$n / nrow(o), p_pays = 100 * mean(s(o$stress_pays_aq) == "TRUE"), p_reg = 100 * mean(s(o$stress_region) == "TRUE"),
       caches = t["FALSE", "TRUE"], inverse = t["TRUE", "FALSE"], deux = t["TRUE", "TRUE"], aucun = t["FALSE", "FALSE"], chi2 = unname(test$statistic),
       p_txt = if (test$p.value < 0.001) "p < 0,001" else paste("p =", fmt(test$p.value, 3)))
})
# Radar de la fiche pays : chaleur (degrés-jours > 18 °C sur l'année) et pluie annuelle, normales 1991-2020
CLIM_AN <- d$climat |> group_by(iso3) |> summarise(dj = sum(pmax(tas - 18, 0) * 30.4), pluie = sum(pr), .groups = "drop")

# Évolution : croissance annuelle moyenne et test de tendance de Mann-Kendall (tau de Kendall année / valeur)
tcam <- function(v, a) { ok <- !is.na(v); v <- v[ok]; a <- a[ok]
  if (length(v) < 2) NA_real_ else 100 * ((v[length(v)] / v[1])^(1 / (a[length(a)] - a[1])) - 1) }
mk <- function(v, a) { ok <- !is.na(v); if (sum(ok) < 4) return(NULL)
  if (length(unique(v[ok])) == 1) return(list(p.value = 1, estimate = 0, constante = TRUE))   # même valeur reprise chaque année
  suppressWarnings(cor.test(a[ok], v[ok], method = "kendall")) }
EVO <- local({
  dc <- d$aie |> filter(cas == "Historique") |> arrange(annee)
  list(dc = dc, t_dc = tcam(dc$twh, dc$annee))
})
# Séries 2015-2022 : 30 pays les plus équipés (total) ou un pays (panel du Rapport 2)
SERIE_DEB <- 2015; SERIE_FIN <- 2022
serie_evo <- function(iso) {
  if (iso == "TOP30") return(d$hist |> filter(year >= SERIE_DEB) |> select(year, eau, elec))
  d$hist_pays |> filter(iso3 == iso, year >= SERIE_DEB, year <= SERIE_FIN) |> arrange(year) |> select(year, eau, elec)
}
CHOIX_EVO <- local({
  ok <- d$hist_pays |> filter(year >= SERIE_DEB, year <= SERIE_FIN) |> group_by(iso3) |>
    summarise(n_eau = sum(!is.na(eau)), n_elec = sum(!is.na(elec)), .groups = "drop") |>
    filter(n_eau >= 4, n_elec >= 4) |> inner_join(d$pays |> select(iso3, country, n_dc), by = "iso3") |> arrange(country)
  c("Les 30 pays les plus équipés" = "TOP30", setNames(ok$iso3, ok$country))
})
p_txt <- function(p) if (p < 0.001) "p < 0,001" else if (p < 0.01) paste("p =", fmt(p, 3)) else paste("p =", fmt(p, 2))

MCN_AIDE <- paste0("Test de McNemar sur ", fmt(MCN$n, 0), " sites (données appariées : chaque site est classé deux fois). ",
                   fmt(MCN$p_pays, 0), " % en stress élevé à l'échelle du pays, ", fmt(MCN$p_reg, 0), " % à l'échelle de la région. ",
                   fmt(MCN$caches, 0), " sites sont en région sèche alors que leur pays ne l'est pas (", fmt(MCN$inverse, 0),
                   " dans l'autre sens). Khi² = ", fmt(MCN$chi2, 0), ", ", MCN$p_txt, ". « Région sèche » : stress hydrique Aqueduct ≥ 40 %. ",
                   "Attention : les sites ne sont pas indépendants, ils sont groupés en grappes (", fmt(MCN$top_pct, 0), " % en ", MCN$top_region,
                   ") ; la p-value est donc optimiste et le résultat principal est descriptif. Avec une seule ligne par région (",
                   fmt(d$mcn_regions$n, 0), " régions) : ", fmt(d$mcn_regions$p_reg, 0), " % de régions sèches contre ", fmt(d$mcn_regions$p_pays, 0),
                   " % vu du pays (", p_txt(d$mcn_regions$p), ").")

# ------------------------------------------------ Chiffres clés (calculés une seule fois) -----
# Toute valeur affichée dans l'interface vient d'ici ou d'un calcul du serveur, jamais d'un nombre tapé à la main.
ARCEP_TWH <- 2.7   # Arcep 2024 : électricité des 23 opérateurs suivis (base des coefficients 0,21 et 2,41 L/kWh)
K <- local({
  p <- d$pays; w0 <- wue[["central"]]
  part <- 100 * p$e0 * w0 / (p$prelev0 * 1e3)
  s <- p |> filter(!is.na(prelev0), !is.na(pct_agri), !is.na(pct_indus), !is.na(pct_menages))
  agri <- sum(s$prelev0 * s$pct_agri) / sum(s$prelev0 * (s$pct_agri + s$pct_indus + s$pct_menages)) * 1e4
  dc <- 1e4 * sum(s$e0 * w0) / (sum(s$prelev0) * 1e3)
  f35 <- d$scen |> filter(horizon == 2035)
  fr <- p |> filter(iso3 == "FRA"); g1 <- d$google |> slice_max(eau_m3, n = 1)
  list(n_sites = sum(p$n_dc), n_pays = nrow(p), twh = d$twh_monde,
       eau_c = d$twh_monde * w0, eau_b = d$twh_monde * wue[["bas"]], eau_h = d$twh_monde * wue[["haut"]],
       pct_pays_01 = 100 * mean(part < 0.1, na.rm = TRUE), n_pays_part = sum(!is.na(part)),
       agri_sur_dc = agri / dc, dc_litres = dc,
       f35_min = min(f35$f_monde), f35_max = max(f35$f_monde),
       indirecte = 100 * (1 - wue[["bas"]] / wue[["haut"]]),
       fr_e0 = fr$e0, fr_eau = fr$e0 * w0, google_site = g1$site, google_m3 = g1$eau_m3,
       gwh_eur = d$gwh_site_zone$gwh_site[d$gwh_site_zone$zone == "EUR"],
       gwh_rdm = d$gwh_site_zone$gwh_site[d$gwh_site_zone$zone == "RDM"],
       cales = d$pays$country[d$pays$cale_aie])
})
sur_10 <- function(x) paste0(fmt(x / 10, 0), " pays sur 10")

calculer <- function(w, scenario, horizon, base = d$pays) {
  p <- base
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

# Variante de répartition de l'électricité 2024 : capacité des sites IA connus (Epoch AI) + nombre de sites pour le reste.
# Électricité d'un site = puissance IT (MW) x PUE x facteur de charge x 8 760 h. Pays calés sur l'AIE : total inchangé.
PUE_IA <- 1.2   # hypothèse : rapport électricité totale / électricité IT pour les grands sites IA
alloc_capacite <- function(fc) {
  cap <- d$ia |> group_by(iso3) |> summarise(mw = sum(mw_it), n_ia = n(), .groups = "drop")
  d$pays |> left_join(cap, by = "iso3") |> left_join(d$zones |> select(zone, twh_zone), by = "zone") |>
    mutate(mw = coalesce(mw, 0), n_ia = coalesce(n_ia, 0L), e_cap = mw * PUE_IA * fc * 8760 / 1e6) |>
    group_by(zone) |>
    mutate(n_reste = ifelse(cale_aie, 0, pmax(n_dc - n_ia, 0)),
           reste = pmax(first(twh_zone) - sum(e0[cale_aie]) - sum(e_cap[!cale_aie]), 0),
           e0 = ifelse(cale_aie, e0, e_cap + reste * n_reste / max(sum(n_reste), 1)),
           methode = ifelse(cale_aie, "calé AIE", ifelse(e_cap > 0, "capacité", "nombre de sites"))) |>
    ungroup() |> mutate(gwh_site = 1000 * e0 / n_dc)
}

# Évolution 2024 -> 2035 d'un pays : MÊME calcul dans Usages et Demain.
# Volume = eau consommée par les data centers ; part = volume / eau prélevée du pays.
# Dénominateur : prévision du modèle de dérive pour la France, constant (moyenne 2015-2022) pour les autres pays.
evol_pays <- function(iso, w, scen, base = d$pays) {
  a <- calculer(w, scen, 2024, base) |> filter(iso3 == iso); b <- calculer(w, scen, 2035, base) |> filter(iso3 == iso)
  prel35 <- if (iso == "FRA") d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2035] else a$prelev0
  list(f_vol = b$eau / a$eau, eau24 = a$eau, eau35 = b$eau, part24 = a$part,
       part35 = 100 * b$eau / (prel35 * 1e3), denominateur = if (iso == "FRA") "prévision des prélèvements (dérive)" else "prélèvements constants")
}

# Accueil : une réponse = une phrase-titre, une explication, une preuve éventuelle, un lien vers l'onglet
reponse <- function(titre, texte, preuve, lien, id)
  div(class = "reponse", h3(titre), p(texte), preuve, actionLink(id, lien, class = "voir"))

# Lien « Télécharger (PNG) » sous un graphique plotly
png_lien <- function(id) div(class = "png-lien",
  tags$a(href = "#", onclick = sprintf("Plotly.downloadImage(document.getElementById('%s'), {format: 'png', filename: '%s', width: 1400, height: 800}); return false;", id, id),
         "Télécharger (PNG)"))

# Un bloc = un titre (question), le graphique, une phrase à retenir. L'aide est dans l'icône (i).
bloc <- function(titre, sortie, aide, retenir, h = "380px", plot = plotlyOutput)
  card(full_screen = TRUE,
       card_header(class = "d-flex justify-content-between align-items-center gap-2",
                   span(class = "titre-bloc", titre), tooltip(bs_icon("info-circle", class = "aide"), aide)),
       plot(sortie, height = h),
       if (identical(plot, plotlyOutput)) png_lien(sortie),
       card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", retenir))

# Seuil de « région sèche » (classe de stress Aqueduct) et test de McNemar, recalculés selon le réglage
SEUILS <- c("≥ 20 % (moyen et plus)" = "2", "≥ 40 % (élevé et plus)" = "3", "≥ 80 % (extrême)" = "4")
seuil_txt <- function(s) c("2" = "≥ 20 %", "3" = "≥ 40 %", "4" = "≥ 80 %")[[as.character(s)]]
classe_aq <- function(x) { i <- match(as.character(x), d$lab_aq); ifelse(is.na(i) | i == 6, NA, i - 1) }   # 0 à 4 ; « aride » = NA
est_sec <- function(x, s) { k <- classe_aq(x); !is.na(k) & k >= as.integer(s) }
mcn_calc <- function(s = 3) {
  o <- d$osm |> left_join(d$pays |> select(iso3, stress_pays_aq), by = "iso3") |> filter(!is.na(stress_region), !is.na(stress_pays_aq)) |>
    mutate(ps = est_sec(stress_pays_aq, s), rs = est_sec(stress_region, s))
  t <- table(pays = factor(o$ps, c(FALSE, TRUE)), region = factor(o$rs, c(FALSE, TRUE)))
  test <- suppressWarnings(mcnemar.test(t)); top <- o |> count(region, sort = TRUE) |> slice(1)
  r <- o |> group_by(iso3, region) |> summarise(ps = first(ps), rs = first(rs), .groups = "drop")
  tr <- table(factor(r$ps, c(FALSE, TRUE)), factor(r$rs, c(FALSE, TRUE))); test_r <- suppressWarnings(mcnemar.test(tr))
  list(seuil = s, n = nrow(o), p_pays = 100 * mean(o$ps), p_reg = 100 * mean(o$rs),
       caches = t["FALSE", "TRUE"], inverse = t["TRUE", "FALSE"], deux = t["TRUE", "TRUE"], aucun = t["FALSE", "FALSE"],
       chi2 = unname(test$statistic), p = test$p.value, p_txt = p_txt(test$p.value),
       top_region = top$region, top_pct = 100 * top$n / nrow(o),
       n_reg = nrow(r), p_pays_reg = 100 * mean(r$ps), p_reg_reg = 100 * mean(r$rs), p_r = test_r$p.value)
}
# Variables de l'ACP (recalculée dans l'application)
VARS_ACP <- setdiff(names(d$acp_base), c("country", "iso3", "part_central"))
NOMS_ACP <- c(n_dc = "Nb data centers", prelev_vol = "Prélèvements d'eau", stress_hydrique = "Stress hydrique", eau_renouv_hab = "Eau dispo / hab.",
              pib_hab = "PIB / hab.", elec_generation_twh = "Production électrique", elec_emissions_intensity_gco2e_kwh = "Intensité carbone élec.",
              population = "Population", part_central = "Part eau DC (suppl.)")

# ------------------------------------------------------------- Interface -----
theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#1f6f8b", base_font = font_google("Inter")) |>
  bs_add_rules("
    body{font-size:15px;}
    /* Grands écrans (salle, vidéoprojecteur haute résolution) : tout grandit */
    :root{--k:1;}
    @media (min-width:1700px){ :root{--k:1.3;} html{font-size:20px;} body{font-size:19px;} .container-fluid{max-width:none;} }
    @media (min-width:2300px){ :root{--k:1.6;} html{font-size:24px;} body{font-size:23px;} }
    @media (min-width:1700px){ .bande-logos img{height:calc(62px * var(--k)) !important;}
      .hero-contenu{max-width:62rem !important;} .hero h1{font-size:4.2rem !important;white-space:nowrap;}
      .chiffre{flex:1 1 15rem !important;max-width:22rem !important;} .chiffre .nb{font-size:2.6rem !important;white-space:nowrap;}
      .reponses-grille{max-width:80rem !important;} .photo{height:calc(300px * var(--k)) !important;}
      .card-header .shiny-input-container, .parcours .shiny-input-container{width:auto !important;min-width:14rem;}
      .card-header .selectize-input{white-space:nowrap;}
      .leaflet-container .info.legend, .leaflet-container .legend{font-size:calc(12px * var(--k)) !important;line-height:1.35 !important;}      .bslib-sidebar-layout{--_sidebar-width:calc(260px * var(--k)) !important;} }
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
    .marque-sous{font-size:.78rem;color:rgba(255,255,255,.72);margin-top:2px;}
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
    .etape-det{font-weight:400;font-size:.75rem;color:#6b7b85;margin-top:3px;line-height:1.3;}
    .png-lien{text-align:right;padding:0 14px 6px;font-size:.78rem;} .png-lien a{color:#6b7b85;text-decoration:underline;}
    .chiffres .reglages{flex:1 1 100%;text-align:center;font-size:.78rem;color:#9aa6ad;margin-top:18px;}
    .hypo{background:#fff;border:1px solid #e3e9ee;border-left:6px solid #1f6f8b;border-radius:8px;padding:16px 20px;margin-bottom:16px;}
    .hypo-tete{display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;}
    .hypo-code{font-weight:800;color:#123f52;font-size:1.1rem;} .hypo-verdict{font-weight:700;}
    .hypo h3{font-size:1.15rem;font-weight:700;color:#123f52;margin:4px 0 12px;}
    .hypo-dl{display:grid;grid-template-columns:120px 1fr;gap:6px 14px;margin:0;font-size:.92rem;}
    .hypo-dl dt{color:#6b7b85;font-weight:600;} .hypo-dl dd{margin:0;color:#333;}
    .note-limite{font-size:.82rem;color:#6b7b85;background:#f6f8fa;border-left:3px solid #c9d3da;padding:6px 10px;margin:6px 0 10px;border-radius:4px;}
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
    .famille{padding:8px 12px;margin-bottom:10px;background:#fafbfc;border-radius:6px;}
    .famille-nom{font-weight:700;} .famille-n{font-weight:400;color:#888;font-size:.85rem;margin-left:6px;}
    .famille-val{font-size:.85rem;color:#444;margin-top:2px;} .famille-ex{font-size:.8rem;color:#888;font-style:italic;}
    .famille-vt{font-size:.78rem;color:#1f6f8b;margin-top:3px;}
    /* Accueil : pas de panneau « Hypothèses » ni de flèche */
    body.sur-accueil .bslib-sidebar-layout > .sidebar,
    body.sur-accueil .bslib-sidebar-layout > .collapse-toggle{display:none !important;}
    body.sur-accueil .bslib-sidebar-layout{grid-template-columns:0 1fr !important;}
    /* Accueil */
    .accueil{margin:-1.5rem -1.5rem 0 -1.5rem;}
    .bande-logos{display:flex;justify-content:space-between;align-items:center;padding:14px 48px;background:#fff;}
    .bande-logos img{height:62px;width:auto;}
    .bande-logos span{font-size:.9rem;color:#7a8a94;}
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
    .chiffre .base{margin-top:6px;color:#9aa6ad;font-size:.78rem;line-height:1.35;}
    .photos{display:grid;grid-template-columns:1fr 1fr;gap:24px;padding:0 48px 40px;background:#fff;}
    .photo{height:300px;border-radius:16px;background-size:cover;background-position:center;position:relative;overflow:hidden;
           box-shadow:0 8px 30px rgba(0,0,0,.12);}
    .photo .legende{position:absolute;left:0;right:0;bottom:0;padding:22px 24px;color:#fff;font-size:.95rem;
                    background:linear-gradient(0deg,rgba(0,0,0,.75),rgba(0,0,0,0));}
    .photo .legende b{font-size:1.15rem;}
    /* Méthode : Évolution et test */
    .evo-ligne{display:flex;justify-content:space-between;align-items:center;gap:12px;padding:12px 0;border-bottom:1px solid #eef2f4;}
    .evo-ligne:last-of-type{border-bottom:0;}
    .evo-lib{font-size:.95rem;color:#333;} .evo-tend{font-size:.8rem;color:#7a8a94;margin-top:2px;}
    .evo-val{font-size:1.5rem;font-weight:800;color:#123f52;white-space:nowrap;}
    .mcn{display:grid;grid-template-columns:120px 1fr 1fr;gap:8px;padding:20px 16px;align-items:stretch;}
    .mcn-col{text-align:center;font-weight:600;color:#444;font-size:.9rem;align-self:end;}
    .mcn-lig{font-weight:600;color:#444;font-size:.9rem;display:flex;align-items:center;}
    .mcn-c{background:#f4f6f8;border-radius:8px;padding:22px 12px;text-align:center;color:#666;font-size:.9rem;display:flex;flex-direction:column;justify-content:center;}
    .mcn-c b{display:block;font-size:1.8rem;color:#5b6b75;} .mcn-pct{font-size:.78rem;opacity:.75;margin-top:2px;}
    .mcn-c.change{background:#fdf1e7;color:#8a4b14;} .mcn-c.change b{display:block;font-size:1.8rem;color:#d35400;}
    .mcn-c.fort{background:#fbe3e1;color:#8e2b20;} .mcn-c.fort b{color:#c0392b;font-size:2.2rem;}
    .reponses{padding:16px 48px 56px;background:#fff;}
    .reponses h2{text-align:center;font-size:1.6rem;font-weight:700;color:#123f52;margin:0 0 32px;}
    .reponses-grille{display:grid;grid-template-columns:repeat(3,1fr);max-width:1200px;margin:0 auto;}
    .reponse{display:flex;flex-direction:column;align-items:flex-start;padding:0 32px;border-left:1px solid #e3e9ee;}
    .reponse:first-child{border-left:0;}
    .reponse h3{font-size:1.2rem;font-weight:700;color:#123f52;margin:0 0 10px;}
    .reponse p{color:#4a5a63;font-size:1rem;line-height:1.55;flex:1;margin-bottom:10px;}
    .reponse .preuve{font-size:.85rem;color:#7a8a94;margin-bottom:10px;cursor:help;border-bottom:1px dotted #9aa6ad;}
    .reponse .voir{color:#1f6f8b;text-decoration:underline;text-underline-offset:3px;}
    .credits{text-align:center;font-size:.72rem;color:#9aa6ad;padding:0 0 24px;background:#fff;}
    @media (max-width:900px){.hero h1{font-size:2.6rem;} .reponses{padding:8px 16px 32px;} .reponses-grille{grid-template-columns:1fr;} .reponse{border-left:0;padding:0 0 28px;} .photos{grid-template-columns:1fr;padding:0 16px 30px;}
      .bande-logos{padding:10px 16px;} .bande-logos span{display:none;} .chiffre{border-left:0;margin-bottom:24px;} .fleche{transform:rotate(90deg);}}")

ui <- page_navbar(
  id = "nav", fillable = FALSE, theme = theme,
  title = div(class = "marque",
              div(div(class = "marque-titre", "L'eau des data centers"),
                  div(class = "marque-sous", "SAÉ 5.EMS.01 · IUT de Carcassonne"))),
  window_title = "L'eau des data centers - SAÉ 5.EMS.01",
  sidebar = sidebar(id = "sb", width = 260, open = "closed",
    h6(bs_icon("sliders"), " Hypothèses"),
    sliderInput("horizon", "Année", min = 2024, max = 2035, value = 2024, step = 1, sep = "", ticks = FALSE,
                animate = animationOptions(interval = 2200, loop = FALSE)),
    radioButtons("scen", "Scénario AIE (2025-2035)", c("Lente" = "bas", "Centrale" = "central", "Rapide" = "haut"), "central"),
    sliderInput("coef", "Eau par kWh (litres)", min = round(wue[["bas"]], 2), max = round(wue[["haut"]], 2), value = wue[["central"]],
                step = 0.01, ticks = FALSE),
    div(class = "small text-muted mb-3", paste0(fmt(wue[["bas"]], 2), " : sur place (Arcep) · ", fmt(wue[["central"]], 2),
        " : sur place, estimation centrale · ", fmt(wue[["haut"]], 2), " : en ajoutant l'eau des centrales électriques")),
    radioButtons("seuil", "« Région sèche » : stress Aqueduct", SEUILS, "3"),
    radioButtons("repartition", "Répartition de l'électricité 2024", c("Par nombre de sites (référence)" = "sites", "Avec la capacité des sites IA (Epoch AI)" = "capacite"), "sites"),
    conditionalPanel("input.repartition == 'capacite'",
      sliderInput("fc", "Facteur de charge des sites IA", min = 0.5, max = 0.9, value = 0.7, step = 0.05, ticks = FALSE)),
    div(class = "small text-muted mb-3", textOutput("couverture", inline = TRUE)),
    div(class = "small text-muted", "▶ fait défiler les années. Entre 2024, 2030 et 2035 (scénarios AIE), les valeurs sont interpolées. Tous les chiffres de l'application, accueil compris, suivent ces réglages.")),

  # ---- Accueil
  nav_panel("Accueil", value = "Accueil",
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
          h1("L'eau des data centers"),
          p(class = "hero-question", "Les ressources en eau suffiront-elles, aujourd'hui et en 2035 ?"),
          actionButton("entrer", "Découvrir l'étude", class = "btn-hero"),
          div(class = "hero-noms", "Ayi AMAVIGAN · Ada GADJI · Aboubakar COULIBALY", br(),
              span("Professeure : Mme Viguier-Pla · IUT de Carcassonne")))),
      uiOutput("accueil_chiffres"),
      div(class = "reponses",
        h2("Alors, l'eau suffira-t-elle ?"),
        uiOutput("accueil_reponses")),
      div(class = "photos",
        div(class = "photo", style = "background-image:url('photo_ashburn.jpg');",
            div(class = "legende", tags$b("Ashburn, Virginie"), br(), "La plus forte concentration de data centers au monde")),
        div(class = "photo", style = "background-image:url('photo_council_bluffs.jpg');",
            div(class = "legende", tags$b("Council Bluffs, Iowa"), br(), paste0("Le site Google qui consomme le plus d'eau : ", fmt(K$google_m3 / 1e6, 1), " millions de m³ en 2025 (", fmt(K$fr_eau, 1), " pour toute la France dans notre estimation)")))),
    )
  ),

  # ---- Carte du monde
  nav_panel("Carte",
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "La carte de l'étude en 4 étapes"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              "Étape 3 : couleur = manque d'eau du pays (ressource), bulle = eau des data centers (demande). Étape 4 : pays cumulant au moins 2 signaux : eau des data centers ≥ 1 % de celle des habitants ; ≥ 25 % des sites en région sèche ; stress hydrique du pays ≥ 50 %. Survol d'un pays : fourchette selon le coefficient d'eau."),
            selectInput("pays_sel", NULL, width = "210px", CHOIX_PAYS, "MONDE"))),
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
                 plotlyOutput("fiche_clim", height = "150px"), png_lien("fiche_clim"))))
    )
  ),

  # ---- Régions
  nav_panel("Régions",
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "Où se concentrent les data centers, et où l'eau manque-t-elle ?"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              "Densité : estimation par noyau gaussien (méthode non paramétrique) sur les sites OpenStreetMap (20/09/2026). Largeur du noyau : écart-type de 0,75° (environ 80 km) pour le monde ; pour un pays, règle de référence normale divisée par 2. Les contours entourent les zones qui regroupent les 50 %, 25 % et 10 % de sites les plus concentrés. « Région sèche » : stress hydrique Aqueduct au-dessus du seuil choisi à gauche (40 % par défaut). 2030 et 2050 : scénario « business as usual » d'Aqueduct 4.0 (SSP3 / RCP 7.0) ; l'horizon d'Aqueduct (2030, 2050) n'est pas celui de l'électricité (2035). Test pays / région : onglet Méthode > Pays ou région ?"),
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
           plotlyOutput("reg_barres", height = "430px"), png_lien("reg_barres"))
    )
  ),

  # ---- Saisons
  nav_panel("Saisons",
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", "Effet ciseaux : plus de chaleur, moins de pluie"),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              "Barres bleues : pluie moyenne du mois (normales 1991-2020, Banque mondiale CCKP). Courbe rouge : degrés-jours de refroidissement = (température moyenne du mois − 18 °C) × nombre de jours, si positif. En orange : mois « chauds et secs » = température au-dessus de la moyenne annuelle du pays ET pluie en dessous de la moyenne mensuelle du pays. Limite : la demande en eau du refroidissement dépend surtout de la température humide (chaleur + humidité), pas seulement de la température de l'air."),
            selectInput("pays_clim", NULL, width = "210px", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)], "FRA"))),
        div(class = "px-3 pt-2", div(class = "note-limite", "Échelle nationale : la moyenne du pays peut masquer des régions plus chaudes ou plus sèches (nos données climatiques mensuelles ne descendent pas sous le pays). C'est la même limite que celle montrée dans l'onglet Régions.")),
        uiOutput("clim_chiffres"),
        plotlyOutput("clim", height = "330px"), png_lien("clim"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("clim_retenir", inline = TRUE))),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span(class = "titre-bloc", "La tension voyage avec les saisons"),
          tooltip(bs_icon("info-circle", class = "aide"),
            "Pour le mois choisi, chaque pays est comparé à sa propre moyenne de l'année : plus chaud (température > moyenne annuelle) ? plus sec (pluie < moyenne mensuelle) ? Pays entiers : un grand pays peut cacher des régions très différentes. Appuyez sur ▶ pour faire défiler les mois.")),
        div(class = "px-3 pt-2 curseur-mois",
          sliderInput("mois_carte", NULL, min = 1, max = 12, value = 7, step = 1, ticks = FALSE, width = "100%",
                      animate = animationOptions(interval = 1600, loop = TRUE))),
        div(class = "px-3 text-center fw-bold", textOutput("mois_nom", inline = TRUE)),
        leafletOutput("carte_mois", height = "380px"),
        card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("mois_retenir", inline = TRUE)))
    ),
    card(full_screen = TRUE,
      card_header(class = "d-flex justify-content-between align-items-center",
        span(class = "titre-bloc", "Calendrier de la tension (20 pays les plus équipés)"),
        tooltip(bs_icon("info-circle", class = "aide"),
          "Pays classés du nord au sud. Rouge : mois chaud et sec ; orange clair : chaud ; jaune pâle : sec ; bleu : ni l'un ni l'autre (par rapport à la moyenne du pays). Pour les très grands pays (États-Unis, Chine, Inde, Australie), la moyenne nationale mélange des climats très différents.")),
      plotlyOutput("calendrier", height = "520px"), png_lien("calendrier"),
      card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ",
        textOutput("calendrier_retenir", inline = TRUE)))
  ),

  # ---- Usages de l'eau (arbitrages)
  nav_panel("Usages",
    card(card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
           span(class = "titre-bloc", textOutput("litres_titre", inline = TRUE)),
           div(class = "d-flex align-items-center gap-2",
             tooltip(bs_icon("info-circle", class = "aide"),
               "Répartition des prélèvements par usage : FAO AQUASTAT, dernière année connue (souvent 2017 ou avant ; l'année est indiquée sous les chiffres). Data centers : eau consommée sur place estimée pour l'année choisie à gauche (2024 par défaut). On compare une consommation à des prélèvements : la part des data centers est donc minorée."),
             selectInput("pays_us", NULL, width = "210px",
                         c("Monde" = "MONDE", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)][!is.na(d$pays$pct_menages[order(d$pays$country)])]), "MONDE"))),
         card_body(uiOutput("litres")),
         card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("litres_retenir", inline = TRUE))),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span(class = "titre-bloc", "Un data center, en équivalents"),
          tooltip(bs_icon("info-circle", class = "aide"),
            "Un an de fonctionnement. Électricité : GWh par MW installé mesurés sur les data centers européens (Commission européenne, 2024), soit un taux de charge moyen. Eau : coefficient choisi à gauche (eau consommée sur place).")),
        div(class = "parcours d-flex flex-wrap align-items-center column-gap-4 px-3 pt-3",
          radioButtons("mw", NULL, inline = TRUE, c("1 MW" = 1, "10 MW" = 10, "100 MW" = 100), 10),
          div(class = "d-flex align-items-center gap-2", span(class = "small text-muted", "Électricité de :"),
              selectInput("mix", NULL, width = "220px", setNames(d$pays$iso3, d$pays$country)[order(d$pays$country)][!is.na(d$pays$gco2_kwh[order(d$pays$country)])], "FRA"))),
        card_body(uiOutput("conv")),
        card_footer(class = "small text-muted", uiOutput("conv_hyp", inline = TRUE))),
      bloc(paste0("France : ", fmt(K$indirecte, 0), " % de l'eau des data centers est indirecte"), "directe",
           paste0("Arcep 2024 : 23 opérateurs suivis, ", fmt(ARCEP_TWH, 1), " TWh (une partie seulement du parc français). ",
                  fmt(ARCEP_TWH * wue[["bas"]], 2), " million de m³ consommés sur place (", fmt(wue[["bas"]], 2), " L/kWh) ; ",
                  fmt(ARCEP_TWH * wue[["haut"]], 1), " millions de m³ en ajoutant l'eau utilisée pour produire leur électricité (",
                  fmt(wue[["haut"]], 2), " L/kWh). Notre chiffre central (", fmt(wue[["central"]], 2), " L/kWh) ne compte que l'eau consommée sur place."),
           "Le levier principal : le mix électrique.", "300px")),
    bloc("Usages de l'eau, 1990-2035", "tendance",
         "Prélèvements par usage (FAO AQUASTAT) et eau des data centers estimée 2024-2035 (bande : croissance lente à rapide). Échelle logarithmique.",
         textOutput("tendance_retenir", inline = TRUE), "360px")
  ),

  # ---- Demain
  nav_panel("Demain",
    layout_columns(col_widths = c(6, 6),
      bloc(paste0("×", fmt(K$f35_min, 1), " à ×", fmt(K$f35_max, 1), " d'ici 2035 (monde)"), "traj",
           "Électricité mondiale des data centers : mesures 2020-2024 (trait plein) puis les 4 scénarios de l'Agence internationale de l'énergie (Energy and AI, 2025). Le facteur porte sur le monde ; chaque zone (États-Unis, Chine, Europe, reste du monde) a son propre facteur, plus faible en Europe.",
           "La hausse est certaine, son ampleur ne l'est pas.", "340px"),
      bloc("Les pays les plus exposés en 2035", "top10_fut",
           "Part de l'eau prélevée du pays consommée par les data centers en 2024, 2030 et 2035. Dénominateur gardé constant (prélèvements moyens 2015-2022), faute de prévision pays par pays. Barre rose : écart entre croissance lente et rapide.",
           textOutput("fut_retenir", inline = TRUE), "340px")),
    bloc(textOutput("fr_titre", inline = TRUE), "fr_scen",
         "Eau consommée sur place par les data centers français selon la croissance (facteur de la zone Europe, AIE). En rouge : la part due à l'IA. Étiquette : part de l'eau prélevée en France, avec au dénominateur la prévision du modèle de dérive (voir Méthode). Même calcul que dans l'onglet Usages.",
         textOutput("frscen_retenir", inline = TRUE), "330px")
  ),

  # ---- IA : data centers dédiés à l'IA (Epoch AI) face au stress hydrique
  nav_panel("IA",
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
          span(class = "titre-bloc", textOutput("ia_titre", inline = TRUE)),
          div(class = "d-flex align-items-center gap-2",
            tooltip(bs_icon("info-circle", class = "aide"),
              paste0("Grands data centers dédiés à l'IA recensés par Epoch AI (« AI data centers », CC BY 4.0, mis à jour le 23/11/2026) : ", nrow(d$ia),
                     " sites, environ 43 % du calcul IA mondial selon Epoch AI. Taille du cercle : puissance IT actuelle (MW, hors refroidissement). Couleur : stress hydrique de la région (WRI Aqueduct) ; un site est placé au centre des sites OpenStreetMap de sa région (pas à son adresse exacte). Région déduite de l'adresse (État américain) ou, à défaut, du nom du site (table manuelle dans prep.R).")),
            radioButtons("ia_horiz", NULL, inline = TRUE, c("Aujourd'hui" = "stress_region", "2030" = "stress_2030", "2050" = "stress_2050"), "stress_region"))),
        leafletOutput("ia_carte", height = "520px")),
      card(card_header(span(class = "titre-bloc", "Sites IA et manque d'eau")),
           uiOutput("ia_chiffres"),
           plotlyOutput("ia_regions", height = "330px"), png_lien("ia_regions"))),
    bloc(textOutput("ia_eau_titre", inline = TRUE), "ia_eau",
         paste0("Eau consommée par les data centers du monde, découpée entre IA et hors IA : hypothèse de l'AIE (", fmt(100 * d$part_ia[["monde"]], 0),
                " % de l'électricité des data centers pour l'IA en 2024, puis ", fmt(100 * d$part_ia[["hausse"]], 0),
                " % de la hausse). Losange : les grands sites IA d'Epoch AI, avec leur puissance actuelle (2026) × PUE ", fmt(PUE_IA, 1),
                " × facteur de charge × 8 760 h × coefficient d'eau. Coefficient et scénario : réglages à gauche."),
         textOutput("ia_eau_retenir", inline = TRUE), "340px")),

  # ---- Hypothèses : énoncé, test, statistique, p-value, verdict, robustesse
  nav_panel("Hypothèses",
    p(class = "text-muted", "Les trois hypothèses de l'étude, testées avec les réglages choisis à gauche (coefficient d'eau, scénario, seuil de « région sèche »)."),
    uiOutput("hypotheses")),

  # ---- Méthode
  nav_panel("Méthode",
    navset_card_pill(
      nav_panel("Estimation",
        p(class = "text-muted mb-2", "Estimation en 5 étapes (exemple : la France)."),
        uiOutput("etapes"),
        layout_columns(col_widths = c(6, 6), class = "mt-3",
          bloc("88 à 999 millions de m³ selon le coefficient", "coefs",
               paste0("Eau de tous les data centers du monde en 2024 (", fmt(K$twh, 0), " TWh) avec chacun des 3 coefficients : ",
                      fmt(wue[["bas"]], 2), " L/kWh (eau consommée sur place, Arcep 2024), ", fmt(wue[["central"]], 2),
                      " (eau consommée sur place, moyenne pondérée par type de refroidissement ; source d'origine À VÉRIFIER, citée dans notre Rapport 1), ",
                      fmt(wue[["haut"]], 2), " (sur place + production de l'électricité, Arcep 2024). Les deux premiers mesurent la même chose ; le troisième ajoute l'eau des centrales."),
               "Principale incertitude : d'où une fourchette.", "300px"),
          bloc("Pays calés sur l'AIE : contrôle de cohérence, pas une validation", "validation",
               "Part des data centers dans l'électricité du pays : notre modèle (bleu) et la valeur publiée (gris). États-Unis, Chine, Irlande, Japon et Inde sont calés sur l'AIE : l'écart ne vient que du dénominateur (électricité nationale). Seul contrôle indépendant : la France, comparée au chiffre ADEME-Arcep 2020 pour tout le parc.",
               textOutput("validation_retenir", inline = TRUE), "300px"))),
      nav_panel("Prévision",
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE,
            card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
              span(class = "titre-bloc", textOutput("bt_titre", inline = TRUE)),
              div(class = "d-flex align-items-center gap-2",
                tooltip(bs_icon("info-circle", class = "aide"),
                  "On estime les 4 modèles avec les années jusqu'à l'année choisie (zone grise), puis on compare leur prévision à ce qui s'est réellement passé ensuite (points creux). Seulement 8 années observées (2015-2022) : le choix du modèle reste fragile."),
                span(class = "small text-muted", "Estimer jusqu'en"),
                selectInput("prev_fin", NULL, width = "100px", 2017:2021, 2019))),
            plotlyOutput("backtest", height = "360px"), png_lien("backtest"),
            card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("bt_retenir", inline = TRUE))),
          bloc(textOutput("mae_titre", inline = TRUE), "mae",
               "MAE = erreur absolue moyenne ; RMSE = racine de l'erreur quadratique moyenne (pénalise les grosses erreurs), en milliards de m³. Gris : test 2020-2022. Bleu : validation croisée glissante (on refait la prévision à 1 an chaque année à partir de 2019, fonction tsCV du package forecast).",
               "Modèle retenu : dérive (hypothèse prudente vu le peu de données).", "360px")),
        bloc("France : l'eau prélevée continue de baisser", "fr_prev",
             "Prélèvements d'eau de la France (tous usages), observés 2015-2022 puis prévus par le modèle de dérive, avec intervalle de confiance à 95 %. Cette prévision sert de dénominateur pour la part de l'eau en 2030-2035, pour la France seulement ; pour les autres pays, les prélèvements sont gardés constants.",
             textOutput("prev_retenir", inline = TRUE), "320px")),
      nav_panel("ACP",
        div(class = "parcours d-flex flex-wrap align-items-center column-gap-4 mb-3",
          checkboxInput("acp_log", "Variables asymétriques en log (recommandé)", TRUE),
          div(class = "d-flex align-items-center gap-2", span(class = "small text-muted", "Axes affichés :"),
              selectInput("acp_axes", NULL, width = "110px", c("1 et 2" = "1-2", "1 et 3" = "1-3", "2 et 3" = "2-3"), "1-2"))),
        layout_columns(col_widths = c(5, 7),
          bloc(textOutput("acp_eb_titre", inline = TRUE), "acp_eboulis",
               paste0("Analyse en composantes principales (FactoMineR, PCA avec scale.unit = TRUE) sur 8 indicateurs centrés-réduits, ", nrow(d$classes),
                      " pays. Variables très asymétriques passées en log(1 + x) : nombre de data centers, prélèvements, stress hydrique, eau disponible par habitant, PIB par habitant, production électrique, population ; intensité carbone gardée telle quelle (déjà symétrique). Barres : part de l'information (inertie) par axe ; pointillé : seuil de Kaiser (valeur propre = 1)."),
               textOutput("acp_eb_retenir", inline = TRUE), "340px"),
          bloc(textOutput("acp_cercle_titre", inline = TRUE), "acp_cercle",
               "Cercle des corrélations : plus une flèche est longue, mieux la variable est représentée. En rouge : part de l'eau des data centers, variable supplémentaire (elle ne construit pas les axes). L'ACP est recalculée en direct (case « log », choix des axes) ; les familles de couleur viennent toujours de la classification de référence (version en log).",
               "La part de l'eau des data centers est plus forte dans les petits pays ; c'est en partie mécanique : elle dépend du nombre de sites et des prélèvements, deux variables actives.", "340px")),
        layout_columns(col_widths = c(7, 5),
          bloc(paste0("Les ", nrow(d$classes), " pays sur le plan choisi"), "acp_ind",
               "Coordonnées des pays sur les axes choisis, colorées selon leur famille (onglet Classification).",
               "Les pays très secs se détachent vers le bas, mais ont encore peu de data centers.", "420px"),
          card(card_header(class = "d-flex justify-content-between align-items-center",
                 span(class = "titre-bloc", "Contributions et qualité de représentation"),
                 tooltip(bs_icon("info-circle", class = "aide"), "Contribution : part (en %) de chaque variable dans la construction de l'axe. cos² : qualité de représentation de la variable sur le plan choisi (1 = parfaite).")),
               card_body(uiOutput("acp_tables")),
               card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ",
                 "Le passage au log du stress hydrique rééquilibre l'analyse : les pays du Golfe ne tirent plus un axe à eux seuls.")))),
      nav_panel("Classification",
        layout_columns(col_widths = c(7, 5),
          bloc(paste0(d$cah_info$k, " familles, ", fmt(d$cah_info$inter, 0), " % de l'inertie expliquée"), "dendro",
               paste0("Classification ascendante hiérarchique (méthode de Ward sur les coordonnées factorielles) sur les ", d$cah_info$axes,
                      " axes retenus de l'ACP, puis consolidation par k-means (fonction HCPC de FactoMineR). Plus deux pays se rejoignent bas, plus ils se ressemblent. Critère de coupure : saut de hauteur (perte d'inertie) entre deux fusions. Le plus grand saut ne sépare que 2 groupes, trop grossiers ; parmi 3 à 6 familles, on retient ", d$cah_info$k,
                      " : groupes interprétables, aucun de moins de 3 pays. Rapport inertie inter-classes / inertie totale : ", fmt(d$cah_info$inter, 0), " %."),
               paste0("Hauteurs des dernières fusions : ", paste(fmt(d$cah_info$hauteurs[1:5], 2), collapse = " · "), " ; coupe en ", d$cah_info$k, "."), "760px"),
          card(card_header(class = "d-flex justify-content-between align-items-center",
                 span(class = "titre-bloc", paste("Portrait des", d$cah_info$k, "familles")),
                 tooltip(bs_icon("info-circle", class = "aide"), "Valeurs médianes de chaque famille (moins sensibles aux pays extrêmes que les moyennes). En dessous : variables qui distinguent le plus la famille (v-test : au-delà de ±2, la moyenne de la famille diffère nettement de la moyenne générale).")),
               card_body(uiOutput("portrait")),
               card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("portrait_retenir", inline = TRUE)))),
        card(card_header(class = "d-flex justify-content-between align-items-center",
               span(class = "titre-bloc", "Robustesse : les familles changent-elles avec une autre méthode ?"),
               tooltip(bs_icon("info-circle", class = "aide"), "On refait le classement par k-means seul (25 départs aléatoires) sur les mêmes axes, puis on croise les deux classements. Une famille robuste se retrouve presque entière dans un seul groupe k-means.")),
             card_body(uiOutput("cah_robuste"))),
        bloc("Carte des 4 familles", "carte_classes",
             "Pays colorés selon leur famille. Gris : pays non classés (moins de 10 data centers).",
             "Les grands pays équipés ne sont pas ceux où l'eau des data centers pèse le plus.", "380px", leafletOutput)),
      nav_panel("ACM",
        layout_columns(col_widths = c(4, 8),
          bloc(paste0("2 axes résument ", fmt(d$acm_eig$cumul[2], 0), " % de l'inertie"), "acm_eboulis",
               paste0("Analyse des correspondances multiples (FactoMineR, MCA) sur ", nrow(d$acm_ind), " pays (", paste(d$acm_exclus, collapse = " et "),
                      " exclus : aucun site géolocalisé). 7 indicateurs découpés en classes : taille du parc, classe de stress hydrique du pays (Aqueduct), PIB par habitant, famille de mix électrique (intensité carbone en terciles, faute de données sur la part de chaque énergie), part de l'eau des data centers, part des sites en région sèche (seuil 40 %), zone climatique (température et pluie annuelles 1991-2020). Chaque modalité regroupe au moins 5 % des pays. La famille de la classification (CAH) est ajoutée en variable illustrative. En ACM, les pourcentages d'inertie sont mécaniquement faibles : ils sous-estiment l'information portée par les axes."),
               "Complète l'ACP avec des variables qualitatives, moins sensibles aux valeurs extrêmes.", "320px"),
          bloc("Là où les data centers pèsent le plus sur l'eau, l'eau ne manque pas", "acm_plan",
               "Plan des modalités (axes 1 et 2). Deux modalités proches sont souvent portées par les mêmes pays. En rouge : familles de la classification (supplémentaires, elles ne construisent pas les axes). Taille du point : nombre de pays.",
               textOutput("acm_retenir", inline = TRUE), "520px"))),
      nav_panel("Évolution",
        div(class = "parcours d-flex align-items-center gap-2 mb-3",
          span(class = "small text-muted", "Voir l'évolution de :"),
          selectInput("pays_evo", NULL, width = "260px", CHOIX_EVO, "TOP30")),
        layout_columns(col_widths = c(7, 5),
          bloc(textOutput("evo_titre", inline = TRUE), "evo_courbes",
               "Base 100 en 2020 (ou l'année disponible la plus proche). Data centers : électricité mondiale publiée par l'AIE (Energy and AI, 2025) pour 2020, 2023 et 2024 seulement ; elle sert de repère, même quand un pays est choisi. Électricité produite et eau prélevée : panel annuel du Rapport 2 (FAO AQUASTAT pour l'eau), 2015-2022. Attention : les périmètres diffèrent (monde pour les data centers, 30 pays ou un pays pour le reste). Pour un pays, l'eau n'est pas mesurée tous les ans : seuls les points disponibles sont tracés.",
               textOutput("evo_retenir", inline = TRUE), "380px"),
          card(card_header(class = "d-flex justify-content-between align-items-center",
                 span(class = "titre-bloc", "Croissance par an, et tendance"),
                 tooltip(bs_icon("info-circle", class = "aide"),
                   "Taux de croissance annuel moyen : data centers 2020-2024, électricité et eau entre la première et la dernière année disponibles (2015-2022). Test de Mann-Kendall : la série monte-t-elle (ou baisse-t-elle) régulièrement, au-delà du hasard ? Il repose sur le tau de Kendall entre l'année et la valeur. Seuil : 5 %. Il faut au moins 4 années mesurées.")),
               card_body(uiOutput("evo_chiffres")),
               card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ",
                 paste0("Avec une part de l'ordre de ", pct(K$dc_litres / 100), " de l'eau prélevée dans le monde, l'effet ne peut pas se voir dans le total national : c'est à l'échelle locale qu'il faut regarder."))))),
      nav_panel("Pays ou région ?",
        layout_columns(col_widths = c(7, 5),
          bloc(textOutput("mcn_titre", inline = TRUE), "mcn_cases",
               "Chaque site géolocalisé est classé deux fois avec WRI Aqueduct : selon le stress de son pays, puis selon celui de sa région. Seuil de « région sèche » : réglage à gauche.",
               "Site par site, regarder le pays entier sous-estime le risque.", "330px", uiOutput),
          card(card_header(class = "d-flex justify-content-between align-items-center",
                 span(class = "titre-bloc", "Le test de McNemar"),
                 tooltip(bs_icon("info-circle", class = "aide"),
                   "Les deux pourcentages portent sur les mêmes sites : on ne peut pas les comparer comme deux échantillons indépendants. Le test de McNemar ne regarde que les sites qui changent de case : si l'échelle ne changeait rien, ils seraient à peu près aussi nombreux dans les deux sens. Il suppose des sites indépendants, ce qui n'est pas le cas (grappes).")),
               card_body(uiOutput("mcn_chiffres")),
               card_footer(class = "retenir", bs_icon("arrow-right-circle-fill"), " ", textOutput("mcn_retenir", inline = TRUE)))))
    )
  ),

  # ---- Sources
  nav_panel("Sources",
    navset_card_pill(
      nav_panel("Données par pays",
        div(class = "d-flex justify-content-end gap-2 mb-2", downloadButton("dl", "Télécharger les données (CSV)", class = "btn-sm"),
            downloadButton("dl_dico", "Dictionnaire des variables (CSV)", class = "btn-sm")),
        DTOutput("tab_pays"),
        tags$details(class = "mt-3", tags$summary("Dictionnaire des variables et imputations"),
          tags$table(class = "table table-sm mt-2",
            tags$thead(tags$tr(tags$th("Variable"), tags$th("Définition"), tags$th("Unité"), tags$th("Source"), tags$th("Traitement / imputation"))),
            tags$tbody(lapply(seq_len(nrow(d$dictionnaire)), function(i) do.call(tags$tr, lapply(d$dictionnaire[i, ], function(v) tags$td(v)))))))),
      nav_panel("Sources",
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
            tags$tr(tags$td("Data centers dédiés à l'IA (puissance IT)"),
                    tags$td("Epoch AI, « AI data centers », epoch.ai/data/ai-data-centers, licence CC BY 4.0, consulté le 24/11/2026"), tags$td("mis à jour le 23/11/2026")),
            tags$tr(tags$td("Data centers en France"), tags$td("Arcep, ADEME, RTE"), tags$td("2021-2030")),
            tags$tr(tags$td("Photos de l'accueil"), tags$td("Wikimedia Commons : BalticServers.com (CC BY-SA 3.0), Theodore Christopher (CC0), Chad Davis (CC BY 2.0)"), tags$td(""))))),
      nav_panel("Limites",
        tags$ul(
          tags$li(paste0("Ce que mesure l'eau estimée : le chiffre central (", fmt(K$eau_c, 0), " Mm³, ", fmt(wue[["central"]], 2),
                         " L/kWh) est l'eau consommée sur place pour le refroidissement. En ajoutant l'eau utilisée pour produire l'électricité (", fmt(wue[["haut"]], 2),
                         " L/kWh), on atteint ", fmt(K$eau_h, 0), " Mm³ ; en France, cette eau indirecte représente ", fmt(K$indirecte, 0), " % du total (Arcep).")),
          tags$li("Consommation comparée à des prélèvements : les coefficients en L/kWh mesurent de l'eau consommée (évaporée), AQUASTAT mesure de l'eau prélevée (dont une grande partie est rendue). Aucune donnée de consommation nationale n'étant disponible, la « part de l'eau du pays » est minorée."),
          tags$li(paste0("Électricité répartie selon le nombre de sites, pas leur taille : pays calés sur l'AIE (", paste(K$cales, collapse = ", "),
                         ") = valeur publiée ; autres pays = reste de leur zone réparti par site (≈ ", fmt(K$gwh_eur, 0), " GWh par site en Europe, ",
                         fmt(K$gwh_rdm, 0), " ailleurs). Un seul site Google (", K$google_site, ", ", fmt(K$google_m3 / 1e6, 1),
                         " Mm³ en 2025) consomme presque autant que notre estimation pour toute la France (", fmt(K$fr_eau, 1),
                         " Mm³) : répartir par nombre de sites sous-estime les très grands sites.")),
          tags$li("Contrôle de la répartition : les pays comparés à l'AIE sont ceux qui y sont calés (contrôle de cohérence, pas une validation). Seul contrôle indépendant : la France, sous-estimée par rapport à ADEME-Arcep."),
          tags$li(paste0("Un seul coefficient d'eau pour tous les pays : d'où une fourchette (", fmt(wue[["bas"]], 2), " à ", fmt(wue[["haut"]], 2), " L/kWh).")),
          { lux <- d$pays |> filter(iso3 == "LUX")
            tags$li(paste0("Deux indicateurs de stress différents : le « stress hydrique » du tableau par pays vient de la FAO (prélèvements / ressources renouvelables du pays) ; les « sites en région sèche » viennent de WRI Aqueduct (par région et par bassin). Exemple : Luxembourg, ",
                           fmt(lux$stress0, 1), " % pour la FAO, mais ", fmt(lux$pct_sites_region_stress, 0), " % de ses ", lux$sites_osm,
                           " sites géolocalisés dans une région classée ≥ 40 % par Aqueduct (canton de Luxembourg). Ce n'est pas une erreur de géocodage.")) },
          tags$li(textOutput("limite_mcn", inline = TRUE)),
          tags$li(paste0("Prévision des prélèvements français : ", d$prev_fr$n_train + d$prev_fr$n_test, " années seulement (", d$prev_fr$n_train,
                         " d'apprentissage, ", d$prev_fr$n_test, " de test, dont le Covid et la sécheresse 2022) : le choix du modèle est fragile. Pour les autres pays, les prélèvements sont gardés constants.")),
          tags$li("Saisons à l'échelle nationale seulement ; la température humide, qui pilote vraiment l'eau du refroidissement, n'est pas prise en compte."),
          tags$li(paste0("Sites IA (Epoch AI) : ", nrow(d$ia), " grands sites seulement (environ 43 % du calcul IA mondial, surtout aux États-Unis), mis en service depuis 2024 ; puissance IT estimée (exacte à un facteur 1,4 près dans 80 % des cas). L'électricité suppose un PUE de ",
                         fmt(PUE_IA, 1), " et un facteur de charge réglable. Capacités de 2026 appliquées à l'électricité de 2024 : la variante « capacité » est donc indicative. Sites placés au centre de leur région, pas à leur adresse.")),
          tags$li("ACP : la part de l'eau des data centers (variable supplémentaire) dépend en partie mécaniquement du nombre de sites et des prélèvements, qui sont des variables actives."),
          tags$li("Échelle nationale pour l'estimation ; échelle régionale seulement pour le stress hydrique (Aqueduct)."),
          tags$li("Sites OpenStreetMap : inventaire partiel ; la densité dépend de la largeur du noyau."),
          tags$li("Degrés-jours calculés sur la température moyenne du mois (approximation)."),
          tags$li("Part de l'IA : hypothèse de l'AIE, pas une mesure.")),
        p(class = "small text-muted", "Ayi AMAVIGAN, Ada GADJI, Aboubakar COULIBALY · BUT3 Science des données · IUT de Carcassonne · 2026-2027"))))
)

# --------------------------------------------------------------- Serveur -----
server <- function(input, output, session) {
  observeEvent(input$entrer, nav_select("nav", "Carte"))
  observeEvent(input$rep_usages, nav_select("nav", "Usages"))
  observeEvent(input$rep_regions, nav_select("nav", "Régions"))
  observeEvent(input$rep_demain, nav_select("nav", "Demain"))

  # ---- Méthode > Évolution
  evo <- reactive({
    iso <- input$pays_evo %||% "TOP30"; s <- serie_evo(iso)
    nom <- if (iso == "TOP30") "des 30 pays" else paste0("(", names(CHOIX_EVO)[CHOIX_EVO == iso], ")")
    list(s = s, nom = nom, t_elec = tcam(s$elec, s$year), t_eau = tcam(s$eau, s$year),
         mk_elec = mk(s$elec, s$year), mk_eau = mk(s$eau, s$year))
  })
  output$evo_titre <- renderText({
    e <- evo(); r <- EVO$t_dc / e$t_elec
    if (is.na(r) || e$t_elec <= 0.1) "Les data centers grandissent bien plus vite que l'électricité du pays"
    else paste0("Les data centers (monde) grandissent ", fmt(r, 0), " fois plus vite que l'électricité ", e$nom)
  })
  output$evo_retenir <- renderText({
    t <- evo()$mk_eau
    if (is.null(t)) "Trop peu d'années mesurées pour l'eau."
    else if (t$p.value >= 0.05) "L'eau prélevée, elle, n'a pas de tendance significative (seuil de 5 %)."
    else if (t$estimate < 0) "L'eau prélevée, elle, baisse."
    else "L'eau prélevée augmente aussi : à surveiller."
  })
  output$evo_courbes <- renderPlotly({
    e <- evo(); s <- e$s; dc <- EVO$dc
    b <- function(v, a) { ok <- !is.na(v); if (!any(ok)) return(v); 100 * v / v[ok][which.min(abs(a[ok] - 2020))] }
    se <- s |> filter(!is.na(eau)); sl <- s |> filter(!is.na(elec))
    plot_ly() |>
      add_trace(x = dc$annee, y = b(dc$twh, dc$annee), name = "Électricité des data centers (monde)", type = "scatter",
                line = list(color = "#d35400", width = 3), mode = "lines+markers", marker = list(size = 8, color = "#d35400"),
                hovertemplate = "%{x} : %{y:.0f}<extra>Data centers</extra>") |>
      add_trace(x = sl$year, y = b(sl$elec, sl$year), name = paste("Électricité", e$nom), type = "scatter", mode = "lines+markers",
                line = list(color = "#1f6f8b", width = 2.5), marker = list(size = 5, color = "#1f6f8b"),
                hovertemplate = "%{x} : %{y:.0f}<extra>Électricité</extra>") |>
      add_trace(x = se$year, y = b(se$eau, se$year), name = paste("Eau prélevée", e$nom), type = "scatter", mode = "lines+markers",
                line = list(color = "#2e86c1", width = 2.5, dash = "dot"), marker = list(size = 5, color = "#2e86c1"),
                hovertemplate = "%{x} : %{y:.0f}<extra>Eau prélevée</extra>") |>
      layout(xaxis = list(title = "", dtick = 1, range = c(2014.5, 2024.5)), yaxis = list(title = "Indice (2020 = 100)"),
             legend = list(orientation = "h", x = 0, y = -0.12), hovermode = "x unified",
             shapes = list(list(type = "line", x0 = 2015, x1 = 2024, y0 = 100, y1 = 100, line = list(color = "#bbb", width = 1))))
  })
  output$evo_chiffres <- renderUI({
    e <- evo()
    ligne <- function(lib, taux, coul, tend = NULL)
      div(class = "evo-ligne", div(class = "evo-lib", lib, if (!is.null(tend)) div(class = "evo-tend", tend)),
          div(class = "evo-val", style = paste0("color:", coul),
              if (is.na(taux)) "n.d." else { t1 <- round(taux, 1) + 0; paste0(ifelse(t1 > 0, "+", ""), fmt(t1, 1), " %") }))
    tend <- function(t) if (is.null(t)) "trop peu d'années pour le test" else
      if (isTRUE(t$constante)) "même valeur chaque année (chiffre officiel non actualisé)" else
      paste0(if (t$p.value < 0.05) (if (t$estimate > 0) "hausse nette" else "baisse nette") else "pas de tendance significative au seuil de 5 %",
             " (Mann-Kendall, ", p_txt(t$p.value), ")")
    div(div(class = "small text-muted mb-2", "Croissance moyenne par an"),
        ligne("Électricité des data centers (monde)", EVO$t_dc, "#d35400"),
        ligne(paste("Électricité", e$nom), e$t_elec, "#1f6f8b", tend(e$mk_elec)),
        ligne(paste("Eau prélevée", e$nom), e$t_eau, "#2e86c1", tend(e$mk_eau)))
  })

  # ---- Méthode > Pays ou région ?
  output$mcn_titre <- renderText(paste(fmt(mcn()$caches, 0), "data centers cachés par la moyenne du pays"))
  output$mcn_retenir <- renderText({ m <- mcn()
    sens <- if (m$p_reg > m$p_pays) "la région révèle plus de sites en zone sèche que le pays" else "le pays paraît plus sec que la région"
    paste0("Avec ce seuil, ", sens, " : ", fmt(m$p_reg, 0), " % des sites vu de la région, ", fmt(m$p_pays, 0), " % vu du pays (", m$p_txt,
           ") ; région par région : ", fmt(m$p_reg_reg, 0), " % contre ", fmt(m$p_pays_reg, 0), " % (", p_txt(m$p_r), ").") })
  output$limite_mcn <- renderText({ m <- mcn()
    paste0("Test de McNemar : les sites sont groupés (", fmt(m$top_pct, 0), " % en ", m$top_region, "), la p-value est optimiste. Région par région (",
           m$n_reg, " régions), l'écart est de ", fmt(m$p_pays_reg, 0), " % contre ", fmt(m$p_reg_reg, 0), " % (", p_txt(m$p_r), ").") })
  output$mcn_cases <- renderUI({
    MCN <- mcn()
    n <- MCN$n
    case <- function(v, lib, cl = "") div(class = paste("mcn-c", cl), tags$b(fmt(v, 0)), lib, div(class = "mcn-pct", paste0(fmt(100 * v / n, 0), " % des sites")))
    div(class = "mcn",
      div(), div(class = "mcn-col", "Région sèche"), div(class = "mcn-col", "Région non sèche"),
      div(class = "mcn-lig", "Pays sec"), case(MCN$deux, "secs des deux points de vue"), case(MCN$inverse, "secs seulement vu du pays", "change"),
      div(class = "mcn-lig", "Pays non sec"), case(MCN$caches, "secs seulement vu de la région", "change fort"), case(MCN$aucun, "secs d'aucun point de vue"))
  })
  output$mcn_chiffres <- renderUI({ MCN <- mcn(); div(
    div(class = "evo-ligne", div(class = "evo-lib", "Stress élevé, vu du pays"), div(class = "evo-val", paste0(fmt(MCN$p_pays, 0), " %"))),
    div(class = "evo-ligne", div(class = "evo-lib", "Stress élevé, vu de la région"), div(class = "evo-val", style = "color:#c0392b", paste0(fmt(MCN$p_reg, 0), " %"))),
    div(class = "evo-ligne", div(class = "evo-lib", "Khi² de McNemar (1 ddl)"), div(class = "evo-val", fmt(MCN$chi2, 0))),
    div(class = "evo-ligne", div(class = "evo-lib", "Probabilité que ce soit le hasard"), div(class = "evo-val", MCN$p_txt)),
    div(class = "small text-muted mt-2", paste0(fmt(MCN$n, 0), " sites, chacun classé deux fois (WRI Aqueduct). « Région sèche » : stress ", seuil_txt(MCN$seuil), ".")),
    div(class = "note-limite mt-2", paste0("Les sites ne sont pas indépendants : ils sont groupés en grappes (", fmt(MCN$top_pct, 0), " % en ", MCN$top_region,
        "). La p-value est donc optimiste ; le résultat principal est descriptif.")),
    div(class = "evo-ligne", div(class = "evo-lib", "Contrôle : une ligne par région", div(class = "evo-tend", paste(fmt(MCN$n_reg, 0), "régions"))),
        div(class = "evo-val", style = "font-size:1.1rem", paste0(fmt(MCN$p_pays_reg, 0), " % → ", fmt(MCN$p_reg_reg, 0), " % · ", p_txt(MCN$p_r))))) })
  observeEvent(input$nav, toggle_sidebar("sb", open = input$nav != "Accueil"), ignoreInit = FALSE)

  # ---------- Réglages globaux : coefficient d'eau, scénario, seuil de « région sèche »
  w  <- reactive(as.numeric(input$coef %||% wue[["central"]]))
  seuil <- reactive(as.integer(input$seuil %||% "3"))
  base_pays <- reactive(if (identical(input$repartition, "capacite")) alloc_capacite(as.numeric(input$fc %||% 0.7)) else d$pays)
  calc <- function(w, scenario, horizon) calculer(w, scenario, horizon, base_pays())
  output$couverture <- renderText({
    b <- if (identical(input$repartition, "capacite")) base_pays() else d$pays |> mutate(methode = ifelse(cale_aie, "calé AIE", "nombre de sites"))
    t <- tapply(b$e0, b$methode, sum); tot <- sum(b$e0)
    paste0("Électricité 2024 répartie : ", paste0(fmt(100 * t / tot, 0), " % ", names(t), collapse = " · "), ".") })
  mcn <- reactive(mcn_calc(seuil()))
  sites_secs <- reactive(d$osm |> filter(!is.na(stress_region)) |> group_by(iso3) |>
                           summarise(pct_sites_region_stress = 100 * mean(est_sec(stress_region, seuil())), .groups = "drop"))
  hz <- reactive(as.integer(input$horizon))
  sc <- reactive(if (hz() == 2024) "central" else input$scen)
  res <- reactive(calc(w(), sc(), hz()) |> select(-pct_sites_region_stress) |> left_join(sites_secs(), by = "iso3"))
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
  titre_leg <- c(n_dc = "Nombre de data centers", part = "Part de l'eau prélevée du pays<br>consommée par les data centers",
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
             "<br>Eau consommée sur place : ", fmt(m$eau, 2), " millions de m³",
             "<br>Part de l'eau prélevée du pays : ", pct(m$part), " <span style='color:#888'>(entre ", pct(m$part_bas), " et ", pct(m$part_haut), ")</span>",
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
        addControl(htmltools::HTML("<div class='leg-bulle'><span class='rond'></span> Taille de la bulle =<br>eau consommée sur place par les data centers</div>"),
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
    ligne <- function(icone, ...) div(class = "etape-texte", span(class = "etape-num", icone), div(...))
    if (input$pays_sel != "MONDE") {
      x <- r |> filter(iso3 == input$pays_sel); rg <- d$regions |> filter(iso3 == input$pays_sel) |> arrange(-sites)
      return(ligne(bs_icon("zoom-in"), tags$b(x$country), " · ",
        if (nrow(rg)) paste0(sum(rg$sites), " sites géolocalisés, surtout en ", rg$region[1], " (", rg$sites[1], ")") else "pas de site géolocalisé",
        " · ", actionLink("retour_monde", "Monde")))
    }
    top5 <- r |> slice_max(n_dc, n = 5)
    switch(et,
      "1" = ligne(et, tags$b(paste0(fmt(sum(r$n_dc), 0), " data centers")), paste0(" · ", fmt(100 * sum(top5$n_dc) / sum(r$n_dc), 0), " % dans 5 pays")),
      "2" = { mx <- p |> filter(n_dc >= 5) |> slice_max(part, n = 1)
              ligne(et, tags$b(paste0(fmt(sum(r$eau), 0), " millions de m³ consommés sur place ", quand())),
                    paste0(" · moins de 0,1 % de l'eau prélevée du pays dans ", fmt(100 * mean(p$part < 0.1, na.rm = TRUE), 0), " % des cas · maximum : ", mx$country, " (", pct(mx$part), ")")) },
      "3" = ligne(et, tags$b(paste0(fmt(100 * sum(r$n_dc[!is.na(r$stress0) & r$stress0 < 50]) / sum(r$n_dc), 0), " % des sites")),
                  " sont dans des pays où l'eau ne manque pas"),
      "4" = { s4 <- arb() |> filter(signaux >= 2) |> arrange(-signaux)
              ligne(et, tags$b(paste0(nrow(s4), " pays à surveiller")), paste0(" · ", paste(s4$country, collapse = ", "))) })
  })
  observeEvent(input$retour_monde, updateSelectInput(session, "pays_sel", selected = "MONDE"))

  # ---------- Fiche (jauge, « sur 10 000 litres », comparaison)
  jauge <- function(v) {
    if (is.na(v)) return(div(class = "text-muted small", "Part de l'eau : donnée manquante"))
    pos <- max(0, min(100, (log10(max(v, 1e-4)) + 3) / 4 * 100))
    div(class = "jauge",
      div(class = "jauge-titre", "Part de l'eau prélevée du pays consommée par les data centers"),
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
        p(class = "small text-muted", bs_icon("hand-index"), " Cliquez sur un pays pour zoomer.")))
    }
    x <- r |> filter(iso3 == input$pays_sel)
    card_body(class = "fiche", h4(x$country), p(class = "text-muted small mb-2", quand()),
      div(class = "chiffres-fiche",
        div(div(class = "n", x$n_dc), "data centers"),
        div(div(class = "n", fmt(x$eau, 1)), "millions de m³"),
        div(div(class = "n", grand(x$eau * 1e6 / HAB_M3)), "habitants (équivalent)")),
      jauge(x$part),
      div(class = "d-flex justify-content-between align-items-center",
        div(class = "fiche-sous", "Profil face à l'eau"),
        tooltip(bs_icon("info-circle", class = "aide"),
          "Chaque axe classe le pays parmi tous les pays de l'étude : 0 = le moins concerné, 100 = le plus concerné. Plus la forme est grande, plus le pays est exposé. Choisissez un pays dans « Comparer avec… » pour superposer son profil. Valeurs exactes au survol.")),
      plotlyOutput("radar", height = "290px"), png_lien("radar"),
      litres_pays(x))
  })
  output$radar <- renderPlotly({
    req(input$pays_sel != "MONDE")
    r <- res() |> left_join(CLIM_AN, by = "iso3")
    rg <- function(v) { k <- rank(v, na.last = "keep", ties.method = "min"); 100 * (k - 1) / (sum(!is.na(v)) - 1) }
    axes <- tibble(
      lib = c("Data centers", "Poids sur l'eau", "Manque d'eau (pays)", "Sites en région sèche", "Chaleur", "Peu de pluie"),
      var = list(r$n_dc, r$part, r$stress0, r$pct_sites_region_stress, r$dj, -r$pluie),
      txt = list(fmt(r$n_dc, 0), pct(r$part), paste0(fmt(r$stress0, 0), " %"), pct(r$pct_sites_region_stress),
                 paste(fmt(r$dj, 0), "degrés-jours"), paste(fmt(r$pluie, 0), "mm/an")))
    trace <- function(p, iso, coul) {
      i <- match(iso, r$iso3)
      sc <- vapply(axes$var, function(v) rg(v)[i], numeric(1)); tx <- vapply(axes$txt, function(v) v[i], character(1))
      sc[is.na(sc)] <- 0; ferme <- c(seq_along(sc), 1)
      add_trace(p, type = "scatterpolar", r = sc[ferme], theta = axes$lib[ferme], name = r$country[i], fill = "toself",
                fillcolor = paste0(coul, "33"), line = list(color = coul, width = 2), marker = list(color = coul, size = 5),
                text = paste0(tx[ferme], " (rang ", fmt(sc[ferme], 0), "/100)"), hovertemplate = "%{theta} : %{text}<extra>%{fullData.name}</extra>")
    }
    p <- plot_ly() |> trace(input$pays_sel, "#1f6f8b")
    if (!is.null(input$pays_comp) && input$pays_comp != "" && input$pays_comp != input$pays_sel) p <- trace(p, input$pays_comp, "#d35400")
    p |> layout(polar = list(radialaxis = list(range = c(0, 100), tickvals = c(50, 100), tickfont = list(size = 9, color = "#999"), gridcolor = "#e3e9ee"),
                             angularaxis = list(tickfont = list(size = 10), gridcolor = "#e3e9ee")),
                showlegend = TRUE, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08),
                margin = list(l = 50, r = 50, t = 20, b = 20))
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
  sec <- function(x) est_sec(x, seuil())

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
    if (is.null(ds) || nrow(o) == 0) return(div(class = "etape-texte", span(class = "etape-num", bs_icon("info")), div("Trop peu de sites pour estimer une densité.")))
    dn <- ds$o |> filter(dense); hs <- chauds(); rg <- reg_sel()
    plus_sec <- if (input$horiz_reg != "stress_region") {
      niv <- function(x) match(as.character(x), d$lab_aq[1:5])
      paste0(" · ", sum(niv(rg[[input$horiz_reg]]) > niv(rg$stress_region), na.rm = TRUE), " régions plus sèches")
    } else ""
    div(class = "etape-texte", span(class = "etape-num", bs_icon("fire")), div(
      tags$b(paste0(fmt(100 * mean(sec(dn[[input$horiz_reg]]), na.rm = TRUE), 0), " %")),
      paste0(" des sites situés dans les zones les plus denses (les 25 % les plus concentrés) sont en région sèche ", nom_hor(),
             ", contre ", fmt(100 * mean(sec(o[[input$horiz_reg]]), na.rm = TRUE), 0), " % pour l'ensemble des sites"), plus_sec,
      if (nrow(hs)) paste0(" · point chaud : ", paste(hs$region, collapse = ", "))))
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
    if (nrow(c) == 0) return("Pas de données climatiques.")
    if (max(c$dj) < 1) return(paste0(n, " : jamais plus de 18 °C, refroidissement par l'air extérieur."))
    m <- c |> slice_max(dj, n = 1, with_ties = FALSE); ecart <- 100 * (m$pr / mean(c$pr) - 1)
    paste0(n, " : besoin de froid maximal en ", tolower(mois_long[m$mois]), ", pluie ", ifelse(ecart >= 0, "+", ""), fmt(ecart, 0), " %",
           ifelse(ecart < 0, " : effet ciseaux.", " : pas d'effet ciseaux."))
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
    paste0(mois_long[input$mois_carte], " : ", fmt(p, 0), " % des data centers en pays chaud et sec.")
  })

  output$calendrier_retenir <- renderText({
    top <- d$pays |> slice_max(n_dc, n = 20, with_ties = FALSE) |> select(iso3) |> inner_join(CENTRES |> select(iso3, lat), by = "iso3")
    mois_hemi <- function(nord) { cl <- CLIM |> inner_join(top, by = "iso3") |> filter(if (nord) lat > 0 else lat <= 0)
      if (!nrow(cl)) return(NULL)
      m <- cl |> group_by(mois) |> summarise(chaud = mean(etat %in% etat_lab[1:2]), .groups = "drop") |> filter(chaud >= 0.5) |> pull(mois)
      if (!length(m)) return(NULL); paste(mois_fr[m], collapse = ", ") }
    n <- mois_hemi(TRUE); s <- mois_hemi(FALSE)
    paste0("Mois chauds (plus chauds que la moyenne du pays pour au moins la moitié des pays) — nord : ", n %||% "aucun",
           if (!is.null(s)) paste0(" ; sud : ", s) else "", ".")
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
    if (!is.null(input$pays_us) && input$pays_us != "MONDE") p <- p |> filter(iso3 == input$pays_us)
    tot <- sum(p$prelev0) * 1e3                                    # millions de m3
    s <- c(agri = sum(p$prelev0 * p$pct_agri), indus = sum(p$prelev0 * p$pct_indus), men = sum(p$prelev0 * p$pct_menages))
    s <- 1e4 * s / sum(s)
    dc <- 1e4 * sum(p$eau) / tot
    list(agri = s[["agri"]], indus = s[["indus"]], men = s[["men"]] , dc = dc,
         nom = if (is.null(input$pays_us) || input$pays_us == "MONDE") "Monde" else p$country[1],
         annees = if (nrow(p) == 1) as.character(p$annee_secteurs) else paste0(min(p$annee_secteurs, na.rm = TRUE), " à ", max(p$annee_secteurs, na.rm = TRUE)))
  })
  output$litres_titre <- renderText(paste0("Sur 10 000 litres prélevés — ", litres()$nom))
  output$litres <- renderUI({
    l <- litres()
    tuile <- function(ic, val, txt, cls = "") div(class = paste("tuile", cls), bs_icon(ic), div(class = "gros", val), txt)
    tagList(layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
      tuile("flower1", paste(fmt(l$agri, 0), "L"), "agriculture (irrigation)"),
      tuile("building", paste(fmt(l$indus, 0), "L"), "industrie et énergie"),
      tuile("house-door", paste(fmt(l$men, 0), "L"), "habitants (eau du robinet)"),
      tuile("hdd-rack", paste(fmt(l$dc, 1), "L"), paste("data centers (consommés sur place),", quand()), "dc")),
      div(class = "small text-muted mt-2", paste0("Années — usages : FAO AQUASTAT, ", l$annees, " ; data centers : ", sub("^en ", "", quand()), ".")))
  })
  output$litres_retenir <- renderText({ l <- litres()
    paste0(l$nom, " : ", fmt(l$dc, 1), ifelse(l$dc >= 2, " litres", " litre"), " sur 10 000 pour les data centers ; l'agriculture en prélève environ ",
           grand(signif(l$agri / max(l$dc, 1e-9), 2)), " fois plus. Faible à l'échelle nationale : le risque est local.") })

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
    layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
      tuile("lightning-charge", paste(fmt(x$mwh / 1e3, 1), "GWh"), paste("≈", grand(x$mwh / FOYER_MWH), "foyers")),
      tuile("droplet", paste(grand(x$eau), "m³"), paste("≈", grand(x$eau / HAB_M3), "habitants")),
      tuile("cloud", paste(grand(x$co2_t), "t CO₂"), paste("≈", avec_unite(x$co2_t * 1e3 / KM_KG, "km"), "en voiture")))
  })
  output$conv_hyp <- renderUI({ x <- conv()
    paste0("Hypothèses : taux de charge ", fmt(100 * GWH_PAR_MW / 8.76, 0), " % (", fmt(GWH_PAR_MW, 2), " GWh par MW et par an) · foyer ",
           fmt(FOYER_MWH, 1), " MWh/an · habitant ", fmt(HAB_M3, 0), " m³/an, soit ", fmt(HAB_M3 * 1000 / 365, 0), " L/jour (Arcep) · voiture ",
           fmt(KM_KG * 1000, 0), " g CO₂/km · électricité de ", x$pays, " : ", fmt(x$g, 0), " g CO₂/kWh (Ember) · eau : ", fmt(w(), 2), " L/kWh.") })
  output$conv_retenir <- renderText({
    x <- conv()
    paste0("Un data center de ", x$mw, " MW utilise autant d'électricité que ", grand(x$mwh / FOYER_MWH), " foyers et autant d'eau que ",
           grand(x$eau / HAB_M3), " habitants. Mix électrique choisi : ", x$pays, " (", fmt(x$g, 0), " g de CO₂ par kWh), ",
           "soit le CO₂ de ", avec_unite(x$co2_t * 1e3 / KM_KG, "km"), " en voiture.")
  })

  output$directe <- renderPlotly({
    dir <- ARCEP_TWH * wue[["bas"]]; tot <- ARCEP_TWH * wue[["haut"]]
    plot_ly(labels = c("Refroidissement sur place (eau directe)", "Production de l'électricité (eau indirecte)"),
            values = c(dir, tot - dir), type = "pie", hole = 0.55, sort = FALSE,
            marker = list(colors = c("#1f6f8b", "#f0ad4e"), line = list(color = "#fff", width = 2)),
            textinfo = "percent", textfont = list(size = 16, color = "#fff"),
            hovertext = c(paste0(fmt(dir, 3), " million de m³ (", fmt(wue[["bas"]], 2), " L/kWh)"),
                          paste0(fmt(tot - dir, 1), " millions de m³ (", fmt(wue[["haut"]] - wue[["bas"]], 1), " L/kWh)")), hoverinfo = "text+label") |>
      layout(showlegend = TRUE, legend = list(orientation = "h", x = 0, y = -0.1),
             annotations = list(list(text = paste0("<b>", fmt(tot, 1), "</b><br>millions m³<br><span style='font-size:10px'>France, Arcep</span>"),
                                     showarrow = FALSE, font = list(size = 15))),
             margin = list(t = 10, b = 10))
  })

  pays_tend <- reactive(if (!is.null(input$pays_sel) && input$pays_sel != "MONDE") input$pays_sel else "FRA")
  output$tendance <- renderPlotly({
    iso <- pays_tend()
    s <- d$secteurs_ts |> filter(iso3 == iso, annee >= 1990)
    dc <- bind_rows(lapply(2024:2035, function(y) tibble(annee = y,
            bas = sum(calc(w(), "bas", y)$eau[d$pays$iso3 == iso]) / 1e3,
            central = sum(calc(w(), "central", y)$eau[d$pays$iso3 == iso]) / 1e3,
            haut = sum(calc(w(), "haut", y)$eau[d$pays$iso3 == iso]) / 1e3)))
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
             title = list(text = paste0("<b>", d$pays$country[d$pays$iso3 == iso], "</b>"), x = 0.01, font = list(size = 14)),
             margin = list(t = 35),
             yaxis = list(title = "Milliards de m³ par an (échelle log)", type = "log",
                          tickvals = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), ticktext = c("0,001", "0,01", "0,1", "1", "10", "100", "1 000")))
  })
  output$tendance_retenir <- renderText({
    iso <- pays_tend()
    e <- evol_pays(iso, w(), "central", base_pays())
    paste0("Data centers (croissance centrale) : volume d'eau ×", fmt(e$f_vol, 1), " d'ici 2035 ; part de l'eau prélevée ",
           pct(e$part24), " → ", pct(e$part35), " (", e$denominateur, "). Toujours loin derrière les autres usages.")
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
    top <- calc(w(), s, 2035) |> filter(!is.na(prelev0), n_dc >= 20) |> slice_max(part, n = 8) |> pull(iso3)
    pts <- bind_rows(lapply(c(2024, 2030, 2035), function(h) calc(w(), s, h) |> filter(iso3 %in% top) |> mutate(h = factor(h))))
    band <- bind_rows(lapply(c("bas", "haut"), function(x) calc(w(), x, 2035) |> filter(iso3 %in% top) |> transmute(country, x, part))) |>
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
    p <- top10()$pts |> filter(h == "2035") |> arrange(desc(part))
    paste0(p$country[1], " en tête (", pct(p$part[1]), ") ; ailleurs, moins de ", pct(ceiling(p$part[2] * 10) / 10), ".")
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
    paste0("≈ ", fmt(pv$prevision, 1), " milliards de m³ en 2035 (intervalle : ", fmt(pv$basse, 1), " à ", fmt(pv$haute, 1), ").")
  })

  fr_scen <- reactive({
    e0f <- base_pays()$e0[base_pays()$iso3 == "FRA"]
    prel <- c(`2024` = d$pays$prelev0[d$pays$iso3 == "FRA"], `2030` = d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2030],
              `2035` = d$prev_fr$prev$prevision[d$prev_fr$prev$annee == 2035])
    nm <- c(bas = "lente", central = "centrale", haut = "rapide")
    bind_rows(tibble(lab = "2024", e_h = e0f, h = "2024"),
      bind_rows(lapply(c(2030, 2035), function(h) bind_rows(lapply(names(nm), function(s)
        tibble(lab = paste0(h, " ", nm[[s]]), e_h = calc(w(), s, h) |> filter(iso3 == "FRA") |> pull(e_h), h = as.character(h))))))) |>
      mutate(e_ia = d$part_ia[["fr"]] * e0f + d$part_ia[["hausse"]] * (e_h - e0f), eau = e_h * w(), eau_ia = e_ia * w(),
             part = 100 * eau / (prel[h] * 1e3), lab = factor(lab, unique(lab)))
  })
  output$fr_scen <- renderPlotly({
    f <- fr_scen()
    plot_ly(f, x = ~lab) |>
      add_bars(y = ~eau - eau_ia, name = "Hors IA", marker = list(color = "#9ecae1")) |>
      add_bars(y = ~eau_ia, name = "IA", marker = list(color = "#d62728"), text = ~pct(part), textposition = "outside") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Millions de m³", range = c(0, max(f$eau) * 1.15)), legend = LEG)
  })
  output$frscen_retenir <- renderText({
    f <- fr_scen(); i35 <- f$h == "2035"
    e <- evol_pays("FRA", w(), "central", base_pays())
    paste0("Volume ×", fmt(min(f$eau[i35]) / f$eau[1], 1), " à ×", fmt(max(f$eau[i35]) / f$eau[1], 1), " d'ici 2035 (centrale : ×", fmt(e$f_vol, 1),
           ") ; part de l'eau prélevée ", pct(e$part24), " → ", pct(e$part35), " en croissance centrale ; l'IA explique environ ",
           fmt(100 * max(f$eau_ia[i35] / f$eau[i35]), 0), " % du volume.")
  })
  output$fr_titre <- renderText({
    f <- fr_scen()
    paste0("France : jusqu'à ", pct(max(f$part)), " de l'eau prélevée en 2035")
  })


  # ---------- Méthode
  output$etapes <- renderUI({
    fr <- d$pays |> filter(iso3 == "FRA")
    et <- function(n, titre, val) div(class = "etape", span(class = "num", n), div(tags$b(titre)), div(class = "val", val))
    fl <- div(class = "fleche", bs_icon("arrow-right"))
    div(class = "etapes",
      et(1, "Électricité mondiale des data centers (AIE)", paste(fmt(K$twh, 0), "TWh")), fl,
      et(2, tagList("Répartition par pays", div(class = "etape-det", paste0("Pays calés sur l'AIE : valeur publiée. Autres pays : reste de leur zone réparti selon le nombre de sites (≈ ",
                    fmt(K$gwh_eur, 0), " GWh par site en Europe). France : ", fr$n_dc, " sites"))), paste(fmt(fr$e0, 1), "TWh")), fl,
      et(3, "× litres d'eau consommés sur place par kWh", paste(fmt(fr$e0 * w(), 1), "Mm³")), fl,
      et(4, tagList("÷ eau prélevée en France", div(class = "etape-det", "consommation comparée à un prélèvement : part minorée")),
         pct(100 * fr$e0 * w() / (fr$prelev0 * 1e3))), fl,
      et(5, "Projection 2035, zone Europe (AIE)", { f <- d$scen_zone |> filter(zone == "EUR", horizon == 2035)
         paste0("×", fmt(min(f$f_zone), 1), " à ×", fmt(max(f$f_zone), 1)) }))
  })
  output$coefs <- renderPlotly({
    k <- tibble(nom = c("0,21 L/kWh", "0,81 L/kWh", "2,41 L/kWh"), w = unname(wue)) |> mutate(eau = 415 * w, nom = factor(nom, nom))
    plot_ly(k, x = ~nom, y = ~eau, type = "bar", marker = list(color = c("#9ecae1", "#1f6f8b", "#08306b")),
            text = ~paste(fmt(eau, 0), "Mm³"), textposition = "outside", hoverinfo = "none") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Millions de m³ (monde, 2024)", range = c(0, 1200)))
  })
  validation_fr <- reactive({
    fr <- d$pays |> filter(iso3 == "FRA"); ref <- d$refs_fr |> filter(grepl("tout le parc", source))
    bind_rows(d$validation |> mutate(country = paste0(country, " (calé)")),
              tibble(iso3 = "FRA", country = "France (non calée)", modele_pct = 100 * fr$e0 / fr$elec, ref_pct = 100 * ref$twh / fr$elec,
                     statut = ref$source, twh_mod = fr$e0, twh_ref = ref$twh))
  })
  output$validation_retenir <- renderText({ v <- validation_fr() |> filter(iso3 == "FRA")
    paste0("France, seul contrôle indépendant : ", fmt(v$twh_mod, 1), " TWh dans notre modèle contre ", fmt(v$twh_ref, 0),
           " TWh (ADEME-Arcep, 2020) : la répartition par nombre de sites sous-estime.") })
  output$validation <- renderPlotly({
    v <- validation_fr() |> arrange(modele_pct) |> mutate(country = factor(country, country))
    plot_ly(v, y = ~country) |>
      add_bars(x = ~modele_pct, name = "Notre modèle", marker = list(color = "#1f6f8b"), orientation = "h") |>
      add_bars(x = ~ref_pct, name = "Valeur publiée (AIE ; ADEME-Arcep pour la France)", marker = list(color = "#bdbdbd"), orientation = "h") |>
      layout(barmode = "group", xaxis = list(title = "% de l'électricité du pays", type = "log",
                                            tickvals = c(0.5, 1, 2, 5, 10, 20), ticktext = c("0,5", "1", "2", "5", "10", "20")),
             yaxis = list(title = ""), legend = LEG)
  })
  # Prévision « estimer jusqu'en… » : les 4 modèles sont réestimés en direct, puis comparés aux années suivantes
  bt <- reactive({
    fin <- as.integer(input$prev_fin %||% 2019); o <- d$prev_fr$obs
    tr <- o |> filter(annee <= fin); y <- ts(tr$valeur, start = min(tr$annee)); h <- max(o$annee) - fin + 3
    modeles <- list("Naïf" = function(s) naive(s, h = h), "Dérive" = function(s) rwf(s, h = h, drift = TRUE),
                    "ETS" = function(s) forecast(ets(s), h = h), "ARIMA" = function(s) forecast(auto.arima(s, seasonal = FALSE, max.p = 2, max.q = 2), h = h))
    pr <- bind_rows(lapply(names(modeles), function(m) tryCatch(tibble(modele = m, annee = fin + seq_len(h), pred = as.numeric(modeles[[m]](y)$mean)),
                                                                 error = function(e) NULL)))
    sc <- pr |> inner_join(o, by = "annee") |> group_by(modele) |> summarise(MAE = mean(abs(pred - valeur)), n = n(), .groups = "drop") |> arrange(MAE)
    list(fin = fin, pr = pr, sc = sc, o = o)
  })
  output$bt_titre <- renderText({ b <- bt(); if (!nrow(b$sc)) return("Prévision")
    per <- if (b$fin + 1 == max(b$o$annee)) max(b$o$annee) else paste0(b$fin + 1, "-", max(b$o$annee))
    paste0("Estimé jusqu'en ", b$fin, " : le meilleur sur ", per, " est « ", b$sc$modele[1], " »") })
  output$bt_retenir <- renderText({ b <- bt(); if (!nrow(b$sc)) return("")
    paste0("Erreur absolue moyenne : ", paste0(b$sc$modele, " ", fmt(b$sc$MAE, 2), collapse = " · "), " (milliards de m³, ", b$sc$n[1], " année",
           ifelse(b$sc$n[1] > 1, "s", ""), " de contrôle). Changer l'année montre la fragilité du classement.") })
  output$backtest <- renderPlotly({
    b <- bt(); o <- b$o
    cols <- c("Naïf" = "#7f7f7f", "Dérive" = "#2ca02c", "ETS" = "#9467bd", "ARIMA" = "#ff7f0e")
    av <- o |> filter(annee <= b$fin); ap <- o |> filter(annee > b$fin)
    p <- plot_ly() |>
      add_trace(data = av, x = ~annee, y = ~valeur, type = "scatter", mode = "lines+markers", name = "Réalité (utilisée)",
                line = list(color = "#111", width = 3), marker = list(size = 8, color = "#111")) |>
      add_trace(data = ap, x = ~annee, y = ~valeur, type = "scatter", mode = "markers", name = "Réalité ensuite (non utilisée)",
                marker = list(size = 11, color = "rgba(0,0,0,0)", line = list(color = "#111", width = 2)))
    for (m in intersect(names(cols), unique(b$pr$modele))) {
      bm <- bind_rows(av |> filter(annee == b$fin) |> transmute(annee, pred = valeur), b$pr |> filter(modele == m) |> select(annee, pred))
      p <- p |> add_trace(data = bm, x = ~annee, y = ~pred, type = "scatter", mode = "lines", name = m,
                          line = list(color = cols[[m]], dash = "dash", width = if (m == b$sc$modele[1]) 3.5 else 2))
    }
    p |> layout(shapes = list(list(type = "rect", x0 = min(o$annee) - 0.4, x1 = b$fin + 0.5, y0 = 0, y1 = 1, yref = "paper", fillcolor = "#999", opacity = 0.12, line = list(width = 0))),
                xaxis = list(title = "", dtick = 1), yaxis = list(title = "Eau prélevée en France (Mds m³)"), legend = list(orientation = "h", x = 0, y = -0.12))
  })
  output$mae_titre <- renderText({ s <- d$prev_fr$scores; o <- s |> filter(modele != d$prev_fr$choix)
    ch <- s |> filter(modele == d$prev_fr$choix)
    paste0("MAE ", fmt(ch$MAE, 2), " contre ", fmt(min(o$MAE), 1), " à ", fmt(max(o$MAE), 1), " (test) ; ", fmt(ch$MAE_cv, 2), " contre ",
           fmt(min(o$MAE_cv), 2), " à ", fmt(max(o$MAE_cv), 2), " (validation croisée)") })
  output$mae <- renderPlotly({
    s <- d$prev_fr$scores |> arrange(desc(MAE)) |> mutate(modele = factor(modele, modele))
    plot_ly(s, y = ~modele) |>
      add_bars(x = ~MAE, name = "MAE test 2020-2022", orientation = "h", marker = list(color = "#c7c7c7"),
               text = ~fmt(MAE, 2), textposition = "outside",
               hovertext = ~paste0(modele, " · test : MAE ", fmt(MAE, 2), ", RMSE ", fmt(RMSE, 2)), hoverinfo = "text") |>
      add_bars(x = ~MAE_cv, name = "MAE validation croisée", orientation = "h", marker = list(color = "#1f6f8b"),
               text = ~fmt(MAE_cv, 2), textposition = "outside",
               hovertext = ~paste0(modele, " · validation croisée (", n_cv, " prévisions) : MAE ", fmt(MAE_cv, 2), ", RMSE ", fmt(RMSE_cv, 2)), hoverinfo = "text") |>
      layout(barmode = "group", xaxis = list(title = "Erreur absolue moyenne (Mds m³)", range = c(0, 3)), yaxis = list(title = ""), legend = LEG)
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
  # ---------- Méthode : ACP recalculée en direct (case « log », choix des axes)
  acp_r <- reactive({
    X <- as.data.frame(d$acp_base[, c(VARS_ACP, "part_central")]); rownames(X) <- d$acp_base$country
    if (isTRUE(input$acp_log %||% TRUE)) for (v in c(d$vars_log, "part_central")) X[[v]] <- log1p(X[[v]])
    a <- PCA(X, scale.unit = TRUE, ncp = length(VARS_ACP), graph = FALSE, quanti.sup = ncol(X))
    ax <- as.integer(strsplit(input$acp_axes %||% "1-2", "-")[[1]])
    list(a = a, ax = ax, log = isTRUE(input$acp_log %||% TRUE), kaiser = sum(a$eig[, 1] > 1),
         eig = tibble(axe = seq_len(nrow(a$eig)), valeur_propre = a$eig[, 1], inertie = a$eig[, 2], cumul = a$eig[, 3]))
  })
  nom_axe <- function(r, k) {
    lib <- if (r$log && k == 1) " : taille et pression" else if (r$log && k == 2) " : eau disponible et équipement" else ""
    paste0("Axe ", k, " (", fmt(r$eig$inertie[k], 0), " %)", lib)
  }
  output$acp_eb_titre <- renderText({ r <- acp_r(); paste0("2 axes résument ", fmt(r$eig$cumul[2], 0), " % de l'information",
                                                           if (!r$log) " (sans log)" else "") })
  output$acp_eb_retenir <- renderText({ r <- acp_r()
    paste0("Critère de Kaiser (valeur propre > 1) : ", r$kaiser, " axes, soit ", fmt(r$eig$cumul[r$kaiser], 0), " % de l'information",
           if (r$log) " ; ce sont eux qui servent à la classification." else " ; la classification utilise la version en log.") })
  output$acp_cercle_titre <- renderText({ r <- acp_r(); paste0("Cercle des corrélations : axes ", r$ax[1], " et ", r$ax[2]) })
  output$acp_eboulis <- renderPlotly({
    r <- acp_r(); e <- r$eig |> mutate(axe = factor(axe)); seuil_k <- 100 / nrow(e)
    plot_ly(e, x = ~axe) |>
      add_bars(y = ~inertie, name = "Par axe", marker = list(color = ifelse(seq_len(nrow(e)) <= r$kaiser, "#1f6f8b", "#c6dbef")),
               text = ~paste0(fmt(inertie, 0), " %"), textposition = "outside",
               hovertext = ~paste0("Axe ", axe, " : valeur propre ", fmt(valeur_propre, 2)), hoverinfo = "text") |>
      add_lines(y = ~cumul, name = "Cumul", line = list(color = "#d35400", width = 3), marker = list(color = "#d35400"),
                mode = "lines+markers", hovertext = ~paste0("Cumul : ", fmt(cumul, 0), " %"), hoverinfo = "text") |>
      layout(xaxis = list(title = "Axe"), yaxis = list(title = "% de l'information", range = c(0, 105)), legend = LEG,
             shapes = list(list(type = "line", x0 = -0.5, x1 = nrow(e) - 0.5, y0 = seuil_k, y1 = seuil_k, line = list(dash = "dot", color = "#c0392b"))),
             annotations = list(list(x = nrow(e) - 1, y = seuil_k, text = "Kaiser", showarrow = FALSE, yanchor = "bottom", font = list(color = "#c0392b", size = 11))))
  })
  output$acp_cercle <- renderPlotly({
    r <- acp_r(); a <- r$a; ax <- r$ax; th <- seq(0, 2 * pi, length.out = 200)
    v <- bind_rows(tibble(var = rownames(a$var$coord), x = a$var$coord[, ax[1]], y = a$var$coord[, ax[2]], sup = FALSE),
                   tibble(var = rownames(a$quanti.sup$coord), x = a$quanti.sup$coord[, ax[1]], y = a$quanti.sup$coord[, ax[2]], sup = TRUE)) |>
      mutate(variable = NOMS_ACP[var])
    seg <- function(df) data.frame(x = as.vector(rbind(0, df$x, NA)), y = as.vector(rbind(0, df$y, NA)))
    plot_ly(type = "scatter", mode = "lines") |>
      add_trace(x = cos(th), y = sin(th), type = "scatter", mode = "lines", line = list(color = "#ccc"), hoverinfo = "none", showlegend = FALSE) |>
      add_trace(data = seg(v[!v$sup, ]), x = ~x, y = ~y, type = "scatter", mode = "lines", line = list(color = "#1f6f8b", width = 2), hoverinfo = "none", showlegend = FALSE) |>
      add_trace(data = seg(v[v$sup, ]), x = ~x, y = ~y, type = "scatter", mode = "lines", line = list(color = "#c0392b", width = 2.5, dash = "dash"),
                hoverinfo = "none", showlegend = FALSE) |>
      add_trace(data = v, x = ~x * 1.12, y = ~y * 1.1, text = ~variable, type = "scatter", mode = "text",
                textfont = list(color = ifelse(v$sup, "#c0392b", "#333"), size = 11),
                hovertext = ~paste0(variable, "<br>Axe ", ax[1], " : ", fmt(x, 2), "<br>Axe ", ax[2], " : ", fmt(y, 2)), hoverinfo = "text", showlegend = FALSE) |>
      layout(xaxis = list(title = nom_axe(r, ax[1]), range = c(-1.45, 1.45), zeroline = TRUE),
             yaxis = list(title = nom_axe(r, ax[2]), range = c(-1.2, 1.2), zeroline = TRUE, scaleanchor = "x"))
  })
  output$acp_ind <- renderPlotly({
    r <- acp_r(); ax <- r$ax
    cl <- tibble(country = rownames(r$a$ind$coord), dx = r$a$ind$coord[, ax[1]], dy = r$a$ind$coord[, ax[2]]) |>
      left_join(d$classes |> select(country, nom_classe, n_dc, part_central), by = "country") |>
      mutate(etiq = ifelse(n_dc >= 60 | nom_classe == "Pays très secs" | part_central >= 0.25 |
                             country %in% c("Inde", "Chine", "Indonésie", "Maroc", "Pakistan"), country, ""))
    plot_ly(cl, x = ~dx, y = ~dy, color = ~nom_classe, colors = col_classes, type = "scatter", mode = "markers+text",
            text = ~etiq, textposition = "top center", textfont = list(size = 10, color = "#333"), marker = list(size = 11, opacity = 0.8),
            hovertext = ~paste0("<b>", country, "</b><br>", nom_classe), hoverinfo = "text") |>
      layout(xaxis = list(title = nom_axe(r, ax[1]), zeroline = TRUE), yaxis = list(title = nom_axe(r, ax[2]), zeroline = TRUE),
             legend = list(orientation = "h", x = 0, y = -0.22))
  })
  output$acp_tables <- renderUI({
    r <- acp_r(); a <- r$a; ax <- r$ax; cm <- d$acp_cmp; ex <- d$acp_exclus
    t <- tibble(variable = NOMS_ACP[rownames(a$var$contrib)], c1 = a$var$contrib[, ax[1]], c2 = a$var$contrib[, ax[2]],
                cos2 = rowSums(a$var$cos2[, ax, drop = FALSE])) |> arrange(desc(c1 + c2))
    tagList(
      tags$table(class = "table table-sm mb-2",
        tags$thead(tags$tr(tags$th("Variable"), tags$th(paste("Axe", ax[1])), tags$th(paste("Axe", ax[2])), tags$th(paste0("cos² (", ax[1], "-", ax[2], ")")))),
        tags$tbody(lapply(seq_len(nrow(t)), function(i) tags$tr(tags$td(t$variable[i]), tags$td(paste0(fmt(t$c1[i], 0), " %")),
                                                                tags$td(paste0(fmt(t$c2[i], 0), " %")), tags$td(fmt(t$cos2[i], 2)))))),
      div(class = "fiche-sous", "Avant / après le passage au log du stress hydrique"),
      tags$table(class = "table table-sm mb-2",
        tags$thead(tags$tr(tags$th(""), tags$th("Axes 1+2"), tags$th("cos² stress"), tags$th("Pays le plus influent"))),
        tags$tbody(lapply(seq_len(nrow(cm)), function(i) tags$tr(tags$td(cm$version[i]), tags$td(paste0(fmt(cm$cumul2[i], 0), " %")),
          tags$td(fmt(cm$cos2_stress[i], 2)), tags$td(paste0(cm$pays_max[i], " (", fmt(cm$contrib_max_pays[i], 0), " %)")))))),
      tags$details(tags$summary(paste0("Pourquoi ", nrow(d$classes), " pays et pas ", nrow(d$pays), " ? (", nrow(ex), " exclus)")),
        p(class = "small mb-1", paste0("Critère : au moins 10 data centers (en dessous, les profils reposent sur trop peu de sites) et données complètes. Motifs : ",
          paste(names(table(ex$motif)), table(ex$motif), sep = " = ", collapse = " ; "), ".")),
        p(class = "small text-muted", paste(ex$country, collapse = ", "))))
  })

  # ---------- Accueil : chiffres clés et réponses, recalculés selon les réglages
  litres_calc <- function(p) {
    p <- p |> filter(!is.na(prelev0), !is.na(pct_agri), !is.na(pct_indus), !is.na(pct_menages))
    agri <- sum(p$prelev0 * p$pct_agri) / sum(p$prelev0 * (p$pct_agri + p$pct_indus + p$pct_menages)) * 1e4
    list(agri = agri, dc = 1e4 * sum(p$eau) / (sum(p$prelev0) * 1e3))
  }
  reglages_txt <- reactive(paste0(fmt(w(), 2), " L/kWh · ", sub("^en ", "", quand()), " · région sèche ", seuil_txt(seuil())))
  output$accueil_chiffres <- renderUI({
    r <- res(); p <- pres(); m <- mcn(); e_tot <- sum(r$e_h)
    div(class = "chiffres",
      div(class = "chiffre", div(class = "nb", fmt(sum(r$n_dc), 0)),
          div(class = "lib", paste("data centers dans", nrow(r), "pays")), div(class = "base", "inventaire par pays, 2024")),
      div(class = "chiffre", div(class = "nb", paste(fmt(sum(r$eau), 0), "M m³")),
          div(class = "lib", paste0("d'eau ", if (w() > wue[["central"]] + 0.2) "(sur place et pour produire l'électricité)" else "consommée sur place (refroidissement)", ", ", quand())),
          div(class = "base", paste0(fmt(e_tot * wue[["bas"]], 0), " à ", fmt(e_tot * wue[["haut"]], 0), " M m³ selon le coefficient ; ",
                                     fmt(e_tot * wue[["haut"]], 0), " M m³ en ajoutant l'eau des centrales électriques"))),
      div(class = "chiffre", div(class = "nb", "< 0,1 %"),
          div(class = "lib", paste0("de l'eau prélevée du pays, dans ", sur_10(100 * mean(p$part < 0.1, na.rm = TRUE)))),
          div(class = "base", "comparé à l'eau prélevée, ce qui minore la part")),
      div(class = "chiffre", div(class = "nb", paste0(fmt(m$p_reg, 0), " %")),
          div(class = "lib", "des sites en région qui manque d'eau"),
          div(class = "base", paste0(fmt(m$n, 0), " sites géolocalisés (OpenStreetMap) ; stress ", seuil_txt(m$seuil)))),
      div(class = "reglages", paste("Réglages :", reglages_txt())))
  })
  output$accueil_reponses <- renderUI({
    p <- pres(); m <- mcn(); l <- litres_calc(p); f35 <- d$scen |> filter(horizon == 2035)
    div(class = "reponses-grille",
      reponse("Oui, avec nos hypothèses, à l'échelle d'un pays.",
              paste0("Les data centers consomment moins de 0,1 % de l'eau prélevée dans ", sur_10(100 * mean(p$part < 0.1, na.rm = TRUE)),
                     ". Dans le monde, l'agriculture prélève environ ", grand(signif(l$agri / l$dc, 2)), " fois plus d'eau."),
              NULL, "Voir les usages de l'eau", "rep_usages"),
      reponse("Pas forcément là où ils sont installés.",
              paste0(fmt(m$p_reg, 0), " % des sites géolocalisés se trouvent dans une région qui manque d'eau, alors qu'on n'en compte que ",
                     fmt(m$p_pays, 0), " % en regardant le pays entier."),
              span(class = "preuve", paste0(if (m$p < 0.05) "Écart net" else "Écart faible", " site par site ; région par région : ",
                                            fmt(m$p_pays_reg, 0), " % contre ", fmt(m$p_reg_reg, 0), " % (", p_txt(m$p_r), ")")),
              "Voir les régions", "rep_regions"),
      reponse("Et demain, la pression va monter.",
              paste0("D'ici 2035, l'électricité des data centers serait multipliée par ", fmt(min(f35$f_monde), 1), " à ", fmt(max(f35$f_monde), 1),
                     " dans le monde (AIE). Dans l'hémisphère nord, l'été est la période la plus tendue."),
              NULL, "Voir les projections", "rep_demain"))
  })


  # ---------- IA : sites Epoch AI face au stress hydrique
  ia_sel <- reactive({
    ia <- d$ia |> left_join(CENTRES |> rename(lon_c = lon, lat_c = lat), by = "iso3") |>
      mutate(lon = coalesce(lon, lon_c), lat = coalesce(lat, lat_c), st = .data[[input$ia_horiz %||% "stress_region"]],
             sec = est_sec(st, seuil()))
    ia
  })
  ia_nom_hor <- reactive(c(stress_region = "aujourd'hui", stress_2030 = "en 2030", stress_2050 = "en 2050")[[input$ia_horiz %||% "stress_region"]])
  output$ia_titre <- renderText({ ia <- ia_sel()
    paste0(fmt(100 * mean(ia$sec[!is.na(ia$st)]), 0), " % des grands sites IA sont dans une région qui manque d'eau ", ia_nom_hor()) })
  output$ia_carte <- renderLeaflet({
    ia <- ia_sel() |> filter(!is.na(lon)); pal <- colorFactor(col_aq, levels = d$lab_aq, na.color = "#777777")
    jit <- function(n) (seq_len(n) - (n + 1) / 2) * 0.35
    ia <- ia |> group_by(lon, lat) |> mutate(lon = lon + jit(n())) |> ungroup()
    leaflet(ia) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~pmax(4, sqrt(mw_it) * 0.9), fillColor = ~pal(st), fillOpacity = 0.85,
                       color = "#333", weight = 1,
                       label = ~lapply(paste0("<b>", site, "</b><br>", pays, ifelse(is.na(region), "", paste0(" · ", region)), "<br>",
                                              fmt(mw_it, 0), " MW (puissance IT)", ifelse(is.na(proprietaire) | proprietaire == "", "", paste0("<br>", gsub(" #[a-z]+", "", proprietaire))),
                                              "<br>Manque d'eau de la région ", ia_nom_hor(), " : ", coalesce(as.character(st), "n.d.")), htmltools::HTML)) |>
      addLegend("bottomleft", colors = col_aq[1:5], labels = d$lab_aq[1:5], opacity = 0.9,
                title = htmltools::HTML(paste0("Manque d'eau de la région<br>", ia_nom_hor(), "<br><small>taille = puissance IT</small>"))) |>
      setView(10, 30, 1.6)
  })
  output$ia_chiffres <- renderUI({
    ia <- ia_sel() |> filter(!is.na(st)); m <- mcn()
    card_body(class = "pb-0", div(class = "chiffres-fiche",
      div(div(class = "n", nrow(d$ia)), "grands sites IA"),
      div(div(class = "n", paste(fmt(sum(d$ia$mw_it) / 1000, 1), "GW")), "puissance IT"),
      div(div(class = "n", paste0(fmt(100 * sum(ia$mw_it[ia$sec]) / sum(ia$mw_it), 0), " %")), "de cette puissance en région sèche")),
      div(class = "small text-muted", paste0("Région sèche : stress ", seuil_txt(seuil()), ". Pour comparaison, tous les data centers géolocalisés : ",
                                             fmt(m$p_reg, 0), " % des sites en région sèche aujourd'hui.")))
  })
  output$ia_regions <- renderPlotly({
    r <- ia_sel() |> filter(!is.na(region)) |> group_by(region, pays) |>
      summarise(mw = sum(mw_it), n = n(), st = first(as.character(st)), .groups = "drop") |>
      slice_max(mw, n = 12, with_ties = FALSE) |> arrange(mw) |>
      mutate(lab = ifelse(grepl(pays, region, fixed = TRUE), region, paste0(region, " (", pays, ")")), lab = factor(lab, unique(lab)), st = coalesce(st, "Non renseigné"))
    plot_ly(r, y = ~lab, x = ~mw, type = "bar", orientation = "h", color = ~st, colors = c(col_aq, "Non renseigné" = "#bbbbbb"),
            hovertext = ~paste0("<b>", region, "</b><br>", n, " site(s) · ", fmt(mw, 0), " MW<br>Manque d'eau : ", st), hoverinfo = "text") |>
      layout(xaxis = list(title = "Puissance IT (MW)"), yaxis = list(title = ""), showlegend = FALSE, margin = list(l = 10, r = 10, t = 10))
  })
  ia_eau <- reactive({
    scen_ia <- input$scen %||% "central"
    bind_rows(lapply(c(2024, 2030, 2035), function(h) { r <- calc(w(), scen_ia, h)
      tibble(annee = h, ia = sum(r$eau_ia), hors = sum(r$eau - r$eau_ia)) })) |> mutate(scen = scen_ia)
  })
  ia_epoch_m3 <- reactive(sum(d$ia$mw_it) * PUE_IA * as.numeric(input$fc %||% 0.7) * 8760 / 1e6 * w())   # TWh x L/kWh = Mm³
  output$ia_eau_titre <- renderText({ e <- ia_eau()
    paste0("Eau des data centers : part de l'IA ", fmt(100 * e$ia[1] / (e$ia[1] + e$hors[1]), 0), " % en 2024, ",
           fmt(100 * e$ia[3] / (e$ia[3] + e$hors[3]), 0), " % en 2035") })
  output$ia_eau <- renderPlotly({
    e <- ia_eau() |> mutate(annee = factor(annee))
    plot_ly(e, x = ~annee) |>
      add_bars(y = ~hors, name = "Hors IA", marker = list(color = "#9ecae1"), hovertext = ~paste0("Hors IA : ", fmt(hors, 0), " Mm³"), hoverinfo = "text") |>
      add_bars(y = ~ia, name = "IA (hypothèse AIE)", marker = list(color = "#d62728"), hovertext = ~paste0("IA : ", fmt(ia, 0), " Mm³"), hoverinfo = "text") |>
      add_markers(x = "2024", y = ia_epoch_m3(), name = paste0("Grands sites IA Epoch (", nrow(d$ia), ", capacité 2026)"),
                  marker = list(symbol = "diamond", size = 14, color = "#111"),
                  hovertext = paste0(nrow(d$ia), " sites Epoch AI : ", fmt(ia_epoch_m3(), 0), " Mm³ par an"), hoverinfo = "text") |>
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Millions de m³ par an"), legend = LEG)
  })
  output$ia_eau_retenir <- renderText({ e <- ia_eau()
    paste0("Avec ", fmt(w(), 2), " L/kWh (croissance ", c(bas = "lente", central = "centrale", haut = "rapide")[[e$scen[1]]], ") : eau de l'IA ",
           fmt(e$ia[1], 0), " Mm³ en 2024, ", fmt(e$ia[3], 0), " Mm³ en 2035. Les seuls ", nrow(d$ia), " grands sites d'Epoch AI consommeraient déjà ",
           fmt(ia_epoch_m3(), 0), " Mm³ par an à pleine capacité actuelle.") })

  # ---------- Hypothèses H1, H2, H3
  output$hypotheses <- renderUI({
    p <- pres(); m <- mcn(); k <- sum(p$part < 0.1, na.rm = TRUE); n <- sum(!is.na(p$part))
    b1 <- binom.test(k, n, 0.5, alternative = "greater")
    pire <- calc(wue[["haut"]], "haut", 2035) |> filter(!is.na(prelev0)); r1 <- 100 * mean(pire$part < 0.1, na.rm = TRUE)
    v1 <- if (b1$p.value >= 0.05) "rejetée" else if (r1 > 50) "confirmée" else "nuancée"
    v2 <- if (m$p >= 0.05 || m$p_reg <= m$p_pays) "rejetée" else if (m$p_r < 0.05) "confirmée" else "nuancée"
    f35 <- d$scen |> filter(horizon == 2035); e24 <- sum(calc(w(), "central", 2024)$eau)
    e35 <- sapply(c("bas", "central", "haut"), function(s) sum(calc(w(), s, 2035)$eau))
    mk30 <- mk(d$hist$eau, d$hist$year); mk30e <- mk(d$hist$elec, d$hist$year)
    v3 <- if (min(f35$f_monde) > 1) "confirmée" else "nuancée"
    carte <- function(code, enonce, test, stat, pval, verdict, robuste) {
      coul <- c(confirmée = "#1a9850", nuancée = "#e67e22", rejetée = "#c0392b")[[verdict]]
      div(class = "hypo", style = paste0("border-left-color:", coul),
        div(class = "hypo-tete", span(class = "hypo-code", code), span(class = "hypo-verdict", style = paste0("color:", coul), paste("Hypothèse", verdict))),
        h3(enonce),
        tags$dl(class = "hypo-dl",
          tags$dt("Test"), tags$dd(test), tags$dt("Statistique"), tags$dd(stat), tags$dt("p-value"), tags$dd(pval),
          tags$dt("Robustesse"), tags$dd(robuste)))
    }
    tagList(
      div(class = "small text-muted mb-3", paste("Réglages :", reglages_txt())),
      carte("H1", "À l'échelle d'un pays, les data centers consomment moins de 0,1 % de l'eau prélevée.",
            "Test binomial unilatéral : la proportion de pays sous 0,1 % dépasse-t-elle 50 % ?",
            paste0(k, " pays sur ", n, " sous 0,1 % (", fmt(100 * k / n, 0), " %)"), p_txt(b1$p.value), v1,
            paste0("Cas le plus défavorable (", fmt(wue[["haut"]], 2), " L/kWh, croissance rapide, 2035) : ", fmt(r1, 0),
                   " % des pays restent sous 0,1 %. Limite : consommation comparée à des prélèvements, la part est minorée.")),
      carte("H2", "La région révèle plus de risque que le pays.",
            paste0("Test de McNemar sur données appariées (chaque site classé deux fois, seuil ", seuil_txt(m$seuil), ")"),
            paste0(fmt(m$p_reg, 0), " % des sites en région sèche contre ", fmt(m$p_pays, 0), " % vu du pays ; khi² = ", fmt(m$chi2, 0),
                   " (", fmt(m$caches, 0), " sites cachés, ", fmt(m$inverse, 0), " dans l'autre sens)"), m$p_txt, v2,
            paste0("Sites groupés en grappes (", fmt(m$top_pct, 0), " % en ", m$top_region, ") : avec une ligne par région (", m$n_reg, " régions), ",
                   fmt(m$p_reg_reg, 0), " % contre ", fmt(m$p_pays_reg, 0), " % (", p_txt(m$p_r), "). ",
                   if (v2 == "nuancée") "L'effet est net pour les sites, pas pour les régions." else "")),
      carte("H3", "La pression des data centers sur l'eau augmente d'ici 2035.",
            "Pas de test statistique possible : projection par les 4 scénarios de l'AIE. Contrôle : tendance mesurée (Mann-Kendall)",
            paste0("Électricité des data centers ×", fmt(min(f35$f_monde), 1), " à ×", fmt(max(f35$f_monde), 1), " ; eau consommée : ", fmt(e24, 0),
                   " Mm³ en 2024, ", fmt(e35[["bas"]], 0), " à ", fmt(e35[["haut"]], 0), " Mm³ en 2035"),
            paste0("— (tendance de l'électricité des 30 pays : ", p_txt(mk30e$p.value), ")"), v3,
            paste0("Hausse dans tous les scénarios, même le plus lent. Mais l'eau prélevée des 30 pays les plus équipés n'a pas de tendance significative (",
                   p_txt(mk30$p.value), ") : la hausse ne se voit pas encore à l'échelle nationale.")))
  })

  # Dendrogramme (arbre hclust de HCPC), à l'horizontale
  output$dendro <- renderPlotly({
    h <- d$cah_arbre; n <- length(h$order)
    pos <- numeric(n); pos[h$order] <- seq_len(n)
    xm <- numeric(n - 1); segs <- vector("list", n - 1)
    gx <- function(j) if (j < 0) pos[-j] else xm[j]
    gh <- function(j) if (j < 0) 0 else h$height[j]
    for (i in seq_len(n - 1)) {
      a <- h$merge[i, 1]; b <- h$merge[i, 2]; xa <- gx(a); xb <- gx(b); hi <- h$height[i]; xm[i] <- (xa + xb) / 2
      segs[[i]] <- rbind(c(gh(a), xa, hi, xa), c(gh(b), xb, hi, xb), c(hi, xa, hi, xb))
    }
    sg <- do.call(rbind, segs)
    df <- data.frame(x = as.vector(rbind(sg[, 1], sg[, 3], NA)), y = as.vector(rbind(sg[, 2], sg[, 4], NA)))
    feuilles <- tibble(country = h$labels, y = pos) |> left_join(d$classes |> select(country, nom_classe), by = "country")
    coupe <- mean(sort(h$height, decreasing = TRUE)[(d$cah_info$k - 1):d$cah_info$k])
    p <- plot_ly() |>
      add_trace(data = df, x = ~x, y = ~y, type = "scatter", mode = "lines", line = list(color = "#7a8a94", width = 1.2),
                hoverinfo = "none", showlegend = FALSE)
    for (cl in names(col_classes)) {
      f <- feuilles |> filter(nom_classe == cl)
      p <- p |> add_trace(data = f, x = 0, y = ~y, type = "scatter", mode = "markers", name = cl,
                          marker = list(color = col_classes[[cl]], size = 9), text = ~country, hoverinfo = "text")
    }
    p |> layout(shapes = list(list(type = "line", x0 = coupe, x1 = coupe, y0 = 0, y1 = n + 1, line = list(color = "#c0392b", dash = "dash", width = 2))),
                xaxis = list(title = "Hauteur (perte d'inertie)", zeroline = FALSE),
                yaxis = list(title = "", tickvals = feuilles$y, ticktext = feuilles$country, tickfont = list(size = 9), range = c(0, n + 2)),
                legend = list(orientation = "h", x = 0, y = 1.04), margin = list(l = 10, t = 30))
  })
  output$portrait <- renderUI({
    t <- d$classes |> group_by(nom_classe) |>
      summarise(pays = n(), sites = median(n_dc), hab = median(population) / 1e6, pib = median(pib_hab),
                stress = median(stress_hydrique), part = median(part_central),
                ex = paste(head(country[order(-n_dc)], 3), collapse = ", "), .groups = "drop") |>
      arrange(match(nom_classe, names(col_classes)))
    carte <- function(i) {
      x <- t[i, ]
      div(class = "famille", style = paste0("border-left:5px solid ", col_classes[[x$nom_classe]], ";"),
        div(class = "famille-nom", x$nom_classe, span(class = "famille-n", paste(x$pays, "pays"))),
        div(class = "famille-val", paste0(fmt(x$sites, 0), " sites · ", fmt(x$hab, 0), " M hab. · ", fmt(x$pib / 1000, 0), " k$/hab. · stress ",
                                          fmt(x$stress, 0), " % · eau DC ", pct(x$part))),
        div(class = "famille-ex", x$ex),
        div(class = "famille-vt", vt(x$nom_classe)))
    }
    noms_v <- c(n_dc = "data centers", prelev_vol = "prélèvements", stress_hydrique = "stress hydrique", eau_renouv_hab = "eau disponible/hab.",
                pib_hab = "PIB/hab.", elec_generation_twh = "production électrique", elec_emissions_intensity_gco2e_kwh = "électricité carbonée",
                population = "population", part_central = "part eau DC")
    num_nom <- d$classes |> distinct(classe, nom_classe)
    vt <- function(nom) { k <- num_nom$classe[num_nom$nom_classe == nom]
      v <- d$cah_desc |> filter(classe == k, abs(vtest) >= 2) |> arrange(-abs(vtest)) |> head(3)
      if (!nrow(v)) return("aucune variable très distinctive")
      paste0("Se distingue par : ", paste0(ifelse(v$vtest > 0, "+ ", "− "), noms_v[v$var], " (v = ", fmt(v$vtest, 1), ")", collapse = " · ")) }
    tagList(lapply(seq_len(nrow(t)), carte))
  })
  output$portrait_retenir <- renderText({
    t <- d$classes |> group_by(nom_classe) |> summarise(part = median(part_central), n = n(), .groups = "drop")
    m <- t |> slice_max(part, n = 1); petit <- t |> filter(n <= 2)
    paste0("L'eau des data centers pèse surtout dans la famille « ", m$nom_classe, " » (médiane ", pct(m$part), ").",
           if (nrow(petit)) paste0(" « ", paste(petit$nom_classe, collapse = ", "), " » : groupe de valeurs extrêmes (", paste(petit$n, collapse = ", "), " pays).") else "") })
  output$cah_robuste <- renderUI({
    km <- d$cah_km; num_nom <- d$classes |> distinct(classe, nom_classe)
    noms <- num_nom$nom_classe[match(as.integer(rownames(km)), num_nom$classe)]
    stab <- apply(km, 1, function(r) 100 * max(r) / sum(r))
    tagList(
      p(paste0("Accord global CAH / k-means : ", fmt(d$cah_accord, 0), " % des pays restent ensemble. Famille par famille :")),
      tags$table(class = "table table-sm mb-0",
        tags$thead(tags$tr(tags$th("Famille (CAH)"), lapply(colnames(km), function(j) tags$th(paste("k-means", j))), tags$th("Stabilité"))),
        tags$tbody(lapply(seq_len(nrow(km)), function(i) tags$tr(tags$td(noms[i]), lapply(km[i, ], function(v) tags$td(v)),
                                                                  tags$td(tags$b(paste0(fmt(stab[i], 0), " %"))))))),
      p(class = "small text-muted mt-2", paste0("Famille la moins stable : « ", noms[which.min(stab)], " » ; la plus stable : « ", noms[which.max(stab)], " ».")))
  })

  # ---------- Méthode : ACM
  output$acm_eboulis <- renderPlotly({
    e <- d$acm_eig |> head(8) |> mutate(axe = factor(sub("Axe ", "", axe), sub("Axe ", "", axe)))
    plot_ly(e, x = ~axe, y = ~inertie, type = "bar", marker = list(color = c("#1f6f8b", "#1f6f8b", rep("#c6dbef", nrow(e) - 2))),
            text = ~paste0(fmt(inertie, 0), " %"), textposition = "outside", hoverinfo = "none") |>
      layout(xaxis = list(title = "Axe"), yaxis = list(title = "% de l'inertie", range = c(0, max(e$inertie) * 1.2)))
  })
  output$acm_plan <- renderPlotly({
    m <- d$acm_mod |> mutate(groupe = ifelse(sup, "Famille (supplémentaire)", variable),
                             etiq = sub("^[^:]+ : ", "", modalite), etiq = ifelse(sup, modalite, etiq))
    noms_var <- c(Parc = "Taille du parc", Stress = "Stress du pays", Richesse = "PIB par habitant", Carbone = "Électricité",
                  Part_eau = "Part de l'eau des DC", Sites_secs = "Sites en région sèche")
    m$groupe <- ifelse(m$groupe %in% names(noms_var), noms_var[m$groupe], m$groupe)
    cols <- c(setNames(c("#1f77b4", "#d62728", "#2ca02c", "#7f7f7f", "#ff7f0e", "#9467bd"), unname(noms_var)), "Famille (supplémentaire)" = "#c0392b")
    plot_ly(m, x = ~dim1, y = ~dim2, color = ~groupe, colors = cols, type = "scatter", mode = "markers+text",
            text = ~etiq, textposition = "top center", textfont = list(size = 10),
            marker = list(size = ~ifelse(sup, 12, 6 + 1.2 * sqrt(coalesce(n, 10))), opacity = 0.85),
            hovertext = ~paste0("<b>", modalite, "</b><br>", ifelse(is.na(n), "", paste(n, "pays")),
                                ifelse(sup, "", paste0("<br>Contribution axe 1 : ", fmt(contrib1, 1), " % · axe 2 : ", fmt(contrib2, 1), " %"))), hoverinfo = "text") |>
      layout(xaxis = list(title = paste0("Axe 1 (", fmt(d$acm_eig$inertie[1], 0), " %) : ← eau abondante, pays riches          stress, électricité carbonée →"), zeroline = TRUE),
             yaxis = list(title = paste0("Axe 2 (", fmt(d$acm_eig$inertie[2], 0), " %)"), zeroline = TRUE), legend = list(orientation = "h", x = 0, y = -0.15))
  })
  output$acm_retenir <- renderText({
    m <- d$acm_mod |> filter(!sup) |> arrange(-contrib1)
    paste0("Axe 1 : « ", m$modalite[1], " » et « ", m$modalite[2], " » s'opposent à « ",
           (m |> filter(dim1 < 0) |> slice_max(contrib1, n = 1))$modalite, " ». La part de l'eau des data centers est forte là où le stress est faible.") })

  # ---------- Sources
  tab <- reactive(res() |> transmute(Pays = country, `Data centers` = n_dc, `Électricité (TWh)` = round(e_h, 2),
    `Eau consommée sur place (Mm³)` = round(eau, 2), `Part de l'eau prélevée du pays (%)` = signif(part, 3), `Stress hydrique FAO (%)` = round(stress0, 1),
    `Sites en région sèche (%)` = round(pct_sites_region_stress, 1), `Calé sur l'AIE` = ifelse(cale_aie, "oui", "non"),
    `GWh par site` = round(gwh_site, 1)) |> arrange(desc(`Part de l'eau prélevée du pays (%)`)))
  output$tab_pays <- renderDT(datatable(tab(), rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)))
  output$dl_dico <- downloadHandler(filename = "dictionnaire_variables.csv",
                                    content = function(f) write.csv(d$dictionnaire, f, row.names = FALSE, fileEncoding = "UTF-8"))
  output$dl <- downloadHandler(filename = function() paste0("eau_datacenters_", hz(), ".csv"),
                               content = function(f) write.csv(tab(), f, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
