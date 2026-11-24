# =============================================================================
# SAÉ 5.EMS.01 - Data centers, IA et eau
# Préparation des données de l'application Shiny.
# Reprend à l'identique les calculs du Rapport 2 (analyse_rapport2_v2.Rmd).
# Sortie : app/app_data.rds (seul fichier lu par l'application).
# Lancer depuis ce dossier :  Rscript prep.R
# =============================================================================
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(tibble)
  library(janitor); library(countrycode); library(FactoMineR); library(forecast)
})

# --- 1. Paramètres sourcés (identiques au rapport) ---------------------------
aie <- tribble(
  ~cas,              ~annee, ~twh,
  "Historique",       2020,   269,
  "Historique",       2023,   361,
  "Historique",       2024,   416,
  "Headwinds",        2030,   669,  "Headwinds",       2035,  707,
  "High Efficiency",  2030,   792,  "High Efficiency", 2035,  972,
  "Base",             2030,   946,  "Base",            2035, 1193,
  "Lift-Off",         2030,  1264,  "Lift-Off",        2035, 1719)
twh_2024 <- 416

scen <- aie |>
  filter(cas %in% c("Headwinds", "Base", "Lift-Off")) |>
  transmute(scenario = recode(cas, Headwinds = "bas", Base = "central", `Lift-Off` = "haut"),
            horizon = annee, twh_monde = twh, f_monde = twh / twh_2024)

zones <- tibble(zone = c("USA", "CHN", "EUR", "RDM"),
                hausse_2030 = c(240, 175, 45, NA),
                taux_2030   = c(1.30, 1.70, 0.70, NA)) |>
  mutate(twh_zone = hausse_2030 / taux_2030)
zones$twh_zone[4]    <- 415 - sum(zones$twh_zone[1:3])
zones$hausse_2030[4] <- (946 - twh_2024) - sum(zones$hausse_2030[1:3])
zones <- zones |> mutate(taux_2030 = hausse_2030 / twh_zone,
                         rel = taux_2030 / (946 / twh_2024 - 1))

scen_zone <- crossing(zones |> select(zone, rel), scen |> select(scenario, horizon, f_monde)) |>
  mutate(f_zone = 1 + (f_monde - 1) * rel)

wue <- c(bas = 575000 / 2.7e9 * 1e3, central = 0.81, haut = 6.5e6 / 2.7e9 * 1e3)
part_ia_2024 <- 0.15; part_ia_fr <- 0.13; part_ia_hausse <- 0.50
conso_hab <- 6.5e6 / 1e5

# --- 2. Import ---------------------------------------------------------------
panel <- fread("donnees_brutes/panel_annuel.csv") |> as_tibble() |> clean_names()
dcw_raw <- fread("donnees_brutes/data-centres-worldwide.csv", sep = ",", header = FALSE,
                 fill = TRUE, quote = "\"") |> as_tibble()
names(dcw_raw)[1:3] <- c("region", "country", "n_dc")
dcw <- dcw_raw |>
  mutate(n_dc = suppressWarnings(as.numeric(n_dc))) |>
  filter(!is.na(n_dc), !is.na(country), country != "") |>
  mutate(iso3 = countrycode(country, "country.name", "iso3c",
                            custom_match = c("Korea, South" = "KOR", "South Korea" = "KOR",
                                             "Ivory Coast" = "CIV", "Cape Verde" = "CPV",
                                             "Hong Kong" = "HKG", "Macau" = "MAC",
                                             "New Zeland" = "NZL", "Moldavia" = "MDA",
                                             "Polynesia" = "PYF"))) |>
  filter(!is.na(iso3))

# --- 3. Électricité des data centers par pays (2024) --------------------------
elec_nat <- panel |> filter(year >= 2019) |> group_by(iso3) |>
  summarise(elec = mean(coalesce(elec_demand_twh, elec_generation_twh), na.rm = TRUE), .groups = "drop")

ancres <- tibble(iso3 = c("IRL", "JPN", "IND")) |>
  left_join(elec_nat, by = "iso3") |>
  mutate(e_fix = case_when(iso3 == "IRL" ~ 0.20 * elec,
                           iso3 == "JPN" ~ 15 / 0.80,
                           iso3 == "IND" ~ 0.005 * elec))

dc_pays <- dcw |>
  group_by(iso3) |>
  summarise(country = first(country), n_dc = sum(n_dc), .groups = "drop") |>
  mutate(country = coalesce(countrycode(iso3, "iso3c", "country.name.fr"),
                            countrycode(iso3, "iso3c", "country.name"), country),
         continent = countrycode(iso3, "iso3c", "continent"),
         zone = case_when(iso3 == "USA" ~ "USA", iso3 == "CHN" ~ "CHN",
                          continent == "Europe" & iso3 != "RUS" ~ "EUR", TRUE ~ "RDM")) |>
  left_join(ancres |> select(iso3, e_fix), by = "iso3") |>
  left_join(zones |> select(zone, twh_zone), by = "zone") |>
  group_by(zone) |>
  mutate(reste = first(twh_zone) - sum(e_fix, na.rm = TRUE),
         n_libre = ifelse(is.na(e_fix), n_dc, 0),
         e0 = ifelse(is.na(e_fix), reste * n_libre / sum(n_libre), e_fix)) |>
  ungroup() |>
  select(iso3, country, zone, n_dc, e0) |>
  left_join(elec_nat, by = "iso3")
TWH_MONDE <- sum(dc_pays$e0)   # 415 TWh répartis (AIE 2024, hors cryptomonnaies)
stopifnot(abs(sum(dc_pays$e0) - 415) < 1e-6)
# Pays « calés sur l'AIE » : valeur publiée (Irlande, Japon, Inde) ou total de zone AIE (États-Unis, Chine).
# Les autres reçoivent le reste de leur zone, réparti selon le nombre de sites (GWh par site identique dans la zone).
dc_pays <- dc_pays |> mutate(cale_aie = iso3 %in% ancres$iso3 | zone %in% c("USA", "CHN"),
                             gwh_site = 1000 * e0 / n_dc)
gwh_site_zone <- dc_pays |> filter(!cale_aie) |> group_by(zone) |> summarise(gwh_site = first(gwh_site), .groups = "drop")

# --- 4. Ressource en eau -------------------------------------------------------
ressource <- panel |>
  filter(year >= 2015, !is.na(prelev_vol)) |>
  group_by(iso3) |>
  summarise(prelev0 = mean(prelev_vol),
            stress0 = mean(stress_hydrique, na.rm = TRUE),
            pib_hab = mean(pib_hab, na.rm = TRUE),
            population = mean(population, na.rm = TRUE), .groups = "drop") |>
  mutate(across(-iso3, ~ ifelse(is.nan(.x), NA, .x)))

pays <- dc_pays |> left_join(ressource, by = "iso3")

# --- 5. ACP + CAH (identique au rapport) -------------------------------------
vars_actives <- c("n_dc", "prelev_vol", "stress_hydrique", "eau_renouv_hab", "pib_hab",
                  "elec_generation_twh", "elec_emissions_intensity_gco2e_kwh", "population")
vars_log <- c("n_dc", "prelev_vol", "eau_renouv_hab", "pib_hab", "elec_generation_twh", "population")

present <- pays |> filter(!is.na(prelev0)) |>
  mutate(part_central = 100 * e0 * wue[["central"]] / (prelev0 * 1e3))

profil <- panel |>
  filter(year >= 2015, year <= 2022) |>
  group_by(iso3) |>
  summarise(across(c(prelev_vol, stress_hydrique, eau_renouv_hab, pib_hab, elec_generation_twh,
                     elec_emissions_intensity_gco2e_kwh, population), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x))) |>
  inner_join(dc_pays |> select(iso3, country, n_dc), by = "iso3") |>
  inner_join(present |> select(iso3, part_central), by = "iso3")

base_acp <- profil |> filter(n_dc >= 10) |> drop_na(all_of(vars_actives), part_central)
# Pays exclus de l'ACP et motif (affichés dans l'onglet ACP)
acp_exclus <- pays |>
  left_join(profil |> select(iso3, all_of(setdiff(vars_actives, "n_dc")), part_central), by = "iso3") |>
  filter(!iso3 %in% base_acp$iso3) |>
  mutate(motif = ifelse(n_dc < 10, "moins de 10 data centers", "données incomplètes")) |>
  transmute(country, n_dc, motif) |> arrange(motif, desc(n_dc))

# Asymétrie : justifie le passage au log (relecture du 24/11/2026)
asymetrie <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }
acp_asym <- tibble(variable = vars_actives,
                   brute = sapply(vars_actives, function(v) asymetrie(base_acp[[v]])),
                   log = sapply(vars_actives, function(v) asymetrie(log1p(base_acp[[v]]))))

# Version initiale (stress brut) gardée pour comparaison avant / après
X_avant <- base_acp |> mutate(across(all_of(c(vars_log, "part_central")), log1p)) |>
  select(country, all_of(vars_actives), part_central) |> column_to_rownames("country")
acp_avant <- PCA(X_avant, scale.unit = TRUE, ncp = 5, graph = FALSE, quanti.sup = which(names(X_avant) == "part_central"))
# Version retenue : stress hydrique aussi en log (asymétrie 5,5 -> 1,0) ; intensité carbone brute (asymétrie 0,0)
vars_log <- c(vars_log, "stress_hydrique")
X <- base_acp |>
  mutate(across(all_of(c(vars_log, "part_central")), log1p)) |>
  select(country, all_of(vars_actives), part_central) |>
  column_to_rownames("country")
acp <- PCA(X, scale.unit = TRUE, ncp = 8, graph = FALSE,
           quanti.sup = which(names(X) == "part_central"))
acp_kaiser <- sum(acp$eig[, 1] > 1)                     # critère de Kaiser (valeur propre > 1)
acp_cmp <- tibble(version = c("Avant (stress brut)", "Après (stress en log)"),
                  axe1 = c(acp_avant$eig[1, 2], acp$eig[1, 2]), axe2 = c(acp_avant$eig[2, 2], acp$eig[2, 2]),
                  cumul2 = c(acp_avant$eig[2, 3], acp$eig[2, 3]),
                  cos2_stress = c(sum(acp_avant$var$cos2["stress_hydrique", 1:2]), sum(acp$var$cos2["stress_hydrique", 1:2])),
                  contrib_max_pays = c(max(acp_avant$ind$contrib[, 1:2]), max(acp$ind$contrib[, 1:2])),
                  pays_max = c(rownames(acp_avant$ind$contrib)[which.max(apply(acp_avant$ind$contrib[, 1:2], 1, max))],
                               rownames(acp$ind$contrib)[which.max(apply(acp$ind$contrib[, 1:2], 1, max))]))

# CAH (Ward) sur les axes retenus par Kaiser, consolidée par k-means (HCPC)
acp_k <- PCA(X, scale.unit = TRUE, ncp = acp_kaiser, graph = FALSE, quanti.sup = which(names(X) == "part_central"))
cah0 <- HCPC(acp_k, nb.clust = -1, graph = FALSE, consol = FALSE)
haut <- sort(cah0$call$t$tree$height, decreasing = TRUE)
NB_CLASSES <- 4   # À VÉRIFIER : plus grand saut = 2 groupes (trop grossier) ; 4 = groupes lisibles, aucun < 3 pays
cah <- HCPC(acp_k, nb.clust = NB_CLASSES, graph = FALSE, consol = TRUE)
W <- cah$call$t$within
cah_info <- list(k = NB_CLASSES, axes = acp_kaiser, hauteurs = head(haut, 8),
                 inter_k = 100 * (1 - W[2:8] / W[1]), inter = 100 * (1 - W[NB_CLASSES] / W[1]))
# Robustesse : k-means directement sur les mêmes axes (25 départs), tableau croisé des appartenances
set.seed(2026)
km <- kmeans(acp_k$ind$coord, centers = NB_CLASSES, nstart = 25)
cah_km <- table(CAH = cah$data.clust$clust[match(rownames(acp_k$ind$coord), rownames(cah$data.clust))], kmeans = km$cluster)
cah_accord <- 100 * sum(apply(cah_km, 1, max)) / sum(cah_km)
# Description des classes : variables significatives (v-test)
cah_desc <- bind_rows(lapply(names(cah$desc.var$quanti), function(k) {
  t <- cah$desc.var$quanti[[k]]; if (is.null(t)) return(NULL)
  tibble(classe = as.integer(k), var = rownames(t), vtest = t[, "v.test"], moy_classe = t[, "Mean in category"],
         moy_tot = t[, "Overall mean"], p = t[, "p.value"]) }))

noms_var <- c(n_dc = "Nb data centers", prelev_vol = "Prélèvements d'eau",
              stress_hydrique = "Stress hydrique", eau_renouv_hab = "Eau dispo / hab.",
              pib_hab = "PIB / hab.", elec_generation_twh = "Production électrique",
              elec_emissions_intensity_gco2e_kwh = "Intensité carbone élec.",
              population = "Population", part_central = "Part eau DC (suppl.)")

classes <- tibble(country = rownames(cah$data.clust), classe = as.integer(cah$data.clust$clust)) |>
  left_join(base_acp |> select(country, iso3, n_dc, population, pib_hab, stress_hydrique, part_central),
            by = "country") |>
  bind_cols(as_tibble(acp$ind$coord[match(rownames(cah$data.clust), rownames(acp$ind$coord)), 1:3]) |>
              setNames(c("dim1", "dim2", "dim3")))

# Nom des classes à partir de leurs médianes (même lecture que le rapport)
med <- classes |> group_by(classe) |>
  summarise(sites = median(n_dc), pop = median(population), pib = median(pib_hab),
            stress = median(stress_hydrique), .groups = "drop")
nom_classe <- character(nrow(med))
nom_classe[which.max(med$stress)] <- "Pays très secs"
nom_classe[which.max(med$sites)]  <- "Grands pays équipés"
reste <- which(nom_classe == "")
nom_classe[reste[which.max(med$pib[reste])]] <- "Petits pays riches"
nom_classe[nom_classe == ""] <- "Émergents, revenus plus faibles"
classes <- classes |> mutate(nom_classe = nom_classe[classe])

acp_var <- tibble(variable = noms_var[rownames(acp$var$coord)],
                  dim1 = acp$var$coord[, 1], dim2 = acp$var$coord[, 2], sup = FALSE) |>
  bind_rows(tibble(variable = noms_var["part_central"],
                   dim1 = acp$quanti.sup$coord[, 1], dim2 = acp$quanti.sup$coord[, 2], sup = TRUE))
acp_eig <- tibble(axe = paste("Axe", seq_len(nrow(acp$eig))), valeur_propre = acp$eig[, 1],
                  inertie = acp$eig[, 2], cumul = acp$eig[, 3])
acp_contrib <- tibble(variable = noms_var[rownames(acp$var$contrib)], axe1 = acp$var$contrib[, 1], axe2 = acp$var$contrib[, 2],
                      cos2 = rowSums(acp$var$cos2[, 1:2])) |> arrange(desc(axe1 + axe2))


# --- 6. Historique 30 pays les plus équipés ------------------------------------
pays_dc <- dc_pays |> slice_max(n_dc, n = 30) |> pull(iso3)
hist <- panel |>
  filter(iso3 %in% pays_dc, year >= 2000, year <= 2022) |>
  group_by(year) |>
  summarise(eau = sum(prelev_vol, na.rm = TRUE), elec = sum(elec_generation_twh, na.rm = TRUE),
            n_eau = sum(!is.na(prelev_vol)), n_elec = sum(!is.na(elec_generation_twh)), .groups = "drop") |>
  filter(n_eau == max(n_eau), n_elec == max(n_elec)) |>
  mutate(eau_ind = 100 * eau / first(eau), elec_ind = 100 * elec / first(elec))
# Séries par pays (onglet Méthode > Évolution) : électricité produite et eau prélevée
hist_pays <- panel |> filter(year >= 2010, year <= 2022) |>
  transmute(iso3, year, eau = prelev_vol, elec = elec_generation_twh)

# --- 7. France -------------------------------------------------------------------
fr_arcep <- tibble(annee = 2021:2024,
                   elec_twh = 2.7 / c(1.14 * 1.08 * 1.12, 1.08 * 1.12, 1.12, 1),
                   eau_directe_m3 = 575000 / c(1.17 * 1.16 * 0.85, 1.16 * 0.85, 0.85, 1))

fr <- panel |> filter(iso3 == "FRA") |> arrange(year)
prevoir <- function(annees, valeurs, fin = 2035, n_test = 3) {
  ok <- !is.na(valeurs); annees <- annees[ok]; valeurs <- valeurs[ok]
  y <- ts(valeurs, start = min(annees)); n <- length(y); h <- fin - max(annees)
  train <- ts(valeurs[1:(n - n_test)], start = min(annees)); test <- valeurs[(n - n_test + 1):n]
  modeles <- list(
    "Naïf"   = function(s, h) naive(s, h = h, level = 95),
    "Dérive" = function(s, h) rwf(s, h = h, drift = TRUE, level = 95),
    "ETS"    = function(s, h) forecast(ets(s), h = h, level = 95),
    "ARIMA"  = function(s, h) forecast(auto.arima(s, seasonal = FALSE, max.p = 2, max.q = 2), h = h, level = 95))
  mae <- sapply(modeles, function(f) tryCatch(mean(abs(as.numeric(f(train, n_test)$mean) - test)),
                                              error = function(e) NA_real_))
  rmse <- sapply(modeles, function(f) tryCatch(sqrt(mean((as.numeric(f(train, n_test)$mean) - test)^2)),
                                               error = function(e) NA_real_))
  choix <- names(which.min(mae)); prev <- modeles[[choix]](y, h)
  list(scores = tibble(modele = names(mae), MAE = mae, RMSE = rmse), choix = choix, n_train = n - n_test, n_test = n_test,
       obs = tibble(annee = annees, valeur = valeurs),
       prev = tibble(annee = max(annees) + seq_len(h), prevision = as.numeric(prev$mean),
                     basse = as.numeric(prev$lower[, 1]), haute = as.numeric(prev$upper[, 1])))
}
prev_fr <- prevoir(fr$year, fr$prelev_vol)
# Validation croisée glissante (origine mobile, horizon 1 an) : plus robuste qu'un seul découpage 5 + 3 ans
fr_y <- ts(fr$prelev_vol[!is.na(fr$prelev_vol)], start = min(fr$year[!is.na(fr$prelev_vol)]))
fonctions_cv <- list(
  "Naïf"   = function(x, h) naive(x, h = h),
  "Dérive" = function(x, h) rwf(x, h = h, drift = TRUE),
  "ETS"    = function(x, h) forecast(ets(x), h = h),
  "ARIMA"  = function(x, h) forecast(auto.arima(x, seasonal = FALSE, max.p = 2, max.q = 2), h = h))
prev_cv <- bind_rows(lapply(names(fonctions_cv), function(m) {
  e <- suppressWarnings(tsCV(fr_y, fonctions_cv[[m]], h = 1, initial = 3))
  tibble(modele = m, MAE_cv = mean(abs(e), na.rm = TRUE), RMSE_cv = sqrt(mean(e^2, na.rm = TRUE)), n_cv = sum(!is.na(e))) }))
test_vals <- tail(as.numeric(fr_y), 3)
prev_fr$scores <- prev_fr$scores |> left_join(prev_cv, by = "modele")

# =============================================================================
# 8. AJOUTS suite aux remarques du jury (20/11/2026)
#    - disparités à l'intérieur des pays (régions)  - saisons (mois chauds et secs)
#    - arbitrages entre usages de l'eau            - équivalents du quotidien, CO2
# =============================================================================
suppressPackageStartupMessages({ library(sf); library(jsonlite) })
lab_aq <- c("Faible (<10 %)", "Faible à moyen (10-20 %)", "Moyen à élevé (20-40 %)",
            "Élevé (40-80 %)", "Extrêmement élevé (>80 %)", "Aride, faible usage")
cat_aq <- function(cat) factor(lab_aq[ifelse(cat < 0, 6, cat + 1)], levels = lab_aq)

# --- 8.1 Stress régional (WRI Aqueduct 4.0, provinces) x sites OpenStreetMap (20/09/2026)
prov <- fread("donnees_brutes/base_provinces.csv", na.strings = "", encoding = "UTF-8") |> as_tibble() |>
  transmute(gid_1, region = name_1, iso3 = gid_0, bws_cat = aq_bws_baseline_cat,
            bws_2030 = aq_bws_bau_2030_score, bws_2050 = aq_bws_bau_2050_score, secheresse = aq_drr_baseline_score)
osm <- fread("donnees_brutes/osm_datacenters_gid1.csv", na.strings = "", encoding = "UTF-8") |> as_tibble() |>
  transmute(lat, lon, iso3, gid_1 = gid_1_gadm, nom = coalesce(name, operator, "Site sans nom"),
            operateur = operator) |>
  left_join(prov |> select(gid_1, region, bws_cat, bws_2030, bws_2050, secheresse), by = "gid_1") |>
  mutate(stress_region = cat_aq(bws_cat),
         pays = coalesce(countrycode(iso3, "iso3c", "country.name.fr", custom_match = c(XKX = "Kosovo")), iso3))

aq_pays <- fread("donnees_brutes/stress_hydrique_aqueduct_baseline.csv") |> as_tibble() |>
  filter(indicator_name == "bws", weight == "Tot") |> transmute(iso3, bws_cat_pays = cat)
osm <- osm |> left_join(aq_pays, by = "iso3") |> mutate(stress_pays = cat_aq(bws_cat_pays))

# Même indicateur (Aqueduct) à deux échelles : pays entier vs région du site
echelles <- bind_rows(
  osm |> filter(!is.na(stress_pays), !is.na(stress_region)) |> count(classe = stress_pays) |> mutate(echelle = "Pays entier"),
  osm |> filter(!is.na(stress_pays), !is.na(stress_region)) |> count(classe = stress_region) |> mutate(echelle = "Région du site")) |>
  group_by(echelle) |> mutate(part = 100 * n / sum(n)) |> ungroup()

# McNemar : les sites sont groupés en grappes (même région = même classement). Contrôle : une seule ligne par région.
sec_aq <- function(cat) !is.na(cat) & cat >= 3          # « région sèche » = stress Aqueduct ≥ 40 % (classes Élevé et Extrêmement élevé)
par_region <- osm |> filter(!is.na(stress_pays), !is.na(stress_region)) |>
  group_by(gid_1) |> summarise(pays_sec = first(sec_aq(bws_cat_pays)), region_sec = first(sec_aq(bws_cat)), sites = n(), .groups = "drop")
t_reg <- table(factor(par_region$pays_sec, c(FALSE, TRUE)), factor(par_region$region_sec, c(FALSE, TRUE)))
mcn_regions <- list(n = nrow(par_region), p_pays = 100 * mean(par_region$pays_sec), p_reg = 100 * mean(par_region$region_sec),
                    caches = t_reg["FALSE", "TRUE"], inverse = t_reg["TRUE", "FALSE"], p = mcnemar.test(t_reg)$p.value)

score_cat <- function(x) ifelse(is.na(x), NA, ifelse(x < 0, -1, pmin(4, floor(x))))
osm <- osm |> mutate(stress_2030 = cat_aq(score_cat(bws_2030)), stress_2050 = cat_aq(score_cat(bws_2050)))
regions <- osm |> filter(!is.na(region)) |>
  group_by(iso3, pays, gid_1, region, stress_region) |>
  summarise(sites = n(), lat = mean(lat), lon = mean(lon), stress_2030 = first(stress_2030), stress_2050 = first(stress_2050),
            secheresse = first(secheresse), .groups = "drop")

# Noms de régions doublement encodés dans le fichier source (ex. « ÃŽle-de-France ») : réparation
fix_enc <- function(x) { y <- iconv(x, "UTF-8", "CP1252"); Encoding(y) <- "UTF-8"
  ifelse(grepl("Ã|Å", x) & !is.na(y) & validUTF8(y), y, x) }
osm <- osm |> mutate(region = fix_enc(region))
regions <- regions |> mutate(region = fix_enc(region))
# Noms de régions en français (les plus fréquentes ; les autres restent dans la langue locale)
noms_fr <- c("Virginia" = "Virginie", "England" = "Angleterre", "California" = "Californie", "Noord-Holland" = "Hollande-Septentrionale",
  "Hessen" = "Hesse", "Washington" = "État de Washington", "New South Wales" = "Nouvelle-Galles du Sud", "Pennsylvania" = "Pennsylvanie",
  "Georgia" = "Géorgie (États-Unis)", "New York" = "État de New York", "Bayern" = "Bavière", "Nordrhein-Westfalen" = "Rhénanie-du-Nord-Westphalie",
  "Lombardia" = "Lombardie", "Vlaanderen" = "Flandre", "North Carolina" = "Caroline du Nord", "South Carolina" = "Caroline du Sud",
  "Moscow City" = "Moscou", "Moskva" = "Moscou (oblast)", "Región Metropolitana de Santiago" = "Région de Santiago", "Southern Finland" = "Finlande du Sud",
  "Baden-Württemberg" = "Bade-Wurtemberg", "Comunidad de Madrid" = "Communauté de Madrid", "Niedersachsen" = "Basse-Saxe",
  "Zürich" = "Zurich", "Australian Capital Territory" = "Territoire de la capitale australienne", "Mazowieckie" = "Mazovie",
  "New Mexico" = "Nouveau-Mexique", "Florida" = "Floride", "Missouri" = "Missouri", "Sachsen" = "Saxe", "Scotland" = "Écosse",
  "Prague" = "Prague", "Jawa Barat" = "Java occidental", "Hovedstaden" = "Région de Copenhague", "Rheinland-Pfalz" = "Rhénanie-Palatinat",
  "British Columbia" = "Colombie-Britannique", "Noord-Brabant" = "Brabant-Septentrional", "Zuid-Holland" = "Hollande-Méridionale",
  "Cataluña" = "Catalogne", "Lazio" = "Latium", "Wales" = "Pays de Galles", "Hamburg" = "Hambourg", "Bruxelles" = "Bruxelles",
  "Moscow" = "Moscou", "Suðurnes" = "Suðurnes (Islande)", "Tokyo" = "Tokyo", "North Dakota" = "Dakota du Nord",
  "South Dakota" = "Dakota du Sud", "West Virginia" = "Virginie-Occidentale", "Louisiana" = "Louisiane")
traduire <- function(x) ifelse(!is.na(x) & x %in% names(noms_fr), noms_fr[x], x)
osm <- osm |> mutate(region = traduire(region))
regions <- regions |> mutate(region = traduire(region))

# Rapport 1 : stress futur de la région des sites (WRI Aqueduct, scénario tendanciel « business as usual »)
futur_sites <- osm |>
  summarise(`Aujourd'hui` = 100 * mean(bws_cat >= 3, na.rm = TRUE),
            `2030` = 100 * mean(bws_2030 >= 3, na.rm = TRUE),
            `2050` = 100 * mean(bws_2050 >= 3, na.rm = TRUE)) |>
  pivot_longer(everything(), names_to = "horizon", values_to = "part")
print(futur_sites)

# --- 8.2 Arbitrages : eau des DC face à l'eau des ménages (AQUASTAT, dernière année <= 2017)
secteurs <- fread("donnees_brutes/aquastat_sectoriel_mon_extrait.csv") |> as_tibble() |>
  filter(grepl("as %", Variable)) |>
  mutate(secteur = case_when(grepl("Agricultural", Variable) ~ "agri",
                             grepl("Industrial", Variable) ~ "indus", TRUE ~ "menages")) |>
  group_by(iso3, secteur) |> slice_max(Year, n = 1, with_ties = FALSE) |> ungroup() |>
  select(iso3, secteur, Value, Year) |>
  pivot_wider(names_from = secteur, values_from = c(Value, Year)) |>
  transmute(iso3, pct_agri = Value_agri, pct_indus = Value_indus, pct_menages = Value_menages,
            annee_secteurs = Year_menages)

sites_stress <- osm |> filter(!is.na(bws_cat)) |> group_by(iso3) |>
  summarise(sites_osm = n(), pct_sites_region_stress = 100 * mean(bws_cat >= 3), .groups = "drop")

# Intensité carbone de l'électricité (Ember, moyenne 2019-2022) pour le CO2 des DC
co2 <- panel |> filter(year >= 2019) |> group_by(iso3) |>
  summarise(gco2_kwh = mean(elec_emissions_intensity_gco2e_kwh, na.rm = TRUE), .groups = "drop") |>
  mutate(gco2_kwh = ifelse(is.nan(gco2_kwh), NA, gco2_kwh))

pays <- pays |> left_join(secteurs, by = "iso3") |> left_join(sites_stress, by = "iso3") |>
  left_join(co2, by = "iso3") |> left_join(aq_pays, by = "iso3") |>
  mutate(stress_pays_aq = cat_aq(bws_cat_pays))

# --- 8.2 bis. ACM (analyse des correspondances multiples) sur les mêmes 61 pays -----
# Chaque indicateur est découpé en 3 classes (terciles ou seuils lisibles) ; famille CAH en supplémentaire.
tercile <- function(x, noms) cut(x, quantile(x, c(0, 1/3, 2/3, 1), na.rm = TRUE), labels = noms, include.lowest = TRUE)
base_acm <- base_acp |>
  left_join(pays |> select(iso3, bws_cat_pays, pct_sites_region_stress), by = "iso3") |>
  left_join(classes |> select(iso3, nom_classe), by = "iso3") |>
  transmute(country,
            Parc = cut(n_dc, c(9, 29, 99, Inf), labels = c("Parc : 10 à 29 sites", "Parc : 30 à 99", "Parc : 100 et plus")),
            Stress = factor(ifelse(is.na(bws_cat_pays), NA, ifelse(bws_cat_pays <= 1, "Stress pays : faible (< 20 %)",
                            ifelse(bws_cat_pays == 2, "Stress pays : moyen (20-40 %)", "Stress pays : élevé (≥ 40 %)")))),
            Richesse = tercile(pib_hab, c("PIB/hab. : bas", "PIB/hab. : moyen", "PIB/hab. : haut")),
            Carbone = tercile(elec_emissions_intensity_gco2e_kwh, c("Électricité : peu carbonée", "Électricité : moyenne", "Électricité : très carbonée")),
            Part_eau = tercile(part_central, c("Part eau DC : faible", "Part eau DC : moyenne", "Part eau DC : forte")),
            Sites_secs = cut(pct_sites_region_stress, c(-Inf, 0, 49.999, Inf),
                             labels = c("Sites en région sèche : aucun", "Sites en région sèche : < 50 %", "Sites en région sèche : ≥ 50 %")),
            Famille = factor(nom_classe)) |>
  filter(!is.na(Sites_secs), !is.na(Stress)) |>      # pays sans site géolocalisé : exclus de l'ACM (modalité < 5 %)
  mutate(across(where(is.factor), droplevels)) |>
  column_to_rownames("country")
acm_exclus <- setdiff(base_acp$country, rownames(base_acm))
acm_effectifs <- bind_rows(lapply(names(base_acm), function(v) tibble(variable = v, modalite = names(table(base_acm[[v]])),
                                                                      n = as.integer(table(base_acm[[v]]))))) |>
  mutate(pct = 100 * n / nrow(base_acm))
acm <- MCA(base_acm, quali.sup = which(names(base_acm) == "Famille"), graph = FALSE, ncp = 5)
acm_eig <- tibble(axe = paste("Axe", seq_len(nrow(acm$eig))), inertie = acm$eig[, 2], cumul = acm$eig[, 3])
acm_mod <- bind_rows(
  tibble(modalite = rownames(acm$var$coord), dim1 = acm$var$coord[, 1], dim2 = acm$var$coord[, 2],
         contrib1 = acm$var$contrib[, 1], contrib2 = acm$var$contrib[, 2], sup = FALSE),
  tibble(modalite = rownames(acm$quali.sup$coord), dim1 = acm$quali.sup$coord[, 1], dim2 = acm$quali.sup$coord[, 2],
         contrib1 = NA_real_, contrib2 = NA_real_, sup = TRUE)) |>
  left_join(acm_effectifs |> select(modalite, n, variable), by = "modalite")
acm_ind <- tibble(country = rownames(acm$ind$coord), dim1 = acm$ind$coord[, 1], dim2 = acm$ind$coord[, 2]) |>
  left_join(classes |> select(country, nom_classe), by = "country")
acm_eta2 <- tibble(variable = rownames(acm$var$eta2), axe1 = acm$var$eta2[, 1], axe2 = acm$var$eta2[, 2])

# --- 8.3 Saisons : normales mensuelles 1991-2020 (Banque mondiale CCKP, CRU TS 4.08)
cl <- fromJSON("donnees_brutes/climat_mensuel_cckp_1991_2020.json")$data
mk <- function(v) bind_rows(lapply(names(cl[[v]]), function(i)
  tibble(iso3 = i, mois = as.integer(substr(names(cl[[v]][[i]]), 6, 7)), val = unlist(cl[[v]][[i]])))) |>
  rename(!!v := val)
climat <- mk("tas") |> inner_join(mk("pr"), by = c("iso3", "mois")) |>
  group_by(iso3) |>
  mutate(sensible = tas > mean(tas) & pr < mean(pr)) |> ungroup()

# --- 8.4 Sites Google : eau consommée mesurée par site (rapport environnemental 2026, données 2025)
coords <- tribble(~site, ~lat, ~lon,
  "Ashburn, VA", 39.04, -77.49, "Berkeley County, SC", 33.06, -80.03, "Bristow, VA", 38.72, -77.54,
  "Council Bluffs, IA", 41.26, -95.86, "The Dalles, OR", 45.60, -121.18, "Douglas County, GA", 33.70, -84.77,
  "Dublin, Ireland", 53.35, -6.26, "Eemshaven, Netherlands", 53.44, 6.83, "Frankfurt, Germany", 50.11, 8.68,
  "Fredericia, Denmark", 55.57, 9.75, "Hamina, Finland", 60.57, 27.20, "Hanau, Germany", 50.13, 8.92,
  "Henderson, NV", 36.04, -114.98, "Inzai, Japan", 35.83, 140.15, "Jackson County, AL", 34.78, -85.99,
  "Lancaster, OH", 39.71, -82.60, "Leesburg, VA", 39.12, -77.56, "Lenoir, NC", 35.91, -81.54,
  "Lockbourne, OH", 39.81, -82.97, "Mayes County, OK", 36.24, -95.33, "Mesa, AZ", 33.42, -111.83,
  "Middenmeer, Netherlands", 52.81, 4.99, "Midlothian, TX", 32.48, -96.99, "Montgomery County, TN", 36.53, -87.36,
  "Montreal, Canada", 45.50, -73.57, "New Albany, OH", 40.08, -82.81, "New Haven, IN", 41.07, -85.01,
  "Omaha, NE", 41.26, -95.93, "Papillion, NE", 41.15, -96.04, "Pflugerville, TX", 30.44, -97.62,
  "Phoenix, AZ", 33.45, -112.07, "Quilicura, Chile", -33.36, -70.73, "Red Oak, TX", 32.52, -96.80,
  "San Bernardo, Chile", -33.59, -70.70, "St. Ghislain, Belgium", 50.47, 3.82, "Sterling, VA", 39.01, -77.43,
  "Storey County, NV", 39.45, -119.53, "Sydney, Australia", -33.87, 151.21, "Wilmer, TX", 32.59, -96.68,
  "Winschoten, Netherlands", 53.14, 7.03)
google <- fread("donnees_brutes/google_dc_water_by_site_2025.csv", encoding = "UTF-8") |> as_tibble() |>
  inner_join(coords, by = "site") |>
  transmute(site, iso3, lat, lon, eau_m3 = consumption_million_gallons * 3785.41,
            preleve_m3 = withdrawal_million_gallons * 3785.41)
# Région de chaque site Google = région du site OSM le plus proche (< 150 km)
dist_km <- function(la1, lo1, la2, lo2) {
  r <- pi / 180; a <- sin((la2 - la1) * r / 2)^2 + cos(la1 * r) * cos(la2 * r) * sin((lo2 - lo1) * r / 2)^2
  12742 * asin(sqrt(a)) }
osm_ok <- osm |> filter(!is.na(bws_cat))
google <- google |> rowwise() |>
  mutate(k = which.min(dist_km(lat, lon, osm_ok$lat, osm_ok$lon)),
         dk = dist_km(lat, lon, osm_ok$lat[k], osm_ok$lon[k]),
         region = ifelse(dk < 150, osm_ok$region[k], NA),
         bws_cat = ifelse(dk < 150, osm_ok$bws_cat[k], NA)) |>
  ungroup() |> mutate(stress_region = cat_aq(bws_cat)) |> select(-k, -dk)

# Historique des prélèvements par usage (milliards de m3), FAO AQUASTAT
secteurs_ts <- fread("donnees_brutes/aquastat_sectoriel_mon_extrait.csv") |> as_tibble() |>
  filter(!grepl("%", Variable)) |>
  mutate(usage = case_when(grepl("Agricultural", Variable) ~ "Agriculture", grepl("Industrial", Variable) ~ "Industrie",
                           TRUE ~ "Habitants")) |>
  transmute(iso3, annee = Year, usage, mds_m3 = Value)

eu <- fread("donnees_brutes/eu_dc_report_electricite_eau.csv") |> as_tibble() |>
  transmute(iso3, puissance_mw = puissance_it_mw, elec_gwh = conso_electrique_gwh, eau_m3 = conso_eau_m3)

# --- 8.5 Fond de carte des pays (Natural Earth 1:50m, simplifié)
monde <- st_read("donnees_brutes/_ref_natural_earth_50m_admin0.geojson", quiet = TRUE) |>
  transmute(iso3 = ifelse(ISO_A3_EH == "-99", ADM0_A3, ISO_A3_EH)) |>
  filter(iso3 != "ATA")
sf_use_s2(FALSE)
monde <- monde |> st_simplify(dTolerance = 0.03, preserveTopology = TRUE) |> st_cast("MULTIPOLYGON")
sf_use_s2(TRUE)
stopifnot(!any(st_is_empty(monde)), all(c("LUX", "SGP", "MLT", "FRA") %in% monde$iso3))

# --- 8.6 Graphiques de méthode et de modélisation (onglet Analyse)
# Étape 1 : électricité 2024 par zone (déduite des hausses AIE)
zones_2024 <- zones |> select(zone, twh_zone)
# Contrôle : part des DC dans l'électricité nationale, notre modèle vs valeurs publiées par l'AIE
validation <- dc_pays |>
  inner_join(tibble(iso3 = c("USA", "CHN", "JPN", "IRL", "IND"), ref_pct = c(4, 1.1, 2, 20, 0.5),
                    statut = c("total de zone (AIE)", "total de zone (AIE)", "fixé sur l'AIE", "fixé sur l'AIE", "fixé sur l'AIE")),
             by = "iso3") |>
  transmute(iso3, country, modele_pct = 100 * e0 / elec, ref_pct, statut)
# France : notre modèle face aux sources françaises (TWh)
refs_fr <- tribble(~periode, ~source, ~twh, ~mini, ~maxi,
  "2024", "Arcep 2024 (23 opérateurs, une partie du parc)", 2.7, NA, NA,
  "2024", "ADEME-Arcep 2020 (tout le parc)", 12, NA, NA,
  "2030", "ADEME-Arcep (projection)", 16, NA, NA,
  "2030", "RTE, scénario central", 17.5, 15, 20)
# Prévision des prélèvements d'eau de la France : prévisions de chaque modèle sur 2020-2022 (période test)
fr_obs <- fr |> filter(!is.na(prelev_vol))
train <- ts(head(fr_obs$prelev_vol, -3), start = min(fr_obs$year))
h_test <- 3
backtest <- bind_rows(
  tibble(modele = "Naïf", pred = as.numeric(naive(train, h = h_test)$mean)),
  tibble(modele = "Dérive", pred = as.numeric(rwf(train, h = h_test, drift = TRUE)$mean)),
  tibble(modele = "ETS", pred = as.numeric(forecast(ets(train), h = h_test)$mean)),
  tibble(modele = "ARIMA", pred = as.numeric(forecast(auto.arima(train, seasonal = FALSE, max.p = 2, max.q = 2), h = h_test)$mean))) |>
  group_by(modele) |> mutate(annee = tail(fr_obs$year, 3)) |> ungroup()

# --- 9. Sauvegarde -------------------------------------------------------------
dir.create("app", showWarnings = FALSE)
saveRDS(list(
  pays = pays, scen = scen, scen_zone = scen_zone, aie = aie, wue = wue,
  part_ia = c(monde = part_ia_2024, fr = part_ia_fr, hausse = part_ia_hausse),
  conso_hab = conso_hab, classes = classes, acp_var = acp_var, acp_eig = acp_eig,
  hist = hist, hist_pays = hist_pays, fr_arcep = fr_arcep, prev_fr = prev_fr,
  osm = osm |> select(lat, lon, iso3, pays, nom, region, stress_region, stress_2030, stress_2050),
  regions = regions, echelles = echelles, climat = climat, google = google, eu = eu,
  monde = monde, lab_aq = lab_aq, zones_2024 = zones_2024, validation = validation,
  refs_fr = refs_fr, backtest = backtest, futur_sites = futur_sites, secteurs_ts = secteurs_ts,
  cah_arbre = cah$call$t$tree, acp_cor = acp$var$cor[, 1:3],
  # Ajouts relecture du 24/11/2026
  twh_monde = TWH_MONDE, gwh_site_zone = gwh_site_zone,
  acp_contrib = acp_contrib, acp_cmp = acp_cmp, acp_asym = acp_asym, acp_exclus = acp_exclus, acp_kaiser = acp_kaiser,
  cah_info = cah_info, cah_km = cah_km, cah_accord = cah_accord, cah_desc = cah_desc,
  acm_eig = acm_eig, acm_mod = acm_mod, acm_ind = acm_ind, acm_eta2 = acm_eta2, acm_effectifs = acm_effectifs,
  mcn_regions = mcn_regions, acm_exclus = acm_exclus
), "app/app_data.rds")
cat("Sites OSM :", nrow(osm), "| % en région stress élevé/extrême :",
    round(100 * mean(osm$bws_cat >= 3, na.rm = TRUE), 1), "\n")
print(echelles |> select(echelle, classe, part) |> mutate(part = round(part, 1)))
print(google |> select(site, eau_m3, region, stress_region) |> arrange(-eau_m3) |> head(8))
cat("Taille rds (Ko) :", round(file.size("app/app_data.rds") / 1024), "\n")

# Contrôles : doivent retrouver les chiffres du rapport
cat("Sites :", sum(pays$n_dc), "| pays :", nrow(pays), "\n")
cat("Eau monde 2024 (Mm3) :", round(415 * wue), "\n")
cat("Luxembourg part centrale :", round(present$part_central[present$iso3 == "LUX"], 2), "%\n")
cat("France eau 2024 (Mm3) :", round(pays$e0[pays$iso3 == "FRA"] * 0.81, 1), "\n")
cat("Classes :", paste(table(classes$nom_classe), names(table(classes$nom_classe)), collapse = " ; "), "\n")
cat("Modèle France :", prev_fr$choix, "\n")
