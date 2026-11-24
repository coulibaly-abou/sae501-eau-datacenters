# =============================================================================
# Contrôles des chiffres clés de l'application (relecture du 24/11/2026)
# Lancer depuis ce dossier, après prep.R :  Rscript checks.R
# Chaque ligne affiche la valeur recalculée ; « OK » / « ÉCHEC » quand un contrôle est automatique.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
d <- readRDS("app/app_data.rds")
ok <- function(test, lib) cat(sprintf("%-6s %s\n", if (isTRUE(test)) "OK" else "ÉCHEC", lib))
f <- function(x, k = 1) formatC(x, format = "f", digits = k, big.mark = " ", decimal.mark = ",")

cat("\n== 1. Eau mondiale : TWh x coefficient ==\n")
for (n in names(d$wue)) cat(sprintf("  %s TWh x %s L/kWh = %s Mm3\n", f(d$twh_monde, 0), f(d$wue[[n]], 2), f(d$twh_monde * d$wue[[n]], 0)))
ok(abs(d$twh_monde - 415) < 1e-6, "électricité répartie = 415 TWh")

cat("\n== 2. France ==\n")
fr <- d$pays |> filter(iso3 == "FRA")
cat(sprintf("  %s TWh (%d sites, %s GWh/site, calée sur l'AIE : %s) x 0,81 = %s Mm3 ; part de l'eau prélevée = %s %%\n",
            f(fr$e0, 2), fr$n_dc, f(fr$gwh_site, 1), fr$cale_aie, f(fr$e0 * 0.81, 2), f(100 * fr$e0 * 0.81 / (fr$prelev0 * 1e3), 4)))
eur <- d$pays |> filter(zone == "EUR", !cale_aie)
ok(diff(range(eur$gwh_site)) < 1e-9, sprintf("même GWh par site pour tous les pays européens non calés (%s)", f(eur$gwh_site[1], 1)))

cat("\n== 3. McNemar (sites, puis une ligne par région) ==\n")
o <- d$osm |> left_join(d$pays |> select(iso3, stress_pays_aq), by = "iso3") |> filter(!is.na(stress_region), !is.na(stress_pays_aq))
s <- function(x) factor(as.character(x) %in% d$lab_aq[4:5], c(FALSE, TRUE))
t <- table(pays = s(o$stress_pays_aq), region = s(o$stress_region)); print(t)
m <- mcnemar.test(t)
cat(sprintf("  n = %d ; pays %s %% ; région %s %% ; khi2 = %s ; p = %.2g\n", nrow(o), f(100 * mean(s(o$stress_pays_aq) == "TRUE"), 1),
            f(100 * mean(s(o$stress_region) == "TRUE"), 1), f(m$statistic, 1), m$p.value))
ok(t["FALSE", "TRUE"] == 904 && t["TRUE", "FALSE"] == 97, "b = 97, c = 904")
r <- d$mcn_regions
cat(sprintf("  Par région : n = %d ; pays %s %% ; région %s %% ; p = %.3f\n", r$n, f(r$p_pays, 1), f(r$p_reg, 1), r$p))

cat("\n== 4. ACP, classification, ACM ==\n")
cat(sprintf("  Axes retenus (Kaiser) : %d ; cumul : %s %%\n", d$acp_kaiser, f(d$acp_eig$cumul[d$acp_kaiser], 1)))
tab <- table(d$classes$nom_classe); print(tab)
ok(sum(tab) == nrow(d$classes) && nrow(d$classes) == 61, sprintf("somme des familles (%d) = nombre de pays de l'ACP (%d)", sum(tab), nrow(d$classes)))
ok(sum(tab) + nrow(d$acp_exclus) == nrow(d$pays), sprintf("pays de l'ACP + exclus (%d) = pays de l'étude (%d)", nrow(d$acp_exclus), nrow(d$pays)))
cat(sprintf("  Inertie inter / totale : %s %% ; accord CAH / k-means : %s %%\n", f(d$cah_info$inter, 1), f(d$cah_accord, 1)))
ok(all(d$acm_effectifs$pct[d$acm_effectifs$variable != "Famille"] >= 5), "ACM : chaque modalité active regroupe au moins 5 % des pays")

cat("\n== 5. Facteurs 2035 (AIE) ==\n")
f35 <- d$scen |> filter(horizon == 2035)
cat(sprintf("  Monde : x%s à x%s\n", f(min(f35$f_monde), 2), f(max(f35$f_monde), 2)))
ok(round(min(f35$f_monde), 1) == 1.7 && round(max(f35$f_monde), 1) == 4.1, "x1,7 et x4,1")
print(d$scen_zone |> filter(horizon == 2035) |> select(zone, scenario, f_zone) |> pivot_wider(names_from = scenario, values_from = f_zone))

cat("\n== 6. Même facteur 2035 dans Usages et Demain (France, croissance centrale) ==\n")
txt <- NULL; suppressMessages(shiny::testServer(shiny::shinyAppDir("app"), {
  session$setInputs(horizon = 2024, scen = "central", coef = 0.81, seuil = "3", pays_sel = "FRA", pays_us = "FRA", nav = "Usages")
  txt <<- list(usages = output$tendance_retenir, demain = output$frscen_retenir)
}))
fa <- regmatches(txt$usages, regexpr("×[0-9,]+", txt$usages)); fb <- regmatches(txt$demain, regexpr("centrale : ×[0-9,]+", txt$demain))
cat("  Usages :", txt$usages, "\n  Demain :", txt$demain, "\n")
ok(identical(fa, sub("centrale : ", "", fb)), sprintf("facteur de volume identique (%s)", fa))

cat("\n== 7. Variante « capacité des sites IA » (Epoch AI) ==\n")
cat(sprintf("  %d sites IA, %s GW de puissance IT, %d rattachés à une région\n", nrow(d$ia), f(sum(d$ia$mw_it) / 1000, 1), sum(!is.na(d$ia$region))))
tot <- NULL; suppressMessages(shiny::testServer(shiny::shinyAppDir("app"), {
  session$setInputs(horizon = 2024, scen = "central", coef = 0.81, seuil = "3", repartition = "capacite", fc = 0.7)
  tot <<- c(capacite = sum(base_pays()$e0), couverture = output$couverture) }))
cat(" ", tot[["couverture"]], "\n")
ok(abs(as.numeric(tot[["capacite"]]) - 415) < 1e-6, "la variante capacité conserve 415 TWh")
