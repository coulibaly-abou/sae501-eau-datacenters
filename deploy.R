# Mise en ligne sur shinyapps.io
# 1) Une seule fois : coller ici la commande donnée par shinyapps.io
#    (Account > Tokens > Show > Copy to clipboard), du type :
# rsconnect::setAccountInfo(name = "XXX", token = "XXX", secret = "XXX")
#
# 2) Puis, depuis ce dossier :  Rscript deploy.R
rsconnect::deployApp(appDir = "app", appName = "eau-datacenters-sae501",
                     appTitle = "Data centers, IA et eau - SAÉ 5.EMS.01",
                     forceUpdate = TRUE, launch.browser = FALSE)
