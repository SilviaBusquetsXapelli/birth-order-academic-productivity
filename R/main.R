# ============================================================
# BROTHERS RESEARCH - MAIN.R
# Proyecto: datos > modelos > tablas > revisiones > graficos
# ============================================================

if (!file.exists("Data/Persons.xlsx")) {
  stop("No encuentro Data/Persons.xlsx. Abre el .Rproj y copia los Excel a Data/.")
}
dir.create("Output", showWarnings = FALSE, recursive = TRUE)
dir.create("Write/figures", showWarnings = FALSE, recursive = TRUE)

# En main.R se limpia el entorno una sola vez, para reproducibilidad.
rm(list = ls())

# 01 | Bibliotecas, configuración y carga de datos
source("scripts/01_load_data.R", encoding = "UTF-8", local = FALSE)

# 02 | Construcción de pares desde familias y Brothers without fathers; desduplicación
source("scripts/02_build_sibling_pairs.R", encoding = "UTF-8", local = FALSE)

# 03 | Biografías, productividad, afiliaciones y fechas
source("scripts/03_enrich_scholars.R", encoding = "UTF-8", local = FALSE)

# 04 | Orden relativo, características, t-tests y márgenes extensivo/intensivo
source("scripts/04_birth_order_and_margins.R", encoding = "UTF-8", local = FALSE)

# 05 | Rankability, clasificación Todd, NUTS3 y descriptivos históricos
source("scripts/05_rankability_inheritance.R", encoding = "UTF-8", local = FALSE)

# 06 | Mapa de lugares de nacimiento; herencia por país
source("scripts/06_geographical_maps.R", encoding = "UTF-8", local = FALSE)

# 07 | Variables de resultado, modelos principales 1-6
source("scripts/07_main_regressions.R", encoding = "UTF-8", local = FALSE)

# 08 | Alternativas LOG/LEVELS, muestras restringidas y otros indicadores
source("scripts/08_robustness_and_alternatives.R", encoding = "UTF-8", local = FALSE)

# 09 | Efectos fijos familiares, individuos únicos, muestra 3+
source("scripts/09_family_fixed_effects.R", encoding = "UTF-8", local = FALSE)

# 10 | Descriptivos generales, gráficos y regresiones históricas/interacciones
source("scripts/10_descriptives_and_period_models.R", encoding = "UTF-8", local = FALSE)

# 11 | Único Excel: Results_Brothers_Clear.xlsx
source("scripts/11_export_clear_excel.R", encoding = "UTF-8", local = FALSE)

# 12 | Integridad de IDs, coherencia individual, tamaños de muestras y missing
source("scripts/12_validation_checks.R", encoding = "UTF-8", local = FALSE)

# 13 | Figura herencia por siglo y exportación de gráficos
source("scripts/13_save_figures.R", encoding = "UTF-8", local = FALSE)

cat("\nProyecto terminado. Excel: Output/Results_Brothers_Clear.xlsx\n")
