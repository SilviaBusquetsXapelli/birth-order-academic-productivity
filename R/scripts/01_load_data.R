# 01_load_data.R
# Bibliotecas, configuración y carga de datos
# Ejecutado desde main.R

# === 0. Clean Environment ===
  

# === 1. Load required packages ===
  
  library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(openxlsx)
library(broom)
library(giscoR)
library(fixest)
library(modelsummary)
library(kableExtra)
library(tidyr)
  
# setwd("C:/Users/32468/OneDrive - UCL/Thesis/Coding")

# === 2. Load input files ===
  
  persons       <- read_excel("Data/Persons.xlsx")
brothers <- read_excel("Data/Brothers without fathers.xlsx") %>%
  select(1:3)  # Solo las tres primeras columnas correctas
families_same <- read_excel("Data/Families.xlsx")
families_diff <- read_excel("Data/Families different institutions.xlsx")
dictionary    <- read_excel("Data/Dictionary-full.xlsx")
activities    <- read_excel("Data/Activities.xlsx")
locations     <- read_excel("Data/Locations.xlsx") %>%
  select(LKey, LName, Country2016, Latitude, Longitude)
