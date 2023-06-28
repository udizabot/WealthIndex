
###########################################################################s
# Asset-Base Wealth Index -------------------------------------------------
# 2008 --------------------------------------------------------------------
###########################################################################

## Already considering the Updated Data of March 2021
## Version 3.0

## In this script, we explore further the Kolenikov and Angeles (2009). 
## We also create an ordinal PCA index, an Filmer-Pritchett PCA index and a Simple Index
## Besides, we consider the adjustment based on Banerjee (2010)

# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, SciViews, psych, matlib, fastDummies)

# Working Directory -------------------------------------------------------
setwd("C:/EmpiricalResearch/DataSourcePOF/DataSourcePOF/POF2008/Dados") 


# Import DataSets ---------------------------------------------------------

Morador <- readRDS("MORADOR.P08.rds") 

Domicilio <- readRDS("DOMICILIO.P08.rds") 

Inventario <- readRDS("INVENTARIO.P08.rds") 





# Previous Restrictions ---------------------------------------------------

## Household Member Cut-Off
cut.off <- 5
