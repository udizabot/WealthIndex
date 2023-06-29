
###########################################################################s
# Asset-Base Wealth Index -------------------------------------------------
# 2002 --------------------------------------------------------------------
###########################################################################

## Already considering the Updated Data of March 2021
## Version 3.0

## In this script, we explore further the Kolenikov and Angeles (2009). 
## We also create an ordinal PCA index, an Filmer-Pritchett PCA index and a Simple Index
## Besides, we consider the adjustment based on Banerjee (2010)

# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, SciViews, psych, matlib, fastDummies)

# Working Directory -------------------------------------------------------
setwd("C:/EmpiricalResearch/DataSourcePOF/DataSourcePOF/POF2002/Dados") 



# Import DataSets ---------------------------------------------------------

Morador <- readRDS("MORADOR.P02.rds") 

Domicilio <- readRDS("DOMICILIO.P02.rds") %>%
    transform( . ,
               PESO_DESENHO = str_remove(PESO_DESENHO, "^0+"), 
               PESO_FINAL = str_remove(PESO_FINAL, "^0+"), 
               RENDA_TOTAL = str_remove(RENDA_TOTAL, "^0+") %>% parse_number()) %>% as_tibble()

Inventario <- readRDS("INVENTARIO.P02.rds") %>%
    transform( . ,
               NUM_SEQ = str_pad(NUM_SEQ, 3, pad = "0"),
               NUM_DV = str_pad(NUM_DV, 2, pad = "0"),
               NUM_DOM = str_pad(NUM_DOM, 2, pad = "0"),
               NUM_UC = str_pad(NUM_UC, 2, pad = "0"),
               PESO_DESENHO = str_remove(PESO_DESENHO, "^0+"), 
               PESO_FINAL = str_remove(PESO_FINAL, "^0+"), 
               COD_ITEM = str_pad(COD_ITEM, 5, pad = "0"),
               RENDA_TOTAL = str_remove(RENDA_TOTAL, "^0+") %>% parse_number()) %>% as_tibble()



# Previous Restrictions ---------------------------------------------------

## Household Member Cut-Off
cut.off <- 5



###########################################################################
# Inventory of Durable Goods ----------------------------------------------

## In this case, we consider a variable of indicator for ownership of durable good.
## Also, we create a variable for the number of durable good owned by household

Inventory <- Inventario %>%
    transform( . ,
               house.control = paste0(UF, NUM_SEQ, NUM_DV, NUM_DOM),
               control = paste0(UF, NUM_SEQ, NUM_DV, NUM_DOM, NUM_UC), 
               code = paste0(QUADRO, COD_ITEM) %>% parse_number()) %>%
    transform( . , 
               code = round(code/100)) %>%
    transform( . ,
               stove = case_when(code == 14001 ~ V9005), 
               freezer = case_when(code == 14002 ~ V9005), 
               refrigerator = case_when(code == 14003 ~ V9005), 
               dishwasher = case_when(code == 14004 ~ V9005), 
               washing = case_when(code == 14011 ~ V9005), 
               microwave = case_when(code == 14030 ~ V9005),
               purifier = case_when(code == 14029 ~ V9005),
               air.condic = case_when(code == 14020 ~ V9005), 
               television = case_when(code == 14013 ~ V9005),
               computer = case_when(code == 14027 ~ V9005),
               car = case_when(code == 14024 ~ V9005), 
               moto = case_when(code == 14026 ~ V9005), 
               bike = case_when(code == 14025 ~ V9005)) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0))) %>%
    group_by(control) %>%
    mutate( . , 
            stove = max(stove), 
            freezer = max(freezer),
            refrigerator = max(refrigerator),
            dishwasher = max(dishwasher),
            washing = max(washing),
            microwave = max(microwave), 
            purifier = max(purifier), 
            air.condic = max(air.condic), 
            television = max(television), 
            computer = max(computer), 
            car = max(car), 
            moto = max(moto),
            bike = max(bike)) %>%
    distinct( . , 
              control, .keep_all = T) %>% 
    select( . , 
            UF, PESO_FINAL, RENDA_TOTAL, house.control, control, 
            stove, freezer, refrigerator, dishwasher, washing, 
            microwave, purifier, air.condic, television, 
            computer, car, moto, bike)



###########################################################################
# Housing Characteristics -------------------------------------------------

Housing <- Domicilio %>%
    transform( . , 
               house.control = paste0(UF, NUM_SEQ, NUM_DV, NUM_DOM)) %>%
    transform( . , 
               rooms = V0205, 
               bedrooms = V0206,
               bathrooms = V02111, 
               c.water = V0207,
               c.flush = V0212, 
               c.property = V0217,
               power.network = V0214, 
               c.floor = V0204,
               street.pave = V0220) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0))) %>%
    transform( . , 
               floor = case_when(c.floor == 2 ~ 1, c.floor == 3 ~ 2, c.floor == 4 ~ 3, c.floor == 6 ~ 4, c.floor == 1 | c.floor == 5 | c.floor == 7 ~ 5), 
               flush = case_when(c.flush == 1 ~ 1, c.flush == 2 | c.flush == 3 | c.flush == 4 ~ 2, c.flush == 5 ~ 3, c.flush == 6 | c.flush == 7 ~ 4),
               water = case_when(c.water == 1 | c.water == 4 ~ 1, c.water == 2 | c.water == 5 ~ 2, c.water == 3 | c.water == 6 ~ 1),
               power = case_when(power.network == 1 ~ 1, power.network == 2 | power.network == 3 | power.network == 4 ~ 2), 
               pave = case_when(street.pave == 1 ~ 1, street.pave == 2 ~ 2), 
               house = case_when(c.property == 1 ~ 1, c.property == 2 ~ 2, c.property == 3 ~ 3, c.property == 4 | c.property == 5 ~ 4, c.property == 6 ~ 5)) %>%
    select( . , 
            UF, PESO_FINAL, RENDA_TOTAL, rooms, bedrooms, bathrooms, floor, water, power, flush, pave, house)
    





            








