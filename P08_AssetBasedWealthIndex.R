
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

Domicilio <- readRDS("DOMICILIO.P08.rds") %>%
    transform( . ,
               NUM_SEQ = str_pad(NUM_SEQ, 3, pad = "0"),
               NUM_DV = str_pad(NUM_DV, 2, pad = "0"),
               NUM_DOM = str_pad(NUM_DOM, 2, pad = "0")) %>% as_tibble()

Inventario <- readRDS("INVENTARIO.P08.rds") %>%
    transform( . ,
               NUM_SEQ = str_pad(NUM_SEQ, 3, pad = "0"),
               NUM_DV = str_pad(NUM_DV, 2, pad = "0"),
               NUM_DOM = str_pad(NUM_DOM, 2, pad = "0"),
               NUM_UC = str_pad(NUM_UC, 2, pad = "0"),
               COD_ITEM = str_pad(COD_ITEM, 5, pad = "0")) %>% as_tibble()





# Previous Restrictions ---------------------------------------------------

## Household Member Cut-Off
cut.off <- 5







###########################################################################
# Housing Characteristics -------------------------------------------------

Housing <- Domicilio %>%
    transform( . , 
               house.control = paste0(UF, NUM_SEQ, NUM_DV, NUM_DOM)) %>%
    transform( . , 
               wall = V0202, 
               roof = V0203,
               floor = V0204,
               rooms = V0205,
               bedrooms = V0206,
               bathrooms = V02111, 
               water = V0207,
               flush = V0212, 
               trash = V0213,
               house = V0217,
               power = V02141, 
               pave = V0220) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0))) %>%
    transform( . ,
               wall = case_when(wall == 1 ~ 1, wall == 2 ~ 2, wall == 3 ~ 3, wall == 4 ~ 4, wall == 5 | wall == 6 ~ 5),
               roof = case_when(roof == 1 ~ 1, roof == 2 ~ 2, roof == 3 ~ 3, roof == 4 ~ 4, roof == 5 | roof == 6 | roof == 7 ~ 5),
               floor = case_when(floor == 2 ~ 1, floor == 3 ~ 2, floor == 4 ~ 3, floor == 6 ~ 4, floor == 1 | floor == 5 | floor == 7 ~ 5), 
               flush = case_when(flush == 1 ~ 1, flush == 2 | flush == 3 | flush == 4 ~ 2, flush == 5 ~ 3, flush == 6 | flush == 7 ~ 4),
               trash = case_when(trash == 1 | trash == 2 ~ 1, trash == 3 ~ 2, trash == 4 | trash == 5 ~ 3, trash == 6 ~ 4),
               water = case_when(water == 1 ~ 1, water == 2 ~ 2, water == 3 ~ 3),
               power = case_when(power == 1 ~ 1, power == 2 ~ 2), 
               pave = case_when(pave == 1 ~ 1, pave == 2 ~ 2), 
               house = case_when(house == 1 ~ 1, house == 2 ~ 2, house == 6 ~ 3, house == 3 | house == 4 ~ 4, house == 5 ~ 5)) %>%
    select( . , 
            UF, PESO_FINAL, RENDA_TOTAL, house.control, rooms, bedrooms, bathrooms, floor, water, power, flush, pave, house, wall, roof, trash)






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
               refrigerator = case_when(code == 14003 | code == 14004 ~ V9005), 
               dishwasher = case_when(code == 14032 ~ V9005), 
               washing = case_when(code == 14012 ~ V9005), 
               microwave = case_when(code == 14026 ~ V9005),
               purifier = case_when(code == 14025 ~ V9005),
               air.condic = case_when(code == 14017 ~ V9005), 
               television = case_when(code == 14013 ~ V9005),
               computer = case_when(code == 14024 ~ V9005),
               car = case_when(code == 14021 ~ V9005), 
               moto = case_when(code == 14023 ~ V9005), 
               bike = case_when(code == 14022 ~ V9005), 
               eletric.oven = case_when(code == 14010 ~ V9005)) %>%
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
            elec.oven = max(eletric.oven),
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
            microwave, elec.oven, purifier, air.condic, television, 
            computer, car, moto, bike)










