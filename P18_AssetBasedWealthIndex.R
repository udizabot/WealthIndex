
###########################################################################
# Household Wealth Index --------------------------------------------------
###########################################################################

## Considering the Updated Data of March 2021
## Version 3.0

## In this script, we explore further the Kolenikov and Angeles (2009). 
## We also create an ordinal PCA index, an Filmer-Pritchett PCA index and a Simple Index
## Besides, we consider the adjustment based on Banerjee (2010)


# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, SciViews, psych, matlib, fastDummies)



# Working Directory -------------------------------------------------------
setwd("C:/Users/udiza/OneDrive/Área de Trabalho/Thesis Empirical Analysis/DataBase POF2018 [updated 03.21]/Dados") 


# Import DataSets ---------------------------------------------------------
Morador <- readRDS("MORADOR.rds") %>%
    transform( . ,
               NUM_DOM = str_pad(NUM_DOM, width = 2, side = "left", pad = "0"), 
               COD_INFORMANTE = str_pad(COD_INFORMANTE, width = 2, side = "left", pad = "0"))  


Inventario <- readRDS("INVENTARIO.rds") %>%
    transform( . ,
               NUM_DOM = str_pad(NUM_DOM, width = 2, side = "left", pad = "0")) 


Domicilio <- readRDS("DOMICILIO.rds") %>%
    transform( . ,
               NUM_DOM = str_pad(NUM_DOM, width = 2, side = "left", pad = "0")) 




###########################################################################
# Household Socioeconomic Status ------------------------------------------




# Housing Characteristics -------------------------------------------------

## In this section, we create dichotomy variables for housing characteristics

## Note that the suffix c stand for Categorical
## Note that the suffix n stand for Count (number)
## Note that the suffix d stand for Dummy 

## We create order categorical variables 

Housing <- transform(Domicilio, 
                     house.control = paste0(COD_UPA, NUM_DOM),
                     c.wall = V0202,
                     c.roof = V0203, 
                     c.floor = V0204, 
                     n.rooms = V0205,
                     n.bedrooms = V0206,
                     c.water = V0207,
                     n.bathrooms = V02111, 
                     c.flush = V0212,
                     c.trash = V0213, 
                     power.network = V02141, 
                     c.property = V0217, 
                     street.pave = V0220) %>%
    replace_na(list(c.flush = 0)) %>%
    transform( . , 
               c.wall = ifelse(c.wall == 1, 4, 
                               ifelse(c.wall == 2 | c.wall == 3, 2, 
                                      ifelse(c.wall == 4, 3, 1))),
               c.roof = ifelse(c.roof == 1 | c.roof == 2 | c.roof == 3, 4, 
                               ifelse(c.roof == 4, 3, 
                                      ifelse(c.roof == 5, 2, 1))),
               c.floor = ifelse(c.floor == 1, 4,
                                ifelse(c.floor == 2, 3, 
                                       ifelse(c.floor == 3, 2, 1))),
               c.water = ifelse(c.water == 1, 4, 
                                ifelse(c.water == 2, 3, 
                                       ifelse(c.water == 3 | c.water == 4, 2, 1))),
               c.flush = ifelse(c.flush == 1, 4, 
                                ifelse(c.flush == 2 | c.flush == 3, 3,
                                       ifelse(c.flush == 4, 2, 1))), 
               c.trash = ifelse(c.trash == 1, 4, 
                                ifelse(c.trash == 2, 3, 
                                       ifelse(c.trash == 3 | c.trash == 4, 2, 1))),
               c.property = ifelse(c.property == 1 | c.property == 2, 4, 
                                   ifelse(c.property == 3, 3, 
                                          ifelse(c.property == 4, 2, 1))),
               d.power = ifelse(power.network == 1, 1, 0), 
               d.pave = ifelse(street.pave == 1, 1, 0)
    ) %>%
    select( . , 
            UF, PESO_FINAL, house.control, c.wall, c.roof, c.floor, n.rooms, 
            c.water, n.bathrooms, c.flush, c.trash, c.property, d.power, d.pave ) %>%
    as_tibble()



# Durable Goods Ownership -------------------------------------------------

## In this case, we consider a variable of indicator for ownership of durable good.
## Also, we create a variable for the number of durable good owned by household

Inventory <- transform(Inventario, 
                       code = round(V9001/100), 
                       control = paste0(COD_UPA, NUM_DOM, NUM_UC), 
                       house.control = paste0(COD_UPA, NUM_DOM)) %>%
    transform( . , 
               stove = ifelse(code == 14001, 1, 0), 
               freezer = ifelse(code == 14002, 1, 0), 
               refrigerator = ifelse(code == 14003 | code == 14004, 1, 0), 
               refrigerator11 = ifelse(code == 14003, 1, 0),
               refrigerator12 = ifelse(code == 14004, 1, 0),
               purifier = ifelse(code == 14007, 1, 0),
               dishwasher = ifelse(code == 14008, 1, 0), 
               microwave = ifelse(code == 14009, 1, 0),
               eletric.oven = ifelse(code == 14010, 1, 0),
               washing = ifelse(code == 14012, 1, 0), 
               television = ifelse(code == 14014, 1, 0), 
               computer = ifelse(code == 14019, 1, 0), 
               tablet = ifelse(code == 14020, 1, 0),
               air = ifelse(code == 14021, 1, 0), 
               car = ifelse(code == 14030, 1, 0), 
               moto = ifelse(code ==14031, 1, 0), 
               bike = ifelse(code == 14032, 1, 0)) %>%
    group_by(control) %>%
    mutate( . , 
            d.stove = max(stove),
            d.freezer = max(freezer), 
            d.refrigerator = ifelse(max(refrigerator) >= 1, 1, 0),
            d.refrigerator1 = max(refrigerator11),
            d.refrigerator2 = max(refrigerator12),
            d.purifier = max(purifier), 
            d.dishwasher = max(dishwasher), 
            d.microwave = max(microwave),
            d.eletric.oven = max(eletric.oven),
            d.washing = max(washing), 
            d.television = max(television), 
            d.computer = max(computer),
            d.tablet = max(tablet),
            d.air = max(air), 
            d.car = max(car), 
            d.moto = max(moto), 
            d.bike = max(bike)) %>%
    distinct( . , 
              control, .keep_all = T) %>%
    select( . , 
            UF, PESO_FINAL, RENDA_TOTAL, house.control, control, 
            d.stove, d.freezer, d.refrigerator, d.refrigerator1, d.refrigerator2, d.purifier, 
            d.dishwasher, d.microwave, d.eletric.oven, d.washing, d.television, 
            d.computer, d.tablet, d.air, d.car, d.moto, d.bike) %>%
    as_tibble()





# Household Head Characteristics ------------------------------------------

## In this case, we consider a variable for instruction level of Household Head

Demographics <- filter(Morador,
                       !(V0306 >= 15)) %>%
    transform( . , 
               ESTRATO = ESTRATO_POF,
               member = V0306,
               age = V0403,
               schooling = ANOS_ESTUDO,
               instruction = INSTRUCAO, 
               control = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
    transform( . ,
               degree = ifelse(instruction >= 1 & instruction <= 2, 1,
                               ifelse(instruction >= 3 & instruction <= 4, 2,
                                      ifelse(instruction >= 5 & instruction <= 6, 3, 4 ))),
               control = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
    group_by( . , 
              control) %>%
    filter( . , 
            member == 1) %>%
    select( . , 
            UF, PESO_FINAL, control, member, age, degree) %>%
    as_tibble()






###########################################################################
# Asset Complete Dataset --------------------------------------------------

## Here, we create a data set with all the variables created above

Asset <- full_join(Housing, Inventory, 
                   by = c("UF", "PESO_FINAL", "house.control")) %>%
    full_join( . , 
               Demographics, 
               by = c("UF", "PESO_FINAL", "control")) %>%
    filter( . , 
            !(is.na(RENDA_TOTAL))) %>%
    select( . , 
            control, PESO_FINAL,
            c.wall, c.roof, c.floor, c.water, c.flush, c.trash, 
            n.rooms, n.bathrooms, 
            d.pave, d.power, c.property,
            d.stove, d.freezer, d.refrigerator, d.purifier,  
            d.refrigerator1, d.refrigerator2, d.dishwasher, 
            d.microwave, d.eletric.oven, d.washing, d.television, 
            d.computer, d.tablet, d.air, d.car, d.moto, d.bike, 
            degree) 


## Basic Descriptive Statistics
round(sapply(Asset[,3:30], mean), 4)
round(sapply(Asset[,3:30], sd), 4)


## In what follows, we do not consider the Degree of Education in this exercise




###########################################################################
# Household Socioeconomic Status ------------------------------------------

## We apply the KMO test and evaluate the Squared Multiple Correlations (SMC) 

Asset1 <- select(Asset, 
                 starts_with(c("c.", "n.", "d.")), degree)

## First, we exclude assets owned by more than 95% or less the 5% of the households
## Then, we exclude variables for which SMC is less than 5%
## Moreover, we choose the Distribution Matrix with the highest KMO 

round(smc(Asset1, covar = F), 3)

KMO(Asset1)

## We consider all discrete variables. 
## We exclude "d.stove" and "d.power" because they are present in more than 95% households

Asset1 <- select(Asset, 
                 c.wall, 
                 #c.roof, c.property, 
                 c.floor, c.water, c.flush, c.trash, 
                 d.pave, n.rooms, n.bathrooms,  
                 #d.power, d.stove, d.refrigerator, d.refrigerator1, 
                 d.refrigerator2, 
                 #d.dishwasher, 
                 d.washing, d.freezer, d.purifier, d.microwave, d.eletric.oven, d.television, 
                 #d.moto, d.bike, 
                 d.computer, d.tablet, d.air, d.car, degree)







###########################################################################
# Principal Components Analysis -------------------------------------------

## Having chosen the set of indicator variables, we proceed with the PCA

# Ordinal PCA -------------------------------------------------------------

model.or <- principal(Asset1, nfactors = 1, rotate = "none", scores = T, covar = F, short = T, residuals = T,
                      missing = T, impute = "median", cor = "cor", oblique.scores = T, use = "pairwise.complete.obs", correct = T,
                      method = "regression", cor.smooth(Asset1, eig.tol = 10^-12))

range(model.or$scores)



# Polychoric PCA ----------------------------------------------------------

model.ka <- principal(Asset1, nfactors = 1, rotate = "none", scores = T, covar = F, short = T, residuals = T,
                      missing = T, impute = "median", cor = "mixed", oblique.scores = F, use = "pairwise.complete.obs", correct = T,
                      method = "regression")

## Extracting all information needed
model.ka$loadings     ### A Standard Loading Matrix
model.ka$values       ### Useful for a Scree plot
model.ka$scores       ### Estimate of the Component Scores
model.ka$weights      ### Beta Weights to find the Component Scores from Data

range(model.ka$scores)



# An Understanding of the Process -----------------------------------------

## Based on Polychoric PCA, how to obtain the component scores

## Alternatively, we could estimate first the Polychoric Correlation Matrix
pcorr <- mixedCor(Asset1, smooth = T, ncat = 8, use = "pairwise.complete.obs", global = F)

model3 <- principal(pcorr$rho, nfactors = 1, rotate = "none", scores = T, covar = T, short = T, residuals = T,
                    missing = T, impute = "median", cor = "mixed", oblique.scores = F, use = "pairwise.complete.obs", correct = T,
                    method = "regression")

## Note: The component scores are found by regression where the regression weights (beta) are product of 
## the inverse of the correlation matrix and the component loadings. Therefore, we have

det(pcorr$rho)      ### Check if invertible

RI <- inv(pcorr$rho)    ### Obtain the Inverse of the Correlation Matrix
RI%*%model3$loadings    ### Obtain the component scores


# -------------------------------------------------------------------------










###########################################################################
# Filmer and Pritchett (2001) Approach -------------------------------------

AssetD <- select(Asset1, 
                 starts_with("c."), 
                 starts_with("d.")) %>%
    dummy_cols( . , 
                c("c.wall", "c.floor", "c.water", "c.flush", "c.trash")) %>%
    select( . , 
            starts_with("d."), 
            ends_with("_1"), ends_with("_2"), ends_with("_3"), ends_with("_4"))

model.fp <- principal(AssetD, nfactors = 1, rotate = "none", scores = T, covar = F, short = T, residuals = T,
                      missing = T, impute = "median", cor = "cor", oblique.scores = T, use = "pairwise.complete.obs", correct = T, 
                      method = "regression")

range(model.fp$scores)









###########################################################################
# Wealth Index Based on Durable Goods -------------------------------------

Simple1 <- transform(Inventario, 
                     code = round(V9001/100), 
                     control = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
    transform( . , 
               stove1 = ifelse(code == 14001, 1, 0), 
               freezer1 = ifelse(code == 14002, 1, 0), 
               refrigerator11 = ifelse(code == 14003, 1, 0),
               refrigerator12 = ifelse(code == 14004, 1, 0),
               purifier1 = ifelse(code == 14007, 1, 0),
               dishwasher1 = ifelse(code == 14008, 1, 0), 
               microwave1 = ifelse(code == 14009, 1, 0),
               eletric.oven1 = ifelse(code == 14010, 1, 0),
               washing1 = ifelse(code == 14012, 1, 0), 
               television1 = ifelse(code == 14014, 1, 0), 
               computer1 = ifelse(code == 14019, 1, 0), 
               tablet1 = ifelse(code == 14020, 1, 0),
               air1 = ifelse(code == 14021, 1, 0), 
               car1 = ifelse(code == 14030, 1, 0), 
               moto1 = ifelse(code ==14031, 1, 0), 
               bike1 = ifelse(code == 14032, 1, 0)) %>%
    group_by(control) %>%
    mutate( . , 
            stove2 = max(stove1),
            freezer2 = max(freezer1), 
            refrigerator21 = max(refrigerator11),
            refrigerator22 = max(refrigerator12),
            purifier2 = max(purifier1), 
            dishwasher2 = max(dishwasher1), 
            microwave2 = max(microwave1),
            eletric.oven2 = max(eletric.oven1),
            washing2 = max(washing1), 
            television2 = max(television1), 
            computer2 = max(computer1),
            tablet2 = max(tablet1),
            air2 = max(air1), 
            car2 = max(car1), 
            moto2 = max(moto1), 
            bike2 = max(bike1)) %>%
    distinct( . ,
              control, .keep_all = T) %>%
    transform( . ,
               stove = 1 - mean(stove2), 
               freezer = 1 - mean(freezer2), 
               refrigerator1 = 1 - mean(refrigerator21),
               refrigerator2 = 1 - mean(refrigerator22),
               purifier = 1 - mean(purifier2), 
               dishwasher = 1 - mean(dishwasher2), 
               microwave = 1 - mean(microwave2),
               eletric.oven = 1 - mean(eletric.oven2),
               washing = 1 - mean(washing2), 
               television = 1 - mean(television2), 
               computer = 1 - mean(computer2), 
               tablet = 1 - mean(tablet2),
               air = 1 - mean(air2), 
               car = 1 - mean(car2), 
               moto = 1 - mean(moto2), 
               bike = 1 - mean(bike2)) %>%
    transform( . , 
               summation = 
                   stove + freezer + refrigerator1 + refrigerator2 +
                   purifier + dishwasher + microwave + eletric.oven + 
                   washing + television + computer + tablet + air + 
                   car + moto + bike) %>%
    transform( . ,
               w.stove =  stove / summation, 
               w.freezer = freezer / summation,
               w.refrigerator1 = refrigerator1 / summation,
               w.refrigerator2 = refrigerator2 / summation,
               w.purifier = purifier / summation, 
               w.dishwasher = dishwasher / summation, 
               w.microwave = microwave / summation,
               w.eletric.oven = eletric.oven / summation, 
               w.washing = washing / summation, 
               w.television = television / summation, 
               w.computer = computer / summation, 
               w.tablet = tablet / summation,
               w.air = air / summation, 
               w.car = car / summation, 
               w.moto = moto / summation, 
               w.bike = bike / summation ) %>% 
    transform( . , 
               index.w3 =
                   stove2*w.stove +
                   freezer2*w.freezer +
                   refrigerator21*w.refrigerator1 +
                   refrigerator22*w.refrigerator2 +
                   purifier2*w.purifier +
                   dishwasher2*w.dishwasher +
                   microwave2*w.microwave +
                   eletric.oven2*w.eletric.oven +
                   washing2*w.washing +
                   television2*w.television +
                   computer2*w.computer +
                   tablet2*w.tablet +
                   air2*w.air +
                   car2*w.car +
                   moto2*w.moto + 
                   bike2*w.bike ) %>%
    select( . , 
            control, index.w3) %>%
    as_tibble()







###########################################################################
# Adjusted Wealth Index ---------------------------------------------------

## Following Banerjee (2010)
## However, we consider the weights estimated by a Polychoric PCA Model


# Adjustment of the Weights  ----------------------------------------------

weights2 <- model.ka$weights

adj.weights <- matrix(0, length(weights2), 1)

for (i in 1:length(weights2)) {
    adj.weights[i,1] <- weights2[i,1] / sum(weights2[,1])
}

adj.weights
sum(adj.weights)



# Adjustment of the Attributes on the Distribution Matrix -----------------

Asset.Adjusted <- transform(Asset, 
                            c.wall = c.wall / mean(c.wall), 
                            c.roof = c.roof / mean(c.roof), 
                            c.floor = c.floor / mean(c.floor), 
                            c.water = c.water / mean(c.water), 
                            c.flush = c.flush / mean(c.flush), 
                            c.trash = c.trash / mean(c.trash), 
                            n.rooms = n.rooms / mean(n.rooms), 
                            n.bathrooms = n.bathrooms / mean(n.bathrooms), 
                            d.pave = d.pave / mean(d.pave), 
                            d.power = d.power / mean(d.power), 
                            d.stove = d.stove / mean(d.stove), 
                            d.freezer = d.freezer / mean(d.freezer), 
                            d.refrigerator = d.refrigerator / mean(d.refrigerator), 
                            d.purifier = d.purifier / mean(d.purifier), 
                            d.refrigerator1 = d.refrigerator1 / mean(d.refrigerator1),
                            d.refrigerator2 = d.refrigerator2 / mean(d.refrigerator2), 
                            d.dishwasher = d.dishwasher / mean(d.dishwasher), 
                            d.microwave = d.microwave / mean(d.microwave), 
                            d.eletric.oven = d.eletric.oven / mean(d.eletric.oven), 
                            d.washing = d.washing / mean(d.washing), 
                            d.television = d.television / mean(d.television), 
                            d.computer = d.computer / mean(d.computer), 
                            d.tablet = d.tablet / mean(d.tablet), 
                            d.air = d.air / mean(d.air), 
                            d.car = d.car / mean(d.car), 
                            d.moto = d.moto / mean(d.moto), 
                            d.bike = d.bike / mean(d.bike), 
                            degree = degree / mean(degree) ) %>%
    select( . , 
            c.wall, 
            #c.roof, 
            c.floor, c.water, c.flush, c.trash, 
            d.pave, n.rooms, n.bathrooms, d.refrigerator2,  
            #d.power, d.stove, d.refrigerator, d.refrigerator1, d.dishwasher, 
            d.washing, d.freezer, d.purifier, d.microwave, d.eletric.oven, d.television, 
            #d.moto, d.bike, 
            d.computer, d.tablet, d.air, d.car, degree) 


# Estimating the Adjusted Scores  -----------------------------------------

DMS <- Asset.Adjusted
adj.scores <- matrix(0, nrow(Asset1), 1)

for (i in 1:nrow(Asset1)) {
    adj.scores[i,1] <- adj.weights[1]*DMS[i,1] + adj.weights[2]*DMS[i,2] + adj.weights[3]*DMS[i,3] + adj.weights[4]*DMS[i,4] + 
        adj.weights[5]*DMS[i,5] + adj.weights[6]*DMS[i,6] + adj.weights[7]*DMS[i,7] + adj.weights[8]*DMS[i,8] + 
        adj.weights[9]*DMS[i,9] + adj.weights[10]*DMS[i,10] + adj.weights[11]*DMS[i,11] + adj.weights[12]*DMS[i,12] + 
        adj.weights[13]*DMS[i,13] + adj.weights[14]*DMS[i,14] + adj.weights[15]*DMS[i,15] + adj.weights[16]*DMS[i,16] + 
        adj.weights[17]*DMS[i,17] + adj.weights[18]*DMS[i,18] + adj.weights[19]*DMS[i,19] + adj.weights[20]*DMS[i,20]
}

range(adj.scores)










###########################################################################

# Obtain the Score from First Principal Component -------------------------
Asset$index.fp <- model.fp$scores[,1]
Asset$index.or <- model.or$scores[,1]
Asset$index.ka <- model.ka$scores[,1]

Asset$wealth <- adj.scores[,1]


###########################################################################
# Socioeconomic Status DataBase -------------------------------------------

AssetIndex <- transform(Asset,
                        index.ka0 = index.ka + abs(min(index.ka))) %>%
    select( . , 
            control, PESO_FINAL, wealth, index.or, index.fp, index.ka, index.ka0) %>%
    as_tibble()
    

skew(AssetIndex$wealth, type = 3, na.rm = T)
kurtosi(AssetIndex$wealth)

range(AssetIndex$wealth)

cor(AssetIndex$wealth, AssetIndex$index.ka, method = "spearman")





###########################################################################
# Aggregate Both Indices --------------------------------------------------

Household.Wealth <- full_join(AssetIndex, Simple1,
                              by = c("control")) 


saveRDS(Household.Wealth, "Household.Wealth.2018.rds")

write_csv(Household.Wealth, "C:/Empirical Research/WealthIndex/WealthIndex/HouseholdWealth.csv")



rm(Demographics, Domicilio, Housing, Inventario, Inventory, Morador)
rm(Asset, AssetD, AssetIndex, Asset.Adjusted, Asset1, pcorr, RI, weights2, i, adj.scores, adj.weights, Simple1, DMS)
rm(model.fp, model.ka, model.or)




