# NAPEROMU TERMELES BECSLESE  

# TARTALOM
# I. adattisztitas
# II. modellezes
# III. vizualizacio


setwd("C:/Users/Gabor/Documents/00_Munka/42_eNet/sent")


#####################
# I. adattisztitas  #
#####################

library(tidyverse)
library(lubridate)
library(imputeTS)

# 1. idojaras adatok exportalasa -----------------------------------------

ido_teljes = read.csv("Debrecen_napi-adatok.txt", sep = ";", stringsAsFactors = F,
                     header= T)

names(ido_teljes) = c("datum", "hom_koz", "hom_max", "hom_min", "csap_ossz", "csap_faj",
              "napf_ossz", "globsug_ossz")

ido = ido_teljes[ido_teljes$datum >= "2018-12-01" ,]

rm(ido_teljes)
ido$napf_ossz = NULL


# 2. datum valtozo kezelese  -----------------------------------------

datumv = ymd(ido$datum)

ido$ho =  month(datumv) 
ido$ev =  year(datumv)
ido$nap =  day(datumv)

# transzformaljuk a cirkularis datumvaltotokat sin es cos-szal
ido$ho_sin = sin(((2*pi)/12)*ido$ho)
ido$ho_cos = cos(((2*pi)/12)*ido$ho)

ido = group_by(ido, ho, ev) %>%
  mutate(nap_sin = ifelse(ho == 2, sin(((2*pi)/28)*nap), 
                          ifelse(ho %in% c(4,6,9,11), sin(((2*pi)/30)*nap), 
                                 sin(((2*pi)/31)*nap))
                          )
         ) %>%
  mutate(nap_cos = ifelse(ho == 2, cos(((2*pi)/28)*nap), 
                            ifelse(ho %in% c(4,6,9,11), cos(((2*pi)/30)*nap), 
                                   cos(((2*pi)/31)*nap))
                            )
         ) 


ido = ungroup(ido)

# 3. korrelacio vizsgalata, idojaras rekodolasa  -----------------------------------------

corrplot::corrplot(cor(ido[,-1], use = "complete.obs"))
cor(ido[,-1], use = "complete.obs")

ido$hom_max = NULL # hom_max es hom_min eltavolitasa,
ido$hom_min = NULL # mert legalabb r = .95 a koz_hommel valo korrelaciojuk.

ido[is.na(ido$csap_faj),]["csap_faj"] = -1 # hozzunk letre egy "-1" kategoriat, ami a csap.hianyat jelolni 
ido$csap_faj = as.ordered(ido$csap_faj) # legyen ordered factor, mert a kulonbsegek nem egyenloek a valtozo ertekei kozt.


# 4. termeles adatok kapcsolasa -----------------------------------------

term = read.csv("termeles.csv", sep = ";", stringsAsFactors = F,
                     header= T, colClasses = c("POSIXct", "integer")
                )

temptime = lubridate::ymd_hms(term$time)

term$ev =  year(temptime)
term$ho =  month(temptime) 
term$nap =  day(temptime) 
term$ora =  hour(temptime) 
term$perc =  minute(temptime) 

term$ora_sin = sin(((2*pi)/24)*term$ora)
term$ora_cos = cos(((2*pi)/24)*term$ora)

term$perc_sin = sin(((2*pi)/60)*term$perc)
term$perc_cos = cos(((2*pi)/60)*term$perc)

df = full_join(x = term, y = ido, by = c("ev" = "ev", "ho" = "ho", "nap" = "nap"))

df$time = NULL
df$datum = NULL
rm(ido)
rm(term)


# 5. imputalas, termeles rekodolasa -----------------------------------------

df$globsug_ossz = na_mean(df$globsug_ossz) # imputalas

df[df$ev==2019,]["ev"]= 1 # dummyzzuk az evet
df[df$ev==2018,]["ev"]= 0

names(df)[names(df) == 'ev'] <- 'ev_19'




##################
# II. modellezes #
##################

library(tidyverse)
library(randomForest)
library(e1071)
library(glmnet)


# 0. data prep -----------------------------------------

# standardize variables
df[,!(colnames(df) %in% c("kw", "csap_faj"))] = as.data.frame(scale(df[,!(colnames(df) %in% c("kw", "csap_faj"))], center = F))


# train/tesztre oszt

test_dec = df[is.na(df$kw),] # ez a felad

df2 = df[!is.na(df$kw),]
set.seed(3)
train_index = sample(1:nrow(df2), 2*nrow(df2)/3)
train = df2[train_index,]
test = df2[-train_index,]
rm(df2)


# hanyomanyos ("vanilla") es trig valtozos modellek

vanilla = c( "kw", "ev_19", "ho", "nap", "ora", "perc", 
             "hom_koz", "csap_ossz", "csap_faj", "globsug_ossz")
trig = c( "kw", "ev_19", "ho_sin", "ho_cos", "nap_sin", "nap_cos", 
          "ora_sin", "ora_cos", "perc_sin", "perc_cos",  
          "hom_koz", "csap_ossz", "csap_faj", "globsug_ossz")

train_vanilla = train[,vanilla]
train_trig = train[,trig]
test_vanilla = test[,vanilla]
test_trig = test[,trig]
test_dec_vanilla = test_dec[,vanilla]
test_dec_trig = test_dec[,trig]

test_dec_vanilla$kw = 0

# kieertekelo fv irasa
ertekelo = function(tenyleges = test_trig$kw, becsult = pred_lm){
  becs_hiba = abs(becsult - tenyleges)
  korlat = tenyleges * 0.5
  kiertekelesi_alap = round(max(korlat-becs_hiba, 0)/4,10)
  RMSE = sqrt( mean((tenyleges - becsult)^2) ) 
  eredmeny <- data.frame(cbind(tenyleges, becsult , becs_hiba, korlat, kiertekelesi_alap, RMSE))  
  return(eredmeny)
  
}


# 1. linear model (trig)  -----------------------------------------

lm <- lm(kw ~ ., data=train_trig)  

pred_lm <- predict(lm, test_trig) 
pred_lm_dec <- predict(lm, test_dec_trig) 

result_lm = ertekelo(tenyleges = test_trig$kw, becsult = pred_lm)

KA = round(unique(result_lm$kiertekelesi_alap),1)
E = round(unique(result_lm$RMSE),1)

cat("A lin modell kiertekelesi alapja:", KA, ", RMSE erteke:", E)


# 2. ridge model (trig)  -----------------------------------------

y = train_trig$kw
x <- model.matrix(kw ~., train_trig)[,-1]

set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 0) 

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)

x.test <- model.matrix(kw ~., test_trig)[,-1]
predictions_ridge <- model %>% predict(x.test) %>% as.vector()

result_ridge = ertekelo(tenyleges = test_trig$kw, becsult = predictions_ridge)

KA = round(unique(result_ridge$kiertekelesi_alap),1)
E = round(unique(result_ridge$RMSE),1)

cat("A ridge modell kiertekelesi alapja:", KA, ", RMSE erteke:", E)


# 3. lasso model (trig)  -----------------------------------------

y = train_trig$kw
x <- model.matrix(kw ~., train_trig)[,-1]

set.seed(123) 
cv_lasso <- cv.glmnet(x, y, alpha = 1) 

model <- glmnet(x, y, alpha = 1, lambda = cv_lasso$lambda.min)

x.test <- model.matrix(kw ~., test_trig)[,-1]
predictions_lasso <- model %>% predict(x.test) %>% as.vector()

result_lasso = ertekelo(tenyleges = test_trig$kw, becsult = predictions_lasso)

KA = round(unique(result_lasso$kiertekelesi_alap),1)
E = round(unique(result_lasso$RMSE),1)

cat("A lasso modell kiertekelesi alapja:", KA, ", RMSE erteke:", E)

# gyakorlatilag nincs kulonbseg a regressziok kozott.


# 5. SVM model (vanilla)  -----------------------------------------

# 5%-os veletlen mintan optimalizaltam, mert sokaig fut:

# set.seed(3)
# train_samp = sample(1:nrow(train_vanilla), nrow(train_vanilla)/20)
# train_vanilla_samp = train_vanilla[train_samp,]
# set.seed(3)
# test_samp = sample(1:nrow(test_vanilla), nrow(test_vanilla)/20)
# test_vanilla_samp = test_vanilla[test_samp,]

# set.seed (1)
# tune_svm = tune(svm , kw ~., data=train_vanilla_samp, kernel ="radial",
#                type = "nu-regression",
#                ranges =list(cost=c(0.1, 1, 10),
#                             gamma=c(0.01, 0.1,1),
#                             epsilon=c(0.01, 0.1,1)))

# summary (tune_svm)
# tune_svm$best.parameters # cost = 10, gamma = 1, epsilon = 0.01


# alkalmazzuk az svm hiperparametereket a teljes vanilla adatsorra

svm_model_nu <- svm(kw ~ . , train_vanilla, kernel = "radial",
                    type = "nu-regression",
                    nu = 0.1,
                    cost = 10, 
                    gamma = 1, 
                    epsilon = 0.001)

save(svm_model_nu, file = "svm-nu_model.Rdata")
# load("svm-nu_model.Rdata")
# 28 p.

pred_svm <- predict(svm_model_nu, test_vanilla)
summary(pred_svm)
# pred_svm0 = ifelse(pred_svm<0,0,pred_svm)

result_svm = ertekelo(tenyleges = test_trig$kw, becsult = pred_svm)

KA = round(unique(result_svm$kiertekelesi_alap),1)
E = round(unique(result_svm$RMSE),1)

cat("A radial SVM modell kiertekelesi alapja:", KA, ", RMSE erteke:", E)


# 6. random forest (vanilla)  -----------------------------------------

rf <- randomForest(kw ~ ., data=train_vanilla)
save(rf, file = "rf_model.Rdata")
# 6 p.

pred_rf = predict(rf, test_vanilla)

result_rf = ertekelo(tenyleges = test_trig$kw, becsult = pred_rf)

KA = round(unique(result_rf$kiertekelesi_alap),1)
E = round(unique(result_rf$RMSE),1)

cat("A random forest modell kiertekelesi alapja:", KA, ", RMSE erteke:", E)



# 7. a legjobb modell decemberi becslese  -----------------------------------------

pred_svm_dec <- predict(svm_model_nu, test_dec_vanilla)
pred_svm_dec_df = as.data.frame(pred_svm_dec)

termeles = read.csv("termeles.csv", sep = ";", stringsAsFactors = F,
                    header= T, colClasses = c("POSIXct", "integer"))

outfile0 = merge(termeles, pred_svm_dec_df, by = 0, all = T) # 2 es fel p.
names(outfile0)[(names(outfile0) == "Row.names")] = "sorrend"

ooutfile2 = outfile0 %>% mutate(mycol = coalesce(kw, pred_svm_dec)) %>%
  select(time, sorrend, mycol)

ooutfile2 = ooutfile2[order(ooutfile2$time),]
ooutfile2 = subset(ooutfile2, select = -c(sorrend))
names(ooutfile2)[2] = "kw"
ooutfile2$kw = round(ooutfile2$kw,0)

write.table(ooutfile2, 'result.csv', 
            sep = ";", dec = ",", na = "NA", row.names = F)




######################
# III. vizualizacio  #
######################


df = read.csv("result_vizuhoz.csv", sep = ";", stringsAsFactors = F, header= T)

library(lubridate)
temptime = lubridate::ymd_hms(df$time)
df$ev =  year(temptime)
df$ho =  month(temptime) 
df$nap =  day(temptime) 
df$ora =  hour(temptime) 
df$perc =  minute(temptime)

df_dec = df[df$ho == 12,]
even = df_dec[df_dec$nap %% 2 == 0,]
odds = df_dec[df_dec$nap %% 2 != 0,]

summary(even$kw_test)
length(even$kw_test)

dat = odds[!is.na(odds$kw_test),]

# aggregate

library(dplyr)
a_dat = dat %>% 
  group_by(nap) %>% 
  summarise(kw = mean(kw), kw_test = mean(kw_test, na.rm = T))

ungroup(a_dat)
a_dat

# napi atlagban merve ennyire volt jo a becsles:
a_dat$z = as.factor(ifelse(a_dat$nap %% 2 == 0,2,1))
par(mar = c(5, 5, 4, 8),                    
    xpd = TRUE)
plot(a_dat$nap, a_dat$kw, col = a_dat$z, type = "b", pch = 16,
     ylim = c(0,60), ylab = "kw", xlab = "december", 
     main = "a teszthalmazon")
points(a_dat$nap, a_dat$kw_test)
legend("right", inset = c(- 0.3, 0),                   
       legend = c("becsült","valós"),
       pch = c(1,16))
dev.copy(jpeg,filename="01_svm_teszthalmaz.jpg");
dev.off ();
# teli a valos, ures a becsult

# aggregate
# ez pedig a predikcionk

library(dplyr)
a_df = df_dec %>% 
  group_by(nap) %>% 
  summarise(kw = mean(kw))

ungroup(df_dec)
print(as_tibble(a_df), n=31)

a_df$z = as.factor(ifelse(a_df$nap %% 2 == 0,2,1))
par(mar = c(5, 5, 4, 8),                    
    xpd = TRUE)
plot(a_df$nap, a_df$kw, col = a_df$z, type = "b", pch = 16,
     ylim = c(0,60), ylab = "kw", xlab = "december",
     main = "a teljes adathalmazon")
legend("right", inset = c(- 0.3, 0),                   
       legend = c("becsült","valós"),
       pch = 16,
       col = c("red", "black")
)
dev.copy(jpeg,filename="02_teljes_halmaz.jpg");
dev.off ();