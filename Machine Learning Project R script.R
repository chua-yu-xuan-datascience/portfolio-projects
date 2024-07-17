rm(list=ls())
library(readxl)
HDB_prices <- read_xlsx(path = "C:/Users/Admin/OneDrive/Desktop/DSE1101/Project/HDB_data_2021_sample.xlsx")


set.seed(11011)                             # for reproducibility
train = sample(6000, 4200, replace = FALSE) # a 70/30 split, randomly put 70% of observation in training set, other 30% in test set
HDB_train = HDB_prices[train, ]             # training set for K-fold cross validation
HDB_test = HDB_prices[-train, ] 

sum(is.na(HDB_prices)) #locate NA values #0
summary((HDB_prices$resale_price))
# mean higher than median, right-skewed

################################
##Unsupervised learning methods
###############################


########################
###K-Means clustering
########################
grp4 = kmeans(scale(na.omit(HDB_train[,c("Dist_nearest_ADF","Dist_CBD","Dist_nearest_university", "Dist_nearest_GAI_jc",
                                         "Dist_nearest_GAI_primary_school","Dist_nearest_station","Dist_nearest_CC",
                                         "Dist_nearest_GHawker", "Remaining_lease", "max_floor_lvl", "floor_area_sqm",
                                         "Dist_nearest_mall", "Dist_nearest_beach", "Dist_nearest_waterbody", "Dist_nearest_primary_school", "Dist_nearest_G_primary_school",
                                         "Dist_nearest_secondary_school", "Dist_nearest_GAI_secondary_school", "Dist_nearest_G_secondary_school", 
                                         "Dist_nearest_jc", "Dist_nearest_G_jc", "Dist_nearest_polytechnic", 
                                         "Dist_nearest_hospital", "Dist_nearest_A_hospital")])), 
              centers = 4, nstart = 20)

#windows()
plot(x = HDB_train$floor_area_sqm,y = HDB_train$resale_price, 
     main = "K = 4", xlab="floor_area_sqm", ylab="Resale Price", type="n")

text(HDB_train$floor_area_sqm, HDB_train$resale_price, col = rainbow(4)[grp4$cluster])
#dev.off()

#Choosing K with AICc and BIC 

n = nrow(HDB_train) #get sample size # calculate no. of rows in dataframe

d=2 #dimension of the data (for naive degrees of freedom estimate)


kt = 1:20 
bic = rep(0,20)
aicc = rep(0,20) 
for(ii in 1:20) {
  fit = kmeans(scale(HDB_train[,c("Dist_nearest_ADF","Dist_CBD","Dist_nearest_university", "Dist_nearest_GAI_jc",
                                  "Dist_nearest_GAI_primary_school","Dist_nearest_station","Dist_nearest_CC",
                                  "Dist_nearest_GHawker", "Remaining_lease", "max_floor_lvl", "floor_area_sqm",
                                  "Dist_nearest_mall", "Dist_nearest_beach", "Dist_nearest_waterbody", "Dist_nearest_primary_school", "Dist_nearest_G_primary_school",
                                  "Dist_nearest_secondary_school", "Dist_nearest_GAI_secondary_school", "Dist_nearest_G_secondary_school", 
                                  "Dist_nearest_jc", "Dist_nearest_G_jc", "Dist_nearest_polytechnic", 
                                  "Dist_nearest_hospital", "Dist_nearest_A_hospital")]), centers = ii, nstart = 20) #do k-means with ii clusters
  df = d*ii #estimate for degrees of freedom
  #The measure of sum of squares here is total within sum of squares (tot.withinss): 
  bic[ii] = fit$tot.withinss + log(n)*df 
  aicc[ii] = fit$tot.withinss + 2*df*n/(n-df-1) 
} 


#Get selected K from the IC:
bicsel=which.min(bic) #K=20
aiccsel=which.min(aicc) #K=20


########################
## Hierachial clustering
########################
sd.dataSmall = scale(HDB_train[,c("Dist_nearest_ADF","Dist_CBD","Dist_nearest_university", "Dist_nearest_GAI_jc",
                                  "Dist_nearest_GAI_primary_school","Dist_nearest_station","Dist_nearest_CC",
                                  "Dist_nearest_GHawker", "Remaining_lease", "max_floor_lvl", "floor_area_sqm",
                                  "Dist_nearest_mall", "Dist_nearest_beach", "Dist_nearest_waterbody", "Dist_nearest_primary_school", "Dist_nearest_G_primary_school",
                                  "Dist_nearest_secondary_school", "Dist_nearest_GAI_secondary_school", "Dist_nearest_G_secondary_school", 
                                  "Dist_nearest_jc", "Dist_nearest_G_jc", "Dist_nearest_polytechnic", 
                                  "Dist_nearest_hospital", "Dist_nearest_A_hospital")])

distSmall = dist(sd.dataSmall) 

#Plot the dendgrogram using complete linkage
#windows()
plot(hclust(distSmall, method = "complete"), 
     main = "HDB - Complete Linkage", xlab = "", sub = "")
abline(h =13, col='red') 
#dev.off()
#cut at 4 clusters also seem appropriate


###############
##PCA and PCR
###############
library(pls)
library(janitor)

pcahdb = remove_constant(HDB_train, na.rm= TRUE)


# add all continuous variables +mature
disc = c("mature", "Dist_nearest_ADF","Dist_CBD","Dist_nearest_university", "Dist_nearest_GAI_jc",
         "Dist_nearest_GAI_primary_school","Dist_nearest_station","Dist_nearest_CC",
         "Dist_nearest_GHawker", "Remaining_lease", "max_floor_lvl", "floor_area_sqm",
         "Dist_nearest_mall", "Dist_nearest_beach", "Dist_nearest_waterbody", "Dist_nearest_primary_school", "Dist_nearest_G_primary_school",
         "Dist_nearest_secondary_school", "Dist_nearest_GAI_secondary_school", "Dist_nearest_G_secondary_school", 
         "Dist_nearest_jc", "Dist_nearest_G_jc", "Dist_nearest_polytechnic", 
         "Dist_nearest_hospital", "Dist_nearest_A_hospital")
data2 = pcahdb[,(names(pcahdb) %in% disc)]

#biplot
#windows()
prall = prcomp(data2, scale = TRUE)
biplot(prall)

# proportion of variance of x explained
prall.s = summary(prall)
prall.s$importance

# scree plot
scree = prall.s$importance[2,]
plot(scree, main = "Scree Plot", xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", ylim = c(0,0.5), type = 'b', cex = .8)
#dev.off()
# First PC only explains 0.190850 of the total variance of X
# though the biplot was somewhat useful to look at which variables have high loadings

#Perform PCR
set.seed(11011)
pcr.fit=pcr((resale_price) ~  mature+(Dist_nearest_ADF)
            +(Dist_CBD) +(Dist_nearest_university)
            
            + (Dist_nearest_GAI_primary_school)
            + (Dist_nearest_station)+ (Dist_nearest_CC)
            + (Dist_nearest_GHawker)+ (Remaining_lease) 
            + (max_floor_lvl) +(floor_area_sqm)
            + Dist_nearest_mall + Dist_nearest_beach + Dist_nearest_waterbody + Dist_nearest_primary_school + Dist_nearest_G_primary_school
            + Dist_nearest_secondary_school + Dist_nearest_GAI_secondary_school + Dist_nearest_G_secondary_school
            + Dist_nearest_jc + Dist_nearest_G_jc + Dist_nearest_polytechnic 
            + Dist_nearest_hospital + Dist_nearest_A_hospital,data=pcahdb, scale=TRUE, validation="CV")

#windows()
plot(pcr.fit, "loadings")
plot(pcr.fit, "loadings", comps = 1:3, legendpos = "topleft")
abline(h = 0) #add the zero line for reference

set.seed(11011)
validationplot(pcr.fit, val.type="MSEP", main="10CV",legendpos = "topright")
abline(v=20)
#dev.off()
pcr.pred=predict(pcr.fit, newdata=pcahdb, ncomp=20) #looking at the cv plot, M=20 is the one that minimises MSE
mean((pcahdb$resale_price-pcr.pred)^2) #MSE for PCR
# 4059121405
# sqrt(4059121405)
# 63711.23
histav=lm(resale_price~1, data=pcahdb)
predlm=predict.lm(histav, newdata=pcahdb)
mean((pcahdb$resale_price-predlm)^2)# MSE of constant model
# 26157920736


################################
##Supervised learning methods
###############################


###########################
##Mutiple Linear Regression
###########################

#Training model
final_lm = lm(log(resale_price) ~ (Dist_nearest_ADF) + ADF_within_2km  +ADF_within_1km
              +(Dist_CBD) +(Dist_nearest_university) + Dist_nearest_hospital
              
              +(Dist_nearest_GAI_jc)
              
              + Nearest_secondary_school_gender_GIRLS..SCHOOL  
              + Nearest_G_secondary_school_gender_GIRLS..SCHOOL 
              
              + no_G_primary_schools_1km 
              + (Dist_nearest_GAI_primary_school)+ no_GAI_primary_schools_2km 
              
              + (Dist_nearest_station)+ (Dist_nearest_CC) + waterbody_within_2km  + beach_within_2km 
              
              + NSL+ EWL + NEL +CCL + DTL + LRT
              
              + no_malls_0.5km + no_malls_2km+ unique_no_mrt_0.5km  + (unique_no_mrt_1km)
              + (Dist_nearest_GHawker)+ nearest_ghawker_no_of_stalls + nearest_ghawker_no_of_cooked_food_stalls
              
              + mature+ (Remaining_lease) 
              + (total_dwelling_units) + X4room_sold  
              + flat_type_2.ROOM+ flat_type_3.ROOM + flat_type_4.ROOM
              + flat_model_type.s2  + flat_model_terrace +flat_model_standard
              
              + flat_model_multi.generation + flat_model_model.a.maisonette + flat_model_model.a
              + flat_model_improved  + flat_model_maisonette
              + flat_model_dbss + flat_model_apartment 
              + storey_range_01.TO.03 + storey_range_04.TO.06 + storey_range_07.TO.09 + storey_range_10.TO.12
              + storey_range_13.TO.15 + storey_range_16.TO.18 
              
              + town_ANG.MO.KIO + town_BISHAN + town_BUKIT.BATOK+ town_BUKIT.MERAH  +town_BUKIT.PANJANG + town_BUKIT.TIMAH
              + town_CLEMENTI + town_CHOA.CHU.KANG +town_HOUGANG 
              + town_JURONG.EAST + town_MARINE.PARADE + town_KALLANG.WHAMPOA
              + town_QUEENSTOWN  + town_SEMBAWANG + town_SENGKANG + town_SERANGOON + town_TAMPINES
              
              + postal_2digits_08 
              + postal_2digits_15 + postal_2digits_16 + postal_2digits_18 + postal_2digits_20 
              + postal_2digits_37 + postal_2digits_39 + postal_2digits_41
              
              + month_Jan + month_Feb
              + (max_floor_lvl) +(floor_area_sqm), data = HDB_train)


summary(final_lm)

#windows()
par(mfrow = c(2, 2))
plot(final_lm)
#dev.off()

# K fold cross validation for MLR
library(boot)
set.seed(11011)
final_lm_glm = glm(log(resale_price) ~ (Dist_nearest_ADF) + ADF_within_2km  +ADF_within_1km
                   +(Dist_CBD) +(Dist_nearest_university) + Dist_nearest_hospital
                   
                   +(Dist_nearest_GAI_jc)

                   + Nearest_secondary_school_gender_GIRLS..SCHOOL  
                   + Nearest_G_secondary_school_gender_GIRLS..SCHOOL 
                    
                   + no_G_primary_schools_1km 
                   + (Dist_nearest_GAI_primary_school)+ no_GAI_primary_schools_2km 
                   
                   + (Dist_nearest_station)+ (Dist_nearest_CC) + waterbody_within_2km  + beach_within_2km 
                   
                   + NSL+ EWL + NEL +CCL + DTL + LRT
                   
                   + no_malls_0.5km + no_malls_2km+ unique_no_mrt_0.5km  + (unique_no_mrt_1km)
                   + (Dist_nearest_GHawker)+ nearest_ghawker_no_of_stalls + nearest_ghawker_no_of_cooked_food_stalls
                   
                   + mature+ (Remaining_lease) 
                   + (total_dwelling_units) + X4room_sold  
                   + flat_type_2.ROOM+ flat_type_3.ROOM + flat_type_4.ROOM
                   + flat_model_type.s2  + flat_model_terrace +flat_model_standard
                   
                   + flat_model_multi.generation + flat_model_model.a.maisonette + flat_model_model.a
                   + flat_model_improved  + flat_model_maisonette
                   + flat_model_dbss + flat_model_apartment 
                   + storey_range_01.TO.03 + storey_range_04.TO.06 + storey_range_07.TO.09 + storey_range_10.TO.12
                   + storey_range_13.TO.15 + storey_range_16.TO.18 
                     
                   + town_ANG.MO.KIO + town_BISHAN + town_BUKIT.BATOK+ town_BUKIT.MERAH  +town_BUKIT.PANJANG + town_BUKIT.TIMAH
                   + town_CLEMENTI + town_CHOA.CHU.KANG +town_HOUGANG 
                   + town_JURONG.EAST + town_MARINE.PARADE + town_KALLANG.WHAMPOA
                   + town_QUEENSTOWN  + town_SEMBAWANG + town_SENGKANG + town_SERANGOON + town_TAMPINES
                    
                   + postal_2digits_08 
                   + postal_2digits_15 + postal_2digits_16 + postal_2digits_18 + postal_2digits_20 
                   + postal_2digits_37 + postal_2digits_39 + postal_2digits_41
                   
                   + month_Jan + month_Feb
                   + (max_floor_lvl) +(floor_area_sqm), data = HDB_train)

set.seed(11011)
cv.glm(HDB_train, (final_lm_glm), K = 10)$delta[1]
##0.005993104

# Variables I am surprised did not make the cut: 
# no_primary_schools_2km + no_primary_schools_1km+ Dist_nearest_primary_school
# GAI secondary schools


#Evaluate the model - use MSE to evaluate performance
predlm=predict.lm(final_lm, newdata= HDB_test)
predlm_trans=exp(predlm)
mean((HDB_test$resale_price-predlm_trans)^2) #compute the MSE
# 1916182367
#sqrt( 1916182367)
# 43774.22/468000
# 0.09353466 off the median

# Summary of resale price
hist(I(HDB_train$resale_price/1000),
     xlab = "Resale Prices", main = "Resale Prices of HDB (in thousands)")
# right skewed, quite a few outliers at the high end

hist(log(HDB_train$resale_price),
     xlab = "Resale Prices", main = "Resale Prices of HDB (in natural log)")
# natural log transform the dependent variable because linear regression sensitive to outliers
# improves performance during cross validation


# scatter plot for floor area and remaining lease
#windows()
lm1 = lm(log(resale_price)~ Remaining_lease, data = HDB_train)
plot(x = (HDB_train$Remaining_lease), y = log(HDB_train$resale_price),
     xlab = "Remaining Lease", ylab = "Resale Price (in natural log)", col = "red", pch = 19)
abline(lm1, col= 'blue', lwd = 3)

lm2 = lm(log(resale_price)~ floor_area_sqm, data = HDB_train)
plot(x = (HDB_train$floor_area_sqm), y = log(HDB_train$resale_price),
     xlab = "Floor area (in sqm)", ylab = "Resale Price (in natural log)", col = "maroon", pch = 19)
abline(lm2, col= 'blue', lwd = 3)
#dev.off()

# investigate clementi and university
lm21= lm11 = lm((resale_price)~ town_CLEMENTI, data = HDB_train)
summary(lm21)
lm22 = lm((resale_price)~ Dist_nearest_university, data = HDB_train)
summary(lm22)
lm23 = lm((resale_price)~ Dist_nearest_university+ town_CLEMENTI, data = HDB_train)
summary(lm23)
# clementi is correlated to university
# its coefficient was positive but when considered with university it flipped signs

#####################
##KNN for Regression
#####################
library(kknn)

set.seed(11011)

#kknn package already standardises variables for us
# only the more significant continuous variables from linear regression model
# find best K
hdbknn=train.kknn((resale_price) ~ 
                  +(Dist_CBD) +(Dist_nearest_university)
                  + Dist_nearest_GAI_jc
                  + (Dist_nearest_GAI_primary_school)
                  + (Dist_nearest_station)+ (Dist_nearest_CC) 
                  + (Dist_nearest_GHawker)+ (Remaining_lease) 
                  + (max_floor_lvl) +(floor_area_sqm), data=HDB_train,kmax=100, kernel = "rectangular")


#Examine the behavior of the LOOCV MSE:
#windows()
plot((1:100),hdbknn$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE", xlab="Complexity: K", ylab="MSE")
#dev.off()

#Find the best K:
kbest=hdbknn$best.parameters$k

#We find K=3 works best according to LOOCV


#Fit for the selected K:
knnreg = kknn((resale_price) ~  
              +(Dist_CBD) +(Dist_nearest_university)
              + Dist_nearest_GAI_jc
              + (Dist_nearest_GAI_primary_school)
              + (Dist_nearest_station)+ (Dist_nearest_CC) 
              + (Dist_nearest_GHawker)+ (Remaining_lease) 
              + (max_floor_lvl) +(floor_area_sqm),HDB_train,HDB_test,k=kbest,kernel = "rectangular")

knnmse=mean((HDB_test$resale_price-knnreg$fitted.values)^2) #test set MSE
#2788790859
# sqrt(2788790859)
# 52809/468000
# 0.1128397 off the median

#################
##Decision Tree
#################
library(tree)
library(rpart)

##################
# use tree package 
##################
bigtree = tree(I(resale_price/1000) ~., data=HDB_train, mindev=0.0001)

#Count leaves on big.tree
length(unique(bigtree$where)) #237 leaves
nrow(bigtree$frame) #473 nodes


#determine best tree size based on CV:
cv.bigtree = cv.tree(bigtree, , prune.tree) #10-fold cross-validation

# find the tree that gives us smallest dev (loss function) via K CV
bestcp = cv.bigtree$size[max(which(cv.bigtree$dev == min(cv.bigtree$dev)))]

hdb.tree=prune.tree(bigtree, best=bestcp)

#plot the tree
#windows()
plot(hdb.tree,type="uniform")
text(hdb.tree,col="blue",label=c("yval"),cex=.8)
#dev.off()

#test
treefit=predict(hdb.tree, newdata= HDB_test, type = "vector") #prediction on test data
resale_trans=I(HDB_test$resale_price/1000)
mean((resale_trans-treefit)^2) 
# output = 6326.615  
# actual MSE = 6326.615*1000000 = 6326615000
#sqrt(6326615360)
# 79540.02 (actual RMSE)
## 0.1699573 off the median 468000
