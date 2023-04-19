model = lm(texture_mean~., data=cancer_data_subset[2:11])
summary(model)
ols_step_best_subset(model)



library(tidyverse)
cancer_data <- read_csv("breast_cancer_data.csv")
#read data and took just mean values along with diagnosis also
cancer_data_subset <- cancer_data %>% 
  select(diagnosis, radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean,
         compactness_mean, concavity_mean, concave_points_mean, symmetry_mean, fractal_dimension_mean)

#scatter matrix
library(GGally)
library(olsrr)
library(ggfortify)
ggpairs(cancer_data_subset, aes(color=cancer_data_subset$diagnosis))+ ggtitle("Scatter Matrix")

ggplot(cancer_data_subset,aes(x=perimeter_mean,y=radius_mean)) +
geom_point(aes(colour=diagnosis)) + ggtitle("Correlation between perimeter mean and radius mean")
ggplot(cancer_data_subset,aes(x=area_mean,y=radius_mean)) +
  geom_point(aes(colour=diagnosis)) + ggtitle("Correlation between area mean and radius mean")
ggplot(cancer_data_subset,aes(x=area_mean,y=perimeter_mean)) +
  geom_point(aes(colour=diagnosis)) + ggtitle("Correlation between area mean and perimeter mean")
ggplot(cancer_data_subset,aes(x=concave_points_mean,y=concavity_mean)) +
  geom_point(aes(colour=diagnosis)) + ggtitle("Correlation between concave points mean and concavity mean")
ggplot(cancer_data_subset,aes(x=perimeter_mean,colour=diagnosis)) +
  geom_boxplot() + ggtitle("Boxplot between perimeter mean and diagnosis")
ggplot(cancer_data_subset,aes(x=radius_mean,colour=diagnosis)) +
  geom_boxplot() + ggtitle("Boxplot between radius mean and diagnosis")

# Best one-predictor model of texture_mean: perimeter_mean
modela = lm(texture_mean~perimeter_mean, data=cancer_data_subset)
summary(modela)#R-squared: 0.1086
AIC(modela)#AIC value :3214.521
a <-autoplot(modela)
gridExtra::grid.arrange(grobs = a@plots, top = "Diagnostic Plot of linear model")

#Removing the samples by considering the Leverage value plot whose
#standardised residuals are greater than 3 or less than −3
updated_cancer_data_subset = cancer_data_subset[((scale(modela$residuals)>-3) & (scale(modela$residuals) < 3)),]
model_updated = lm(texture_mean~perimeter_mean, data = updated_cancer_data_subset)
summary(model_updated)#R-squared:0.1147
AIC(model_updated)#AIC value :3150.838
b <-autoplot(model_updated)
gridExtra::grid.arrange(grobs = b@plots, top = "Diagnostic Plot After removing samples")



#2a
#2b

model <- lm(texture_mean~., data=cancer_data_subset[,2:11])
summary(model)
ols_step_best_subset(model)

# Best one-predictor model of texture_mean: perimeter_mean
model1 <- lm(texture_mean~perimeter_mean, data=cancer_data_subset)
summary(model1) #R-squared:0.1086
AIC(model1)#AIC value :3214.521
c<-autoplot(model1)
gridExtra::grid.arrange(grobs = c@plots, top = "Diagnostic Plot of model1(one-predictor)")

# Best two-predictor model 
model2 <- lm(texture_mean~smoothness_mean+concavity_mean, data=cancer_data_subset)
summary(model2)#R-squared:0.1366
AIC(model2)#AIC value :3198.35
d<-autoplot(model2)
gridExtra::grid.arrange(grobs = d@plots, top = "Diagnostic Plot of model2(two-predictor)")

# Best four-predictor model 
model3 <- lm(texture_mean~smoothness_mean+compactness_mean+concavity_mean+fractal_dimension_mean, data=cancer_data_subset)
summary(model3)#R-squared:0.1562
AIC(model3)#AIC value :3189.26
e<-autoplot(model3)
gridExtra::grid.arrange(grobs = e@plots, top = "Diagnostic Plot of model3(four-predictor)")

# Best SUBSET model 
model4 <- lm(texture_mean~smoothness_mean+compactness_mean+concavity_mean+fractal_dimension_mean, data=cancer_data_subset)
summary(model4)#R-squared:0.1562
AIC(model4)#AIC value :3189.26
f<-autoplot(model4)
gridExtra::grid.arrange(grobs = f@plots, top = "Diagnostic Plot of model4(SUBSET model)")

#2c

#best two models from above part and investigated
model2 <- lm(texture_mean~smoothness_mean+concavity_mean, data=cancer_data_subset)
summary(model2)#R-squared:0.1366
AIC(model2)#AIC value :3198.35

model3 <- lm(texture_mean~smoothness_mean+compactness_mean+concavity_mean+fractal_dimension_mean, data=cancer_data_subset)
summary(model3)#R-squared:0.1562
AIC(model3)#AIC value :3189.26



#3

# Subset the dataset into two groups based on diagnosis
data_m <- subset(cancer_data_subset, diagnosis == "M")
data_b <- subset(cancer_data_subset, diagnosis == "B")

#linear models for texture using any subset of variables for M=malignant
model_m <- lm(texture_mean~smoothness_mean+concavity_mean+concave_points_mean, data = data_m)
summary(model_m)#R-squared:0.07823
AIC(model_m)#AIC value :1157.102
g<-autoplot(model_m)
gridExtra::grid.arrange(grobs = g@plots, top = "Diagnostic Plot of model_m(Linear model for M)")


#linear models for texture using any subset of variables for B
model_b <- lm(texture_mean~area_mean+smoothness_mean+compactness_mean+concave_points_mean+fractal_dimension_mean, data = data_b)
summary(model_b)#R-squared:0.09794
AIC(model_b)#AIC value :1978.266
h<-autoplot(model_b)
gridExtra::grid.arrange(grobs = h@plots, top = "Diagnostic Plot of model_b(Linear model for B)")


# Create residuals vs fitted values plot for both models
M_fitted_val <- model_m$fitted.values
M_residuals <- model_m$residuals
B_fitted_val <- model_b$fitted.values
B_residuals <- model_b$residuals

par(mfrow = c(1, 1))
plot(M_fitted_val, M_residuals, col = "maroon", pch = 16, xlab = "Fitted values", ylab = "Residuals",title(main = "including fitted values VS residuals from both models on one plot"))
points(B_fitted_val, B_residuals, col = "orange", pch = 15)
legend("topright", c("M=malignant", "B=benign"), col = c("maroon", "orange"),  pch = c(16,15))
ggplot() +
  geom_point(aes(x=M_fitted_val,y=M_residuals), shape=16, col = "maroon",size=2) +
  geom_point(aes(x=B_fitted_val,y=B_residuals), shape=15,col = "orange", size=2)+
  xlab("fitted values") +
  ylab("residuals")+
  ggtitle("Including fitted values VS residuals from both models on one plot")+
  labs("topright", c("M=malignant", "B=benign"), col = c("maroon", "orange"),  pch = c(16,15))
  #guides(shape = guide_legend(override.aes = list(aes("M=malignant", "B=benign"),col = c("maroon", "orange"),shape= c(16,15))))
 














model <- lm(texture_mean~., data=data_m[,2:11])
ols_step_best_subset(model)
"""
 1         smoothness_mean                                                                                                                               
     2         smoothness_mean concavity_mean                                                                                                                
     3         smoothness_mean concavity_mean concave_points_mean                                                                                            
     4         smoothness_mean concavity_mean concave_points_mean symmetry_mean                                                                              
     5         smoothness_mean concavity_mean concave_points_mean symmetry_mean fractal_dimension_mean                                                       
     6         perimeter_mean smoothness_mean concavity_mean concave_points_mean symmetry_mean fractal_dimension_mean                                        
     7         radius_mean perimeter_mean smoothness_mean concavity_mean concave_points_mean symmetry_mean fractal_dimension_mean                            
     8         radius_mean perimeter_mean area_mean smoothness_mean concavity_mean concave_points_mean symmetry_mean fractal_dimension_mean                  
     9         radius_mean perimeter_mean area_mean smoothness_mean compactness_mean concavity_mean concave_points_mean symmetry_mean fractal_dimension_mean 
                       Adj.        Pred                                                                                                
Model    R-Square    R-Square    R-Square     C(p)         AIC         SBIC         SBC         MSEP         FPE       HSP       APC  
--------------------------------------------------------------------------------------------------------------------------------------
  1        0.0207      0.0160      0.0032     9.5653    1165.9338    564.2003    1176.0036    2979.6895    14.1877    0.0672    0.9979 
  2        0.0628      0.0539      0.0382     2.2069    1158.6128    557.0915    1172.0392    2865.2288    13.7062    0.0650    0.9641 
  3        0.0782      0.0649      0.0443     0.7870    1157.1018    555.7511    1173.8847    2831.7812    13.6089    0.0645    0.9572 
  4        0.0845      0.0669      0.0426     1.3847    1157.6450    556.4351    1177.7845    2826.0417    13.6438    0.0647    0.9597 
  5        0.0886      0.0664      0.0401     2.4923    1158.7128    557.6419    1182.2089    2827.3669    13.7128    0.0651    0.9645 
  6        0.0891      0.0624      0.0328     4.3794    1160.5945    559.6256    1187.4472    2839.6424    13.8352    0.0657    0.9731 
  7        0.0906      0.0594      0.0245     6.0458    1162.2448    561.3983    1192.4541    2848.9276    13.9435    0.0662    0.9808 
  8        0.0908      0.0549      0.0138     8.0047    1164.2017    563.4575    1197.7675    2862.4483    14.0730    0.0668    0.9899 
  9        0.0908      0.0503      0.0038    10.0000    1166.1967    565.5520    1203.1192    2876.6226    14.2063    0.0675    0.9992 
-----------------------------------------------------------------------------------------------------------------------"""
model <- lm(texture_mean~., data=data_b[,2:11])
ols_step_best_subset(model)
"""
 1         smoothness_mean                                                                                                                               
     2         area_mean smoothness_mean                                                                                                                     
     3         smoothness_mean compactness_mean concave_points_mean                                                                                          
     4         area_mean smoothness_mean compactness_mean fractal_dimension_mean                                                                             
     5         area_mean smoothness_mean compactness_mean concave_points_mean fractal_dimension_mean                                                         
     6         radius_mean perimeter_mean smoothness_mean compactness_mean concave_points_mean fractal_dimension_mean                                        
     7         radius_mean perimeter_mean area_mean smoothness_mean compactness_mean concave_points_mean fractal_dimension_mean                              
     8         radius_mean perimeter_mean area_mean smoothness_mean compactness_mean concavity_mean concave_points_mean fractal_dimension_mean               
     9         radius_mean perimeter_mean area_mean smoothness_mean compactness_mean concavity_mean concave_points_mean symmetry_mean fractal_dimension_mean 
                     Adj.        Pred                                                                                                
Model    R-Square    R-Square    R-Square     C(p)         AIC         SBIC         SBC         MSEP         FPE       HSP       APC  
--------------------------------------------------------------------------------------------------------------------------------------
  1        0.0547      0.0520      0.0446    11.9356    1986.9934    973.7834    1998.6266    5401.7487    15.2157    0.0427    0.9560 
  2        0.0625      0.0572      0.0475    10.9194    1986.0306    972.8266    2001.5415    5372.2795    15.1747    0.0426    0.9534 
  3        0.0796      0.0718       0.062     6.3043    1981.4451    968.3611    2000.8337    5289.0696    14.9811    0.0421    0.9412 
  4        0.0895      0.0791      0.0679     4.5046    1979.6067    966.6404    2002.8731    5247.4146    14.9041    0.0419    0.9364 
  5        0.0979      0.0851      0.0733     3.2309    1978.2662    965.4450    2005.4104    5213.3964    14.8483    0.0417    0.9329 
  6        0.0994      0.0839      0.0699     4.6795    1979.7005    966.9523    2010.7224    5220.0561    14.9081    0.0419    0.9367 
  7        0.1010      0.0829      0.0671     6.0691    1981.0731    968.4072    2015.9727    5225.8644    14.9656    0.0421    0.9403 
  8        0.1011      0.0804      0.0644     8.0067    1983.0089    970.4035    2021.7863    5239.9819    15.0470    0.0423    0.9454 
  9        0.1011      0.0778       0.059    10.0000    1985.0021    972.4547    2027.6572    5255.0256    15.1313    0.0425    0.9507 
--------------------------------------------------------------------------------------------------------------------"""





