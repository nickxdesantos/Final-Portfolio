#################### - Data - ###################################

A <- c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
B <- c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1)
C <- c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1)
D <- c(-1,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,-1,1)
AB <- A*B
AC <- A*C
AD <- A*D
Density <- c(1.8024,1.8870,1.9938,1.8526,1.9174,1.8074,1.8106,1.8617,1.8016,1.8900,1.9904,1.8503,1.919,1.8067,1.8118,1.8603)
Microhardness <- c(53.76,77.85,94.37,89.10,74.13,63.28,67.02,65.82,54.95,77.74,93.55,90.74,73.67,62.08,67.74,64.45)

################ - Effects - #######################################
#For Density
d.effect.A <- mean(Density[which(A == 1)])-mean(Density[which(A == -1)]) #-0.028875
d.effect.B <- mean(Density[which(B == 1)])-mean(Density[which(B == -1)]) #0.025
d.effect.C <- mean(Density[which(C == 1)])-mean(Density[which(C == -1)]) #-0.03415
d.effect.D <- mean(Density[which(D == 1)])-mean(Density[which(D == -1)]) #0.097025
d.effect.AB <- mean(Density[which(AB == 1)])-mean(Density[which(AB == -1)]) #-0.01655
d.effect.AC <- mean(Density[which(AC == 1)])-mean(Density[which(AC == -1)]) #-0.0018
d.effect.AD <- mean(Density[which(AD == 1)])-mean(Density[which(AD == -1)]) #-0.051525

#For Micro Hardness
m.effect.A <- mean(Microhardness[which(A == 1)])-mean(Microhardness[which(A == -1)]) #1.48375
m.effect.B <- mean(Microhardness[which(B == 1)])-mean(Microhardness[which(B == -1)]) #11.91625
m.effect.C <- mean(Microhardness[which(C == 1)])-mean(Microhardness[which(C == -1)]) #-11.73375
m.effect.D <- mean(Microhardness[which(D == 1)])-mean(Microhardness[which(D == -1)]) #9.11375
m.effect.AB <- mean(Microhardness[which(AB == 1)])-mean(Microhardness[which(AB == -1)]) #-4.62625
m.effect.AC <- mean(Microhardness[which(AC == 1)])-mean(Microhardness[which(AC == -1)]) #-8.21625
m.effect.AD <- mean(Microhardness[which(AD == 1)])-mean(Microhardness[which(AD == -1)]) #-13.94875

###################### - Regression - ###########################################

#for Density
density.model <- lm(Density~A+B+C+D+AB+AC+AD)
summary(density.model)

#              Estimate Std. Error  t value Pr(>|t|)    
#(Intercept)  1.8664375  0.0003587 5203.213  < 2e-16
# A           -0.0144375  0.0003587  -40.249 1.60e-10 
# B            0.0125000  0.0003587   34.847 5.03e-10 
# C           -0.0170750  0.0003587  -47.601 4.20e-11 
# D            0.0485125  0.0003587  135.242 9.99e-15 
# AB          -0.0082750  0.0003587  -23.069 1.32e-08 
# AC          -0.0009000  0.0003587   -2.509   0.0364  
# AD          -0.0257625  0.0003587  -71.820 1.57e-12 

#Residual standard error: 0.001435 on 8 degrees of freedom
#Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9995 
#F-statistic:  4155 on 7 and 8 DF,  p-value: 1.34e-13

#For micro hardness
microhardness.model <- lm(Microhardness~A+B+C+D+AB+AC+AD)
summary(microhardness.model)


#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  73.1406     0.1858 393.662  < 2e-16 
# A             0.7419     0.1858   3.993  0.00399 
# B             5.9581     0.1858  32.068 9.74e-10
# C            -5.8669     0.1858 -31.577 1.10e-09 
# D             4.5569     0.1858  24.526 8.16e-09 
# AB           -2.3131     0.1858 -12.450 1.62e-06 
# AC           -4.1081     0.1858 -22.111 1.85e-08 
# AD           -6.9744     0.1858 -37.538 2.78e-10

#Residual standard error: 0.7432 on 8 degrees of freedom
#Multiple R-squared:  0.9983,	Adjusted R-squared:  0.9968 
#F-statistic: 670.9 on 7 and 8 DF,  p-value: 1.956e-10

##################### - Diagnostics - #################################
#for Density
plot(density.model)

#for Microhardness
plot(microhardness.model)

names(density.model)
plot(density.model$residuals)
plot(density.model)
