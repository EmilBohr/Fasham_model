################ Project 2 - Fasham
library(deSolve)
library(fields)

# Create an empty list to add the parameters
param <- list()

# Define parameters
param$Dv <- 10.8*2 # Diffusivity (m^2/d)
param$PAR0 <- 0.41 # unit less, PAR/ total irradiance
param$I0 <- 200 # W/m2, annual average surface irradiance 
param$C <- 4 # Oktas, cloudinessl
param$kw <- 0.04 # 1/m, light attenuation due to water
param$gP <- 2 # 1/d, phytoplankton maximum growth rate
param$alpha <- 0.025 # 1/(W/m2) / d, initial slope of the P-I curve
param$HP <- 0.5 # mMol/m3, half saturation for phytoplankton nutrient uptake
param$mP <- 0.1 # 1/d,phytoplankton specific mortality rate
param$kc <- 0.03 # m2/(mMol N), light attenuation by phytoplankton and detritus
param$gamma <- 0.03 # phytoplankton, exudation fraction
param$Sigma <- 1.5 # 1/(mMol N), NH4 inhibition parameter for phytoplankton
param$gZ <- 0.5 # 1/d, zooplankton maximum growth rate
param$beta <- 0.75 # zooplankton assimilation efficiency
param$muZ <- 0.025 # 1/d, zooplankton specific excretion rate
param$mZ <- 0.04 # 1/d, zooplankton specific mortality rate
param$HZ <- 1 # mMol/m3, zooplankton half saturation for ingestion
param$Omega <- 0.33 # detrital fraction of zooplankton mortality
param$eps <- 0.75 # ammonium fraction of zooplankton excretion
param$gB <- 2 # 1/d, bacterial maximum growth rate
param$muB <- 0.0125 # 1/d, bacterial specific excretion rate
param$HB <- 0.1 # (mMol N) / m3, bacterial half saturation for uptake
param$eta <- 0.6 # NH4/DON uptake ratio (bacteria?)
param$mD <- 0.05 # 1/d, detrital breakdown rate
param$uD <- 10 # m/d, detrital sinking rate 
param$uP <- 1 # m/d, Phytoplankton sinking rate 
param$dz <- c(rep(10,20),rep(50,16)) # grid spacing (m)
param$z <- c(seq(5,200, by=10),seq(225,1000,by=50)) # depth (m)
param$n <- length(param$z) # number of grid cells
param$theta <- 70 # degrees north, latitude
param$pP <- 0.7 # preference for phytoplankton
param$pB <- 0.1 # preference for bacteria
param$pD <- 0.2 # preference for detritus
param$Nb <- 15 # nutrient content at the bottom (mmol/m^3)
param$dzb <- 50
param$Wcenter <- 60  #Winter start day
param$Wlength <- 4*30 #length of ice coverage [days]

# Define initial conditions - concentrations of N_P_DON_A_B_Z_D


A0 <- c(4.515675,  4.659412,  4.941237,  5.350410,  5.872103,  6.488694,  7.181135,  7.930229,  8.717676,  9.526838, 10.343202,
        11.154591, 11.951183, 12.725384, 13.471628, 14.186133, 14.866644, 15.512180, 16.122795, 16.699357, 18.331318, 20.380168,
        21.976303, 23.237257, 24.229720, 25.003974, 25.602742, 26.061726, 26.409865, 26.670311, 26.861554, 26.998377, 27.092591,
        27.153621, 27.189013, 27.204750)

Z0 <- c(2.142023e-02, 2.098607e-02, 2.014563e-02, 1.895284e-02, 1.748225e-02, 1.582108e-02, 1.405992e-02, 1.228372e-02,
        1.056447e-02, 8.956873e-03, 7.496986e-03, 6.203606e-03, 5.081338e-03, 4.124424e-03, 3.320497e-03, 2.653739e-03,
        2.107221e-03, 1.664427e-03, 1.310115e-03, 1.030685e-03, 3.813613e-04, 8.056039e-05, 1.416177e-05, 2.133761e-06,
        2.822541e-07, 3.339400e-08, 3.585332e-09, 3.533797e-10, 3.227622e-11, 2.753160e-12, 2.207824e-13, 1.674412e-14,
        1.208193e-15, 8.358667e-17, 5.621874e-18, 4.049431e-19)

P0 <- c(1.868697e-05, 3.752231e-05, 6.166908e-05, 8.751959e-05, 1.103087e-04, 1.257617e-04, 1.314626e-04, 1.273575e-04,
        1.153603e-04, 9.843709e-05, 7.964607e-05, 6.145818e-05, 4.546277e-05, 3.239201e-05, 2.232521e-05, 1.494330e-05,
        9.749552e-06, 6.223294e-06, 3.910114e-06, 2.480030e-06, 5.537359e-07, 1.145265e-07, 2.392545e-08, 5.300273e-09,
        1.292902e-09, 3.451485e-10, 9.635415e-11, 2.678766e-11, 7.186493e-12, 1.832933e-12, 4.422608e-13, 1.009517e-13,
        2.184744e-14, 4.496273e-15, 8.834067e-16, 2.014287e-16)

D0 <- c(6.238284e-05, 1.170439e-04, 1.755467e-04, 2.378342e-04, 3.031769e-04, 3.702633e-04, 4.374678e-04, 5.031834e-04,
        5.661033e-04, 6.253846e-04, 6.806827e-04, 7.320889e-04, 7.800188e-04, 8.250931e-04, 8.680367e-04, 9.096109e-04,
        9.505934e-04, 9.918877e-04, 1.035233e-03, 1.087048e-03, 1.362764e-03, 1.718342e-03, 2.125068e-03, 2.540854e-03,
        2.917248e-03, 3.211758e-03, 3.396222e-03, 3.460132e-03, 3.409332e-03, 3.261663e-03, 3.041478e-03, 2.774607e-03,
        2.484658e-03, 2.190920e-03, 1.907912e-03, 1.653156e-03)

DON0 <- c(3.875936e-05, 3.805200e-05, 3.616964e-05, 3.340505e-05, 3.013593e-05, 2.670951e-05, 2.339265e-05, 2.035909e-05,
          1.770029e-05, 1.544619e-05, 1.358659e-05, 1.208889e-05, 1.091082e-05, 1.000877e-05, 9.342467e-06, 8.877375e-06,
          8.585593e-06, 8.446741e-06, 8.453005e-06, 8.638638e-06, 1.038180e-05, 1.470588e-05, 2.157952e-05, 3.099547e-05,
          4.283586e-05, 5.680066e-05, 7.240285e-05, 8.901835e-05, 1.059704e-04, 1.226171e-04, 1.384167e-04, 1.529476e-04,
          1.658229e-04, 1.762698e-04, 1.818117e-04, 1.770801e-04)

N0 <- c(0.5177186,  0.5394547,  0.5823008,  0.6450587,  0.7260563,  0.8232766,  0.9344997,  1.0574399,  1.1898658,  1.3296935,
        1.4750517,  1.6243182,  1.7761348,  1.9294021,  2.0832643,  2.2370845,  2.3904173,  2.5429790,  2.6946195,  2.8452943,
        3.2945324,  4.0272263,  4.7568318,  5.4872583,  6.2184752,  6.9499827,  7.6815719,  8.4132104,  9.1449050,  9.8766577,
        10.6084637, 11.3403157, 12.0722062, 12.8041276, 13.5360721, 14.2680322)

B0 <- c(0.17931210, 0.18247650, 0.18866956, 0.19763920, 0.20903113, 0.22240620, 0.23726104, 0.25305112, 0.26921553, 0.28520175,
        0.30048926, 0.31461017, 0.32716592, 0.33783937, 0.34640213, 0.35271729, 0.35673804, 0.35850228, 0.35812368, 0.35577897,
        0.34351471, 0.29787956, 0.25032043, 0.20861604, 0.17357746, 0.14431359, 0.11985440, 0.09940887, 0.08233662, 0.06810581,
        0.05626873, 0.04645035, 0.03835214, 0.03179017, 0.02680397, 0.02384295)


INIT <- c(N0, P0, DON0, A0, B0, Z0, D0)

# Light function
CalLight <- function(t, P,D, param) {
  #  season <-(1-0.8 * sin(pi*param$theta/180) * cos(2*pi*t/365))
  a <- 0.5936313662
 # season <- 1+cos(pi+2*pi/365*t+4*pi/73)
  season <- 1 - (0.5936313659 - 0.5936313659*cos(pi*param$theta/90))*cos(4/73*pi + 2/365*pi*t)
  if(season<0){
    season <- 0
  }
  if(season>2){
    season <- 2
  }
  
  # Ice
    b <- param$Wcenter-param$Wlength/2
   c <- sin(pi*b/(365/2)+pi*25/146)
  d <- 1/(sin(pi*(param$Wcenter-param$Wlength/6)/(365/2)+pi*25/146)-c)
  ice <- -c*d+d*sin(pi*t/(365/2)+pi*25/146)

  if(ice < 0){
    ice <- 0
  }
  if(ice >1){
    ice <- 1
  }
  
  damp <- param$kc * param$dz * (cumsum(P) - P/2+cumsum(D)+D/2)
  
  I <- param$I0 * param$PAR0 * exp(-param$kw * param$z - damp) * season* (1-ice)
  
  return(I)
}


# Create function that creates the differential equation at each step
FASHAM <- function(t, INIT, param) {
  N <- INIT[1:param$n]
  P <- INIT[(1+param$n):(2*param$n)]
  DON <- INIT[(2*param$n+1):(3*param$n)]
  A <- INIT[(3*param$n+1):(4*param$n)]
  B <- INIT[(4*param$n+1):(5*param$n)]
  Z <- INIT[(5*param$n+1):(6*param$n)]
  D <- INIT[(6*param$n+1):(7*param$n)]
  FD <- INIT[(7*param$n+1):(8*param$n)]
  
  # Advection and diffusion
  #####
  # Nitrate flux
  JdN <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdN[i] <- -param$Dv * (N[i] - N[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdN[1] = 0
  JdN[param$n+1] = -param$Dv*(param$Nb-N[param$n])/param$dzb
  
  JN = JdN
  
  # Phytoplankton flux
  JaP <- rep(0,param$n+1)
  JdP <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JaP[i] <- param$uP * P[i-1]
    JdP[i] <- -param$Dv * (P[i] - P[i-1]) / (param$dz[i]+param$dz[i-1])/2
  }
  
  # Advective flux boundary
  JaP[1] = 0
  JaP[param$n+1] = 0
  
  # Diffusive flux boundary
  JdP[1] = 0
  JdP[param$n+1] = 0
  
  JP = JaP + JdP
  
  
  # DON flux
  JdDON <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdDON[i] <- -param$Dv * (DON[i] - DON[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdDON[1] = 0
  JdDON[param$n+1] = 0
  
  JDON = JdDON
  
  
  # Ammonium flux
  JdA <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdA[i] <- -param$Dv * (A[i] - A[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdA[1] = 0
  JdA[param$n+1] = 0
  
  JA = JdA
  
  # Bacteria flux
  JdB <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdB[i] <- -param$Dv * (B[i] - B[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdB[1] = 0
  JdB[param$n+1] = 0
  
  JB = JdB
  
  # Zooplankton flux
  JdZ <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JdZ[i] <- -param$Dv * (Z[i] - Z[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  
  # Diffusive flux boundary
  JdZ[1] = 0
  JdZ[param$n+1] = 0
  
  JZ = JdZ
  
  
  # Detritus flux
  JaD <- rep(0,param$n+1)
  JdD <- rep(0,param$n+1)
  
  for (i in 2:param$n){
    JaD[i] <- param$uD * D[i-1]
    JdD[i] <- -param$Dv * (D[i] - D[i-1]) / ((param$dz[i]+param$dz[i-1])/2)
  }
  
  # Advective flux boundary
  JaD[1] = 0
  JaD[param$n+1] = param$uD * D[param$n]
  
  # Diffusive flux boundary
  JdD[1] = 0
  JdD[param$n+1] = 0
  
  JD = JaD + JdD
  
  
  #####
  
  
  # Call the light function
  I <- CalLight(t, P,D, param)
  
  # Function describing P-I curve (Light limited growth rate)
  J <- param$gP * param$alpha * I / (param$gP^2 + param$alpha^2 * I^2)^(0.5)
  
  # Nutrient limiting growth rate
  Q <- N * exp(-param$Sigma*A) / (param$HP + N) + A / (param$HP + A)
  
  # Nutrient limiting growth rate (nitrate)
  Q1 <- N * exp(-param$Sigma*A) / (param$HP + N)
  
  # Nutrient limiting growth rate (Ammonium)
  Q2 <- A / (param$HP + A)
  
  # Light and nutrient limitation
  sig <- J * Q
  
  # Total food available 
  food <- param$pP*P + param$pB*B + param$pD*D 
  
  # Zooplankton grazing function on phytoplankton
  GrazP <- param$gZ * Z * (param$pP * P) / (param$HZ + food)
  
  # Zooplankton grazing function on bacteria
  GrazB <- param$gZ * Z * (param$pB * B) / (param$HZ + food)
  
  # Zooplankton grazing function on detritus
  GrazD <- param$gZ * Z * (param$pD * D) / (param$HZ + food)
  
  # Differential equation for phytoplankton 
  dPdt <- (1-param$gamma) * sig * P - GrazP - param$mP*P -(JP[2:(param$n+1)] - JP[1:param$n]) / param$dz 
  
  # Differential equation for zooplankton 
  dZdt <- param$beta*GrazP + param$beta*GrazB + param$beta*GrazD - param$muZ*Z - param$mZ*Z-(JZ[2:(param$n+1)] - JZ[1:param$n]) / param$dz 
  
  # Total bacterial nitrogenous substrate S
  S <- min(A,param$eta*DON)
  
  # Bacterial DON uptake
  U1 <- param$gB*B*DON / (param$HB + S + DON)
  
  # Bacterial ammonia uptake
  U2 <- param$gB*B*S / (param$HB + S + DON)
  
  # Differential equation for bacteria
  dBdt <- U1 + U2 - GrazB - param$muB * B -(JB[2:(param$n+1)] - JB[1:param$n]) / param$dz 
  
  # Differential equation for detritus
  dDdt <- (1-param$beta)*GrazP + (1-param$beta)*GrazB - param$beta*GrazD - param$mD*D + param$mP*P -(JD[2:(param$n+1)] - JD[1:param$n]) / param$dz 
  
  # Differential equation for nitrate
  dNdt <- -(J*Q1) * P -(JN[2:(param$n+1)] - JN[1:param$n]) / param$dz 
  
  # Differential equation for ammonium
  dAdt <- -(J*Q2) * P - U2 + param$muB*B + (param$eps*param$muZ+(1-param$Omega)*param$mZ) * Z -(JA[2:(param$n+1)] - JA[1:param$n]) / param$dz 
  
  # Differential equation for dissolved organic nitrogen (DON)
  dDONdt <- param$gamma*sig*P + param$mD*D + (1-param$eps)*param$muZ*Z - U1 -(JDON[2:(param$n+1)] - JDON[1:param$n]) / param$dz 

  return(list(c(dNdt, dPdt, dDONdt, dAdt, dBdt, dZdt, dDdt)))
}

# Define time step
time <- seq(1,365*10, by=1)

# Solve our differential equations
# How much time does the run take?
start.time <- Sys.time()
res <- ode(INIT, time, FASHAM, param)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Runtime=",as.character(round(time.taken,1)),"s"))
#,method = "ode45", atol = 1e-14, rtol = 1e-14

N <- res[,2:(param$n+1)]
P <- res[,(2+param$n):(2*param$n+1)]
DON <- res[,(2*param$n+2):(3*param$n+1)]
A <- res[,(3*param$n+2):(4*param$n+1)]
B <- res[,(4*param$n+2):(5*param$n+1)]
Z <- res[,(5*param$n+2):(6*param$n+1)]
D <- res[,(6*param$n+2):(7*param$n+1)]



#Fluxes for martin curve
FV <-  rep(0,length(param$z))

ns <- 1:length(param$z)

for (i in 1:length(param$z)){
  FV[i] <- sum(param$uD * D[(9*365+1):(10*365),ns[i]])
}
FV

plot(FV, param$z, ylim = rev(range(param$z)), type="l")

# MARTIN CURVE WITH AVERAGES
Fav <-  rep(0,length(param$z))

ns <- 1:length(param$z)

for (i in 1:length(param$z)){
  Fav[i] <- mean(param$uD * D[(9*365+1):(10*365),ns[i]])
}
Fav

#write.csv(Fav, "Fav4m.csv")

plot(Fav, param$z, ylim = rev(range(param$z)), type="l")

Fav2m <- read.csv("Fav2m.csv")
Fav6m <- read.csv("Fav6m.csv")
Fav4m <- read.csv("Fav4m.csv")
max(Fav4m)



# Plots of season function

#Season plot
xx <- seq(1,365, by=1)
season <- 1 - (0.5936313659 - 0.5936313659*cos(pi*70/90))*cos(4/73*pi + 2/365*pi*xx)

for (i in 1:length(xx)){
  if(season[i]<0){
    season[i] <- 0
  }
  if(season[i]>2){
    season[i] <- 2
  }
}

#seasonxx <- 1 - (0.5936313659 - 0.5936313659*cos(pi*param$theta/90))*cos(4/73*pi + 2/365*pi*xx)

plot(xx, season, type="l", col="orange", lwd=2, ylab=substitute(paste(bold("Seasonality factor"))), xlab=substitute(paste(bold("Day"))))
#points(xx,seasonxx, type="l", col="gold", lwd=2)

# Ice
icesvarl <- c(2*30, 4*30, 6*30)
monthsce <- c(2,4,6)
ices_names <- paste0("ice_scenario_", monthsce)

for (l in 1:length(icesvarl)){
  param$Wcenter <- 60  #Winter start day
  param$Wlength <- icesvarl[l] #length of ice coverage [days]
  
  bx <- param$Wcenter-param$Wlength/2
  cx <- sin(pi*bx/(365/2)+pi*25/146)
  dx <- 1/(sin(pi*(param$Wcenter-param$Wlength/6)/(365/2)+pi*25/146)-cx)
  icex <- -cx*dx+dx*sin(pi*xx/(365/2)+pi*25/146)
  
  for (i in 1:length(xx)){
    if(icex[i] < 0){
      icex[i] <- 0
    }
    if(icex[i] >1){
      icex[i] <- 1
    }
  }
  
  assign(ices_names[l], icex)
  
}

plot(xx, ice_scenario_2, type="l", lwd=2, col="cyan", xlab=substitute(paste(bold("Day"))), ylab=substitute(paste(bold("Ice cover (%)"))))
points(xx, ice_scenario_4, type="l", lwd=2, col="lightblue", xlab="", ylab="")
points(xx, ice_scenario_6, type="l", lwd=2, col="blue3", xlab="", ylab="")
legend(200, 1, legend = c(substitute(paste(bold("2 months ice"))), substitute(paste(bold("4 months ice"))), substitute(paste(bold("6 months ice")))), col = c("cyan", "lightblue", "blue3"), lty = c(1,1,1), lwd=2, cex = 0.8, bty="n")

totef_names <- paste0("totef_", monthsce)

for (i in 1:length(monthsce)){
  totef <- season*(1-get(ices_names[i]))*param$I0
  assign(totef_names[i], totef)
}

lightir <- bquote(bold(Light~irradiance~at~surface~(W~m^-2)))

plot(xx, totef_2, type="l", xlab=substitute(paste(bold("Day"))), ylab=lightir, lwd=2, col="lightgoldenrod1")
points(xx, totef_4, type="l", lwd=2, col="lightgoldenrod3")
points(xx, totef_6, type="l", lwd=2, col="lightgoldenrod4")
legend(235, 400, legend = c(substitute(paste(bold("2 months ice"))), substitute(paste(bold("4 months ice"))), substitute(paste(bold("6 months ice")))), col = c("lightgoldenrod1", "lightgoldenrod3", "lightgoldenrod4"), lty = c(1,1,1), lwd=2, cex = 0.8, bty="n")


### Phytoplankton production

t <- (9*365+1):3650

Pc <- P[t,]
Dc <- D[t,]
Nc <- N[t,]
Ac <- A[t,]

#  season <-(1-0.8 * sin(pi*param$theta/180) * cos(2*pi*t/365))
a <- 0.5936313659
# season <- 1+cos(pi+2*pi/365*t+4*pi/73)
season <- 1 - (a - a*cos(pi*param$theta/90))*cos(4/73*pi + 2/365*pi*t)
for (i in 1:365){ 
  if(season[i]<0){
    season[i] <- 0
  }
  if(season[i]>2){
    season[i] <- 2
  }
}
# Ice
b <- param$Wcenter-param$Wlength/2
c <- sin(pi*b/(365/2)+pi*25/146)
d <- 1/(sin(pi*(param$Wcenter-param$Wlength/6)/(365/2)+pi*25/146)-c)
ice <- -c*d+d*sin(pi*t/(365/2)+pi*25/146)
for (i in 1:365){
  if (ice[i]<0){
    ice[i] <- 0
  }
  if (ice[i]>1){
    ice[i] <- 1
  }
}
damp <- matrix(0,ncol = 36,nrow=365)
for (i in 1:364){
  
  for (j in 1:36){
    
    damp[i,j] <- param$kc * param$dz[j] * (sum(Pc[i,1:j]) - Pc[i,j]/2+sum(Dc[i,1:j])+Dc[i,j]/2)
  }
}


Law <- matrix(0,ncol = 36,nrow=365)
for (i in 1:365){
  Law[i,] <- param$kw * param$z
}

# Call the light function
I <- param$I0 * param$PAR0 * exp(-Law - damp) * season* (1-ice)




# Function describing P-I curve (Light limited growth rate)
J <- param$gP * param$alpha * I / (param$gP^2 + param$alpha^2 * I^2)^(0.5)

# Nutrient limiting growth rate
Q <- Nc * exp(-param$Sigma*Ac) / (param$HP + Nc) + Ac / (param$HP + Ac)


# Light and nutrient limitation
sig <- J * Q


Prod2 <-(1-param$gamma) * sig * Pc

sum(Prod2)

sum(Prod2)  #units in P d-1

min(Prod2) # as expected is 0

kl <- rep(0,365)
kp <- rep(0,365)
pi <- rep(0,365)

for (i in 1:365){
  kl[i] <- sum(Prod2[i,])
  kp[i] <- sum(Pc[i,])
  pi <- sum(Pc[i,])
}
par(mfrow=c(1,1))
plot(t-9*365,kl, type="l")

max(kl)

write.csv(kl, "klm4.csv")

#Read other productions
kl2m <- read.csv("klm2.csv")
kl6m <- read.csv("klm6.csv")

phytpC <- bquote(bold(Phytoplankton~production~(mmol~C~m^-2~d^-1)))

par(mfrow=c(1,1))
par(mar = c(4, 4, 4, 4) + 0.5) 
plot(t-9*365,kl*106/16,type="l",lwd=2, ylab=phytpC, col="darkolivegreen3", xlab=substitute(paste(bold("Day"))))
points(t-9*365,kl6m[1:365,2]*106/16,type="l",lwd=2, ylab="", col="darkolivegreen", xlab="")
points(t-9*365,kl2m[1:365,2]*106/16,type="l",lwd=2, ylab="", col="darkolivegreen1", xlab="")
legend(250, 25, legend = c(substitute(paste(bold("2 months ice"))), substitute(paste(bold("4 months ice"))), substitute(paste(bold("6 months ice")))), col = c("darkolivegreen1", "darkolivegreen3", "darkolivegreen"), lty = c(1,1,1), lwd=2, cex = 0.8, bty="n")





### zooplankton production
t <- (9*365+1):3650

Pc <- P[t,]
Dc <- D[t,]
Nc <- N[t,]
Ac <- A[t,]
Bc <- B[t,]
DONc <- DON[t,]
Zc <- Z[t,]

food <- param$pP*Pc + param$pB*Bc + param$pD*Dc

# Zooplankton grazing function on phytoplankton
GrazP <- param$gZ * Zc * (param$pP * Pc) / (param$HZ + food)

# Zooplankton grazing function on bacteria
GrazB <- param$gZ * Zc * (param$pB * Bc) / (param$HZ + food)

# Zooplankton grazing function on detritus
GrazD <- param$gZ * Zc * (param$pD * Dc) / (param$HZ + food)


ProdZ <- param$beta*GrazP + param$beta*GrazB + param$beta*GrazD

Zl <- rep(0,365)
Zp <- rep(0,365)
Zi <- rep(0,365)

for (i in 1:365){
  Zl[i] <- sum(ProdZ[i,])
  Zp[i] <- sum(Zc[i,])
  
}

zpr <- bquote(bold(Zooplankton~production~(mmol~N~m^-2~d^-1)))

par(mfrow=c(1,1))
par(mar = c(4, 4, 4, 4) + 0.5) 
plot(t-9*365,Zl6m,type="l",lwd=2, ylab=zpr, col="royalblue4", xlab=substitute(paste(bold("Day"))))
points(t-9*365,Zl,type="l",lwd=2, ylab=zpr, col="royalblue1", xlab="")
points(t-9*365,Zl2m,type="l",lwd=2, ylab=zpr, col="lightskyblue1", xlab="")
legend(0, 0.6, legend = c(substitute(paste(bold("2 months ice"))), substitute(paste(bold("4 months ice"))), substitute(paste(bold("6 months ice")))), col = c("lightskyblue1", "royalblue1", "royalblue4"), lty = c(1,1,1), lwd=2, cex = 0.8, bty="n")

# Transfor to carbon with redfield ratio
zprC <- bquote(bold(Zooplankton~production~(mmol~C~m^-2~d^-1)))

par(mfrow=c(1,1))
par(mar = c(4, 4, 4, 4) + 0.5) 
plot(t-9*365,Zl6m*106/16,type="l",lwd=2, ylab=zprC, col="royalblue4", xlab=substitute(paste(bold("Day"))))
points(t-9*365,Zl*106/16,type="l",lwd=2, ylab="", col="royalblue1", xlab="")
points(t-9*365,Zl2m*106/16,type="l",lwd=2, ylab="", col="lightskyblue1", xlab="")
legend(0, 0.6*106/16, legend = c(substitute(paste(bold("2 months ice"))), substitute(paste(bold("4 months ice"))), substitute(paste(bold("6 months ice")))), col = c("lightskyblue1", "royalblue1", "royalblue4"), lty = c(1,1,1), lwd=2, cex = 0.8, bty="n")


max(Zl)

sum(ProdZ)



