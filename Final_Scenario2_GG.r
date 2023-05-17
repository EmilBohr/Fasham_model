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
param$Wlength <- 2*30 #length of ice coverage [days]


A0 <- c(3.800456,  3.932143,  4.190369 , 4.565361,  5.043659,  5.609317,  6.245161 , 6.933953 , 7.659329 , 8.406459,  9.162441,  9.916452, 10.659731, 11.385446, 12.088499, 12.765299, 13.413544, 14.032002,
        14.620319, 15.178838, 16.767650, 18.789011, 20.367228, 21.616196, 22.604134, 23.380663, 23.986624, 24.455906, 24.816028, 25.089051, 25.292608, 25.440788, 25.544851, 25.613793, 25.654810,
        25.673528)

N0 <- c(0.4599157,  0.4809184,  0.5223239,  0.5829859,  0.6613109,  0.7553856 , 0.8631158 , 0.9823575 , 1.1110286 , 1.2471937,  1.3891186,  1.5353000 , 1.6844726,  1.8356020,  1.9878672,  2.1406382,  2.2934514,
        2.4459845,  2.5980329,  2.7494878,  3.2019717,  3.9422750,  4.6786050,  5.4150374,  6.1520357,  6.8892783,  7.6265705,  8.3638675,  9.1011723,  9.8384904, 10.5758225, 11.3131668, 12.0505213, 12.7878838,
        13.5252525, 14.2626252)
DON0 <- c(3.477578e-05, 3.423801e-05, 3.269985e-05, 3.039544e-05, 2.763425e-05, 2.470644e-05, 2.184201e-05 ,1.919813e-05, 1.686497e-05, 1.488033e-05, 1.324550e-05 ,1.193920e-05, 1.092959e-05,
          1.018247e-05, 9.664603e-06, 9.345819e-06, 9.201403e-06, 9.214291e-06 ,9.382511e-06 ,9.760930e-06, 1.244896e-05, 1.822802e-05, 2.707202e-05, 3.897277e-05, 5.366747e-05, 7.064285e-05,
          8.920020e-05, 1.085479e-04, 1.279052e-04, 1.465905e-04, 1.640723e-04, 1.799588e-04, 1.938558e-04, 2.048334e-04, 2.099471e-04, 2.032400e-04)

P0 <- c(2.704982e-05, 5.434126e-05, 8.938645e-05, 1.270021e-04 ,1.603085e-04, 1.830998e-04, 1.918254e-04, 1.863337e-04, 1.693238e-04, 1.450398e-04, 1.178900e-04, 9.146225e-05, 6.808969e-05,
        4.887467e-05, 3.397460e-05, 2.296315e-05, 1.514648e-05, 9.785558e-06, 6.228960e-06, 4.001655e-06, 9.188180e-07, 1.927328e-07, 3.926301e-08, 7.866246e-09, 1.575163e-09, 3.219994e-10,
        6.871036e-11, 1.550475e-11, 3.680077e-12, 8.995365e-13, 2.207451e-13, 5.327346e-14, 1.247759e-14, 2.816215e-15, 6.112065e-16, 1.579331e-16)

B0 <- c(0.18237724, 0.18515930 ,0.19058378, 0.19839363 ,0.20823392, 0.21967560, 0.23224154, 0.24543309, 0.25875513, 0.27173852, 0.28395867, 0.29504970, 0.30471407, 0.31272762, 0.31894100,
        0.32327783, 0.32573043, 0.32635360, 0.32525636, 0.32259150, 0.31044150, 0.26986490 ,0.22726063, 0.18939004, 0.15735016, 0.13054558, 0.10815564, 0.08946641, 0.07388875, 0.06093098,
        0.05017867, 0.04128519, 0.03397636, 0.02808377, 0.02363223, 0.02099224)


Z0 <- c(1.952941e-02, 1.911455e-02, 1.831108e-02, 1.717027e-02, 1.576372e-02, 1.417611e-02, 1.249625e-02, 1.080799e-02, 9.182648e-03, 7.674301e-03, 6.318192e-03, 5.131935e-03, 4.118574e-03,
        3.270491e-03, 2.573284e-03, 2.009087e-03, 1.559074e-03, 1.205138e-03, 9.308736e-04, 7.220235e-04, 2.556625e-04, 5.428628e-05, 9.978569e-06, 1.613036e-06, 2.328378e-07, 3.040858e-08,
        3.632847e-09, 4.006862e-10, 4.111804e-11, 3.951686e-12, 3.576845e-13, 3.064283e-14, 2.495966e-15, 1.941941e-16, 1.451955e-17, 1.126632e-18)
D0 <- c(5.961013e-05, 1.129535e-04, 1.713173e-04, 2.347102e-04, 3.022697e-04, 3.723791e-04, 4.430252e-04, 5.122380e-04, 5.784585e-04, 6.407413e-04, 6.987847e-04, 7.528331e-04, 8.035181e-04,
        8.516956e-04, 8.983120e-04, 9.443188e-04, 9.906504e-04, 1.038368e-03, 1.089554e-03, 1.152145e-03, 1.493047e-03, 1.929801e-03, 2.416034e-03, 2.894750e-03, 3.307176e-03, 3.607307e-03,
        3.769993e-03 ,3.792095e-03, 3.688399e-03, 3.484931e-03, 3.212113e-03, 2.899351e-03, 2.571649e-03, 2.248144e-03 ,1.942320e-03 ,1.670693e-03)



# Define initial conditions - concentrations of N_P_DON_A_B_Z_D
INIT <- c(N0, P0, DON0, A0, B0, Z0, D0)

# Light function
CalLight <- function(t, P,D, param) {
  #  season <-(1-0.8 * sin(pi*param$theta/180) * cos(2*pi*t/365))
  a <- 0.5936313659
 # season <- 1+cos(pi+2*pi/365*t+4*pi/73)
  season <- 1 - (a - a*cos(pi*param$theta/90))*cos(4/73*pi + 2/365*pi*t)
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
  
  if (ice<0){
    ice <- 0
  }
  if (ice>1){
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

N2m <- res[,2:(param$n+1)]
P2m <- res[,(2+param$n):(2*param$n+1)]
DON2m <- res[,(2*param$n+2):(3*param$n+1)]
A2m <- res[,(3*param$n+2):(4*param$n+1)]
B2m <- res[,(4*param$n+2):(5*param$n+1)]
Z2m <- res[,(5*param$n+2):(6*param$n+1)]
D2m <- res[,(6*param$n+2):(7*param$n+1)]

# Martin curve GG
# MARTIN CURVE WITH AVERAGES
Fav2m <-  rep(0,length(param$z))

ns <- 1:length(param$z)

for (i in 1:length(param$z)){
  Fav2m[i] <- mean(param$uD * D2m[(9*365+1):(10*365),ns[i]])
}
Fav2m



# PHGYTOPLANKTON production

t <- (9*365+1):3650

Pc2m <- P2m[t,]
Dc2m <- D2m[t,]
Nc2m <- N2m[t,]
Ac2m <- A2m[t,]


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

    damp[i,j] <- param$kc * param$dz[j] * (sum(Pc2m[i,1:j]) - Pc2m[i,j]/2+sum(Dc2m[i,1:j])+Dc2m[i,j]/2)
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
Q <- Nc2m * exp(-param$Sigma*Ac2m) / (param$HP + Nc2m) + Ac2m / (param$HP + Ac2m)


# Light and nutrient limitation
sig <- J * Q


Prod22m <-(1-param$gamma) * sig * Pc2m

sum(Prod22m)


kl2m <- rep(0,365)
kp2m <- rep(0,365)
pi2m <- rep(0,365)

for (i in 1:365){
  kl2m[i] <- sum(Prod22m[i,])
  kp2m[i] <- sum(Pc2m[i,])
  pi <- sum(Pc2m[i,])
}
par(mfrow=c(1,1))
plot(t-9*365,kl2m,type="l",lwd=2)

max(kl2m)
length(kl2m)

write.csv(kl2m, "klm2.csv")


## Calculate change
product <- data.frame("day"=seq(1,365, by=1), "production"=rep(0,365))

#Subset 1 year
Pyear <- P[(365*5+1):(6*365),]

for (i in 1:364){
  product$production[i] <- sum(Pc[i+1,]) - sum(Pc[i,])
  
}

max(product$production)


plot(product$day, product$production, type="l", ylab="P biomass change (mmol N m-3 d-1)", xlab="Day")




### Zooplankton production
t <- (9*365+1):3650

Pc <- P2m[t,]
Dc <- D2m[t,]
Nc <- N2m[t,]
Ac <- A2m[t,]
Bc <- B2m[t,]
DONc <- DON2m[t,]
Zc <- Z2m[t,]

food <- param$pP*Pc + param$pB*Bc + param$pD*Dc

# Zooplankton grazing function on phytoplankton
GrazP <- param$gZ * Zc * (param$pP * Pc) / (param$HZ + food)

# Zooplankton grazing function on bacteria
GrazB <- param$gZ * Zc * (param$pB * Bc) / (param$HZ + food)

# Zooplankton grazing function on detritus
GrazD <- param$gZ * Zc * (param$pD * Dc) / (param$HZ + food)


ProdZ2m <- param$beta*GrazP + param$beta*GrazB + param$beta*GrazD

Zl2m <- rep(0,365)
Zp2m <- rep(0,365)
Zi2m <- rep(0,365)

for (i in 1:365){
  Zl2m[i] <- sum(ProdZ2m[i,])
  Zp2m[i] <- sum(Zc[i,])
  
}
par(mfrow=c(1,1))
plot(t-9*365,Zl2m,type="l",lwd=2)

max(Zl2m)

sum(ProdZ2m)
