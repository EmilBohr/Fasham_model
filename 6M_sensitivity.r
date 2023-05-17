########################### DAY 2 ###########################
library(deSolve)
library(fields)


sensitivity <- function(x){
  # Create an empty list to add the parameters
  param <- list()
  
  # Define parameters
  param$PAR0 <- 0.41 # unit less, PAR/ total irradiance
  param$I0 <- 200 # W/m2, annual average surface irradiance 
  param$C <- 4 # Oktas, cloudinessl
  param$kw <- 0.04 # 1/m, light attenuation due to water
  param$gP <- 2 # 1/d, phytoplankton maximum growth rate
  param$alpha <- 0.025 # 1/(W/m2) / d, initial slope of the P-I curve
  param$HP <- 0.5 # mMol N/m3, half saturation for phytoplankton nutrient uptake
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
  param$Wlength <- 6*30 #length of ice coverage [days]
  
 
  
  P_res <- list()
  N_res <- list()
  D_res <- list()
  
  
  
  for (i in 1:(length(x))){ 
    param$Dv <- x[i] # Diffusivity (m^2/d)
    
    
    N0 <- c(0.5741008, 0.5966877, 0.6412008, 0.7063719, 0.7904262,  0.8912137,  1.0063571,  1.1333972,  1.2699233,
            1.4136786,  1.5626360,  1.7150452,  1.8694529,  2.0247023,  2.1799167,  2.3344718,  2.4879628,  2.6401680,
            2.7910128,  2.9405358,  3.3855024,  4.1103471,  4.8334796,  5.5581139, 6.2836851,  7.0095686,  7.7355646,
            8.4616555, 9.1878492,  9.9141432, 10.6405268, 11.3669875, 12.0935125, 12.8200891, 13.5467044, 14.2733457)
    P0 <- c(1.359905e-05, 2.726776e-05, 4.470491e-05, 6.322314e-05, 7.932680e-05, 8.994052e-05, 9.340453e-05, 8.980849e-05,
            8.065832e-05, 6.817520e-05, 5.458569e-05, 4.163948e-05, 3.041920e-05, 2.138185e-05, 1.452369e-05, 9.571936e-06,
            6.144731e-06, 3.858222e-06, 2.385870e-06, 1.493541e-06, 3.369182e-07, 7.861111e-08, 2.155713e-08, 6.756455e-09,
            2.192816e-09, 6.844442e-10, 1.991596e-10, 5.363123e-11, 1.339545e-11, 3.119933e-12, 6.816872e-13, 1.405316e-13,
            2.747760e-14, 5.119312e-15, 9.129665e-16, 1.862683e-16)
    DON0 <- c(4.085559e-05, 4.009135e-05, 3.807320e-05, 3.512770e-05, 3.166806e-05, 2.806605e-05, 2.459890e-05, 2.143958e-05,
              1.867277e-05, 1.632002e-05, 1.436405e-05, 1.276753e-05, 1.148569e-05, 1.047405e-05, 9.693430e-06, 9.110722e-06,
              8.699048e-06, 8.439312e-06, 8.322093e-06, 8.367805e-06, 9.449220e-06, 1.283523e-05, 1.837012e-05, 2.598115e-05,
              3.562677e-05, 4.715466e-05, 6.025044e-05, 7.445801e-05, 8.924285e-05, 1.040687e-04, 1.184660e-04, 1.320727e-04,
              1.445989e-04, 1.555105e-04, 1.628240e-04, 1.614034e-04)
    A0 <- c(5.192440,  5.347434,  5.651276,  6.092262,  6.654191,  7.317738,  8.061935,  8.865557,  9.708298, 10.571644,
            11.439438, 12.298163, 13.137005, 13.947737, 14.724514, 15.463585, 16.162992, 16.822255, 17.442069, 18.024025,
            19.663005, 21.702601, 23.292722, 24.547176, 25.529296, 26.289680, 26.872494, 27.314703, 27.646156, 27.890684,
            28.067320, 28.191283, 28.274732, 28.327355, 28.356901, 28.369575)
    B0 <- c(0.18417695, 0.18789259, 0.19518275, 0.20578317, 0.21931635, 0.23530408, 0.25318397, 0.27233061, 0.29208091,
            0.31176265, 0.33072422, 0.34836333, 0.36415270, 0.37766093, 0.38856754, 0.39667157, 0.40189356, 0.40427095,
            0.40394715, 0.40115485, 0.38627084, 0.33255606, 0.27844986, 0.23191435, 0.19310077, 0.16073924, 0.13369091,
            0.11107081, 0.09216940, 0.07639827, 0.06326265, 0.05234780, 0.04332084, 0.03597152, 0.03034406, 0.02698035)
    Z0 <- c(2.319780e-02, 2.277847e-02, 2.196546e-02, 2.080836e-02, 1.937553e-02, 1.774677e-02, 1.600489e-02, 1.422770e-02,
            1.248167e-02, 1.081828e-02, 9.273029e-03, 7.866768e-03, 6.608456e-03, 5.498409e-03, 4.531353e-03, 3.698823e-03,
            2.990778e-03, 2.396517e-03, 1.905096e-03, 1.505478e-03, 5.488131e-04, 1.082359e-04, 1.733284e-05, 2.350412e-06,
            2.780615e-07, 2.932255e-08, 2.801132e-09, 2.454618e-10, 1.993059e-11, 1.512047e-12, 1.079718e-13, 7.311167e-15,
            4.741869e-16, 3.003034e-17, 1.943522e-18, 1.540521e-19)
    D0 <- c(6.800703e-05, 1.268946e-04, 1.892206e-04, 2.549871e-04, 3.236098e-04, 3.939860e-04, 4.647086e-04, 5.343331e-04,
            6.016103e-04, 6.656293e-04, 7.258637e-04, 7.821429e-04, 8.345851e-04, 8.835192e-04, 9.294183e-04, 9.728494e-04,
            1.014456e-03, 1.055039e-03, 1.096114e-03, 1.143004e-03, 1.378114e-03, 1.674631e-03, 2.016684e-03, 2.373306e-03,
            2.705495e-03, 2.976448e-03, 3.159072e-03, 3.239945e-03, 3.219591e-03, 3.109892e-03, 2.929981e-03, 2.701965e-03,
            2.447385e-03, 2.184855e-03, 1.929128e-03, 1.697672e-03)
    # Define initial conditions - concentrations of N_P_DON_A_B_Z_D
    INIT <- c(N0, P0, DON0, A0, B0, Z0, D0)
    
    # Create a function
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
        ice = 0
      }
      if(ice > 1){
        ice = 1
      }
      
      damp <- param$kc * param$dz * (cumsum(P) - P/2+cumsum(D)+D/2)
      
      I <- param$I0 * param$PAR0 * exp(-param$kw * param$z - damp) * season* (1-ice)
      
      return(I)
    }
    
    
    
    
    # Create function that creates the differential equation at each step
    
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
    time <- seq(1,100*365, by=1)
    
    # Solve our differential equation
    res <- ode(INIT, time, FASHAM, param)
    
    P_res[[i]] <- res[,(2+param$n):(2*param$n+1)]
    
    
    
  }
  return(list(P_res))
}

Dv <- c(10.8*2.5,10.8*2,10.8*1.5,10.8*1)
run <- sensitivity(Dv)  

P <- run[[1]]

P25t <- P[[1]][36136:36500,1:20]
P2t <- P[[2]][36136:36500,1:20]
P15t <- P[[3]][36136:36500,1:20]
P1t <- P[[4]][36136:36500,1:20]
param <- list()
param$z <- c(seq(5,200, by=10),seq(225,1000,by=50)) # depth (m)



par(mfrow=c(1,1))

plot(P25t[228,],param$z[1:20],ylim = rev(range(param$z[1:20])),xlim=c(0,max(c(max(P25t[228,]),max(P2t[228,]),max(P15t[228,]),max(P1t[228,])))),
     type="l",lwd=3,main="Diffusivity sensitivity - Mid August",
     ylab = substitute(paste(bold("Depth (m)"))),xlab =  bquote(bold(Phytoplankton~(mmol~N~m^-3))),col="darkolivegreen3")
lines(P2t[228,],param$z[1:20],ylim = rev(range(param$z[1:20])),type="l",lwd=3,col="darkolivegreen1")
lines(P15t[228,],param$z[1:20],ylim = rev(range(param$z[1:20])),type="l",lwd=3,col="darkolivegreen4")
lines(P1t[228,],param$z[1:20],ylim = rev(range(param$z[1:20])),type="l",lwd=3,col="darkolivegreen")

Dv25 <- bquote(bold(Dv~27.0~(m^-2~d^-1)))
Dv2 <- bquote(bold(Dv~21.6~(m^-2~d^-1)))
Dv15 <- bquote(bold(Dv~16.2~(m^-2~d^-1)))
Dv1 <- bquote(bold(Dv~10.8~(m^-2~d^-1)))
legend(0.8,120,legend = c(Dv25,Dv2,Dv15,Dv1),
       col= c("darkolivegreen", "darkolivegreen4", "darkolivegreen3","darkolivegreen1"),pch=19,cex = 1)


