#Part 1
#(a) 
#St. Laurence

R.st = 0.15
K.st = 15234
tm=50

dlogistic <- function(N0 = K.st, R.st = 0.15, K.st = 15234, tm = 50){
  N = c(N0, numeric(tm))
  
  for (i in 1:tm){
    N[i + 1] = N[i] + R.st*N[i]*(1 - N[i]/K.st) - R.st*K.st/4 
  }
  
  return(N)
}

Nts.st = dlogistic()

plot(0:tm, Nts.st, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- Fish population when harvested at MSY")
abline(h = K.st, lty = 3, col = 'darkgreen')

#North Sea

R.ns = 0.56
K.ns = 185164
tm=50

dlogistic1 <- function(N0 = K.ns, R.ns = 0.56, K.ns = 185164, tm = 50){
  N = c(N0, numeric(tm))
  
  for (i in 1:tm){
    N[i + 1] = N[i] + R.ns*N[i]*(1 - N[i]/K.ns) - R.ns*K.ns/4
  }
  
  return(N)
}

Nts.ns = dlogistic1()

plot(0:tm, Nts.ns, type = 'b', xlab = "Time", ylab = "N", main = "North Sea- Fish population when harvested at MSY")
abline(h = K.ns, lty = 3, col = 'darkred')

##When harvested at MSY, North sea population declines at a higher rate as compared to St. Laurence population.

#(b)
#St. Laurence

dlogistic2 <- function(N0.st = K.st, R.st = 0.15, K.st = 15234, tm = 50){
  N = c(N0.st, numeric(tm))
  
  for (i in 1:tm){
    N[i + 1] = N[i] + R.st*N[i]*(1 - N[i]/K.st) - 0.25*N[i] 
  }
  
  return(N)
}

Nts.st2 = dlogistic2()

plot(0:tm, Nts.st2, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- Fish population when harvested at 0.25N(t)")
abline(h = K.st, lty = 3, col = 'darkgreen')

#North Sea
dlogistic3 <- function(N0.ns = K.ns, R.ns = 0.56, K.ns = 185164, tm = 50){
  N = c(N0.ns, numeric(tm))
  
  for (i in 1:tm){
    N[i + 1] = N[i] + R.ns*N[i]*(1 - N[i]/K.ns) - 0.25*N[i] 
  }
  
  return(N)
}

Nts.ns3 = dlogistic3()

plot(0:tm, Nts.ns3, type = 'b', xlab = "Time", ylab = "N", main = "North sea- Fish population when harvested at 0.25N(t)")
abline(h = K.ns, lty = 3, col = 'darkred')

#When harvested at 0.25N(t), both the populations declines rapidly with North sea population declining even at higher rate.

#Part B
##St. Laurence
N0.st = 0.2*15234
N0.st
R.st = 0.15
K.st = 15234
tm=50

#N[i + 1] = N[i] + R*N[i]*(1 - N[i]/K) - C
#0.2*K = 0.2*K + R*0.2*K*(1-0.2K/K) - C
#C = R*0.2*K*(1 - 0.2)
#C= 0.16*R*K

Cmax.stp = 0.16*R.st*K.st
Cmax.stp
#Cmax for St. Laurence = 365.616

dlogistic_st5 <- function(N0.st = 3046.8, R.st = 0.15, K.st = 15234, tm = 50, C){ 
  N = c(N0.st, numeric(tm))
  for (i in 1:tm){
    N[i + 1] = N[i] + R.st*N[i]*(1 - N[i]/K.st) - C
  }
  return(N)
}

Nts_0 = dlogistic_st5(C = 0)
Nts_01 = dlogistic_st5(C = 0.1*Cmax.stp)
Nts_02 = dlogistic_st5(C = 0.2*Cmax.stp)
Nts_03 = dlogistic_st5(C = 0.3*Cmax.stp)
Nts_04 = dlogistic_st5(C = 0.4*Cmax.stp)
Nts_05 = dlogistic_st5(C = 0.5*Cmax.stp)
Nts_06 = dlogistic_st5(C = 0.6*Cmax.stp)
Nts_07 = dlogistic_st5(C = 0.7*Cmax.stp)
Nts_08 = dlogistic_st5(C = 0.8*Cmax.stp)
Nts_09 = dlogistic_st5(C = 0.9*Cmax.stp)


Rec.st_0 = which(Nts_0 > 0.6*K.st)
Rec.st_01 = which(Nts_01 > 0.6*K.st)
Rec.st_02 = which(Nts_02 > 0.6*K.st)
Rec.st_03 = which(Nts_03 > 0.6*K.st)
Rec.st_04 = which(Nts_04 > 0.6*K.st)
Rec.st_05 = which(Nts_05 > 0.6*K.st)
Rec.st_06 = which(Nts_06 > 0.6*K.st)
Rec.st_07 = which(Nts_07 > 0.6*K.st)
Rec.st_08 = which(Nts_08 > 0.6*K.st)
Rec.st_09 = which(Nts_09 > 0.6*K.st)

Rec.time.st = c(Rec.st_0[1],Rec.st_01[1],Rec.st_02[1],Rec.st_03[1],Rec.st_04[1],Rec.st_05[1],Rec.st_06[1],Rec.st_07[1],Rec.st_08[1],Rec.st_09[1])
Rec.time.st

plot(0:tm, Nts_0, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0")
abline(v= Rec.st_0[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_01, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.1Cmax")
abline(v= Rec.st_01[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_02, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.2Cmax")
abline(v= Rec.st_02[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_03, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.3Cmax")
abline(v= Rec.st_03[1], lty = 3, col = 'darkblue')


plot(0:tm, Nts_04, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.4Cmax")
abline(v= Rec.st_04[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_05, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.5Cmax")
abline(v= Rec.st_05[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_06, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.6Cmax")
abline(v= Rec.st_06[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_07, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.7Cmax")
abline(v= Rec.st_07[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_08, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.8Cmax")
abline(v= Rec.st_08[1], lty = 3, col = 'darkblue')

plot(0:tm, Nts_09, type = 'b', xlab = "Time", ylab = "N", main = "St. Laurence- At C= 0.9Cmax")
abline(v= Rec.st_09[1], lty = 3, col = 'darkblue')


#Plot between recovery time and Harvest level

harvest = c(0,0.1*Cmax.stp,0.2*Cmax.stp,0.3*Cmax.stp,0.4*Cmax.stp,0.5*Cmax.stp,0.6*Cmax.stp,0.7*Cmax.stp,0.8*Cmax.stp,0.9*Cmax.stp)
plot(harvest, Rec.time.st, type = 'b', ylab = "Recovery time (years)", xlab = "Harvest level", main= "St. Laurence - Harvest level vs Recovery time")


##North sea

N0.ns = 0.2*185164
N0.ns
R.ns = 0.56
K.ns = 185164
tm=50

#N[i + 1] = N[i] + R*N[i]*(1 - N[i]/K) - C
#0.2*K = 0.2*K + R*0.2*K*(1-0.2K/K) - C
#C = R*0.2*K*(1 - 0.2)
#C= 0.16*R*K

Cmax.nsp = 0.16*R.ns*K.ns
Cmax.nsp
#Cmax for North sea = 16590.69

dlogistic_ns5 <- function(N0.ns = 37032.8, R.ns = 0.56, K.st = 185164, tm = 50, C){ 
  N = c(N0.ns, numeric(tm))
  for (i in 1:tm){
    N[i + 1] = N[i] + R.ns*N[i]*(1 - N[i]/K.ns) - C
  }
  return(N)
}

Ntns_0 = dlogistic_ns5(C = 0)
Ntns_01 = dlogistic_ns5(C = 0.1*Cmax.nsp)
Ntns_02 = dlogistic_ns5(C = 0.2*Cmax.nsp)
Ntns_03 = dlogistic_ns5(C = 0.3*Cmax.nsp)
Ntns_04 = dlogistic_ns5(C = 0.4*Cmax.nsp)
Ntns_05 = dlogistic_ns5(C = 0.5*Cmax.nsp)
Ntns_06 = dlogistic_ns5(C = 0.6*Cmax.nsp)
Ntns_07 = dlogistic_ns5(C = 0.7*Cmax.nsp)
Ntns_08 = dlogistic_ns5(C = 0.8*Cmax.nsp)
Ntns_09 = dlogistic_ns5(C = 0.9*Cmax.nsp)


Rec.ns_0 = which(Ntns_0 > 0.6*K.ns)
Rec.ns_01 = which(Ntns_01 > 0.6*K.ns)
Rec.ns_02 = which(Ntns_02 > 0.6*K.ns)
Rec.ns_03 = which(Ntns_03 > 0.6*K.ns)
Rec.ns_04 = which(Ntns_04 > 0.6*K.ns)
Rec.ns_05 = which(Ntns_05 > 0.6*K.ns)
Rec.ns_06 = which(Ntns_06 > 0.6*K.ns)
Rec.ns_07 = which(Ntns_07 > 0.6*K.ns)
Rec.ns_08 = which(Ntns_08 > 0.6*K.ns)
Rec.ns_09 = which(Ntns_09 > 0.6*K.ns)

Rec.time.ns = c(Rec.ns_0[1],Rec.ns_01[1],Rec.ns_02[1],Rec.ns_03[1],Rec.ns_04[1],Rec.ns_05[1],Rec.ns_06[1],Rec.ns_07[1],Rec.ns_08[1],Rec.ns_09[1])
Rec.time.ns

plot(0:tm, Ntns_0, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0")
abline(v= Rec.ns_0[1], lty = 3, col = 'red')

plot(0:tm, Ntns_01, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.1Cmax")
abline(v= Rec.ns_01[1], lty = 3, col = 'red')

plot(0:tm, Ntns_02, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.2Cmax")
abline(v= Rec.ns_02[1], lty = 3, col = 'red')

plot(0:tm, Ntns_03, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.3Cmax")
abline(v= Rec.ns_03[1], lty = 3, col = 'red')


plot(0:tm, Ntns_04, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.4Cmax")
abline(v= Rec.ns_04[1], lty = 3, col = 'red')

plot(0:tm, Ntns_05, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.5Cmax")
abline(v= Rec.ns_05[1], lty = 3, col = 'red')

plot(0:tm, Ntns_06, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.6Cmax")
abline(v= Rec.ns_06[1], lty = 3, col = 'red')

plot(0:tm, Ntns_07, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.7Cmax")
abline(v= Rec.ns_07[1], lty = 3, col = 'red')

plot(0:tm, Ntns_08, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.8Cmax")
abline(v= Rec.ns_08[1], lty = 3, col = 'red')

plot(0:tm, Ntns_09, type = 'b', xlab = "Time", ylab = "N", main = "North sea- At C= 0.9Cmax")
abline(v= Rec.ns_09[1], lty = 3, col = 'red')


#Plot between recovery time and Harvest level

harvest.ns = c(0,0.1*Cmax.nsp,0.2*Cmax.nsp,0.3*Cmax.nsp,0.4*Cmax.nsp,0.5*Cmax.nsp,0.6*Cmax.nsp,0.7*Cmax.nsp,0.8*Cmax.nsp,0.9*Cmax.nsp)
plot(harvest.ns, Rec.time.ns, type = 'b', ylab = "Recovery time (years)", xlab = "Harvest level", main= "North sea- Harvest level vs Recovery time")

#Recovery time is less in case of North sea as compared to St. Laurence.
#If maximum catch quotas are kept low, fish stocks seem to be able to recover fairly quickly. 
#In case of St. Laurence population, when no fish are harvested, fishery seems to recover in 14 years and the time keeps on increasing with increase in catch.
#Similarly, in case of North sea population, fishery can recover in about 5 years if no harvesting is done. 







