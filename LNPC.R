library(GDINA)
library(NPCD)
library(devtools)

#### data preparation ####
Q <-sim30DINA$simQ
J <-nrow(Q)
gs <-data.frame(guess=runif(30,0,0.15), slip=runif(J,0,0.15))

# N=30
sim <- simGDINA(N=30, Q, gs.parm = gs, model = "GDINA") 

# 3 time points
y1=y2=y3= sim$dat


stack.dat=rbind(y1,y2,y3)


#### LNPC estimation ####

mod.lnpca=AlphaNP(stack.dat30[,1:30],Q,gate="AND",method="Hamming")
# t1
att.lnpca.t1=mod.lnpca$alpha.est[1:30,]
# t2
att.lnpca.t2=mod.lnpca$alpha.est[31:60,]
# t3
att.lnpca.t3=mod.lnpca$alpha.est[61:90,]
