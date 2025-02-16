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


#### LGNPC estimation ####

mod.lgnpc<-GNPC(stack.dat, Q, initial.dis= "hamming", initial.gate = "AND")

# t1
att.lgnpc.t1=mod.lgnpc$att.est[1:30,]
# t2
att.lgnpc.t2=mod.lgnpc$att.est[31:60,]
# t3
att.lgnpc.t3=mod.lgnpc$att.est[61:90,]
