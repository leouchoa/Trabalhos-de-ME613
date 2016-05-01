hd = c("ID", "County", "State", "Land_Area", "Total_Pop", "Pct_18_34", "Pct_65Plus", "num_Phys", "num_Beds", "Total_Crimes", "Pct_HSG", "Pct_Bdgree", "Pct_Below_Poverty_Level", "Pct_Unemployment", "PCI", "Total_Person_Inc", "Geographic_Region")

CDI = read.table(file = "/home/leouchoa/Documents/papers/2016-1/regressao/codes/teste07/APPENC02.txt", header=F, col.names = hd)
#CDI = read.table(file = "http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC02.txt", header=F, col.names = hd)

detach(CDI)
attach(CDI)

##### 1.43 ##### 2.62 ###### 3.25 ####
vetor.ex143 = data.frame(Total_Pop, num_Beds, Total_Person_Inc)
modelo.ex143 = vector("list", length(vetor.ex143))
betas.ex143 = matrix(nrow = length(modelo.ex143), ncol = 2)
mse.ex143 = rep(0,length(modelo.ex143))
r.squared.ex143 = rep(0,length(modelo.ex143)) # 2.62 #
residual.ex325 = vector("list", length(modelo.ex143)) # 3.25 #

for(i in 1:length(modelo.ex143)){
  modelo.ex143[[i]] = lm(num_Phys ~ data.matrix(vetor.ex143[i]))
  par(pty="s")
  plot(x = data.matrix(vetor.ex143[i]), y = num_Phys)
  abline(modelo.ex143[[i]])
  plot(modelo.ex143[[i]], which = c(1,2))
  
  betas.ex143[i,] = coef(modelo.ex143[[i]])
  mse.ex143[i] = summary(modelo.ex143[[i]])$sigma^2
  r.squared.ex143[i] = summary(modelo.ex143[[i]])$r.squared # 2.62 #
  
  residual.ex325[[i]] = num_Phys - (betas.ex143[i,1] + betas.ex143[i,2]*data.matrix(vetor.ex143[i])) # 3.25 #
  #plot(data.matrix(vetor.ex143[i]), residual.ex325[[i]])
}

###################
#
#
#
#
#
##### 1.44 ####### 2.63 ##### 3.26 #####
modelo.ex144 = vector("list", max(Geographic_Region))
betas.ex144 = matrix(nrow = length(modelo.ex144), ncol = 2)
mse.ex144 = rep(0,length(modelo.ex144))

ic.beta1 = function(beta1, s, dgf){
  alfa = 0.1
  return ( beta1 + c(-1, 1)*qt(1 - alfa/2, dgf)*s )
}

ic.beta1.regiao = matrix(nrow = length(modelo.ex144), ncol = 2) # 2.63 #
residual.ex326 = vector("list", length(modelo.ex144)) # 3.26 #

par(mfrow=c(2,2))
for(i in 1:length(modelo.ex144)){
  modelo.ex144[[i]] = lm(Total_Person_Inc[which(Geographic_Region == i)] ~ Pct_Bdgree[which(Geographic_Region == i)])
  betas.ex144[i,] = coef(modelo.ex144[[i]])
  mse.ex144[i] = summary(modelo.ex144[[i]])$sigma^2
  
  ic.beta1.regiao[i,] = ic.beta1(summary(modelo.ex144[[i]])$coef[2,1], summary(modelo.ex144[[i]])$coef[2,2], summary(modelo.ex144[[i]])$df[2]) # 2.63 #
  
  residual.ex326[[i]] = Total_Person_Inc[which(CDI$Geographic_Region == i)] - (betas.ex144[i,1] + betas.ex144[i,2]*data.matrix(Pct_Bdgree[which(CDI$Geographic_Region == i)])) # 3.26 #
  
  #plot(x = data.matrix(Pct_Bdgree[which(Geographic_Region == i)]), y = residual.ex326[[i]])
  
}

###################################

#### 4.26 ####

# a)

m = length(betas.ex143[1,])
alfa = 0.05

IC.ex426a = matrix(nrow = m, ncol = 2)
colnames(IC.ex426a) = c("Lower Bound", "Upper Bound")
rownames(IC.ex426a) = c("Intercept", "Slope")

for (i in 1:m)
  IC.ex426a[i,] = betas.ex143[1,i] + c(-1,1)*summary(modelo.ex143[[1]])$coef[i,2]*qt(1-alfa/(2*m), df = summary(modelo.ex143[[1]])$df[2])

# b)

vet.ex426b = c(-100, .0028)
vet.logic = vector("logical", length = length(vet.ex426b))

for(i in 1:length(vet.ex426b))
  vet.logic[i] = ifelse(IC.ex426a[i,1] < vet.ex426b[i] && IC.ex426a[i,2] < vet.ex426b[i], TRUE, FALSE)

# c e d)

vet.ex146c = c(500, 1000, 5000)
m = length(vet.ex146c)
alfa = 0.1
IC.ex426c = matrix(nrow = m, ncol = 2)
colnames(IC.ex426c) = c("Lower Bound", "Upper Bound")
rownames(IC.ex426c) = as.character(vet.ex146c)
sigma = summary(modelo.ex143[[1]])$sigma
SXX = sum((Total_Pop - mean(Total_Pop))^2)
qt = qt(alfa/2*m, summary(modelo.ex143[[1]])$df[2], lower.tail = FALSE)

for (i in 1:length(vet.ex146c)){
  #County[which(Total_Pop >= vet.ex146c[i]*10^3 & Total_Pop < vet.ex146c[i]*10^3)]
  y.hat = betas.ex143[1,1] + betas.ex143[1,2]*vet.ex146c[i]*10^3
  IC.ex426c[i,] = y.hat + c(-1,1)*sqrt(sigma)*sqrt(1 + 1/length(Total_Pop)
                  + (vet.ex146c[i] - mean(Total_Pop))^2/SXX)
}

########### 6.28 ############


########## 6.29 #############