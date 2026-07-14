#DISCLAIMER: The database used for these analyses was obtained by merging the INVALSI and ICILS data. 
#The resulting database cannot be disclosed as it contains data provided by INVALSI upon request. 
#This code is general, so it can be used for both the 2018 and 2023 data.

#Library----
library(EdSurvey)
library(openxlsx) 
library(lattice)
library(corrplot)
library(dplyr)
library(writexl)
library(haven)

#Creating the weights----
school_weight= as.vector(database$wgtadj1s*database$wgtfac1)

df <- data.frame(ID = database$idschool,value = school_weight)

total_sum <- sum(df$value)

#Normalizing values
df$normalized_value <- (df$value* 3247) / total_sum

sum(df$normalized_value)

database$w1<-rep(1)
database$w2<-df$normalized_value


#Defining the Wald test----
wald_test <- function(parameter, std_error, hypothesized_value = 0) {
  
  wald_statistic <- (parameter - hypothesized_value) / std_error
  
  
  chi_squared_value <- wald_statistic^2
  
  #degree of freedom
  df <- 1
  
  p_value <- 1 - pchisq(chi_squared_value, df)
  
  return(list(wald_statistic = wald_statistic,
              chi_squared_value = chi_squared_value,
              p_value = p_value))
}

#Empty Model----


COEF0 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4),
  ICC= rep(NA, 4)
)))
colnames(COEF2)[1]<-"MAT"
colnames(COEF2)[2]<-"ITA"
colnames(COEF2)[3]<-"ERE"
colnames(COEF2)[4]<-"ELI"

SE2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4)
)))
colnames(SE2)[1]<-"MAT"
colnames(SE2)[2]<-"ITA"
colnames(SE2)[3]<-"ERE"
colnames(SE2)[4]<-"ELI"

for (j in 1:4) {#j is the reference for the school subject
  
  database_2<-database
  database_2$Y<-database_2[,587+j]#Those are the position in the database of the 4 INVALSi scores
  ml1<-mixed.sdf(Y ~ 1+ (1|idschool) , data = database_2, weightVar=c("w1","w2"), weightTransformation = FALSE, verbose = TRUE)
  for (i in 1:3) {
    coef<-as.data.frame(ml1$coef)
    COEF0[i,j]<-coef[i,1]
    se<-as.data.frame(ml1$se)
    SE2[i,j]<-se[i,1]
  }
  
  var<-as.data.frame(ml1$vars)
  COEF0[2,j]<-var[1,1]
  COEF0[3,j]<-var[2,1]
  COEF0[4,j]<-ml1$ICC
  
}



COEF0<-round(COEF0,3)
SE2<-round(SE2,3)

pvalue2<-as.data.frame(t(data.frame(
  INT = rep(NA,4)
)))
colnames(pvalue2)[1]<-"MAT"
colnames(pvalue2)[2]<-"ITA"
colnames(pvalue2)[3]<-"ERE"
colnames(pvalue2)[4]<-"ELI"

for (j in 1:4) {#Calculating p-value using Wald test
  for (i in 1:1) {
    parameter <- COEF0[i,j]
    std_error <- SE2[i,j]
    hypothesized_value <- 0
    result <- wald_test(parameter, std_error, hypothesized_value)
    pvalue2[i,j]<-round(result$p_value,3)
    
  }
}




#Socio-demographic----

COEF2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  Girl= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4),
  ICC= rep(NA, 4),
  Evj=rep(NA, 4),
  Evij= rep(NA, 4)
)))
colnames(COEF2)[1]<-"MAT"
colnames(COEF2)[2]<-"ITA"
colnames(COEF2)[3]<-"ERE"
colnames(COEF2)[4]<-"ELI"

SE2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  Girl= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4)
)))
colnames(SE2)[1]<-"MAT"
colnames(SE2)[2]<-"ITA"
colnames(SE2)[3]<-"ERE"
colnames(SE2)[4]<-"ELI"

for (j in 1:4) {#j is the reference for the school subject
  
  database_2<-database
  database_2$Y<-database_2[,587+j]#Those are the position in the database of the 4 INVALSi scores
  ml1<-mixed.sdf(Y ~ (s_sex+s_isced+s_nisb)+ (1|idschool) , data = database_2, weightVar=c("w1","w2"), weightTransformation = FALSE, verbose = TRUE)
  for (i in 1:6) {
    coef<-as.data.frame(ml1$coef)
    COEF2[i,j]<-coef[i,1]
    se<-as.data.frame(ml1$se)
    SE2[i,j]<-se[i,1]
  }
  
  var<-as.data.frame(ml1$vars)
  COEF2[5,j]<-var[1,1]
  COEF2[6,j]<-var[2,1]
  COEF2[7,j]<-ml1$ICC
  
}

for (j in 1:4) {#calculating EV_j and EV_ij
  
  
  COEF2[8,j]<-(COEF0[2,j]-COEF2[5,j])/COEF0[2,j]
  COEF2[9,j]<-(COEF0[3,j]-COEF2[6,j])/COEF0[3,j]
  
}

COEF2<-round(COEF2,3)
SE2<-round(SE2,3)


pvalue2<-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  Girl= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4)
)))
colnames(pvalue2)[1]<-"MAT"
colnames(pvalue2)[2]<-"ITA"
colnames(pvalue2)[3]<-"ERE"
colnames(pvalue2)[4]<-"ELI"

for (j in 1:4) {#Calculating p-value using Wald test
  for (i in 1:4) {
    parameter <- COEF2[i,j]
    std_error <- SE2[i,j]
    hypothesized_value <- 0
    result <- wald_test(parameter, std_error, hypothesized_value)
    pvalue2[i,j]<-round(result$p_value,3)
    
  }
}


#ICT usage----

COEF2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  Girl= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4),
  ICC= rep(NA, 4),
  Evj=rep(NA, 4),
  Evij= rep(NA, 4)
)))
colnames(COEF2)[1]<-"MAT"
colnames(COEF2)[2]<-"ITA"
colnames(COEF2)[3]<-"ERE"
colnames(COEF2)[4]<-"ELI"

SE2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  Girl= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4)
)))
colnames(SE2)[1]<-"MAT"
colnames(SE2)[2]<-"ITA"
colnames(SE2)[3]<-"ERE"
colnames(SE2)[4]<-"ELI"

for (j in 1:4) {#j is the reference for the school subject
  
  database_2<-database
  database_2$Y<-database_2[,587+j]#Those are the position in the database of the 4 INVALSi scores
  ml1<-mixed.sdf(Y ~ (s_sex+s_excomp+s_school+s_outschool+s_isced+s_nisb)+ (1|idschool) , data = database_2, weightVar=c("w1","w2"), weightTransformation = FALSE, verbose = TRUE)
  for (i in 1:11) {
    coef<-as.data.frame(ml1$coef)
    COEF2[i,j]<-coef[i,1]
    se<-as.data.frame(ml1$se)
    SE2[i,j]<-se[i,1]
  }
  
  var<-as.data.frame(ml1$vars)
  COEF2[10,j]<-var[1,1]
  COEF2[11,j]<-var[2,1]
  COEF2[12,j]<-ml1$ICC
  
}

for (j in 1:4) {#calculating EV_j and EV_ij
  
  
  COEF2[13,j]<-(COEF0[2,j]-COEF2[10,j])/COEF0[2,j]
  COEF2[14,j]<-(COEF0[3,j]-COEF2[11,j])/COEF0[3,j]
  
}


COEF2<-round(COEF2,3)
SE2<-round(SE2,3)


pvalue2<-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  Girl= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4)
)))
colnames(pvalue2)[1]<-"MAT"
colnames(pvalue2)[2]<-"ITA"
colnames(pvalue2)[3]<-"ERE"
colnames(pvalue2)[4]<-"ELI"

for (j in 1:4) {#Calculating p-value using Wald test
  for (i in 1:9) {
    parameter <- COEF2[i,j]
    std_error <- SE2[i,j]
    hypothesized_value <- 0
    result <- wald_test(parameter, std_error, hypothesized_value)
    pvalue2[i,j]<-round(result$p_value,3)
    
  }
}


#CIL----

COEF2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  CIL= rep(NA, 4),
  Girl= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4),
  ICC= rep(NA, 4),
  EVj=rep(NA, 4),
  EVij=rep(NA, 4)
)))
colnames(COEF2)[1]<-"MAT"
colnames(COEF2)[2]<-"ITA"
colnames(COEF2)[3]<-"ERE"
colnames(COEF2)[4]<-"ELI"

SE2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  CIL= rep(NA, 4),
  Girl= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4)
)))
colnames(SE2)[1]<-"MAT"
colnames(SE2)[2]<-"ITA"
colnames(SE2)[3]<-"ERE"
colnames(SE2)[4]<-"ELI"

for (j in 1:4) {#j is the reference for the school subject
  COEF<-as.data.frame(rep(NA,13))
  SE<-as.data.frame(rep(NA,12))
  for (i in 1:5) {#i is the reference for the plausible values
    database_2<-database
    database_2$pvtemporary<-database_2[,475+i]#Those are the position in the database of the 5 plausible values
    database_2$Y<-database_2[,587+j]#Those are the position in the database of the 4 INVALSi scores
    database_2$cil_school<-database_2[,582+i]#Those are the position in the database of the 5 plausible values at school level
    ml1<-mixed.sdf(Y ~ (pvtemporary+s_sex+s_excomp+s_school+s_outschool+s_isced+s_nisb)+ (1|idschool) , data = database_2, weightVar=c("w1","w2"), weightTransformation = FALSE, verbose = TRUE)
    coef<-as.data.frame(ml1$coef)
    coef <- setNames(coef, "valore")
    coef1<-as.data.frame(ml1$vars)
    coef1 <- setNames(coef1, "valore")
    coef2<-as.data.frame(ml1$ICC)
    coef2 <- setNames(coef2, "valore")
    COEFtemporary<-rbind(coef,coef1,coef2)
    COEF<-cbind(COEF,COEFtemporary) 
    SEtemporary<-as.data.frame(ml1$se)
    SE<-cbind(SE,SEtemporary)
    
  }
  COEF <- COEF[,2:6]
  COEF<-t(COEF)
  SE<-SE[,2:6]
  for (k in 1:13) {#appling the mean for the coefficients 
    COEF2[k,j]<-mean(COEF[,k])
    
  } 
  for (y in 1:12) {#Appling the Rubin formula for s.e.
    meanvar<-sum(SE[y,1]^2,SE[y,2]^2,SE[y,3]^2,SE[y,4]^2,SE[y,5]^2)/5
    sumquad1<-((COEF[1,y]- COEF2[y,j])^2)/4
    sumquad2<-((COEF[2,y]- COEF2[y,j])^2)/4
    sumquad3<-((COEF[3,y]- COEF2[y,j])^2)/4
    sumquad4<-((COEF[4,y]- COEF2[y,j])^2)/4
    sumquad5<-((COEF[5,y]- COEF2[y,j])^2)/4
    sumquad<-sum(sumquad1,sumquad2,sumquad3,sumquad4,sumquad5)
    
    totvar<-meanvar+(1+1/5)*sumquad
    SE2[y,j]<-sqrt(totvar)
  }
}

for (j in 1:4) {#calculating EV_j and EV_ij
  
  
  COEF2[14,j]<-(COEF0[2,j]-COEF2[11,j])/COEF0[2,j]
  COEF2[15,j]<-(COEF0[3,j]-COEF2[12,j])/COEF0[3,j]
  
}

COEF2<-round(COEF2,3)
SE2<-round(SE2,3)


pvalue2<-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  CIL= rep(NA, 4),
  Girl= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4)
)))
colnames(pvalue2)[1]<-"MAT"
colnames(pvalue2)[2]<-"ITA"
colnames(pvalue2)[3]<-"ERE"
colnames(pvalue2)[4]<-"ELI"

for (j in 1:4) {#Calculating p-value using Wald test
  for (i in 1:10) {
    parameter <- COEF2[i,j]
    std_error <- SE2[i,j]
    hypothesized_value <- 0
    result <- wald_test(parameter, std_error, hypothesized_value)
    pvalue2[i,j]<-round(result$p_value,3)
    
  }
}


#Complete model----
COEF2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  CIL= rep(NA, 4),
  Girl= rep(NA, 4),
  Centre= rep(NA, 4),
  South= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  nisb_school= rep(NA, 4),
  CIL_school= rep(NA, 4),
  ratstd= rep(NA, 4),
  ictres= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4),
  ICC= rep(NA, 4),
  EVj=rep(NA, 4),
  EVij=rep(NA, 4)
)))
colnames(COEF2)[1]<-"MAT"
colnames(COEF2)[2]<-"ITA"
colnames(COEF2)[3]<-"ERE"
colnames(COEF2)[4]<-"ELI"

SE2 <-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  CIL= rep(NA, 4),
  Girl= rep(NA, 4),
  Centre= rep(NA, 4),
  South= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  nisb_school= rep(NA, 4),
  CIL_school= rep(NA, 4),
  ratstd= rep(NA, 4),
  ictres= rep(NA, 4),
  VAR2= rep(NA, 4),
  VAR1= rep(NA, 4)
)))
colnames(SE2)[1]<-"MAT"
colnames(SE2)[2]<-"ITA"
colnames(SE2)[3]<-"ERE"
colnames(SE2)[4]<-"ELI"

for (j in 1:4) {#j is the reference for the school subject
  COEF<-as.data.frame(rep(NA,19))
  SE<-as.data.frame(rep(NA,18))
  for (i in 1:5) {#i is the reference for the plausible values
    database_2<-database
    database_2$pvtemporary<-database_2[,475+i]#Those are the position in the database of the 5 plausible values
    database_2$Y<-database_2[,587+j]#Those are the position in the database of the 4 INVALSi scores
    database_2$cil_school<-database_2[,582+i]#Those are the position in the database of the 5 plausible values at school level
    ml1<-mixed.sdf(Y ~ (pvtemporary+s_sex+explicitstratum+s_excomp+s_school+s_outschool+s_isced+s_nisb+nisb_school+cil_school+c_ratstd+c_ictres)+ (1|idschool) , data = database_2, weightVar=c("w1","w2"), weightTransformation = FALSE, verbose = TRUE)
    coef<-as.data.frame(ml1$coef)
    coef <- setNames(coef, "valore")
    coef1<-as.data.frame(ml1$vars)
    coef1 <- setNames(coef1, "valore")
    coef2<-as.data.frame(ml1$ICC)
    coef2 <- setNames(coef2, "valore")
    COEFtemporary<-rbind(coef,coef1,coef2)
    COEF<-cbind(COEF,COEFtemporary) 
    SEtemporary<-as.data.frame(ml1$se)
    SE<-cbind(SE,SEtemporary)
    
  }
  COEF <- COEF[,2:6]
  COEF<-t(COEF)
  SE<-SE[,2:6]
  for (k in 1:19) {#Appling the mean for coefficients
    COEF2[k,j]<-mean(COEF[,k])
    
  } 
  for (y in 1:18) {#Appling the Rubin formula for s.e.
    meanvar<-sum(SE[y,1]^2,SE[y,2]^2,SE[y,3]^2,SE[y,4]^2,SE[y,5]^2)/5
    sumquad1<-((COEF[1,y]- COEF2[y,j])^2)/4
    sumquad2<-((COEF[2,y]- COEF2[y,j])^2)/4
    sumquad3<-((COEF[3,y]- COEF2[y,j])^2)/4
    sumquad4<-((COEF[4,y]- COEF2[y,j])^2)/4
    sumquad5<-((COEF[5,y]- COEF2[y,j])^2)/4
    sumquad<-sum(sumquad1,sumquad2,sumquad3,sumquad4,sumquad5)
    
    totvar<-meanvar+(1+1/5)*sumquad
    SE2[y,j]<-sqrt(totvar)
  }
}

for (j in 1:4) {#calculating EV_j and EV_ij
  
  
  COEF2[20,j]<-(COEF0[2,j]-COEF2[17,j])/COEF0[2,j]
  COEF2[21,j]<-(COEF0[3,j]-COEF2[18,j])/COEF0[3,j]
  
}

COEF2<-round(COEF2,3)
SE2<-round(SE2,3)

pvalue2<-as.data.frame(t(data.frame(
  INT = rep(NA,4),
  CIL= rep(NA, 4),
  Girl= rep(NA, 4),
  Centre= rep(NA, 4),
  South= rep(NA, 4),
  use_pc_more5y= rep(NA, 4),
  use_ICT_school_q_f= rep(NA, 4),
  use_ICT_school_f= rep(NA, 4),
  use_ICT_outschool_q_f= rep(NA, 4),
  use_ICT_outschool_f= rep(NA, 4),
  Continue_school= rep(NA, 4),
  nisb= rep(NA, 4),
  nisb_school= rep(NA, 4),
  CIL_school= rep(NA, 4),
  ratstd= rep(NA, 4),
  ictres= rep(NA, 4)
)))
colnames(pvalue2)[1]<-"MAT"
colnames(pvalue2)[2]<-"ITA"
colnames(pvalue2)[3]<-"ERE"
colnames(pvalue2)[4]<-"ELI"

for (j in 1:4) {#Calculating p-value using Wald test
  for (i in 1:16) {
    parameter <- COEF2[i,j]
    std_error <- SE2[i,j]
    hypothesized_value <- 0
    result <- wald_test(parameter, std_error, hypothesized_value)
    pvalue2[i,j]<-round(result$p_value,3)
    
  }
}

