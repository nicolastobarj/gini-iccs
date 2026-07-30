
######################################################################
#                                                                    #                                                                            
#             R  Program  to  Score  the  ICCS  2022 Items           #
#                     ICCS  2022  User  Guide                        #
#                                                                    #                                                                               
######################################################################
library(tidyverse)

scoreit <- function(data, item, type, right, NR, OM, other){

  #  Code  for  multiple-choice  items
  if  (toupper(type)=="MC"){
    data <- data %>% mutate_at(vars(item), ~case_when ( . == right ~ 1,   # Item Key
                                                    . == NR    ~ NaN, # Not Reached
                                                    . == OM    ~ NaN, # Omitted
                                                    is.na(.)   ~ NaN, # Not Administered
                                                    . == other ~ 0,   # Other Missing
                                                    TRUE       ~ 0))
  }
  
  #  Code  for  constructed-response  items
  if  (toupper(type)=="CR"){
    data <- data %>% mutate_at(vars(item), ~case_when ( . == 3  ~ 3,      # 3 points
                                                    . == 2  ~ 2,      # 2 points
                                                    . == 1  ~ 1,      # 1 point
                                                    . == 0  ~ 0,      # No credit
                                                    . == NR ~ NaN,    # Not Reached
                                                    . == OM ~ NaN,    # Omitted
                                                    is.na(.) ~ NaN,   # Not Administered
                                                    . == other ~ 0,   # Other Missing
                                                    TRUE       ~ 0))
  }
  return(data)
}

doit <- function(indir=getwd(), outdir=getwd(), infile=""){
  output_file <- paste0(outdir, infile, "_SCR.Rdata")
  input_file <- paste0(indir, infile, ".Rdata")
  
  #  Get  the  student  achievement  data
  load(input_file)
  data <- eval(as.symbol(infile))
  
  
  #  Recode several constructed items
  # CI4ZHC1_R
  data$CI4ZHC1_R <- NA
  data$CI4ZHC1_R[data$CI4ZHC1 == 0] <- 0
  data$CI4ZHC1_R[data$CI4ZHC1 == 1] <- 1
  data$CI4ZHC1_R[data$CI4ZHC1 == 2] <- 1
  data$CI4ZHC1_R[data$CI4ZHC1 == 3] <- 2
  data$CI4ZHC1_R[is.na(data$CI4ZHC1)] <- NA
  
  
  # CI4ZHL2_R
  data$CI4ZHL2_TMP <- NA
  data$CI4ZHL2_R <- NA
  
  data$CI4ZHL2_TMP <- data$CI4ZHL2S*100+data$CI4ZHL2M*10+data$CI4ZHL2A
  
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 0] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 1] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 2] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 10] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 11] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 20] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 100] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 101] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 102] <- 1
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 110] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 111] <- 1
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 112] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 120] <- 1
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 121] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 122] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 200] <- 0
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 201] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 202] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 210] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 211] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 212] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 220] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 221] <- 2
  data$CI4ZHL2_R[data$CI4ZHL2_TMP == 222] <- 3
  data$CI4ZHL2_R[is.na(data$CI4ZHL2_TMP)] <- NA
  
  # CI4ZEC2_R
  data$CI4ZEC2_R <- NA
  data$CI4ZEC2_R[data$CI4ZEC2 == 0] <- 0
  data$CI4ZEC2_R[data$CI4ZEC2 == 1] <- 0
  data$CI4ZEC2_R[data$CI4ZEC2 == 2] <- 1
  data$CI4ZEC2_R[is.na(data$CI4ZEC2)] <- NA
  
  
  # CI4ZED3L_R
  data$CI4ZED3L_R <- NA
  data$CI4ZED3L_R[data$CI4ZED3L >= 2 & data$CI4ZED3L <= 7] <- 0
  data$CI4ZED3L_R[data$CI4ZED3L == 8 | data$CI4ZED3L == 9] <- 1
  data$CI4ZED3L_R[data$CI4ZED3L == 10 | data$CI4ZED3L == 12] <- 2
  data$CI4ZED3L_R[is.na(data$CI4ZED3L)] <- NA
  
  # CI4ZSL3_R
  data$CI4ZSL3_TMP <- NA
  data$CI4ZSL3_R <- NA
  
  data$CI4ZSL3_TMP <- data$CI4ZSL3I*100+data$CI4ZSL3P*10+data$CI4ZSL3L
  
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 0  ] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 1  ] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 9  ] <- NA
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 10 ] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 11 ] <- 1
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 19 ] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 90 ] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 91 ] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 96 ] <- NA
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 99 ] <- NA
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 100] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 101] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 109] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 110] <- 1
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 111] <- 2
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 119] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 190] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 191] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 196] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 199] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 666] <- NA
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 888] <- NA
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 900] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 901] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 909] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 911] <- 1
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 910] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 919] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 966] <- NA
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 990] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 991] <- 0
  data$CI4ZSL3_R[data$CI4ZSL3_TMP == 999] <- NA
  data$CI4ZSL3_R[is.na(data$CI4ZSL3_TMP)] <- NA
  
  data <- subset(data, select = -c(CI4ZHL2_TMP, CI4ZSL3_TMP))
  
  #  Score  multiple-choice  items  with  A  key
  Aright <- c('CI3NPM1', 'CI2FSM1', 'CI4NDM1', 'CI4OCM2', 'CI4ECM1', 'CI4ECM3', 'CI3IVM1', 
  						'CI4DDM2', 'CI3CSM1', 'CI4SSM1', 'CI3ULM1', 'CI4NWM1', 'CI4IPM1', 'CI4CFM1', 
  						'CI2GLM2', 'CI3UHM1', 'CI4FRM1', 'CI314M1', 'CI3EPM1', 'CI4RPM1', 'CI4EDM1', 
  						'CI4FTM1', 'CI2CNM1', 'CI4SDM1', 'CI2ECM2', 'CI308M1', 'CI4FKM2')
  
  data <- scoreit(data=data,item=Aright, type="MC", right=1, NR=6, OM=9, other=7)
  
  
  #  Score  multiple-choice  items  with  B  key
  Bright <- c('CI4ZEM1', 'CI2CNM2', 'CI4FFM1', 'CI4TDM1', 'CI2GLM1', 'CI2BPM1', 'CI4JSM1', 
  						'CI3SCM1', 'CI3PLM1', 'CI4ECM2', 'CI4ZHM5', 'CI4INM1', 'CI3NPM2', 'CI2CCM2', 
  						'CI2ASM2', 'CI4DDM1', 'CI2CCM1', 'CI4NHM1', 'CI4NWM2', 'CI4SLM1', 'CI3GMM1', 
  						'CI3RRM1', 'CI4LEM1', 'CI3LSM1', 'CI4JCM1', 'CI4SDM3', 'CI3PEM1')
  
  data <- scoreit(data=data, item=Bright, type="MC", right=2, NR=6, OM=9, other=7)
  
  #  Score  multiple-choice  items  with  C  key
  Cright <- c('CI4MWM1', 'CI4RFM1', 'CI3SDM1', 'CI4PDM2', 'CI4EPM1', 'CI2PGM1', 'CI4RCM1', 
  						'CI4SWM1', 'CI3VGM1', 'CI2BPM2', 'CI3IEM1', 'CI4SCM1', 'CI4SDM2', 'CI4PIM1', 
  						'CI4EAM1', 'CI3DDM1', 'CI2CEM1', 'CI2REM2', 'CI4LEM2', 'CI4TXM1', 'CI4VRM1', 
  						'CI4VRM2', 'CI4GLM1', 'CI4PRM1', 'CI2ORM1', 'CI3CAM1', 'CI3MAM1', 'CI4ZSM1')
 
  data <- scoreit(data=data, item=Cright, type="MC", right=3, NR=6, OM=9, other=7)
  
  #  Score  multiple-choice  items  with  D  key
  Dright <- c('CI4JCM2', 'CI312M1', 'CI3PAM1', 'CI4JSM2', 'CI4MWM2', 'CI4MWM3', 'CI4ERM1', 
  						'CI4GCM1', 'CI2ECM1', 'CI2ASM1', 'CI4PDM1', 'CI4DCM1', 'CI4SJM1', 'CI2CEM2', 
  						'CI4CZM1', 'CI2REM3', 'CI3LTM1', 'CI4REM1', 'CI3MDM1', 'CI4EPM2', 'CI2PGM2', 
  						'CI4OCM1', 'CI4NTM1', 'CI4UWM1', 'CI2FDM1', 'CI2RCM1', 'CI2PRM1', 'CI3LPM1')

  data <- scoreit(data=data, item=Dright, type="MC", right=4, NR=6, OM=9, other=7)

  #  Score  constructed-response  items --- CHECK FOR _R ITEMS
  constr <- c('CI4FWC2', 'CI4DVC1', 'CI2WFO2', 'CI3PRO1', 'CI4FKC1', 'CI3CPO1', 
  						'CI4EDC2', 'CI4SLC3', 'CI4SLC4', 'CI3CPO2', 'CI2WFO1', 'CI4NHC2', 
  						'CI4ZEC4', 'CI4ZEC5', 'CI2BIO1', 'CI4ZSD2', 'CI4ZSC4', 'CI4ZHC3', 
  						'CI4ZHC4', 'CI4ZSC5N', 'CI4ZSC5Y', 'CI4ZHC1_R', 'CI4ZHL2_R', 'CI4ZSL3_R', 
  						'CI4ZEC2_R', 'CI4ZED3L_R')
 
  data<-scoreit(data=data, item=constr, type="CR", right="", NR=6, OM=9, other=7)
  
  # Save file
  env <- new.env()
  data_name <- paste0(infile,"_SCR")
  env[[data_name]] <- data
  save(list=c(data_name), envir=env, file=output_file)
}

doit(indir =  "<Specify location of input Rdata data file using format x:/xxx/>", 
     outdir = "<Specify location of output Rdata data file using format x:/xxx/>", 
     infile = "<Specify name of merged Rdata student achievement data file>")

