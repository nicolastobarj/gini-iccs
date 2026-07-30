* Encoding: UTF-8.
/**************************************************************************************************************/
/*                                                                                                                                       */
/*                      SAS Program to Score the ICCS 2016 International Items                              */        		
/*                                                                        	 	                             */												                                               */
/**************************************************************************************************************/

* This syntax recodes the original student responses into item scores. Omitted items are scored as zero. 
* Items that were not administered to a student receive code 8. The new, scored variable names end with "_R".

recode CI2ASM1 (8=8)(4=1)(else=0) into CI2ASM1_R.
recode CI2ASM2 (8=8)(2=1)(else=0) into CI2ASM2_R.
recode CI2BCM1 (8=8)(3=1)(else=0) into CI2BCM1_R.
recode CI2BIO1 (8=8)(1=1)(2=2)(else=0) into CI2BIO1_R.
recode CI2BPM1 (8=8)(2=1)(else=0) into CI2BPM1_R.
recode CI2BPM2 (8=8)(3=1)(else=0) into CI2BPM2_R.
recode CI2CCM1 (8=8)(2=1)(else=0) into CI2CCM1_R.
recode CI2CCM2 (8=8)(2=1)(else=0) into CI2CCM2_R.
recode CI2CEM1 (8=8)(3=1)(else=0) into CI2CEM1_R.
recode CI2CEM2 (8=8)(4=1)(else=0) into CI2CEM2_R.
recode CI2CNM1 (8=8)(1=1)(else=0) into CI2CNM1_R.
recode CI2CNM2 (8=8)(2=1)(else=0) into CI2CNM2_R.
recode CI2DLM1 (8=8)(4=1)(else=0) into CI2DLM1_R.
recode CI2ECM1 (8=8)(4=1)(else=0) into CI2ECM1_R.
recode CI2ECM2 (8=8)(1=1)(else=0) into CI2ECM2_R.
recode CI2ETM2 (8=8)(1=1)(else=0) into CI2ETM2_R.
recode CI2ETO1 (8=8)(1=1)(2=2)(else=0) into CI2ETO1_R.
recode CI2FDM1 (8=8)(4=1)(else=0) into CI2FDM1_R.
recode CI2FSM1 (8=8)(1=1)(else=0) into CI2FSM1_R.
recode CI2GFM1 (8=8)(3=1)(else=0) into CI2GFM1_R.
recode CI2GLM1 (8=8)(2=1)(else=0) into CI2GLM1_R.
recode CI2GLM2 (8=8)(1=1)(else=0) into CI2GLM2_R.
recode CI2HRM1 (8=8)(1=1)(else=0) into CI2HRM1_R.
recode CI2JOM1 (8=8)(2=1)(else=0) into CI2JOM1_R.
recode CI2ORM1 (8=8)(3=1)(else=0) into CI2ORM1_R.
recode CI2PCM1 (8=8)(3=1)(else=0) into CI2PCM1_R.
recode CI2PCM2 (8=8)(1=1)(else=0) into CI2PCM2_R.
recode CI2PGM1 (8=8)(3=1)(else=0) into CI2PGM1_R.
recode CI2PGM2 (8=8)(4=1)(else=0) into CI2PGM2_R.
recode CI2PJM1 (8=8)(3=1)(else=0) into CI2PJM1_R.
recode CI2PJM2 (8=8)(1=1)(else=0) into CI2PJM2_R.
recode CI2PRM1 (8=8)(4=1)(else=0) into CI2PRM1_R.
recode CI2RCM1 (8=8)(4=1)(else=0) into CI2RCM1_R.
recode CI2REM2 (8=8)(3=1)(else=0) into CI2REM2_R.
recode CI2REM3 (8=8)(4=1)(else=0) into CI2REM3_R.
recode CI2SCM1 (8=8)(4=1)(else=0) into CI2SCM1_R.
recode CI2SCM2 (8=8)(1=1)(else=0) into CI2SCM2_R.
recode CI2VOM1 (8=8)(1=1)(else=0) into CI2VOM1_R.
recode CI2VOM2 (8=8)(4=1)(else=0) into CI2VOM2_R.
recode CI2VOM3 (8=8)(1=1)(else=0) into CI2VOM3_R.
recode CI2WFO1 (8=8)(1=1)(2=2)(else=0) into CI2WFO1_R.
recode CI2WFO2 (8=8)(1=1)(2=2)(else=0) into CI2WFO2_R.
recode CI303M1 (8=8)(3=1)(else=0) into CI303M1_R.
recode CI307M1 (8=8)(4=1)(else=0) into CI307M1_R.
recode CI308M1 (8=8)(1=1)(else=0) into CI308M1_R.
recode CI312M1 (8=8)(4=1)(else=0) into CI312M1_R.
recode CI314M1 (8=8)(1=1)(else=0) into CI314M1_R.
recode CI3CAM1 (8=8)(3=1)(else=0) into CI3CAM1_R.
recode CI3CBO1 (8=8)(1=1)(2=2)(else=0) into CI3CBO1_R.
recode CI3CPO1 (8=8)(1=1)(else=0) into CI3CPO1_R.
recode CI3CPO2 (8=8)(1=1)(else=0) into CI3CPO2_R.
recode CI3CRM1 (8=8)(2=1)(else=0) into CI3CRM1_R.
recode CI3CRM2 (8=8)(4=1)(else=0) into CI3CRM2_R.
recode CI3CSM1 (8=8)(1=1)(else=0) into CI3CSM1_R.
recode CI3DBM1 (8=8)(2=1)(else=0) into CI3DBM1_R.
recode CI3DBM2 (8=8)(4=1)(else=0) into CI3DBM2_R.
recode CI3DDM1 (8=8)(3=1)(else=0) into CI3DDM1_R.
recode CI3EPM1 (8=8)(1=1)(else=0) into CI3EPM1_R.
recode CI3GMM1 (8=8)(2=1)(else=0) into CI3GMM1_R.
recode CI3GTM1 (8=8)(3=1)(else=0) into CI3GTM1_R.
recode CI3ICM1 (8=8)(2=1)(else=0) into CI3ICM1_R.
recode CI3IEM1 (8=8)(3=1)(else=0) into CI3IEM1_R.
recode CI3IVM1 (8=8)(1=1)(else=0) into CI3IVM1_R.
recode CI3LPM1 (8=8)(4=1)(else=0) into CI3LPM1_R.
recode CI3LSM1 (8=8)(2=1)(else=0) into CI3LSM1_R.
recode CI3LTM1 (8=8)(4=1)(else=0) into CI3LTM1_R.
recode CI3MAM1 (8=8)(3=1)(else=0) into CI3MAM1_R.
recode CI3MDM1 (8=8)(4=1)(else=0) into CI3MDM1_R.
recode CI3MPM1 (8=8)(2=1)(else=0) into CI3MPM1_R.
recode CI3MPO2 (8=8)(1=1)(2=2)(else=0) into CI3MPO2_R.
recode CI3NPM1 (8=8)(1=1)(else=0) into CI3NPM1_R.
recode CI3NPM2 (8=8)(2=1)(else=0) into CI3NPM2_R.
recode CI3NWM1 (8=8)(3=1)(else=0) into CI3NWM1_R.
recode CI3PAM1 (8=8)(4=1)(else=0) into CI3PAM1_R.
recode CI3PEM1 (8=8)(2=1)(else=0) into CI3PEM1_R.
recode CI3PLM1 (8=8)(2=1)(else=0) into CI3PLM1_R.
recode CI3PRO1 (8=8)(1=1)(2=2)(else=0) into CI3PRO1_R.
recode CI3REM1 (8=8)(4=1)(else=0) into CI3REM1_R.
recode CI3RPM1 (8=8)(1=1)(else=0) into CI3RPM1_R.
recode CI3RRM1 (8=8)(2=1)(else=0) into CI3RRM1_R.
recode CI3SCM1 (8=8)(2=1)(else=0) into CI3SCM1_R.
recode CI3SDM1 (8=8)(3=1)(else=0) into CI3SDM1_R.
recode CI3SMM1 (8=8)(3=1)(else=0) into CI3SMM1_R.
recode CI3SPM1 (8=8)(2=1)(else=0) into CI3SPM1_R.
recode CI3SWM1 (8=8)(2=1)(else=0) into CI3SWM1_R.
recode CI3UHM1 (8=8)(1=1)(else=0) into CI3UHM1_R.
recode CI3ULM1 (8=8)(1=1)(else=0) into CI3ULM1_R.
recode CI3VGM1 (8=8)(3=1)(else=0) into CI3VGM1_R.
exe.

missing values CI2ASM1_R (8).
missing values CI2ASM1_R (8).
missing values CI2ASM2_R (8).
missing values CI2BCM1_R (8).
missing values CI2BIO1_R (8).
missing values CI2BPM1_R (8).
missing values CI2BPM2_R (8).
missing values CI2CCM1_R (8).
missing values CI2CCM2_R (8).
missing values CI2CEM1_R (8).
missing values CI2CEM2_R (8).
missing values CI2CNM1_R (8).
missing values CI2CNM2_R (8).
missing values CI2DLM1_R (8).
missing values CI2ECM1_R (8).
missing values CI2ECM2_R (8).
missing values CI2ETM2_R (8).
missing values CI2ETO1_R (8).
missing values CI2FDM1_R (8).
missing values CI2FSM1_R (8).
missing values CI2GFM1_R (8).
missing values CI2GLM1_R (8).
missing values CI2GLM2_R (8).
missing values CI2HRM1_R (8).
missing values CI2JOM1_R (8).
missing values CI2ORM1_R (8).
missing values CI2PCM1_R (8).
missing values CI2PCM2_R (8).
missing values CI2PGM1_R (8).
missing values CI2PGM2_R (8).
missing values CI2PJM1_R (8).
missing values CI2PJM2_R (8).
missing values CI2PRM1_R (8).
missing values CI2RCM1_R (8).
missing values CI2REM2_R (8).
missing values CI2REM3_R (8).
missing values CI2SCM1_R (8).
missing values CI2SCM2_R (8).
missing values CI2VOM1_R (8).
missing values CI2VOM2_R (8).
missing values CI2VOM3_R (8).
missing values CI2WFO1_R (8).
missing values CI2WFO2_R (8).
missing values CI303M1_R (8).
missing values CI307M1_R (8).
missing values CI308M1_R (8).
missing values CI312M1_R (8).
missing values CI314M1_R (8).
missing values CI3CAM1_R (8).
missing values CI3CBO1_R (8).
missing values CI3CPO1_R (8).
missing values CI3CPO2_R (8).
missing values CI3CRM1_R (8).
missing values CI3CRM2_R (8).
missing values CI3CSM1_R (8).
missing values CI3DBM1_R (8).
missing values CI3DBM2_R (8).
missing values CI3DDM1_R (8).
missing values CI3EPM1_R (8).
missing values CI3GMM1_R (8).
missing values CI3GTM1_R (8).
missing values CI3ICM1_R (8).
missing values CI3IEM1_R (8).
missing values CI3IVM1_R (8).
missing values CI3LPM1_R (8).
missing values CI3LSM1_R (8).
missing values CI3LTM1_R (8).
missing values CI3MAM1_R (8).
missing values CI3MDM1_R (8).
missing values CI3MPM1_R (8).
missing values CI3MPO2_R (8).
missing values CI3NPM1_R (8).
missing values CI3NPM2_R (8).
missing values CI3NWM1_R (8).
missing values CI3PAM1_R (8).
missing values CI3PEM1_R (8).
missing values CI3PLM1_R (8).
missing values CI3PRO1_R (8).
missing values CI3REM1_R (8).
missing values CI3RPM1_R (8).
missing values CI3RRM1_R (8).
missing values CI3SCM1_R (8).
missing values CI3SDM1_R (8).
missing values CI3SMM1_R (8).
missing values CI3SPM1_R (8).
missing values CI3SWM1_R (8).
missing values CI3UHM1_R (8).
missing values CI3ULM1_R (8).
missing values CI3VGM1_R (8).

FORMATS CI2ASM1_R CI2ASM1_R CI2ASM2_R CI2BCM1_R CI2BIO1_R CI2BPM1_R CI2BPM2_R CI2CCM1_R CI2CCM2_R CI2CEM1_R CI2CEM2_R CI2CNM1_R 
CI2CNM2_R CI2DLM1_R CI2ECM1_R CI2ECM2_R CI2ETM2_R CI2ETO1_R CI2FDM1_R CI2FSM1_R CI2GFM1_R CI2GLM1_R CI2GLM2_R CI2HRM1_R 
CI2JOM1_R CI2ORM1_R CI2PCM1_R CI2PCM2_R CI2PGM1_R CI2PGM2_R CI2PJM1_R CI2PJM2_R CI2PRM1_R CI2RCM1_R CI2REM2_R CI2REM3_R 
CI2SCM1_R CI2SCM2_R CI2VOM1_R CI2VOM2_R CI2VOM3_R CI2WFO1_R CI2WFO2_R CI303M1_R CI307M1_R CI308M1_R CI312M1_R CI314M1_R 
CI3CAM1_R CI3CBO1_R CI3CPO1_R CI3CPO2_R CI3CRM1_R CI3CRM2_R CI3CSM1_R CI3DBM1_R CI3DBM2_R CI3DDM1_R CI3EPM1_R CI3GMM1_R 
CI3GTM1_R CI3ICM1_R CI3IEM1_R CI3IVM1_R CI3LPM1_R CI3LSM1_R CI3LTM1_R CI3MAM1_R CI3MDM1_R CI3MPM1_R CI3MPO2_R CI3NPM1_R 
CI3NPM2_R CI3NWM1_R CI3PAM1_R CI3PEM1_R CI3PLM1_R CI3PRO1_R CI3REM1_R CI3RPM1_R CI3RRM1_R CI3SCM1_R CI3SDM1_R CI3SMM1_R 
CI3SPM1_R CI3SWM1_R CI3UHM1_R CI3ULM1_R CI3VGM1_R  (f1.0).

