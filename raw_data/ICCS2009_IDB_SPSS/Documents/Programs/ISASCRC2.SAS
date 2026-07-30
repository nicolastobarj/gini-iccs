/******************************************************************************/
/*                                                                            */
/*           SAS Program to Score the ICCS 2009 International Items           */
/*                         ICCS 2009 IDB User Guide                           */
/*                                                                            */
/******************************************************************************/

OPTIONS NONOTES ;

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <path> must be edited ;

LIBNAME LIBDAT "<path>" ;

* Specify the grade (I for target grade, J for additional grade) to be processed ;
* <grade> must be edited ;

%LET GRADE = <grade> ;

* Adapt the list of countries to be processed, if desired ;

%LET COUNTRY = AUT BFL BGR CHE CHL COL CYP CZE DNK
			   DOM ENG ESP EST FIN GRC GTM HKG IDN
			   IRL ITA KOR LIE LTU LUX LVA MEX MLT
			   NLD NOR NZL POL PRY RUS SVK SVN SWE
			   THA TWN ;

* List of multiple-choice items with A key ;

%LET ARIGHT = CI2GLM2 CI2FSM1 CI2SCM2 CI2CNM1 CI2ETM2 CI2SRM1
				CI2SRM3 CI2PFM2 CI2PCM2 CI2VOM1 CI2VOM3 CI2HRM1
				CI2ECM2 CI2PJM2 CI121M1  ;

* List of multiple-choice items with B key ;

%LET BRIGHT = CI2SHM1 CI2TGM1 CI2TGM2 CI2BPM1 CI2GLM1 CI2ASM2
				CI2CNM2 CI2CCM1 CI2CCM2 CI2SRM2 CI2HRM2 CI2PFM1
				CI2JOM1 CI101M1 CI113M1 CI115M1  ;

* List of multiple-choice items with C key ;

%LET CRIGHT = CI2COM1 CI2MLM1 CI2MLM2 CI2BPM2 CI2GFM1 CI2BCM1
				CI2OMM2 CI2DCM1 CI2PCM1 CI2PGM1 CI2CEM1 CI2ORM1
				CI2PJM1 CI2REM2 CI108M1 CI119M1 CI120M1 CI127M1
				CI132M1 CI129M1 CI106M1  ;

* List of multiple-choice items with D key ;

%LET DRIGHT = CI2MOM1 CI2RDM2 CI2SHM2 CI2FDM1 CI2SCM1 CI2ASM1
				CI2PRM1 CI2OMM1 CI2OMM3 CI2VOM2 CI2DLM1 CI2PGM2
				CI2ECM1 CI2CEM2 CI2RCM1 CI2REM3 CI109M1 CI128M1
				CI137M1 CI110M1 CI104M1 CI130M1  ;

* List of constructed-response items ;

%LET CONSTR = CA2PDO1 CI2BIO1 CI2ETO1 CI2RRO1 CI2WFO1 CI2WFO2  ;

%MACRO SCOREIT (ITEM, TYPE, RIGHT, NR, NA, OM, OTHER) ;

   * Code for multiple-choice items ;

   %IF %UPCASE(&TYPE) = "MC" %THEN %DO ;
      SCORE = 0 ;
         IF &ITEM = &RIGHT THEN SCORE = 1 ;
         IF &ITEM = &NR    THEN SCORE = 0 ; * Not Reached ;
         IF &ITEM = &NA    THEN SCORE = . ; * Not Administered ;
         IF &ITEM = &OM    THEN SCORE = 0 ; * Omitted ;
         IF &ITEM = &OTHER THEN SCORE = 0 ; * Other Missing ;
      &ITEM = SCORE ;
   %END ;

   * Code for constructed-response items ;

   %IF %UPCASE(&TYPE) = "CR" %THEN %DO ;
      SCORE = 0 ;
         IF &ITEM >= 20 AND &ITEM < 30 THEN SCORE = 2 ;
         IF &ITEM >= 10 AND &ITEM < 20 THEN SCORE = 1 ;
         IF &ITEM >= 70 AND &ITEM < 80 THEN SCORE = 0 ;
         IF &ITEM = &NR    THEN SCORE = 0 ; * Not Reached ;
         IF &ITEM = &NA    THEN SCORE = . ; * Not Administered ;
         IF &ITEM = &OM    THEN SCORE = 0 ; * Omitted ;
         IF &ITEM = &OTHER THEN SCORE = 0 ; * Other Missing ;
      &ITEM = SCORE ;
   %END ;

%MEND SCOREIT ;

%MACRO DOIT ;

   * Start of country processing loop ;

   %LET I = 1 ;
   %LET GRADE = %SCAN(&GRADE,&I) ;
   %DO %WHILE(%LENGTH(%SCAN(&COUNTRY,&I))) ;
      %LET CTRY = %SCAN(&COUNTRY,&I) ;

      * Get the student achievement data ;

      DATA LIBDAT.&GRADE.SC&CTRY.C2; * Output File ;
           SET LIBDAT.&GRADE.SA&CTRY.C2; * Input File  ;

           ARRAY ARIGHT &ARIGHT ;
           ARRAY BRIGHT &BRIGHT ;
           ARRAY CRIGHT &CRIGHT ;
           ARRAY DRIGHT &DRIGHT ;
           ARRAY CONSTR &CONSTR ;

           DO OVER ARIGHT ; %SCOREIT (ARIGHT, "MC", 1, .R, .A, ., .I) ; END ;
           DO OVER BRIGHT ; %SCOREIT (BRIGHT, "MC", 2, .R, .A, ., .I) ; END ;
           DO OVER CRIGHT ; %SCOREIT (CRIGHT, "MC", 3, .R, .A, ., .I) ; END ;
           DO OVER DRIGHT ; %SCOREIT (DRIGHT, "MC", 4, .R, .A, ., .I) ; END ;
           DO OVER CONSTR ; %SCOREIT (CONSTR, "CR",  , .R, .A, ., .I) ; END ;

      RUN ;

      * End of country processing loop ;

      %LET I = %EVAL(&I + 1) ;
   %END ;

%MEND DOIT ;

%DOIT ;
