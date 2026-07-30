/******************************************************************************/
/*                                                                            */
/*                            SAS Macro JACKPV.SAS                            */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  Computes the means and their standard errors for scores based on          */
/*  plausible values (PV1CIV-PV5CIV) for all levels defined by a set of       */
/*  classification variables (CVAR). It also computes the percentages and     */
/*  their standard errors for the last classification variable (&CVAR2).      */
/*                                                                            */
/******************************************************************************/

%MACRO JACKPV (WGT, JKZ, JKR, NJKZ, CVAR, NPV, INFILE) ;

* WGT    : The Sampling Weight ;
* JKZ    : The Zone Assignment ;
* JKR    : The Replicate Assignment ;
* NJKZ   : The Number of Jackknife Zones ;
* CVAR   : The Classification Variable(s) ;
* NPV    : The Number of Plausible Values ;
* INFILE : The File with the Data ;

* Count the number of classification variables and create a vector CVAR2 ;
* with all but the last classification variable ;

%LET I = 1 ;                             * Start counter at 1 ;
%DO %WHILE(%LENGTH(%SCAN(&CVAR,&I))) ;
   %LET I = %EVAL(&I + 1) ;
%END ;
%LET NCVAR = %EVAL(&I - 1) ;             * Diminish by 1 at the end ;

%IF &NCVAR > 1
   %THEN %LET CVAR2 = %SUBSTR(&CVAR,1,
                      %EVAL(%LENGTH(&CVAR) - %LENGTH(%SCAN(&CVAR,&NCVAR)))) ;
   %ELSE %LET CVAR2 = ;

* Generate the string with the PVs ;

%LET ALLPV = ;

%DO I = 1 %TO &NPV ;
   %LET PV&I  = PV&I.CIV ;
   %LET ALLPV = &ALLPV &&PV&I ;
%END ;

* Create the data file with the replicate weights ;

DATA A;
     SET &INFILE (KEEP=&WGT &JKZ &JKR &CVAR &ALLPV) ;
     ARRAY RWT RWT1 - RWT&NJKZ ;         * Replicate Weight ;
     ARRAY WTX WTX1 - WTX&NJKZ ;         * Weighted X ;

     * Used in case no classification variables are specified ;
     K = 1 ;

     * Keep only non-missing ;
     IF &PV1 > .Z ;

     * Compute the replicate weights ;
     DO I=1 TO &NJKZ ;
        IF  &JKZ ~= I             THEN RWT(I) = &WGT * 1 ;
        IF (&JKZ  = I & &JKR = 1) THEN RWT(I) = &WGT * 2 ;
        IF (&JKZ  = I & &JKR = 0) THEN RWT(I) = &WGT * 0 ;
        WTX(I) = RWT(I) * &PV1 ;
     END ;

     * Used to count sample size ;
     N = 1;

     * Weighted X for full weight ;
     WTX0  = &WGT * &PV1 ;

     * Compute the terms for the sum of PVs ;
     %DO I = 1 %TO &NPV ;
        WTPV&I  = &WGT * &&PV&I ;
     %END;

RUN ;

* Compute sums ;

PROC SUMMARY DATA=A MISSING ;
     CLASS K &CVAR ;
     VAR N &WGT RWT1-RWT&NJKZ WTX0 WTX1-WTX&NJKZ
     %DO I = 1 %TO &NPV;
        WTPV&I
     %END ; ;
     OUTPUT OUT=B SUM= ;
RUN;

PROC SUMMARY DATA=B ;
     VAR K _TYPE_;
     OUTPUT OUT=MAXTYPE MAX=K MAXTYPE ;
RUN ;

DATA B ;
     MERGE B MAXTYPE (DROP= _TYPE_ _FREQ_) ;
     BY K ;
     IF K = 1 ;
     MAXTYPE2 = MAXTYPE-1 ;
RUN ;

* Select the record which has the totals for &cvar2 and save to a file ;

DATA TYPE2 (KEEP=K &CVAR2 T2WT T2RWT1-T2RWT&NJKZ) ;
     SET B ;
     WHERE _TYPE_ = MAXTYPE2 ;
     ARRAY RWT   RWT1   - RWT&NJKZ   ;
     ARRAY T2RWT T2RWT1 - T2RWT&NJKZ ;
     T2WT = &WGT ;
     DO I = 1 TO &NJKZ ;
        T2RWT(I) = RWT(I) ;
     END ;
RUN ;

PROC SORT DATA=B OUT=D ;
     BY K &CVAR2 ;
     WHERE _TYPE_ = MAXTYPE ;
RUN ;

DATA E (DROP=T2WT T2RWT1-T2RWT&NJKZ) ;
     MERGE D TYPE2 ;
     BY K &CVAR2 ;
     TWT = T2WT ;
     ARRAY RWT   TRWT1  - TRWT&NJKZ  ;
     ARRAY T2RWT T2RWT1 - T2RWT&NJKZ ;
     DO I = 1 TO &NJKZ ;
        RWT(I) = T2RWT(I) ;
     END ;
RUN ;

DATA FINAL (KEEP=N &CVAR MNX MNX_SE PCT PCT_SE &WGT
                 MNPV MNPV_SE MNX_JVR MNPV_IVR) ;
     SET E ;
     ARRAY WTX  WTX1  - WTX&NJKZ  ; * Weighted X ;
     ARRAY RWT  RWT1  - RWT&NJKZ  ; * Replicate Weight ;
     ARRAY RMNX RMNX1 - RMNX&NJKZ ; * Replicate Mean ;
     ARRAY TRWT TRWT1 - TRWT&NJKZ ; * Replicate Weight for &CVAR2 ;
     ARRAY RP   RP1   - RP&NJKZ   ; * Replicate Percent ;

     * Compute weighted mean and weighted percent ;
     MNX   =  WTX0 / &WGT       ;
     PCT   = 100 * (&WGT / TWT) ;

     * Initialize variance variables ;
     MNX_JVR = 0 ;
     PCT_VAR = 0 ;

     * Loop over replicates ;
     DO I = 1 TO &NJKZ ;

        * Compute the mean for each replicate ;
        IF RWT(I) > 0
           THEN RMNX(I) = WTX(I) / RWT(I) ;
           ELSE RMNX(I) = . ;

        * Compute percentage for each replicate ;
        RP(I) = 100 * (RWT(I) / TRWT(I)) ;

        * Compute JK_VAR for mean ;
        IF RMNX(I) ~= . THEN MNX_JVR = MNX_JVR + (RMNX(I) - MNX) ** 2 ;

        * Compute JK_VAR for percent ;
        PCT_VAR = PCT_VAR + (RP(I) - PCT) ** 2 ;

     END ;

     * Compute the mean for each PV ;
     %DO I = 1 %TO &NPV ;
        MNPV&I = WTPV&I / &WGT ;
     %END ;
     MNPV = MEAN (OF %DO I = 1 %TO &NPV ; MNPV&I %END ;) ;

     * Compute IMP_VAR for mean ;
     MNPV_IVR = 0 ;
     %IF %EVAL(&NPV) > 1 %THEN %DO ;
        MNPV_IVR = (1 + (1 / %EVAL(&NPV))) *
                    VAR (OF %DO I = 1 %TO &NPV ; MNPV&I %END ;) ;
     %END ;

     * Compute SEs ;
     MNX_SE  = SQRT (MNX_JVR) ;
     MNPV_SE = SQRT (MNX_JVR + MNPV_IVR) ;
     PCT_SE  = SQRT (PCT_VAR) ;

RUN ;

%MEND JACKPV ;
