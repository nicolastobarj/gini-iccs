/******************************************************************************/
/*                                                                            */
/*                           SAS Macro  JACKGEN.SAS                           */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  Computes the means and their standard errors for a continuous variable    */
/*  (DVAR) for all levels defined by a set of classification variables        */
/*  (CVAR). It also computes the percentages and their standard errors for    */
/*  the last classification variable (&CVAR2).                                */
/*                                                                            */
/******************************************************************************/

%MACRO JACKGEN (WGT, JKZ, JKR, NJKZ, CVAR, DVAR, INFILE) ;

* WGT    : The Sampling Weight ;
* JKZ    : The Zone Assignment ;
* JKR    : The Replicate Assignment ;
* NJKZ   : The Number of Jackknife Zones ;
* CVAR   : The Classification Variable(s) ;
* DVAR   : The Analysis Variable ;
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
                      %EVAL(%LENGTH(&CVAR)-%LENGTH(%SCAN(&CVAR,&NCVAR)))) ;
   %ELSE %LET CVAR2 = ;

* Create the data file with the replicate weights ;

DATA A ;
     SET &INFILE (KEEP=&WGT &JKZ &JKR &CVAR &DVAR) ;
     ARRAY RWT RWT1 - RWT&NJKZ ;         * Replicate Weight ;
     ARRAY WTX WTX1 - WTX&NJKZ ;         * Weighted X ;

     * Used in case no classification variables are specified ;
     K = 1 ;

     * Keep only non-missing ;
     IF &DVAR > .Z ;

     * Compute the replicate weights ;
     DO I=1 TO &NJKZ ;
        IF  &JKZ ~= I             THEN RWT(I) = &WGT * 1 ;
        IF (&JKZ  = I & &JKR = 1) THEN RWT(I) = &WGT * 2 ;
        IF (&JKZ  = I & &JKR = 0) THEN RWT(I) = &WGT * 0 ;
        WTX(I) = RWT(I) * &DVAR ;
     END;

     * Used to count sample size ;
     N = 1;

     * Weighted X for full weight ;
     WTX0  = &WGT * &DVAR ;

RUN ;

* Compute sums ;

PROC SUMMARY DATA=A MISSING ;
     CLASS K &CVAR ;
     VAR N &WGT RWT1-RWT&NJKZ WTX0 WTX1-WTX&NJKZ ;
     OUTPUT OUT=B SUM= ;
RUN ;

PROC SUMMARY DATA=B ;
     VAR K _TYPE_ ;
     OUTPUT OUT=MAXTYPE MAX=K MAXTYPE ;
RUN ;

DATA B ;
     MERGE B MAXTYPE (DROP=_TYPE_ _FREQ_) ;
     BY K ;
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

DATA FINAL (KEEP=N &CVAR MNX MNX_SE PCT PCT_SE &WGT) ;
     SET E ;
     ARRAY WTX  WTX1  - WTX&NJKZ  ; * Weighted X ;
     ARRAY RWT  RWT1  - RWT&NJKZ  ; * Replicate Weight ;
     ARRAY RMN  RMN1  - RMN&NJKZ  ; * Replicate Mean ;
     ARRAY TRWT TRWT1 - TRWT&NJKZ ; * Replicate Weight for &CVAR2 ;
     ARRAY RP   RP1   - RP&NJKZ   ; * Replicate Percent ;

     * Compute weighted mean and weighted percent ;
     MNX = WTX0 / &WGT ;
     PCT = 100 * (&WGT/ TWT) ;

     * Initialize variance variables ;
     MN_JVAR = 0 ;
     P_JVAR  = 0 ;
     N_JVAR  = 0 ;

     * Loop over replicates ;
     DO I = 1 TO &NJKZ ;

        * Compute the mean for each replicate ;
        IF RWT(I) > 0
           THEN RMN(I) = WTX(I) / RWT(I) ;
           ELSE RMN(I) = . ;

        * Compute percentage for each replicate ;
        RP(I) = 100 * (RWT(I) / TRWT(I)) ;

        * Compute JK_VAR for mean ;
        IF RMN(I) ~= . THEN MN_JVAR = MN_JVAR + (RMN(I) - MNX ) ** 2 ;

        * Compute JK_VAR for percent ;
        P_JVAR = P_JVAR + (RP(I) - PCT) ** 2 ;

        * Compute JK_VAR for N ;
        IF RWT(I) > 0
           THEN N_JVAR = N_JVAR + (&WGT - RWT(I)) ** 2 ;
     END;

     * Compute SEs ;
     MNX_SE = SQRT(MN_JVAR ) ;
     PCT_SE = SQRT(P_JVAR  ) ;
     N_SE   = SQRT(N_JVAR  ) ;

RUN;

%MEND JACKGEN ;
