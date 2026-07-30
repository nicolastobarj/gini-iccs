/******************************************************************************/
/*                                                                            */
/*                           SAS Macro  JACKREG.SAS                           */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  Computes regression coefficients and their standard errors for a          */
/*  multiple linear regression with a dependent variable (DVAR) and           */
/*  independent variables(s) (XVAR) for all levels defined by a set of        */
/*  classification variables (CVAR).                                          */
/*                                                                            */
/******************************************************************************/

%MACRO JACKREG (WGT, JKZ, JKR, NJKZ, CVAR, XVAR, DVAR, INFILE) ;

* WGT    : The Sampling Weight ;
* JKZ    : The Zone Assignment ;
* JKR    : The Replicate Assignment ;
* NJKZ   : The Number of Jackknife Zones ;
* CVAR   : The Classification Variable(s) ;
* XVAR   : The Independent Variable(S) ;
* DVAR   : The Analysis Variable ;
* INFILE : The File with the Data ;

* Count the number of independent variables, including the constant K ;

%LET I = 1 ;
%DO %WHILE(%LENGTH(%SCAN(&XVAR,&I))) ;
   %LET I = %EVAL(&I + 1) ;
%END ;
%LET NXVAR = &I ;

* Create a vector with the constant, the independent variables and ;
* the dependent variable ;

%LET REGVAR  = K &XVAR &DVAR ;

* Count the number of variables in the vector &REGVAR ;

%LET NREGVAR = %EVAL(&NXVAR + 1) ;

* Print the Results ;

%PUT &NXVAR &REGVAR &NREGVAR ;

* Count the number of classification variables and create a vector LASTCVAR ;
* with the last classification variable ;

%LET I = 1 ;
%DO %WHILE(%LENGTH(%SCAN(&CVAR,&I))) ;
   %LET I = %EVAL(&I + 1) ;
%END ;
%LET NCVAR    = %EVAL(&I-1) ;

%LET LASTCVAR = %SCAN(&CVAR,&NCVAR) ;

* Read the data file and keep only what is necessary ;
* sorting by the classification variables (CVAR) ;

PROC SORT DATA=&INFILE (KEEP=&WGT &JKZ &JKR &CVAR &DVAR &XVAR)
          OUT=A ;
     BY &CVAR ;
     WHERE

        %LET I = 1 ;
        %DO %WHILE(%LENGTH(%SCAN(&XVAR &DVAR,&I))) ;

          %SCAN(&XVAR &DVAR,&I) IS NOT MISSING AND

        %LET I = %EVAL(&I + 1) ;
        %END ;

        &WGT > 0 ;

RUN ;

* Create the data file with the replicate weights and summarize ;

DATA SSCP (DROP=&WGT &JKZ &JKR K &XVAR &DVAR RWGT1-RWGT&NJKZ I

        %DO I = 1 %TO &NREGVAR ;
           %IF %EVAL(&I) < 11
              %THEN %LET AI = 0%EVAL(&I-1) ;
              %ELSE %LET AI = %EVAL(&I-1) ;

           %DO J = &I %TO &NREGVAR ;
              %IF %EVAL(&J) < 11
                 %THEN %LET BJ = 0%EVAL(&J-1) ;
                 %ELSE %LET BJ = %EVAL(&J-1) ;

              W&AI&BJ._0 - W&AI&BJ._&NJKZ

           %END ;
        %END ; ) ;

     SET A;
     BY &CVAR;

     ARRAY RWGT RWGT1 - RWGT&NJKZ ; * Replicate Weights ;

     * Compute the replicate weights ;
     DO I = 1 TO &NJKZ;
        IF       &JKZ ~= I             THEN RWGT(I) = &WGT * 1 ;
        ELSE IF (&JKZ  = I & &JKR = 1) THEN RWGT(I) = &WGT * 2 ;
        ELSE IF (&JKZ  = I & &JKR = 0) THEN RWGT(I) = &WGT * 0 ;
     END;

     * Used in case no classification variables are specified ;
     K = 1 ;

     * Define the arrays for the SSCP matrix ;
     %DO I = 1 %TO &NREGVAR;
        %LET A = %SCAN(&REGVAR,&I) ;

        %IF %EVAL(&I) < 11
            %THEN %LET AI = 0%EVAL(&I-1) ;
            %ELSE %LET AI = %EVAL(&I-1) ;

        %DO J = &I %TO &NREGVAR ;
           %LET B = %SCAN(&REGVAR,&J) ;

           %IF %EVAL(&J) < 11
              %THEN %LET BJ = 0%EVAL(&J-1) ;
              %ELSE %LET BJ = %EVAL(&J-1) ;

           %PUT ;
           %PUT ARRAY FOR THE PRODUCT: &A * &B * WEIGHT ;
           %PUT ;

           ARRAY W&AI&BJ W&AI&BJ._1 - W&AI&BJ._&NJKZ ;
           ARRAY S&AI&BJ S&AI&BJ._1 - S&AI&BJ._&NJKZ ;

        %END ;
     %END ;

     * Accumulate here for the sums of square and cross products ;

     * Initialize if the First case in last grouping variable ;
     IF FIRST.&LASTCVAR THEN DO ;
        N = K ;

        %DO I = 1 %TO &NREGVAR ;
           %LET A = %SCAN(&REGVAR,&I) ;

           %IF %EVAL(&I) < 11
              %THEN %LET AI = 0%EVAL(&I-1) ;
              %ELSE %LET AI = %EVAL(&I-1) ;

           %DO J = &I %TO &NREGVAR ;
              %LET B = %SCAN(&REGVAR,&J) ;

              %IF %EVAL(&J) < 11
                 %THEN %LET BJ = 0%EVAL(&J-1) ;
                 %ELSE %LET BJ = %EVAL(&J-1) ;

              W&AI&BJ._0 = &A * &B * &WGT ;
              S&AI&BJ._0 = W&AI&BJ._0 ;
              DO Z = 1 TO &NJKZ ;
                 W&AI&BJ(Z) = &A * &B * RWGT(Z) ;
                 S&AI&BJ(Z) = W&AI&BJ(Z) ;
              END ;

           %END ;
        %END ;

     END ;

     * Accumulate if not the last case for the group ;

     IF ~FIRST.&LASTCVAR THEN DO ;
        N + K ;

        %DO I = 1 %TO &NREGVAR ;
           %LET A = %SCAN(&REGVAR,&I) ;

           %IF %EVAL(&I) < 11
              %THEN %LET AI = 0%EVAL(&I-1) ;
              %ELSE %LET AI = %EVAL(&I-1) ;

           %DO J = &I %TO &NREGVAR ;
              %LET B = %SCAN(&REGVAR,&J) ;

              %IF %EVAL(&J) < 11
                 %THEN %LET BJ = 0%EVAL(&J-1) ;
                 %ELSE %LET BJ = %EVAL(&J-1) ;

              W&AI&BJ._0 = &A * &B * &WGT ;
              S&AI&BJ._0 + W&AI&BJ._0 ;
              DO Z = 1 TO &NJKZ ;
                 W&AI&BJ(Z) = &A * &B * RWGT(Z) ;
                 S&AI&BJ(Z) +  W&AI&BJ(Z) ;
              END ;

           %END ;
        %END ;

     END ;

     IF LAST.&LASTCVAR THEN OUTPUT SSCP ;

RUN ;

* Create the first matrix to be swept and add the elements below the diagonal ;

DATA SWEEPF ;
     SET SSCP ;

     %DO I = 0 %TO %EVAL(&NXVAR-1) ;
        %IF %EVAL(&I) < 10
           %THEN %LET AI = 0&I ;
           %ELSE %LET AI = %EVAL(&I) ;

        %DO J = &I %TO &NXVAR ;

           %IF %EVAL(&J) < 10
              %THEN %LET BJ = 0&J ;
              %ELSE %LET BJ = %EVAL(&J) ;

           ARRAY UP&AI&BJ S&AI&BJ._0 - S&AI&BJ._&NJKZ ;
           ARRAY DN&BJ&AI S&BJ&AI._0 - S&BJ&AI._&NJKZ ;

           DO Z = 1 TO %EVAL(&NJKZ+1) ;
              DN&BJ&AI(Z) = UP&AI&BJ(Z) ;
           END ;

        %END ;
     %END ;

RUN ;

%DO SWEEP = 0 %TO %EVAL(&NXVAR-1) ;
   %IF %EVAL(&SWEEP) < 10
      %THEN %LET SWEEP = 0&SWEEP ;
      %ELSE %LET SWEEP = %EVAL(&SWEEP) ;

   DATA SWEEPF (DROP=PIVOT0 - PIVOT&NJKZ) ;
        SET SWEEPF ;

        * Create the arrays for the sweeping ;

        ARRAY PIVOT PIVOT0 - PIVOT&NJKZ ;

        %DO I = 0 %TO &NXVAR ;
           %IF %EVAL(&I) < 10
              %THEN %LET AI = 0&I ;
              %ELSE %LET AI = %EVAL(&I) ;

           %DO J = 0 %TO &NXVAR ;
              %IF %EVAL(&J) < 10
                 %THEN %LET BJ = 0&J ;
                 %ELSE %LET BJ = %EVAL(&J) ;

              ARRAY S&AI&BJ S&AI&BJ._0 - S&AI&BJ._&NJKZ ;

           %END ;
        %END ;

        * Loop over all zones ;

        DO Z = 1 TO %EVAL(&NJKZ+1) ;
           DET      = 1.0 * S&SWEEP&SWEEP(Z) ;
           PIVOT(Z) = 1.0 / S&SWEEP&SWEEP(Z) ;
           S&SWEEP&SWEEP(Z) = PIVOT(Z) ;

           %DO I = 0 %TO &NXVAR ;
              %IF %EVAL(&I) < 10
                 %THEN %LET AI = 0&I ;
                 %ELSE %LET AI = %EVAL(&I) ;

              %IF %EVAL(&I) ~= %EVAL(&SWEEP) %THEN %DO ;

                 %DO J = 0 %TO &NXVAR ;
                    %IF %EVAL(&J) < 10
                       %THEN %LET BJ = 0&J ;
                       %ELSE %LET BJ = %EVAL(&J) ;

                    %IF %EVAL(&J) ~= %EVAL(&SWEEP) %THEN %DO ;

                       S&AI&BJ(Z) = S&AI&BJ(Z) -
                                   (PIVOT(Z) * S&AI&SWEEP(Z) * S&SWEEP&BJ(Z)) ;

                    %END ;
                 %END ;
              %END ;
           %END ;

           %DO I = 0 %TO &NXVAR ;
              %IF %EVAL(&I) < 10
                 %THEN %LET AI = 0&I ;
                 %ELSE %LET AI = %EVAL(&I) ;

              %IF %EVAL(&I) ~= %EVAL(&SWEEP) %THEN %DO ;
                 S&SWEEP&AI(Z) = 0.0 + S&SWEEP&AI(Z) * PIVOT(Z) ;
                 S&AI&SWEEP(Z) = 0.0 - S&AI&SWEEP(Z) * PIVOT(Z) ;
              %END ;
           %END ;

        END ;

        * Compute the Sums of Squares ;

        %IF &SWEEP = 0 %THEN %DO ;
           SS_TOTAL = S&AI&BJ._0 ;
        %END ;

        %IF &SWEEP = %EVAL(&NXVAR-1) %THEN %DO ;
           SS_RES   = S&AI&BJ._0 ;
           SS_REG   = SS_TOTAL - SS_RES ;
           MULT_R   = SQRT(1 - SS_RES / SS_TOTAL) ;
           MULT_RSQ = MULT_R ** 2 ;
        %END ;

RUN ;

%END;

* Get the regression coefficients and their standard errors ;

DATA REG (KEEP=&CVAR N SS_RES SS_REG SS_TOTAL MULT_RSQ
        %DO I = 0 %TO %EVAL(&NXVAR-1) ;
           %IF %EVAL(&I) < 10
              %THEN %LET AI = 0&I ;
              %ELSE %LET AI = %EVAL(&I) ;
           B&AI B&AI._SE
        %END ; ) ;
     SET SWEEPF ;

     * Define the arrays here ;

     %DO I = 0 %TO %EVAL(&NXVAR-1) ;
        %IF %EVAL(&I) < 10
           %THEN %LET AI = 0&I ;
           %ELSE %LET AI = %EVAL(&I) ;

        %IF %EVAL(&NXVAR) < 10
           %THEN %LET LASTCOL = 0&NXVAR ;
           %ELSE %LET LASTCOL = %EVAL(&NXVAR) ;

        ARRAY B_&AI S&AI&LASTCOL._1 - S&AI&LASTCOL._&NJKZ ;

        B&AI = S&AI&LASTCOL._0 ;
        B&AI._JVAR = 0 ;
        DO Z = 1 TO &NJKZ ;
           B&AI._JVAR = B&AI._JVAR + (B&AI - B_&AI(Z)) ** 2 ;
        END ;
        B&AI._SE = SQRT(B&AI._JVAR) ;

     %END ;

RUN ;

%MEND JACKREG ;
