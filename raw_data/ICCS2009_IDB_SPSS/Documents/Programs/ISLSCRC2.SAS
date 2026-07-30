/******************************************************************************/
/*                                                                            */
/*  SAS Program to Score the ICCS 2009 Latin American Module Knowledge Items  */
/*                         ICCS 2009 IDB User Guide                           */
/*                                                                            */
/******************************************************************************/

OPTIONS NONOTES ;

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <path> must be edited ;

LIBNAME LIBDAT "<path>" ;

* For the Latin American Module, only target grade data is available ;

%LET GRADE = I ;

* Adapt the list of countries to be processed, if desired ;

%LET COUNTRY = CHL COL DOM GTM MEX PRY ;

* List of multiple-choice items with A key ;

%LET ARIGHT = LS2T02 LS2T05 LS2T10 LS2T11 LS2T14  ;

* List of multiple-choice items with B key ;

%LET BRIGHT = LS2T09 LS2T13 LS2T16  ;

* List of multiple-choice items with C key ;

%LET CRIGHT = LS2T03 LS2T07 LS2T08  ;

* List of multiple-choice items with D key ;

%LET DRIGHT = LS2T01 LS2T04 LS2T06 LS2T12 LS2T15  ;

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

%MEND SCOREIT ;

%MACRO DOIT ;

   * Start of country processing loop ;

   %LET I = 1 ;
   %LET GRADE = %SCAN(&GRADE,&I) ;
   %DO %WHILE(%LENGTH(%SCAN(&COUNTRY,&I))) ;
      %LET CTRY = %SCAN(&COUNTRY,&I) ;

      * Get the student achievement data ;

      DATA LIBDAT.&GRADE.CL&CTRY.C2; * Output File ;
           SET LIBDAT.&GRADE.SL&CTRY.C2; * Input File  ;

           ARRAY ARIGHT &ARIGHT ;
           ARRAY BRIGHT &BRIGHT ;
           ARRAY CRIGHT &CRIGHT ;
           ARRAY DRIGHT &DRIGHT ;

           DO OVER ARIGHT ; %SCOREIT (ARIGHT, "MC", 1, .R, .A, ., .I) ; END ;
           DO OVER BRIGHT ; %SCOREIT (BRIGHT, "MC", 2, .R, .A, ., .I) ; END ;
           DO OVER CRIGHT ; %SCOREIT (CRIGHT, "MC", 3, .R, .A, ., .I) ; END ;
           DO OVER DRIGHT ; %SCOREIT (DRIGHT, "MC", 4, .R, .A, ., .I) ; END ;

      RUN ;

      * End of country processing loop ;

      %LET I = %EVAL(&I + 1) ;
   %END ;

%MEND DOIT ;

%DOIT ;
