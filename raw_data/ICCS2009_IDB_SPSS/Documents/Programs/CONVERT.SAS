/******************************************************************************/
/*                                                                            */
/*         SAS Program to Convert SAS Export Files to SAS Data Files          */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/

OPTIONS NONOTES ;

* Specify the file type (ICG, ITG, ISA, ISG, ISR, ISS, ISE, ISL, JSA, JSG, JSR, JSE);
* <type> must be edited ;

%LET TYPE = <type> ;

* Specify the path where the SAS export files are located following the format x:\xxx\ ;
* <exppath> must be edited ;

%LET EXPPATH = <exppath>  ;

* Specify the path for the SAS data files to be saved in following the format x:\xxx\ ;
* <datpath> must be edited ;

%LET DATPATH = <datpath>  ;

%MACRO DOIT ;

   * Adapt the list of countries to be processed for the items, if desired;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = G OR %UPCASE(%SUBSTR(&TYPE,3,1)) = A OR %UPCASE(%SUBSTR(&TYPE,3,1)) = R %THEN %DO ; * All countries (international items);

      %LET COUNTRY = AUT BFL BGR CHE CHL COL CYP CZE DNK
                     DOM ENG ESP EST FIN GRC GTM HKG IDN
                     IRL ITA KOR LIE LTU LUX LVA MEX MLT
                     NLD NOR NZL POL PRY RUS SVK SVN SWE
                     THA TWN ;

   %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = S %THEN %DO ; * Asian Module countries ;

      %LET COUNTRY = HKG IDN KOR THA TWN ;

   %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = E %THEN %DO ; * European Module countries ;

      %LET COUNTRY = AUT BFL BGR CHE CYP CZE
                     DNK ENG ESP EST FIN GRC
                     IRL ITA LIE LTU LUX LVA
                     MLT NLD POL SVK SVN SWE ;

   %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = L %THEN %DO ; * Asian Module countries ;

      %LET COUNTRY = CHL COL DOM GTM MEX PRY ;

   %END ;

   %LET I = 1 ;
   %DO %WHILE(%LENGTH(%SCAN(&COUNTRY,&I))) ;
      %LET CTRY = %SCAN(&COUNTRY,&i) ;

      PROC CIMPORT FILE="&EXPPATH&TYPE&CTRY.C2.EXP"
                   DATA="&DATPATH&TYPE&CTRY.C2" ;
      RUN;

      %LET I = %EVAL(&I + 1) ;
   %END ;

%MEND DOIT ;

%DOIT ;
