*                                                                              .
*      SPSS Program to Score the ICCS 2009 Latin American Module Items         .
*                          ICCS 2009 IDB User Guide                            .
*                                                                              .

DEFINE SCOREIT (TYPE  = !CHAREND('/') /
                ITEM  = !CHAREND('/') /
                RIGHT = !CHAREND('/') /
                NR    = !CHAREND('/') /
                NA    = !CHAREND('/') /
                OM    = !CHAREND('/') /
                OTHER = !CHAREND('/') ) .

* Code for multiple-choice items .

 !IF (!UPCASE(!TYPE)=MC) !THEN
    !DO !I !IN (!ITEM) .
       RECODE !I (!RIGHT = 1      )
                 (!NR    = 0      ) /* Not Reached      */
                 (!NA    = SYSMIS ) /* Not Administered */
                 (!OM    = 0      ) /* Omitted          */
                 (!OTHER = 0      ) /* Other Missing    */
                 (ELSE   = 0      ) .
    !DOEND .
 !IFEND .

!ENDDEFINE .

DEFINE DOIT (COUNTRY = !CHAREND('/') ) .

* Specify the path for the SPSS data files following the format x:\xxx\ .
* <path> must be edited .

!LET !LIBDAT = !UNQUOTE("<path>") .

* Country loop .

!DO !CTY !IN (!COUNTRY) .

* Get the student achievement data .

ADD FILES
  / FILE = !QUOTE(!CONCAT(!LIBDAT,'ISL',!CTY,'C2.sav')) .

* Score multiple-choice items with A key .

SCOREIT TYPE  = MC      /
        ITEM  = LS2T02 LS2T05 LS2T10 LS2T11 LS2T14 /
        RIGHT = 1       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with B key .

SCOREIT TYPE  = MC      /
        ITEM  = LS2T09 LS2T13 LS2T16 /
        RIGHT = 2       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with C key .

SCOREIT TYPE  = MC      /
        ITEM  = LS2T03 LS2T07 LS2T08 /
        RIGHT = 3       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with D key .

SCOREIT TYPE  = MC      /
        ITEM  = LS2T01 LS2T04 LS2T06 LS2T12 LS2T15 /
        RIGHT = 4       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

EXECUTE .

* Save the student data .

SAVE OUTFILE = !QUOTE(!CONCAT(!LIBDAT,'LSC',!CTY,'C2.sav')) .

!DOEND .

!ENDDEFINE .

* Specify the list of countries to be processed .

DOIT COUNTRY = CHL COL DOM GTM MEX PRY .
