*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRETPDTT04......................................*
DATA:  BEGIN OF STATUS_ZRETPDTT04                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRETPDTT04                    .
CONTROLS: TCTRL_ZRETPDTT04
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRETPDTT04                    .
TABLES: ZRETPDTT04                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
