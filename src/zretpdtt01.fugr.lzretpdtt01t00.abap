*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRETPDTT01......................................*
DATA:  BEGIN OF STATUS_ZRETPDTT01                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRETPDTT01                    .
CONTROLS: TCTRL_ZRETPDTT01
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRETPDTT01                    .
TABLES: ZRETPDTT01                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
