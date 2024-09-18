*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRETPDT001T04...................................*
DATA:  BEGIN OF STATUS_ZRETPDT001T04                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRETPDT001T04                 .
CONTROLS: TCTRL_ZRETPDT001T04
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRETPDT001T04                 .
TABLES: ZRETPDT001T04                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
