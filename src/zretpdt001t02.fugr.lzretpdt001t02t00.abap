*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRETPDT001T02...................................*
DATA:  BEGIN OF STATUS_ZRETPDT001T02                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRETPDT001T02                 .
CONTROLS: TCTRL_ZRETPDT001T02
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRETPDT001T02                 .
TABLES: ZRETPDT001T02                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
