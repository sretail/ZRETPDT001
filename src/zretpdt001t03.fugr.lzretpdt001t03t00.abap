*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRETPDT001T03...................................*
DATA:  BEGIN OF STATUS_ZRETPDT001T03                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRETPDT001T03                 .
CONTROLS: TCTRL_ZRETPDT001T03
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRETPDT001T03                 .
TABLES: ZRETPDT001T03                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
