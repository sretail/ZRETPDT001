*&---------------------------------------------------------------------*
*&  Include           ZRETPDT001_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN begin of BLOCK b03 WITH FRAME title text-003.
  parameters: p_mon1  RADIOBUTTON GROUP r02 USER-COMMAND R02,                                       "Cargar Artículos
              p_mon3  RADIOBUTTON GROUP r02,                                                        "Carga reducida
              p_mon2  RADIOBUTTON GROUP r02.                                                        "Log de procesos
SELECTION-SCREEN end of BLOCK b03.

SELECTION-SCREEN begin of BLOCK b01 WITH FRAME title text-001.
  parameters: p_file1 like rlgrap-filename,
              p_file2 like rlgrap-filename,
              p_file3 like rlgrap-filename,
              p_file4 like rlgrap-filename,
              p_lite  like rlgrap-filename.
SELECTION-SCREEN end of BLOCK b01.

SELECTION-SCREEN begin of BLOCK b02 WITH FRAME title text-002.
  parameters: p_opc1 RADIOBUTTON GROUP r01,
              p_opc2 radiobutton group r01.
SELECTION-SCREEN end of BLOCK b02.

SELECTION-SCREEN begin of BLOCK b04 WITH FRAME title text-004.
  select-options: s_MATNR for mara-matnr,
                  s_user  for sy-uname,
                  s_fecha for sy-datum,
                  s_hora  for sy-uzeit.
SELECTION-SCREEN end of BLOCK b04.

AT SELECTION-SCREEN OUTPUT.
  perform f_at_selection_screen_output.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_file1.
  perform f_get_file CHANGING p_file1.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_file2.
  perform f_get_file CHANGING p_file2.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_file3.
  perform f_get_file CHANGING p_file3.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_file4.
  perform f_get_file CHANGING p_file4.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_lite.
  perform f_get_file CHANGING p_lite.
