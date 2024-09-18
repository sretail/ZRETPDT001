*&---------------------------------------------------------------------*
*& Include          ZRETCAN001_RELOJ
*&---------------------------------------------------------------------*
*===================================================================================================
* RELOJ DE PROCESO
*===================================================================================================

************************************************************************
* Definición de variables
************************************************************************
DATA d_max TYPE i.          " Nº Máximo de llamadas
DATA d_tabix LIKE sy-tabix. " Nº de llamada actual
DATA d_ant LIKE d_max       " Porcentaje que se dibujó en la anterior
                     .      " llamada

DATA d_texto_indicador(1000) TYPE c.

************************************************************************
* Definición de macros
************************************************************************
*-- Macro para iniciar un loop
*-- &1 - Nombre de la tabla interna
DEFINE loop_at.
  perform indicador_de_progreso_max tables &1.
  loop at &1.
    if not sy-tabix is initial
   and not sy-tabix = 1.
      d_tabix = sy-tabix.
    else.
      d_tabix = sy-index.
    endif.
  END-OF-DEFINITION.

*-- Macro para finalizar un loop mostrando un indicador de progreso con
*   un texto (pueden ser hasta 5 textos que se concatenaran).
*-- &1 - Texto 1 a mostrar
*-- &2 - Texto 2 a mostrar
*-- &3 - Texto 3 a mostrar
*-- &4 - Texto 4 a mostrar
*-- &5 - Texto 5 a mostrar
  DEFINE endloop_at.

    concatenate &1 &2 &3 &4 &5 into d_texto_indicador
        separated by space.

    perform indicador_de_progreso_de_loop
                            using d_texto_indicador.
  endloop.

END-OF-DEFINITION.

************************************************************************
*
*                  FORMS ADICIONALES
*
************************************************************************

*&--------------------------------------------------------------------*
*&      Form  indicador_de_progreso
*&--------------------------------------------------------------------*
*       Muestra el indicador de progreso del sapgui en el estado
*       P_PERCENT (Porcentaje del proceso) y con el texto P_TEXTO que
*       se elija.
*---------------------------------------------------------------------*
FORM indicador_de_progreso USING p_percent
                                 p_texto.

  IF sy-batch IS INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              percentage = p_percent
              text       = p_texto.

  ENDIF.

ENDFORM.                    " indicador_de_progreso

*&--------------------------------------------------------------------*
*&      Form  indicador_de_progreso_de_loop
*&--------------------------------------------------------------------*
*       Muestra el porcentaje que ocurrencias de una tabla interna que
*	 ya se han procesado mediante un loop mostrando el texto P_TEXTO
*	 Este form se utilizará mediante la macro ENDLOOP_AT para mostrar
*    	 un texto y el porcentaje de ocurrencias realizado al final de
*	 un loop.
*---------------------------------------------------------------------*
FORM indicador_de_progreso_de_loop USING p_texto.

  DATA l_porcentaje TYPE i.
  DATA l_resto LIKE l_porcentaje.
  IF NOT d_max IS INITIAL.

    IF d_tabix <> 1.
      l_porcentaje = ( d_tabix * 100 ) / d_max.
    ENDIF.

    IF d_max < 1000.
      l_resto = l_porcentaje MOD 10.
    ENDIF.

    IF l_resto IS INITIAL AND NOT l_porcentaje = d_ant.

      PERFORM indicador_de_progreso USING l_porcentaje
                                          p_texto.
      d_ant = l_porcentaje.

    ENDIF.
  ENDIF.
ENDFORM.                    " indicador_de_progreso_de_loop


*&--------------------------------------------------------------------*
*&      Form  indicador_de_progreso_max
*&--------------------------------------------------------------------*
*       Lee la cantidad de entradas de una tabla y recoge el tamaño
*       máximo. Este perform se usa mediante la macro LOOP_AT para
*       posteriormente calcular el porcentaje que ocurrencias de una
*	 tabla interna que ya se han procesado con un loop.
*---------------------------------------------------------------------*
*      -->P_TABLE    text
*---------------------------------------------------------------------*
FORM indicador_de_progreso_max TABLES p_table.
  DESCRIBE TABLE p_table LINES d_max.
ENDFORM.
