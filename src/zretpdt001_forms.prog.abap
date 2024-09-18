***********************************************************************
*      RUTINAS DE GESTION DEL ALV
***********************************************************************

*----------------------------------------------------------------------
FORM f_0100_pbo_init_alv.
*----------------------------------------------------------------------

  IF gr_grid_01 IS INITIAL.
    PERFORM f_0100_init_alv_01.
  ELSE.
    PERFORM f_refresh_alv USING gr_grid_01 'X' 'X' 'X'.
  ENDIF.

*----------------------------------------------------------------------
ENDFORM.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
FORM f_0100_init_alv_01.
*----------------------------------------------------------------------

* 0.- Declaracion de variables
*--------------------------------------------------------------------*
  DATA: it_filas  TYPE lvc_t_roid,
        ld_fila   TYPE int4,
        lr_filas  TYPE lvc_s_roid,
        lit_filas TYPE lvc_t_roid.

* 1.- Logica
*--------------------------------------------------------------------*

* Crear el contenedor en el control de la pantalla
  CREATE OBJECT gr_container_01
    EXPORTING
      container_name = 'CONTAINER_01'.

* Crear el ALV en el container
  CREATE OBJECT gr_grid_01
    EXPORTING
      i_parent = gr_container_01.

* Configurar layout
  PERFORM f_gen_layout_01.

* Configurar fieldcatalog
  PERFORM f_gen_fieldcatalog_01.

* Cargar el alv
  CALL METHOD gr_grid_01->set_table_for_first_display
    EXPORTING
      i_buffer_active = 'X'
      is_layout       = gr_layout_01
*     i_save          = 'A'
*     is_variant      = lr_variant
*     it_toolbar_excluding =
    CHANGING
      it_outtab       = git_log_carga
      it_fieldcatalog = git_fieldcatalog_01.

*----------------------------------------------------------------------
ENDFORM.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
FORM f_gen_fieldcatalog_01.
*----------------------------------------------------------------------

* 0.- Declaracion de variables
*--------------------------------------------------------------------*
  DATA: wa_fieldcatalog LIKE LINE OF git_fieldcatalog_01,
        ld_index        LIKE sy-tabix.

* 1.- Logica
*--------------------------------------------------------------------*
  REFRESH git_fieldcatalog_01.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING "I_BUFFER_ACTIVE       =
      i_structure_name       = 'ZRETPDT001S09'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = git_fieldcatalog_01
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT git_fieldcatalog_01 INTO wa_fieldcatalog.
    CASE wa_fieldcatalog-fieldname.
      WHEN 'MATNR'.
        wa_fieldcatalog-reptext   = 'Artículo'.
        wa_fieldcatalog-scrtext_l = 'Artículo'.
        wa_fieldcatalog-scrtext_m = 'Artículo'.
        wa_fieldcatalog-scrtext_s = 'Artículo'.
        wa_fieldcatalog-outputlen = 12.
      WHEN 'MATNRT'.
        wa_fieldcatalog-reptext   = 'Denominación'.
        wa_fieldcatalog-scrtext_l = 'Denominación'.
        wa_fieldcatalog-scrtext_m = 'Denominación'.
        wa_fieldcatalog-scrtext_s = 'Denominación'.
        wa_fieldcatalog-outputlen = 35.
      WHEN 'STATUS'.
        wa_fieldcatalog-reptext   = 'Status'.
        wa_fieldcatalog-scrtext_l = 'Stat'.
        wa_fieldcatalog-scrtext_m = 'Stat'.
        wa_fieldcatalog-scrtext_s = 'Stat'.
        wa_fieldcatalog-outputlen = 5.
      WHEN 'PASO'.
        wa_fieldcatalog-reptext   = 'Paso'.
        wa_fieldcatalog-scrtext_l = 'Paso'.
        wa_fieldcatalog-scrtext_m = 'Paso'.
        wa_fieldcatalog-scrtext_s = 'Paso'.
        wa_fieldcatalog-outputlen = 5.
      WHEN 'PASOT'.
        wa_fieldcatalog-reptext   = 'Denominación'.
        wa_fieldcatalog-scrtext_l = 'Denominación'.
        wa_fieldcatalog-scrtext_m = 'Denominación'.
        wa_fieldcatalog-scrtext_s = 'Denominación'.
        wa_fieldcatalog-outputlen = 25.
    ENDCASE.

    MODIFY git_fieldcatalog_01 FROM wa_fieldcatalog.
  ENDLOOP.

*----------------------------------------------------------------------
ENDFORM.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
FORM f_gen_layout_01.
*----------------------------------------------------------------------

  gr_layout_01-no_toolbar = 'X'.
  gr_layout_01-zebra = 'X'.
*   gr_layout_01-no_rowmark = 'X'.
*  gr_layout_01-cwidth_opt = 'X'.
*  gr_layout_01-info_fname  = 'ROW_COLOR'.
*  gr_layout_01-sel_mode     = 'A'.
*  gr_layout_01-box_fname = 'SEL'.
*  gr_layout_01-stylefname = 'CELLSTYLES'.

*----------------------------------------------------------------------
ENDFORM.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
FORM f_refresh_alv USING pe_alv TYPE REF TO cl_gui_alv_grid
                         e_row
                         e_column
                         pe_soft_refresh.
*----------------------------------------------------------------------

* 0.- Declaración de variables
*==========================================================================
  DATA: ld_lvc    TYPE lvc_s_stbl,
        it_filas  TYPE lvc_t_roid,
        ld_fila   TYPE int4,
        ld_col    TYPE int4,
        lr_filas  TYPE lvc_s_roid,
        lit_filas TYPE lvc_t_roid.

* 1.- Lógica
*==========================================================================
  ld_lvc-row = e_row.
  ld_lvc-col = e_column.

* Recuperar donde esta el cursor en el ALV
  CALL METHOD pe_alv->get_current_cell
    IMPORTING
      e_row = ld_fila
*     e_value   =
      e_col = ld_col.

* Refrescar la tabla para actualizar los cambios
  CALL METHOD pe_alv->refresh_table_display
    EXPORTING
      is_stable      = ld_lvc
      i_soft_refresh = pe_soft_refresh
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

*----------------------------------------------------------------------
ENDFORM.
*----------------------------------------------------------------------

***********************************************************************
***********************************************************************


*----------------------------------------------------------------------
FORM f_get_file  CHANGING ps_file.
*----------------------------------------------------------------------

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name     = ps_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

*----------------------------------------------------------------------
ENDFORM.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
FORM f_cargar_ficheros_lite CHANGING ps_error.
*----------------------------------------------------------------------

* 0.- Declaración de variables
*======================================================================
  DATA: lit_intern       LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
        ld_filas         TYPE kcd_ex_row_n,
        ld_mensaje       TYPE string,
        ld_fila_act      TYPE kcd_ex_row_n,
        ld_plubal        TYPE zzplubal,
        ld_plubal_cont   TYPE zzplubal,
        wa_lite          LIKE git_lite,
        lf_ean_existe(1),
        ld_netpr         TYPE char50,
        lf_error(1).

* 1.- Lógica
*==========================================================================
**Cargar fichero excel
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_lite
      i_begin_col             = 1
      i_begin_row             = 3
      i_end_col               = 150
      i_end_row               = 99999
    TABLES
      intern                  = lit_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO ld_mensaje.
    MESSAGE ld_mensaje TYPE 'I'.
    ps_error = 'X'.
    EXIT.
  ENDIF.

**Mapear fichero excel
  LOOP AT lit_intern.
  ENDLOOP.

  ld_filas = lit_intern-row.

  DO ld_filas TIMES.
    ADD 1 TO ld_fila_act.
    CLEAR: git_lite, ld_netpr.
    git_lite-status = gc_minisemaforo_ambar.
    git_lite-lineaexcel = ld_fila_act + 2.
    LOOP AT lit_intern WHERE row = ld_fila_act.
      CASE lit_intern-col.
        WHEN 1.
*                 Tipo de artículo
          git_lite-mtart = lit_intern-value.
        WHEN 2.
          git_lite-attyp = lit_intern-value.
*                 Categoría de artículo
        WHEN 3.
*                 Grupo de artículos
          git_lite-matkl = lit_intern-value.
        WHEN 4.
*                 Texto breve de artículo
          git_lite-maktx = lit_intern-value.
        WHEN 5.
*                 Clasificación fiscal artículo
          git_lite-taklv = lit_intern-value.
        WHEN 6.
*                 Marca
          git_lite-extwg = lit_intern-value.
        WHEN 7.
*                 Unidad de medida base
          git_lite-meins = lit_intern-value.
        WHEN 8.
*                 Global Trade Item Number (GTIN)
          git_lite-ean11 = lit_intern-value.
        WHEN 9.
*                 Texto breve de artículo corresp.a unidad de medida
          git_lite-maktm_ean = lit_intern-value.
        WHEN 10.
*                 Texto breve de artículo corresp.a unidad de medida
          git_lite-maktm_gondola = lit_intern-value.
        WHEN 11.
*                 Código de formato
          git_lite-zzcodformato = lit_intern-value.
        WHEN 12.
*                 Modelo
          git_lite-zzmodelo = lit_intern-value.
        WHEN 13.
*                 Anexo 1 (Talla/Medidas/Fragancia)
          git_lite-zzanexo1 = lit_intern-value.
        WHEN 14.
*                 Anexo 2 (Color/Composición)
          git_lite-zzanexo2 = lit_intern-value.
        WHEN 15.
*                 Contenido neto
          REPLACE ALL OCCURRENCES OF '.'
               IN lit_intern-value WITH ''.
          REPLACE ALL OCCURRENCES OF ','
               IN lit_intern-value WITH '.'.
          git_lite-inhal = lit_intern-value.
        WHEN 16.
*                 Unidad de medida contenido
          git_lite-inhme = lit_intern-value.
        WHEN 17.
*                 Graduación alcohólica
          git_lite-zzgrad = lit_intern-value.
        WHEN 18.
*                 Balanza: Departamento
          git_lite-zzdepbal = lit_intern-value.
          IF git_lite-zzdepbal IS NOT INITIAL.
*                    si se informó en el Excel un Departamento de
*                    Balanza para el Artículo...
            CLEAR: ld_plubal, ld_plubal_cont.
*                    lo que intenta el bucle siguiente es "calcular" el
*                    valor (secuencial y, por tanto, único) que le toca
*                    al campo MARA-ZZPLUBAL (la tecla de la balanza)
*                    dentro de un Departamento concreto (ZZDEPBAL).
*                    Según indica la condición del bucle, este valor
*                    siempre será inferior a 1000...
*                    NOTA: Algo no debió ir bien en la carga porque,
*                    en Producción, dentro del Departmento 0010, hay
*                    varios artículos que tienen ZZPLUBAL = 000000.
*                    Además, en el Dep. 0001, parece que hay un
*                    articulo que no corresponde.
            WHILE ld_plubal_cont < 1000.
              ADD 1 TO ld_plubal_cont.
              SELECT SINGLE zzplubal FROM mara
                 INTO ld_plubal_cont
                WHERE zzdepbal = git_lite-zzdepbal
                  AND zzplubal = ld_plubal_cont.
              IF sy-subrc NE 0.
                LOOP AT git_lite INTO wa_lite
                     WHERE zzdepbal = git_lite-zzdepbal
                       AND zzplubal = ld_plubal_cont.
                  EXIT.
                ENDLOOP.
                IF sy-subrc NE 0.
                  ld_plubal = ld_plubal_cont.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDWHILE.
*                    Tecla (calculada) de la Balanza...
            git_lite-zzplubal = ld_plubal.
          ENDIF.
        WHEN 19.
*                 Grupo de Balanzas...
          git_lite-zzgrubal = lit_intern-value.
        WHEN 20.
*                 Balanza: días de caducidad...
          git_lite-zzcadbal = lit_intern-value.
        WHEN 21.
*                 Permitido modificar precios...
          git_lite-zzmodprec = lit_intern-value.
        WHEN 22.
*                 Imprimir ingredientes...
          git_lite-zzimping = lit_intern-value.
        WHEN 23.
*                 Global Trade Item Number (GTIN)...
          git_lite-2ean11 = lit_intern-value.
        WHEN 24.
*                 Unidad medida para visualización...
          git_lite-2meinh = lit_intern-value.
        WHEN 25.
*                 Numerador para la conversión en unidades de medida
*                 base...
          git_lite-2umrez = lit_intern-value.
        WHEN 26.
*                 Denominador para la conversión en unidades de
*                 medida base...
          git_lite-2umren = lit_intern-value.
        WHEN 27.
*                 Texto breve de artículo corresp.a unidad de
*                 medida...
          git_lite-2maktm_ean = lit_intern-value.
        WHEN 28.
*                 Góndola...
          git_lite-2maktm_gondola = lit_intern-value.
        WHEN 29.
*                 Código Formato...
          git_lite-2zzcodformato = lit_intern-value.
        WHEN 30.
*                 Margen específico...
          REPLACE ALL OCCURRENCES OF '.'
               IN lit_intern-value WITH space.
          REPLACE ALL OCCURRENCES OF ','
               IN lit_intern-value WITH '.'.
          git_lite-2aufs = lit_intern-value.
          git_lite-2aufs_waers = '%'.
        WHEN 31.
*                 Proveedor...
          git_lite-3lifnr = lit_intern-value.
        WHEN 32.
*                 Organización de compras...
          git_lite-3ekorg = lit_intern-value.
        WHEN 33.
*                 Referencia según proveedor...
          git_lite-3idnlf = lit_intern-value.
        WHEN 34.
          REPLACE ALL OCCURRENCES OF '.'
               IN lit_intern-value WITH space.
          REPLACE ALL OCCURRENCES OF ','
               IN lit_intern-value WITH '.'.
*                 Precio tarifa (base compra)...
          ld_netpr = lit_intern-value.
        WHEN 35.
*                 Moneda Neto...
          git_lite-3konwa = lit_intern-value.
          IF   git_lite-3konwa = 'EUR4'
            OR git_lite-3konwa = 'USD4'.
*                 Precio Neto...
            git_lite-3netpr = ld_netpr * 100.
          ELSE.
            git_lite-3netpr = ld_netpr.
          ENDIF.
        WHEN 36.
*                 Cantidad base...
          git_lite-3kpein = lit_intern-value.
        WHEN 37.
*                 Unidad de Medida de la condición...
          git_lite-3kmein = lit_intern-value.
        WHEN 38.
*                 EAN unidad alternativa...
          git_lite-3ean11 = lit_intern-value.
        WHEN 39.
*                 Unidad de Medida Alternativa (UMA) compra...
          git_lite-3meinh = lit_intern-value.
        WHEN 40.
*                 Numerador para la conversión en unidades de medida
*                 base...
          git_lite-3umrez = lit_intern-value.
        WHEN 41.
*                 Denominador para la conversión en unidades de
*                 medida base...
          git_lite-3umren = lit_intern-value.
        WHEN 42.
*                 Descripción Ticket por EAN 16 dígitos...
          git_lite-3maktm_ean = lit_intern-value.
        WHEN 43.
*                 Góndola...
          git_lite-3maktm_gondola = lit_intern-value.
        WHEN 44.
*                 código formato...
          git_lite-3zzcodformato = lit_intern-value.
      ENDCASE.
    ENDLOOP.

*    Determinar flag descuento permitido
    SELECT SINGLE zzdescp FROM ztmm_jerarquia INTO git_lite-zzdescp
            WHERE subfamilia = git_lite-matkl.
    APPEND git_lite.
  ENDDO.

**Verificaciones
  LOOP AT git_lite.
    git_lite-info     = 'Artículo 100% válido para procesar'.

*      Validación 01 - Error - El articulo tiene informado EAN que ya
*      existe en el sistema...
    IF git_lite-ean11 IS NOT INITIAL.
      PERFORM f_verificar_ean_existe USING git_lite-ean11
                                  CHANGING lf_ean_existe.
      IF lf_ean_existe = 'X'.
*            Error: EAN & ya asignado en BBDD.
        MESSAGE s019(zretpdt001) WITH git_lite-ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF git_lite-2ean11 IS NOT INITIAL.
      PERFORM f_verificar_ean_existe USING git_lite-2ean11
                                  CHANGING lf_ean_existe.
      IF lf_ean_existe = 'X'.
*            Error: EAN & ya asignado en BBDD.
        MESSAGE s019(zretpdt001) WITH git_lite-2ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF git_lite-3ean11 IS NOT INITIAL.
      PERFORM f_verificar_ean_existe USING git_lite-3ean11
                                  CHANGING lf_ean_existe.
      IF lf_ean_existe = 'X'.
*            Error: EAN & ya asignado en BBDD.
        MESSAGE s019(zretpdt001) WITH git_lite-3ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*      Validación 02-1 Verificar que se ha informado Unidad de
*      medida base
    IF git_lite-meins IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Unidad de medida base'
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 02-2 Error - Articulo contiene unidades de medida
*      informadas que no existen en el sistema...
    IF git_lite-meins IS NOT INITIAL.
      PERFORM f_cunit_input USING git_lite-meins
                         CHANGING git_lite-meins lf_error.
      IF lf_error = 'X'.
        git_lite-info = 'Artículo con Unidades erróneas.'.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        MODIFY git_lite.
      ENDIF.
    ENDIF.

    IF git_lite-inhme IS NOT INITIAL.
      PERFORM f_cunit_input USING git_lite-inhme
                         CHANGING git_lite-inhme lf_error.
      IF lf_error = 'X'.
        git_lite-info = 'Artículo con Unidades erróneas.'.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        MODIFY git_lite.
      ENDIF.
    ENDIF.

    IF git_lite-3kmein IS NOT INITIAL.
      PERFORM f_cunit_input USING git_lite-3kmein
                         CHANGING git_lite-3kmein lf_error.
      IF lf_error = 'X'.
        git_lite-info = 'Artículo con Unidades erróneas.'.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        MODIFY git_lite.
      ENDIF.
    ENDIF.

    IF git_lite-3meinh IS NOT INITIAL.
      PERFORM f_cunit_input USING git_lite-3meinh
                         CHANGING git_lite-3meinh lf_error.
      IF lf_error = 'X'.
        git_lite-info = 'Artículo con Unidades erróneas.'.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        MODIFY git_lite.
      ENDIF.
    ENDIF.

    IF git_lite-2meinh IS NOT INITIAL.
      PERFORM f_cunit_input USING git_lite-2meinh
                         CHANGING git_lite-2meinh lf_error.
      IF lf_error = 'X'.
        git_lite-info = 'Artículo con Unidades erróneas.'.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        MODIFY git_lite.
      ENDIF.
    ENDIF.

*      Validación 03-1 Error - El proveedor no se ha informado
    IF git_lite-3lifnr IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001)
         WITH 'Registro info' 'Número de cuenta del proveedor o acreedor'
         INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 03-2 Error - El proveedor no existe
    SELECT SINGLE lifnr FROM lfa1 INTO git_lite-3lifnr
           WHERE lifnr = git_lite-3lifnr
             AND loevm = space.
    IF sy-subrc <> 0.
*        Error: Proveedor & no existe en el sistema.
      MESSAGE s015(zretpdt001) WITH git_lite-3lifnr
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 04-1 Error - Marca no existe
    IF git_lite-extwg IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Grupo de artículos externo'
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 04-2 Error - Marca no existe
    SELECT SINGLE extwg
             FROM twew
             INTO git_lite-extwg
            WHERE extwg = git_lite-extwg.
    IF sy-subrc <> 0.
*         Error: Marca & no existe en el sistema.
      MESSAGE s018(zretpdt001) WITH git_lite-extwg
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

**     Validación 05-1 Verificar que se ha informado EAN (Datos básicos)
*      if git_lite-ean11 is initial and git_lite-meins <> 'KG'.
**        Error: Excel &, campo & no informado
*         message s036(zretpdt001) WITH 'Datos básicos' 'EAN11' into git_lite-info.
*         git_lite-status = gc_minisemaforo_rojo.
*         MODIFY git_lite.
*         CONTINUE.
*      endif.

*      Validación 05-2 EAN duplicado en excel de UM y EANs
    IF git_lite-ean11 IS NOT INITIAL AND git_lite-ean11 <> gc_ean_20000. "APRADAS-30.05.2018
      LOOP AT git_lite INTO wa_lite
           WHERE lineaexcel = git_lite-lineaexcel
             AND (    2ean11 = git_lite-ean11
                   OR 3ean11 = git_lite-ean11 ).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
*            Error: El ean & del artículo & aparece más de una vez
*            en excel...
        MESSAGE s045(zretpdt001) WITH git_lite-ean11 git_lite-matnr
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        LOOP AT git_lite INTO wa_lite
                        WHERE lineaexcel <> git_lite-lineaexcel
                          AND (    ean11  = git_lite-ean11
                                OR 2ean11 = git_lite-ean11
                                OR 3ean11 = git_lite-ean11 ).
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
*               Error: El ean & del artículo & aparece más de una vez
*                      en excel.
          MESSAGE s045(zretpdt001) WITH git_lite-ean11 git_lite-matnr
                                   INTO git_lite-info.
          git_lite-status = gc_minisemaforo_rojo.
          MODIFY git_lite.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.


    IF   git_lite-2ean11 IS NOT INITIAL
     AND git_lite-2ean11 NE gc_ean_20000.
      LOOP AT git_lite INTO wa_lite
                      WHERE lineaexcel = git_lite-lineaexcel
                        AND (   ean11  = git_lite-2ean11
                             OR 3ean11 = git_lite-2ean11 ).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
*              Error: El ean & del artículo & aparece más de una
*              vez en excel.
        MESSAGE s045(zretpdt001) WITH git_lite-2ean11 git_lite-matnr
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        LOOP AT git_lite INTO wa_lite
                        WHERE lineaexcel <> git_lite-lineaexcel
                          AND (   ean11  = git_lite-2ean11
                               OR 2ean11 = git_lite-2ean11
                               OR 3ean11 = git_lite-2ean11 ).
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
*                 Error: El ean & del artículo & aparece más de una
*                 vez en excel.
          MESSAGE s045(zretpdt001) WITH git_lite-2ean11 git_lite-matnr
                                   INTO git_lite-info.
          git_lite-status = gc_minisemaforo_rojo.
          MODIFY git_lite.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.


    IF git_lite-3ean11 IS NOT INITIAL
     AND git_lite-3ean11 <> gc_ean_20000.
      LOOP AT git_lite INTO wa_lite
                      WHERE lineaexcel = git_lite-lineaexcel
                        AND (   2ean11 = git_lite-3ean11
                             OR ean11  = git_lite-3ean11 ).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
*              Error: El ean & del artículo & aparece más de una
*              vez en excel.
        MESSAGE s045(zretpdt001) WITH git_lite-3ean11 git_lite-matnr
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ELSE.
        LOOP AT git_lite INTO wa_lite
                        WHERE lineaexcel <> git_lite-lineaexcel
                          AND (   ean11  = git_lite-3ean11
                               OR 2ean11 = git_lite-3ean11
                               OR 3ean11 = git_lite-3ean11 ).
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
*                 Error: El ean & del artículo & aparece más de
*                 una vez en excel.
          MESSAGE s045(zretpdt001) WITH git_lite-3ean11 git_lite-matnr
                                   INTO git_lite-info.
          git_lite-status = gc_minisemaforo_rojo.
          MODIFY git_lite.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

*      Validación 06-1 Verificar que el tipo de articulo se ha informado
    IF git_lite-mtart IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos'
                                    'Tipo de artículo'
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 06-2 Verificar que el tipo de articulo existe
*      en el sistema
    SELECT SINGLE mtart FROM t134 INTO git_lite-mtart
            WHERE mtart = git_lite-mtart.
    IF sy-subrc <> 0.
*         Error: Tipo de artículo & no existe en el sistema.
      MESSAGE s022(zretpdt001) WITH git_lite-mtart
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validacion 07-1 Verificar que la categoría de articulo
*      se ha informado
    IF git_lite-attyp IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos'
                                    'Categoría de artículo'
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validacion 07-2 Verificar que la categoría de articulo
*      existe en el sistema
    IF   git_lite-attyp <> '00'
     AND git_lite-attyp <> '01'
     AND git_lite-attyp <> '02'
     AND git_lite-attyp <> '10'
     AND git_lite-attyp <> '11'
     AND git_lite-attyp <> '12'
     AND git_lite-attyp <> '20'
     AND git_lite-attyp <> '21'
     AND git_lite-attyp <> '22'
     AND git_lite-attyp <> '30'.
*           Error: Categoría de artículos & no existe en
*           el sistema.
      MESSAGE s024(zretpdt001) WITH git_lite-attyp
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 08-1 Verificar que el grupo de articulos
*      se ha informado
    IF git_lite-matkl IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos'
                                    'Grupo de artículos'
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 08-2 Verificar que el grupo de articulos
*      existe en el sistema
    SELECT SINGLE matkl FROM t023 INTO git_lite-matkl
            WHERE matkl = git_lite-matkl.
    IF sy-subrc <> 0.
*         Error: Grupo de artículos & no existe en el sistema.
      MESSAGE s023(zretpdt001) WITH git_lite-matkl
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 09-1  Verificar que el indicador de impuestos se
*      ha informado
    IF git_lite-taklv IS INITIAL.
*         Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos'
                                    'Clasificación fiscal artículo'
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*      Validación 09-2  Verificar que el indicador de impuestos es válido
    SELECT SINGLE taxkm FROM tskm INTO git_lite-taklv
            WHERE taxkm = git_lite-taklv.
    IF sy-subrc <> 0.
*         Error: Indicador de impuestos & no existe en el sistema.
      MESSAGE s025(zretpdt001) WITH git_lite-taklv
                               INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.
      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*APRADAS-30.05.2018
*   Validación 10 - Verificar que el ean tiene la longitud correcta
    IF git_lite-ean11 IS NOT INITIAL AND git_lite-ean11 <> gc_ean_20000.
      IF strlen( git_lite-ean11 ) <> 13 AND
         strlen( git_lite-ean11 ) <> 12.
*         Error: El EAN & no es de longitud 13.
        MESSAGE s048(zretpdt001) WITH git_lite-ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*APRADAS-30.05.2018
    IF git_lite-2ean11 IS NOT INITIAL AND git_lite-2ean11 <> gc_ean_20000.
      IF strlen( git_lite-2ean11 ) <> 13 OR
         strlen( git_lite-2ean11 ) <> 12.
*         Error: El EAN & no es de longitud 13.
        MESSAGE s048(zretpdt001) WITH git_lite-2ean11 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*APRADAS-30.05.2018
    IF   git_lite-3ean11 IS NOT INITIAL
     AND git_lite-3ean11 <> gc_ean_20000.
      IF strlen( git_lite-3ean11 ) <> 13 AND
         strlen( git_lite-3ean11 ) <> 12.
*           Error: El EAN & no es de longitud 13.
        MESSAGE s048(zretpdt001) WITH git_lite-3ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 11 - Verificar que el código de formato exista en
*   el sistema
    IF git_lite-zzcodformato IS NOT INITIAL.
      SELECT SINGLE codformato FROM zretpdtt01
               INTO git_lite-zzcodformato
              WHERE codformato = git_lite-zzcodformato.
      IF sy-subrc <> 0.
*         Error: El código de formato & del ean & no existe en el sistema.
        MESSAGE s027(zretpdt001) WITH git_lite-zzcodformato
                                      git_lite-ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF git_lite-2zzcodformato IS NOT INITIAL.
      SELECT SINGLE codformato FROM zretpdtt01
               INTO git_lite-2zzcodformato
              WHERE codformato = git_lite-2zzcodformato.
      IF sy-subrc <> 0.
*         Error: El código de formato & del ean & no existe en el sistema.
        MESSAGE s027(zretpdt001) WITH git_lite-2zzcodformato
                                      git_lite-2ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF git_lite-3zzcodformato IS NOT INITIAL.
      SELECT SINGLE codformato FROM zretpdtt01
               INTO git_lite-3zzcodformato
              WHERE codformato = git_lite-3zzcodformato.
      IF sy-subrc <> 0.
*         Error: El código de formato & del ean & no existe en
*         el sistema.
        MESSAGE s027(zretpdt001) WITH git_lite-3zzcodformato
                                      git_lite-3ean11
                                 INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.
        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 12-1 Verificar que la organización de compras esté informada
    IF git_lite-3ekorg IS INITIAL.
*     Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Registro info' 'Organización de compras' INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 12-2 Verificar que la organización de compras exista en el sistema
    SELECT SINGLE ekorg
      FROM t024e
      INTO git_lite-3ekorg
     WHERE ekorg = git_lite-3ekorg.

    IF sy-subrc <> 0.
*     Error: Organización de compras & no existe en el sistema.
      MESSAGE s028(zretpdt001) WITH git_lite-3ekorg INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 13 - Verificar que el proveedor-organización de compras exista en el sistema
    SELECT SINGLE ekorg
      FROM lfm1
      INTO git_lite-3ekorg
     WHERE ekorg = git_lite-3ekorg
       AND lifnr = git_lite-3lifnr.

    IF sy-subrc <> 0.
*     Error: Proveedor & no existe en organización de compras &.
      MESSAGE s029(zretpdt001) WITH git_lite-3lifnr git_lite-3ekorg INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 14 - Verificar que las monedas informadas existan en el sistema
    SELECT SINGLE waers
      FROM tcurc
      INTO git_lite-3konwa
     WHERE waers = git_lite-3konwa.

    IF sy-subrc <> 0.
*     Error: Moneda & no existe en el sistema.
      MESSAGE s030(zretpdt001) WITH git_lite-3konwa INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 15: Verificar que si han informado unicon hayan informado unidad
    IF git_lite-inhme IS NOT INITIAL AND git_lite-inhal IS INITIAL.
*     Error: Se ha informado UNICON pero no unidad formato
      MESSAGE s037(zretpdt001) INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 16: Verificar que si han informado unidad de formado hayan informado unicon
    IF git_lite-inhme IS INITIAL AND git_lite-inhal IS NOT INITIAL.
*     Error: Se ha informado unidad formato pero no UNICON
      MESSAGE s038(zretpdt001) INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.


*   Validación 17: Verificar que se ha informado texto breve del artículo
    IF git_lite-maktx IS INITIAL.
*     Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Texto breve de artículo' INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 18: Verificar que el departamento de la balanza existe
    IF git_lite-zzdepbal IS NOT INITIAL.
      SELECT SINGLE zzdepbal
        FROM zretpdtt04
        INTO git_lite-zzdepbal
       WHERE zzdepbal = git_lite-zzdepbal.

      IF sy-subrc <> 0.
*       Error: El departamento de balanza no es válido (&)
        MESSAGE s047(zretpdt001) WITH git_lite-zzdepbal INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.

        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 19: Verificar que el grupo de la balanza pertenece al departamento
    IF git_lite-zzgrubal IS NOT INITIAL.
      SELECT SINGLE zzdepbal
        FROM zretpdtt04
        INTO git_lite-zzdepbal
       WHERE zzdepbal = git_lite-zzdepbal
         AND zzgrubal = git_lite-zzgrubal.

      IF sy-subrc <> 0.
*       Error: El departamento-grupo de balanza no son válidos (&-&)
        MESSAGE s046(zretpdt001) WITH git_lite-zzdepbal git_lite-zzgrubal INTO git_lite-info.
        git_lite-status = gc_minisemaforo_rojo.

        MODIFY git_lite.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 20: Verificar que se ha informado Texto TPV (Datos básicos)
    IF git_lite-maktm_ean IS INITIAL.
*     Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Texto TPV' INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 21: Verificar que se ha informado Texto Góndola (Datos básicos)
    IF git_lite-maktm_gondola IS INITIAL.
*     Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Texto Góndola' INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 22: Verificar que se ha determinado tecla de balanza
    IF git_lite-zzdepbal IS NOT INITIAL AND
       git_lite-zzplubal IS INITIAL.
*     Error: Ninguna tecla de balanza libre para el departamento &
      MESSAGE s049(zretpdt001) WITH git_lite-zzdepbal INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   APRADAS-Inicio-30.04.2018 08:39:22
*   Validación 23: Verificar que se ha informado precio compra
    IF git_lite-3netpr IS INITIAL.
*     Error: Excel &, campo & no informado
      MESSAGE s036(zretpdt001) WITH 'Datos de compra' 'Precio tarifa' INTO git_lite-info.
      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.
*   APRADAS-Fin-30.04.2018 08:39:22

*   Validación 90 - Warning - Descripción ticket con longitud > 16
    IF git_lite-maktm_ean IS NOT INITIAL AND strlen( git_lite-maktm_ean ) > 16.
*     Error: Descripción ticket > 16 para EAN &.
      MESSAGE s016(zretpdt001) WITH git_lite-ean11 INTO git_lite-info.
      git_lite-status = gc_minisemaforo_ambar.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

    IF git_lite-2maktm_ean IS NOT INITIAL AND strlen( git_lite-2maktm_ean ) > 16.
*     Error: Descripción ticket > 16 para EAN &.
      MESSAGE s016(zretpdt001) WITH git_lite-2ean11 INTO git_lite-info.
      git_lite-status = gc_minisemaforo_ambar.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

    IF git_lite-3maktm_ean IS NOT INITIAL AND strlen( git_lite-3maktm_ean ) > 16.
*     Error: Descripción ticket > 16 para EAN &.
      MESSAGE s016(zretpdt001) WITH git_lite-3ean11 INTO git_lite-info.
      git_lite-status = gc_minisemaforo_ambar.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

*   Validación 91 - Warning - Descripción gondola con longitud > 30
    IF git_lite-maktm_gondola IS NOT INITIAL AND strlen( git_lite-maktm_gondola ) > 30.
*     Error: Descripción góndola > 30 para EAN &.
      MESSAGE s017(zretpdt001) WITH git_lite-ean11 INTO git_lite-info.
      git_lite-status = gc_minisemaforo_ambar.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

    IF git_lite-2maktm_gondola IS NOT INITIAL AND strlen( git_lite-2maktm_gondola ) > 30.
*     Error: Descripción góndola > 30 para EAN &.
      MESSAGE s017(zretpdt001) WITH git_lite-2ean11 INTO git_lite-info.
      git_lite-status = gc_minisemaforo_ambar.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

    IF git_lite-3maktm_gondola IS NOT INITIAL AND strlen( git_lite-3maktm_gondola ) > 30.
*     Error: Descripción góndola > 30 para EAN &.
      MESSAGE s017(zretpdt001) WITH git_lite-3ean11 INTO git_lite-info.
      git_lite-status = gc_minisemaforo_ambar.

      MODIFY git_lite.
      CONTINUE.
    ENDIF.

    MODIFY git_lite.
  ENDLOOP.
ENDFORM.

*===================================================================================================
*&      Form  F_CARGAR_FICHEROS
*===================================================================================================
* Rutina que carga la información de los excels de carga por fases y verifica que los datos son
* correctos
*===================================================================================================
FORM f_cargar_ficheros CHANGING ps_error.
* 0.- Declaración de variables
*======================================================================
  DATA: lit_matnr LIKE zretpdt001s11 OCCURS 0 WITH HEADER LINE,
        BEGIN OF lit_matnr_coinc OCCURS 0,
          preca TYPE zzpreca,
          matnr TYPE matnr,
        END OF lit_matnr_coinc,
        lf_error(1),
        lf_ean_duplicado(1),
        lf_articulo_existe(1),
        ld_index              LIKE sy-tabix,
        ld_index_listado      LIKE sy-tabix,
        ld_mensaje            TYPE string,
        ld_matnr              TYPE matnr,
        ld_preca              TYPE zzpreca,
        ld_like(10),
        lit_matnr_error_um    LIKE zretpdt001s11 OCCURS 0 WITH HEADER LINE,
        ld_cont               TYPE int4,
        "Validaciones a aplicar sobre los campos del excel
        lit_validaciones      LIKE zretpdt001t04 OCCURS 0 WITH HEADER LINE,
        ld_3dec               TYPE p DECIMALS 3,
        ld_numreg             TYPE int4,
        ld_cont_listado       TYPE int4,
        ld_porc               TYPE p DECIMALS 10.

* 1.- Logica
*======================================================================
*>Cargar validaciones
  PERFORM f_get_validaciones TABLES lit_validaciones.

*>Cargar ficheros carga FULL
  PERFORM f_cargar_ficheros_full TABLES git_valores_def lit_matnr_error_um CHANGING ps_error.

  IF ps_error = 'X'.
    EXIT.
  ENDIF.

*>Confeccionar listado a partir de los datos leidos en el excel
  PERFORM f_confeccionar_listado_full TABLES lit_matnr.

*>Verificación de datos
  DESCRIBE TABLE git_listado LINES ld_numreg.

  LOOP AT git_listado.
    ADD 1 TO ld_cont_listado.

    ld_porc = ( ld_cont_listado / ld_numreg ) * 100.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = ld_porc
        text       = 'Verificando datos'.


*   Indicador de loop
    ld_index_listado = sy-tabix.

*   Inicializar error
    CLEAR lf_error.

*   Inicializar articulo a OK
    git_listado-status_g = gc_minisemaforo_ambar.
    git_listado-info     = 'Artículo 100% válido para cargar'.

*   Validación 1.01.- Código de artículo ya existe en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.01' activa = 'X'.

    IF sy-subrc = 0.
*     Inicializar flag
      CLEAR lf_articulo_existe.

*     Buscamos código de artículo en BBDD
      SELECT SINGLE matnr
          FROM mara
          INTO git_listado-matnr
         WHERE matnr = git_listado-matnr.

      IF sy-subrc = 0.
        lf_articulo_existe   = 'X'.
        git_listado-info     = 'Articulo ya existe en el sistema.'.
        git_listado-status_g = gc_minisemaforo_rojo.
      ENDIF.

*     Buscamos artículos en el sistema con el mismo EAN en BBDD/Precarga
      LOOP AT git_file2 WHERE matnr = git_listado-matnr.
*       Para cada EAN del artículo a cargar...

*       Miramos si ese EAN está en un artículo de la BBDD
        SELECT SINGLE matnr
          INTO ld_matnr
          FROM mean
         WHERE ean11 = git_file2-ean11.

        IF sy-subrc = 0.
          lf_articulo_existe = 'X'.
          git_listado-info     = 'Articulo ya existe en el sistema.'.
          git_listado-status_g = gc_minisemaforo_rojo.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_articulo_existe = 'X'.
        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.02- El articulo no contiene datos básicos
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.02' activa = 'X'.

    IF sy-subrc = 0.
      IF git_listado-status_1 = gc_minisemaforo_ambar.
        git_listado-info = 'Artículo no contiene Datos básicos.'.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.03 - Error - El articulo tiene informado EAN que ya existe en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.03' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_ean_duplicado.

      LOOP AT git_file2 WHERE matnr = git_listado-matnr.
        SELECT SINGLE ean11
          FROM mean
          INTO git_file2-ean11
         WHERE ean11 = git_file2-ean11.

        IF sy-subrc = 0.
          lf_ean_duplicado = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_ean_duplicado = 'X'.
*       Error: EAN & ya asignado en BBDD.
        MESSAGE s019(zretpdt001) WITH git_file2-ean11 INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.04.- Articulo contiene unidades de medida informadas que no existen en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.04' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT lit_matnr_error_um WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        git_listado-info = 'Artículo con Unidades erróneas.'.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 3.19 - Error - El proveedor no existe
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.19' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file3 WHERE matnr = git_listado-matnr.
*       Para cada registro info del articulo...

*       Verificamos que el proveedor exista en el sistema
        SELECT SINGLE lifnr
          FROM lfa1
          INTO git_file3-lifnr
         WHERE lifnr = git_file3-lifnr
           AND loevm = space.

        IF sy-subrc <> 0.
*         Si proveedor no existe => Error
          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
*       Error: Proveedor & no existe en el sistema.
        MESSAGE s015(zretpdt001) WITH git_file3-lifnr INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.06.- Grupo artículo externo no existe
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.06' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        SELECT SINGLE extwg
          FROM twew
          INTO git_file1-extwg
         WHERE extwg = git_file1-extwg.

        IF sy-subrc <> 0.
*         Error: Grupo artículo externo & no existe en el sistema.
          MESSAGE s018(zretpdt001) WITH git_file1-extwg INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

*   Validación 1.07.- Artículo duplicado en excel datos básicos
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.07' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-pmata IS INITIAL.
        CLEAR ld_cont.
        LOOP AT git_file1 WHERE matnr = git_listado-matnr.
          ADD 1 TO ld_cont.
        ENDLOOP.

        IF ld_cont > 1.
*         Error: Artículo & aparece más de una vez en excel datos básicos
          MESSAGE s020(zretpdt001) WITH git_listado-matnr INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

*   Validación 1.08.- EAN duplicado en excel de UM y EANs
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.08' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr AND numtp <> 'IE'. "APRADAS-16.09.2020
        CLEAR ld_cont.

        LOOP AT git_file2 WHERE ean11 = git_file2-ean11.
          ADD 1 TO ld_cont.
        ENDLOOP.

        IF ld_cont > 1.
*         Error: El ean & del artículo & aparece más de una vez en excel UM.
          MESSAGE s021(zretpdt001) WITH git_file2-ean11 git_listado-matnr INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.09.- Tipo de artículo no existe en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.09' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      SELECT SINGLE mtart
        FROM t134
        INTO git_file1-mtart
       WHERE mtart = git_file1-mtart.

      IF sy-subrc <> 0.
*       Error: Tipo de artículo & no existe en el sistema.
        MESSAGE s022(zretpdt001) WITH git_file1-mtart INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validacion 1.10 - Verificar que la categoría de articulo existe en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.10' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-attyp <> '00' AND
         git_file1-attyp <> '01' AND
         git_file1-attyp <> '02' AND
         git_file1-attyp <> '10' AND
         git_file1-attyp <> '11' AND
         git_file1-attyp <> '12' AND
         git_file1-attyp <> '20' AND
         git_file1-attyp <> '21' AND
         git_file1-attyp <> '22' AND
         git_file1-attyp <> '30'.
*       Error: Categoría de artículos & no existe en el sistema.
        MESSAGE s024(zretpdt001) WITH git_file1-attyp INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.


*   Validación 1.11 - Verificar que el grupo de articulos existe en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.11' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      SELECT SINGLE matkl
        FROM t023
        INTO git_file1-matkl
       WHERE matkl = git_file1-matkl.

      IF sy-subrc <> 0.
*       Error: Grupo de artículos & no existe en el sistema.
        MESSAGE s023(zretpdt001) WITH git_file1-matkl INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.12 -	Indicador de impuestos & no existe en el sistema.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.12' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      SELECT SINGLE taxkm
        FROM tskm
        INTO git_file1-taklv
       WHERE taxkm = git_file1-taklv.

      IF sy-subrc <> 0.
*       Error: Indicador de impuestos & no existe en el sistema.
        MESSAGE s025(zretpdt001) WITH git_file1-taklv INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.

*     >APRADAS-31.03.2021 16:16:38-Inicio
      IF git_file1-taklv2 IS NOT INITIAL.
        SELECT SINGLE taxkm
          FROM tskm
          INTO git_file1-taklv2
         WHERE taxkm = git_file1-taklv2.

        IF sy-subrc <> 0.
*         Error: Indicador de impuestos & no existe en el sistema.
          MESSAGE s025(zretpdt001) WITH git_file1-taklv2 INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          CONTINUE.
        ENDIF.
      ENDIF.
*     <APRADAS-31.03.2021 16:16:38-Fin
    ENDIF.

*   Validación 2.16 - Verificar que el ean tiene la longitud correcta
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.16' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr
                          AND numtp <> 'IE'.
        IF strlen( git_file2-ean11 ) <> 14 AND
           strlen( git_file2-ean11 ) <> 13 AND
           strlen( git_file2-ean11 ) <> 12.
*         Error: El EAN & no es de longitud 12, 13 o 14
          MESSAGE s048(zretpdt001) WITH git_file2-ean11 INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 2.17 - Verificar que el tipo de EAN exista en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.17' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr
                          AND numtp IS NOT INITIAL.
        SELECT SINGLE numtp
          FROM tntp
          INTO git_file2-numtp
         WHERE numtp = git_file2-numtp.

        IF sy-subrc <> 0.
*       Error: El tipo de ean & del ean & del artículo no existe en el sistema.
          MESSAGE s026(zretpdt001) WITH git_file2-numtp git_file2-ean11 git_listado-matnr INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 3.20 - Verificar que la organización de compras exista en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.20' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file3 WHERE matnr = git_listado-matnr.
        SELECT SINGLE ekorg
          FROM t024e
          INTO git_file3-ekorg
         WHERE ekorg = git_file3-ekorg.

        IF sy-subrc <> 0.
*       Error: Organización de compras & no existe en el sistema.
          MESSAGE s028(zretpdt001) WITH git_file3-ekorg INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 3.21 - Verificar que el proveedor-organización de compras exista en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.21' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file3 WHERE matnr = git_listado-matnr.
        SELECT SINGLE ekorg
          FROM lfm1
          INTO git_file3-ekorg
         WHERE ekorg = git_file3-ekorg
           AND lifnr = git_file3-lifnr.

        IF sy-subrc <> 0.
*       Error: Proveedor & no existe en organización de compras &.
          MESSAGE s029(zretpdt001) WITH git_file3-lifnr git_file3-ekorg INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 3.22 - Verificar que las monedas informadas existan en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.22' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file3 WHERE matnr = git_listado-matnr.
        SELECT SINGLE waers
          FROM tcurc
          INTO git_file3-konwa
         WHERE waers = git_file3-konwa.

        IF sy-subrc <> 0.
*       Error: Moneda & no existe en el sistema.
          MESSAGE s030(zretpdt001) WITH git_file3-konwa INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 4.12 - Verificar que la organización de ventas exista en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.12' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file4 WHERE matnr = git_listado-matnr.
        SELECT SINGLE vkorg
          FROM tvko
          INTO git_file4-vkorg
         WHERE vkorg = git_file4-vkorg.

        IF sy-subrc <> 0.
*       Error: Org.Ventas & no existe en el sistema.
          MESSAGE s031(zretpdt001) WITH git_file4-vkorg INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 4.13 - Verificar que canal de ventas existe en el sistema
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.13' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file4 WHERE matnr = git_listado-matnr.
        SELECT SINGLE vtweg
          FROM tvtw
          INTO git_file4-vtweg
         WHERE vtweg = git_file4-vtweg.

        IF sy-subrc <> 0.
*       Error: Canal de ventas & no existe en el sistema.
          MESSAGE s032(zretpdt001) WITH git_file4-vtweg INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 4.14 - Verificar que el inicio de validez del precio de venta sea inferior al fin de validez.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.14' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file4 WHERE matnr = git_listado-matnr.
        IF git_file4-datab > git_file4-datbi.
*         Error: Fecha inicio pvp > fecha fin pvp
          MESSAGE s033(zretpdt001) INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.22: Verificar que si han informado unicon hayan informado unidad
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.22' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-inhme IS NOT INITIAL AND git_file1-inhal IS INITIAL.
*       Error: Se ha informado UNICON pero no unidad formato
        MESSAGE s037(zretpdt001) WITH git_file1-matkl INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.23: Verificar que si han informado unidad de formado hayan informado unicon
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.23' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-inhme IS INITIAL AND git_file1-inhal IS NOT INITIAL.
*       Error: Se ha informado unidad formato pero no UNICON
        MESSAGE s038(zretpdt001) WITH git_file1-matkl INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.24.- Artículo no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.24' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-matnr IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Número de artículo' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.25.- Tipo de artículo no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.25' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-mtart IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Tipo de artículo' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.26.- Categoría de artículo no informada
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.26' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-attyp IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Categoría de artículo' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.27.- Grupo de artículos no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.27' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-matkl IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Grupo de artículos' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.28.- Texto breve de artículo no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.28' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-maktx IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Texto breve de artículo' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.29.- Clasificación fiscal artículo no informada
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.29' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-taklv IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Clasificación fiscal artículo' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.30.- Grupo de artículos externo no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.30' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-extwg IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Grupo de artículos externo' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.31.- Grupo de compras no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.31' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-ekgrp IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Grupo de compras' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.43.- Datos básicos: Grupo de compras & no existe.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.43' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      SELECT SINGLE ekgrp
        FROM t024
        INTO git_file1-ekgrp
       WHERE ekgrp = git_file1-ekgrp.

      IF sy-subrc <> 0.
*       Error: Datos básicos: Grupo de compras & no existe.
        MESSAGE s054(zretpdt001) WITH git_file1-ekgrp INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.44.- Datos básicos: Marca & no existe.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.44' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-brand_id IS NOT INITIAL. "APRADAS-01.02.2021
        SELECT SINGLE brand_id
          FROM wrf_brands
          INTO git_file1-brand_id
         WHERE brand_id = git_file1-brand_id.

        IF sy-subrc <> 0.
*         Error: Datos básicos: Marca & no existe.
          MESSAGE s055(zretpdt001) WITH git_file1-brand_id INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          CONTINUE.
        ENDIF.
      ENDIF. "APRADAS-01.02.2021
    ENDIF.

*   Validación 1.45: Validar valor LOGGR
    PERFORM f_validacion_full_art_145 TABLES lit_validaciones USING ld_index_listado CHANGING lf_error.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.

*   Validación 1.46: Validar KITs
    PERFORM f_validacion_full_art_146 TABLES lit_validaciones USING ld_index_listado CHANGING lf_error.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.

*   >APRADAS-01.02.2021 08:53:03-Inicio
*   Validación 1.47: Validación MARA-MSTAE y MARA-MSTDE informados conjuntamente
    PERFORM f_validacion_full_art_147 TABLES lit_validaciones USING ld_index_listado CHANGING lf_error.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.

*   Validación 1.48: Validación MARA-MSTAV y MARA-MSTDV informados conjuntamente
    PERFORM f_validacion_full_art_148 TABLES lit_validaciones USING ld_index_listado CHANGING lf_error.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.
*   <APRADAS-01.02.2021 08:53:03-Fin


*   Validación 1.32.- Grupo de carga no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.32' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-ladgr IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Grupo de carga' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.33.- Marca no informada
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.33' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-brand_id IS INITIAL AND git_file1-mtart <> 'ZSER'.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Marca' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.34.- Unidad de medida base no informada
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.34' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-meins IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Unidad de medida base' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.35.- Núm.almacén no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.35' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-lgnum IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Núm.almacén/Complejo alm.' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.36.- Ind. tipo de almacén entrada no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.36' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-ltkze IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Ind. tipo de almacén entrada' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.37.- Ind. tipo de almacén salida no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.37' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-ltkza IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Ind. tipo de almacén salida' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.41.- Contenido no informado
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.41' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-inhal IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'Contenido' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 1.42.- UM Contenido no informada
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.42' activa = 'X'.

    IF sy-subrc = 0.
      LOOP AT git_file1 WHERE matnr = git_listado-matnr.
        EXIT.
      ENDLOOP.

      IF git_file1-inhme IS INITIAL.
*       Error: Excel &, campo & no informado
        MESSAGE s036(zretpdt001) WITH 'Datos básicos' 'UM Contenido' INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*>>>Validaciones Excel 2-UMs/EANs
    CLEAR lf_error.
    LOOP AT git_file2 WHERE matnr = git_listado-matnr.
*     Validacion 2.1.- Código EAN no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.01' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-ean11 IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Código EAN' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validacion 2.2.- Tipo EAN no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.02' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-numtp IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Tipo EAN' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validacion 2.3.- Código artículo no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.03' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-matnr IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Código artículo' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validacion 2.4.- Unidad Medida Alternativa no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.04' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-meinh IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Unidad Medida Alternativa' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validacion 2.5.- Factor conversión (sobre UMB) no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.05' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-umrez IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Factor conversión (sobre UMB)' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validacion 2.6.- Factor conversión (sobre UMA) non informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.06' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-umren IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Factor conversión (sobre UMA)' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 2.10.- Peso bruto no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.10' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-brgew IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Peso Bruto' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 2.11.- Unidad de peso no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.11' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-gewei IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Unidad de peso' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 2.12.- Longitud no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.12' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-laeng IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Longitud' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 2.13.- Ancho no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.13' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-breit IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Ancho' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 2.14.- Altura no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.14' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-hoehe IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Altura' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 2.15.- Unidad de dimensión no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.15' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file2-meabm IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'UM/EANs' 'Unidad de dimensión' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.

*>>>Validaciones EXCEL 2-Registro info
    CLEAR lf_error.
    LOOP AT git_file3 WHERE matnr = git_listado-matnr.
*     Validacion 3.1.- Artículo no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.01' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-matnr IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Código artículo' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.2.- Proveedor no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.02' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-lifnr IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Número de cuenta del proveedor o acreedor' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.3.- Organización de compras no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.03' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-ekorg IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Organización de compras' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.04: Validación precios de compra PB00/PB30
      PERFORM f_validacion_full_art_304 TABLES lit_validaciones USING ld_index_listado git_file3 CHANGING lf_error.

      IF lf_error = 'X'.
        EXIT.
      ENDIF.

*     Validación 3.10.- Clave control confirmación no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.10' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-bstae IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Clave de control de confirmaciones' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.11.- Plazo de entrega no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.11' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-plifz IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Plazo entrega' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.12.- Cantidad pedido estandar no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.12' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-norbm IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Cantidad pedido estándar' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.13.- Indicador IVA no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.13' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-mwskz IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Indicador IVA' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.14.- Infotipo no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.14' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-esokz IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Infotipo' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.15.- Material Proveedor no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.15' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-idnlf IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Material Proveedor' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.16.- Cantidad mínima pedido no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.16' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-minbm IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'Registro info' 'Cantidad mínima pedido' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.17.- Fecha desde > Fecha fin
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.17' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file3-datab > git_file3-datbi.
*         Error: Registro info: Fecha desde > Fecha fin
          MESSAGE s051(zretpdt001) INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.18.- Cantidad mínima pedido tiene que ser inferior a cantidad pedido estandar
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.18' activa = 'X'.

      IF sy-subrc = 0 AND git_file3-minbm IS NOT INITIAL.
        IF git_file3-minbm > git_file3-norbm.
*         Error: Cantidad mínima pedido tiene que ser inferior a cantidad pedido estandar
          MESSAGE s052(zretpdt001) INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 3.23.- Reg.Info: Grupo de compras & no existe.
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.23' activa = 'X'.

      IF sy-subrc = 0.
        SELECT SINGLE ekgrp
          FROM t024
          INTO git_file3-ekgrp
         WHERE ekgrp = git_file3-ekgrp.

        IF sy-subrc <> 0.
*         Error: Reg.Info: Grupo de compras & no existe.
          MESSAGE s060(zretpdt001) WITH git_file3-ekgrp INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.

*>>>Validaciones Excel 4-PVP
    CLEAR lf_error.
    LOOP AT git_file4 WHERE matnr = git_listado-matnr.
*     Validación 4.1.- Organizacion de ventas no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.01' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-vkorg IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Organización de ventas' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.2.- Canal de ventas no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.02' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-vtweg IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Canal de distribución' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.3.- UM Venta no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.03' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-meinh IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'UM Venta' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.4.- Código artículo no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.04' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-matnr IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Código artículo' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validacioón 4.5.- Valor de condición no informado
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.05' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-kwert IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Valor de la condición' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.6.- Moneda condición no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.06' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-konwa IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Moneda condición' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.7.- Por no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.07' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-kpein IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Por' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.8.- UM Condicion no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.08' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-kmein IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'UM Condición' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.9.- Fecha inicio validez no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.09' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-datab IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Fecha inicio validez' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.10.- Fecha fin validez no informada
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.10' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-datbi IS INITIAL.
*         Error: Excel &, campo & no informado
          MESSAGE s036(zretpdt001) WITH 'PVP' 'Fecha fin validez' INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

*     Validación 4.11.- Fecha desde > Fecha Fin
      READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '4.11' activa = 'X'.

      IF sy-subrc = 0.
        IF git_file4-datab > git_file4-datbi.
*         Error: PVP: Fecha desde > Fecha Fin
          MESSAGE s053(zretpdt001) INTO git_listado-info.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
          lf_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lf_error = 'X'.
      CONTINUE.
    ENDIF.

*   Validación 2.21.- Peso no puede contener 3 decimales.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.21' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr.
        ld_3dec = git_file2-brgew MOD '0.010'.

        IF ld_3dec > 0.
          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
*       Error: EAN's UM's: Peso & contiene 3 decimales.
        MESSAGE s056(zretpdt001) WITH git_file2-brgew INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 2.22.- Longitud no puede contener 3 decimales.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.22' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr.
        ld_3dec = git_file2-laeng MOD '0.010'.

        IF ld_3dec > 0.
          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
*       Error: EAN's UM's: Longitud & contiene 3 decimales.
        MESSAGE s057(zretpdt001) WITH git_file2-laeng INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 2.23.- EAN's UM's: Ancho & contiene 3 decimales.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.23' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr.
        ld_3dec = git_file2-breit MOD '0.010'.

        IF ld_3dec > 0.
          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
*       Error: EAN's UM's: Ancho & contiene 3 decimales.
        MESSAGE s058(zretpdt001) WITH git_file2-breit INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación 2.24.- EAN's UM's: Altura & contiene 3 decimales.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '2.24' activa = 'X'.

    IF sy-subrc = 0.
      CLEAR lf_error.
      LOOP AT git_file2 WHERE matnr = git_listado-matnr.
        ld_3dec = git_file2-hoehe MOD '0.010'.

        IF ld_3dec > 0.
          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
*       Error: EAN's UM's: Altura & contiene 3 decimales.
        MESSAGE s059(zretpdt001) WITH git_file2-laeng INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Validación X - Artículo sin datos de EAN y/o registro info y/o precio venta
    IF git_listado-status_2 = gc_minisemaforo_ambar OR
       git_listado-status_3 = gc_minisemaforo_ambar OR
       git_listado-status_4 = gc_minisemaforo_ambar.
      git_listado-info = 'Artículo sin datos de UM/EAN y/o registro y/o info/precio venta'.
      git_listado-status_g = gc_minisemaforo_ambar.

      MODIFY git_listado.
      CONTINUE.
    ENDIF.

    MODIFY git_listado.
  ENDLOOP.

*>Determinar el valor para la columna ORDEN (1=Artículos normales, 2=Artículos KIT)
  LOOP AT git_listado.
    LOOP AT git_file1 WHERE matnr = git_listado-matnr.
      ld_index = sy-tabix.
      EXIT.
    ENDLOOP.

    IF git_file1-mtart = 'ZKIT'.
      git_listado-orden = 2.
    ELSE.
      git_listado-orden = 1.
    ENDIF.

    MODIFY git_listado.
  ENDLOOP.

*>Ordenar listado
  SORT git_listado.

  IF p_opc2 = 'X'.
*   Si nivel de visualización a nivel de detalle...
    PERFORM f_cargar_ficheros_detalle.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_mostrar_datos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_listar_datos USING     pe_estructura     LIKE  dd02l-tabname
                              pe_status         TYPE  slis_formname
                              pe_ucomm          TYPE  slis_formname
                              pe_top            TYPE  slis_formname
                              pe_top_html       TYPE  slis_formname
                              pe_datos
                              pe_color_line
                              pe_sel.

*0.- Declaracion de variables
*===================================================================================================
  DATA: lit_fieldcatalog TYPE         lvc_t_fcat,
        wa_fieldcatalog  TYPE LINE OF lvc_t_fcat,
        lr_layout        TYPE         lvc_s_layo,
        lit_sort         TYPE         slis_t_sortinfo_alv,
        wa_sort          TYPE         slis_sortinfo_alv,
        ld_index         LIKE         sy-tabix,
        lit_sort_lvc     TYPE         lvc_t_sort,
        wa_sort_lvc      TYPE         lvc_s_sort.

* 1.- Logica
*===================================================================================================
  FIELD-SYMBOLS: <fs_tabla> TYPE STANDARD TABLE.

  ASSIGN (pe_datos) TO <fs_tabla>.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = pe_estructura
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = lit_fieldcatalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0. ENDIF.

*  lr_layout-zebra             = 'X'.
*  lr_layout-no_hgridln  = 'X'.
  lr_layout-stylefname = 'FIELD_STYLE'.
  lr_layout-cwidth_opt = 'X'.
  IF pe_sel = 'X'.
    lr_layout-box_fname = 'SEL'.
  ENDIF.

  IF pe_color_line <> ''.
    lr_layout-info_fname = pe_color_line.
  ENDIF.

*  lr_layout-ctab_fname = 'CELLCOLR'.

  IF p_opc1 = 'X'.
    LOOP AT lit_fieldcatalog INTO wa_fieldcatalog WHERE fieldname(2) = 'F1'
                                                     OR fieldname(2) = 'F2'
                                                     OR fieldname(2) = 'F3'
                                                     OR fieldname(2) = 'F4'.
      DELETE lit_fieldcatalog INDEX sy-tabix.
      CONTINUE.
    ENDLOOP.
  ELSE.
    LOOP AT lit_fieldcatalog INTO wa_fieldcatalog WHERE fieldname(2) = 'F1'
                                                     OR fieldname(2) = 'F2'
                                                     OR fieldname(2) = 'F3'
                                                     OR fieldname(2) = 'F4'.
      ld_index = sy-tabix.

      IF wa_fieldcatalog-fieldname(2) = 'F1'.
        IF wa_fieldcatalog-fieldname = 'F1_PMATA' OR
           wa_fieldcatalog-fieldname = 'F1_CANTC'.
          wa_fieldcatalog-emphasize = 'C500'.
        ELSE.
          wa_fieldcatalog-emphasize = 'C511'.
        ENDIF.
      ELSEIF wa_fieldcatalog-fieldname(2) = 'F2'.
        wa_fieldcatalog-emphasize = 'C711'.
      ELSEIF wa_fieldcatalog-fieldname(2) = 'F3'.
        wa_fieldcatalog-emphasize = 'C311'.
      ELSEIF wa_fieldcatalog-fieldname(2) = 'F4'.
        wa_fieldcatalog-emphasize = 'C111'.
      ENDIF.

      MODIFY lit_fieldcatalog FROM wa_fieldcatalog INDEX ld_index.
    ENDLOOP.
  ENDIF.

  LOOP AT lit_fieldcatalog INTO wa_fieldcatalog.
    ld_index = sy-tabix.

    CASE wa_fieldcatalog-fieldname.
      WHEN 'F1_LTKZE' OR 'F1_LTKZA'.
        DELETE lit_fieldcatalog INDEX ld_index.
        CONTINUE.
      WHEN 'SEL'.
        DELETE lit_fieldcatalog INDEX ld_index.
        CONTINUE.
      WHEN 'ZZDESCP' OR 'F1_ZZDESCP'.
        wa_fieldcatalog-checkbox = 'X'.

      WHEN 'ORDEN'.
        wa_fieldcatalog-reptext    = 'Orden'.
        wa_fieldcatalog-scrtext_l  = 'O'.
        wa_fieldcatalog-scrtext_m  = 'O'.
        wa_fieldcatalog-scrtext_s  = 'O'.
      WHEN 'C1T'.
        wa_fieldcatalog-reptext    = 'Tipo Coincidencia 1'.
        wa_fieldcatalog-scrtext_l  = 'TC1'.
        wa_fieldcatalog-scrtext_m  = 'TC1'.
        wa_fieldcatalog-scrtext_s  = 'TC1'.

      WHEN 'C1'.
        wa_fieldcatalog-reptext    = 'Coincidencia 1'.
        wa_fieldcatalog-scrtext_l  = 'Coinc.1'.
        wa_fieldcatalog-scrtext_m  = 'Coinc.1'.
        wa_fieldcatalog-scrtext_s  = 'Coinc.1'.

      WHEN 'C2T'.
        wa_fieldcatalog-reptext    = 'Tipo Coincidencia 2'.
        wa_fieldcatalog-scrtext_l  = 'TC2'.
        wa_fieldcatalog-scrtext_m  = 'TC2'.
        wa_fieldcatalog-scrtext_s  = 'TC2'.

      WHEN 'C2'.
        wa_fieldcatalog-reptext    = 'Coincidencia 2'.
        wa_fieldcatalog-scrtext_l  = 'Coinc.2'.
        wa_fieldcatalog-scrtext_m  = 'Coinc.2'.
        wa_fieldcatalog-scrtext_s  = 'Coinc.2'.

      WHEN 'C3T'.
        wa_fieldcatalog-reptext    = 'Tipo Coincidencia 3'.
        wa_fieldcatalog-scrtext_l  = 'TC3'.
        wa_fieldcatalog-scrtext_m  = 'TC3'.
        wa_fieldcatalog-scrtext_s  = 'TC3'.

      WHEN 'C3'.
        wa_fieldcatalog-reptext    = 'Coincidencia 3'.
        wa_fieldcatalog-scrtext_l  = 'Coinc.3'.
        wa_fieldcatalog-scrtext_m  = 'Coinc.3'.
        wa_fieldcatalog-scrtext_s  = 'Coinc.3'.

      WHEN 'C4T'.
        wa_fieldcatalog-reptext    = 'Tipo Coincidencia 4'.
        wa_fieldcatalog-scrtext_l  = 'TC4'.
        wa_fieldcatalog-scrtext_m  = 'TC4'.
        wa_fieldcatalog-scrtext_s  = 'TC4'.

      WHEN 'C4'.
        wa_fieldcatalog-reptext    = 'Coincidencia 4'.
        wa_fieldcatalog-scrtext_l  = 'Coinc.4'.
        wa_fieldcatalog-scrtext_m  = 'Coinc.4'.
        wa_fieldcatalog-scrtext_s  = 'Coinc.4'.

      WHEN 'STATUS'.
        wa_fieldcatalog-reptext    = 'Status Proceso'.
        wa_fieldcatalog-scrtext_l  = 'Stat'.
        wa_fieldcatalog-scrtext_m  = 'Stat'.
        wa_fieldcatalog-scrtext_s  = 'Stat'.
        wa_fieldcatalog-just       = 'C'.
        wa_fieldcatalog-icon       = 'X'.
      WHEN 'STATUS_1'.
        wa_fieldcatalog-reptext    = 'Carga datos básicos'.
        wa_fieldcatalog-scrtext_l  = 'StatDB'.
        wa_fieldcatalog-scrtext_m  = 'StatDB'.
        wa_fieldcatalog-scrtext_s  = 'StatDB'.
        wa_fieldcatalog-just       = 'C'.
      WHEN 'STATUS_2'.
        wa_fieldcatalog-reptext    = 'Carga EAN''s/UM'.
        wa_fieldcatalog-scrtext_l  = 'StatEAN'.
        wa_fieldcatalog-scrtext_m  = 'StatEAN'.
        wa_fieldcatalog-scrtext_s  = 'StatEAN'.
        wa_fieldcatalog-just       = 'C'.
      WHEN 'STATUS_3'.
        wa_fieldcatalog-reptext    = 'Carga Registros Info'.
        wa_fieldcatalog-scrtext_l  = 'StatRI'.
        wa_fieldcatalog-scrtext_m  = 'StatRI'.
        wa_fieldcatalog-scrtext_s  = 'StatRI'.
        wa_fieldcatalog-just       = 'C'.
      WHEN 'STATUS_4'.
        wa_fieldcatalog-reptext    = 'Carga PVPs'.
        wa_fieldcatalog-scrtext_l  = 'StatPVPs'.
        wa_fieldcatalog-scrtext_m  = 'StatPVPs'.
        wa_fieldcatalog-scrtext_s  = 'StatPVPs'.
        wa_fieldcatalog-just       = 'C'.
      WHEN 'STATUS_G'.
        wa_fieldcatalog-reptext    = 'Status proceso carga'.
        wa_fieldcatalog-scrtext_l  = 'StatCarga'.
        wa_fieldcatalog-scrtext_m  = 'StatCarga'.
        wa_fieldcatalog-scrtext_s  = 'StatCarga'.
        wa_fieldcatalog-just       = 'C'.
      WHEN 'MATNR'.
        wa_fieldcatalog-hotspot    = 'X'.
      WHEN 'PASO'.
        wa_fieldcatalog-reptext    = 'Número de paso'.
        wa_fieldcatalog-scrtext_l  = 'NPaso'.
        wa_fieldcatalog-scrtext_m  = 'NPaso'.
        wa_fieldcatalog-scrtext_s  = 'NPaso'.
      WHEN 'PASOT'.
        wa_fieldcatalog-reptext    = 'Paso'.
        wa_fieldcatalog-scrtext_l  = 'Paso'.
        wa_fieldcatalog-scrtext_m  = 'Paso'.
        wa_fieldcatalog-scrtext_s  = 'Paso'.
      WHEN 'PASOS'.
        wa_fieldcatalog-reptext    = 'Status Paso'.
        wa_fieldcatalog-scrtext_l  = 'StatP'.
        wa_fieldcatalog-scrtext_m  = 'StatP'.
        wa_fieldcatalog-scrtext_s  = 'StatP'.
      WHEN 'PASON'.
        wa_fieldcatalog-reptext    = 'Pos'.
        wa_fieldcatalog-scrtext_l  = 'Pos'.
        wa_fieldcatalog-scrtext_m  = 'Pos'.
        wa_fieldcatalog-scrtext_s  = 'Pos'.
      WHEN 'INFO'.
        wa_fieldcatalog-reptext    = 'Mensaje información'.
        wa_fieldcatalog-scrtext_l  = 'Info'.
        wa_fieldcatalog-scrtext_m  = 'Info'.
        wa_fieldcatalog-scrtext_s  = 'Info'.
        wa_fieldcatalog-fix_column = 'X'.
      WHEN 'LINEAEXCEL'.
        wa_fieldcatalog-reptext    = 'Linea del excel de carga'.
        wa_fieldcatalog-scrtext_l  = 'Excel'.
        wa_fieldcatalog-scrtext_m  = 'Excel'.
        wa_fieldcatalog-scrtext_s  = 'Excel'.
      WHEN 'LINEAEXCEL'.
        wa_fieldcatalog-reptext    = 'Linea del excel de carga'.
        wa_fieldcatalog-scrtext_l  = 'Excel'.
        wa_fieldcatalog-scrtext_m  = 'Excel'.
        wa_fieldcatalog-scrtext_s  = 'Excel'.
      WHEN 'MAKTM_EAN' OR '2MAKTM_EAN' OR '3MAKTM_EAN'.
        wa_fieldcatalog-reptext    = 'Texto TPV'.
        wa_fieldcatalog-scrtext_l  = 'Texto TPV'.
        wa_fieldcatalog-scrtext_m  = 'Texto TPV'.
        wa_fieldcatalog-scrtext_s  = 'Texto TPV'.
      WHEN 'MAKTM_GONDOLA' OR '2MAKTM_GONDOLA' OR '3MAKTM_GONDOLA'.
        wa_fieldcatalog-reptext    = 'Texto Góndola'.
        wa_fieldcatalog-scrtext_l  = 'Texto Góndola'.
        wa_fieldcatalog-scrtext_m  = 'Texto Góndola'.
        wa_fieldcatalog-scrtext_s  = 'Texto Góndola'.
      WHEN 'F1_PRAT1' OR
           'F1_PRAT2' OR
           'F1_PRAT3' OR
           'F1_PRAT4' OR
           'F1_PRAT5'.
        wa_fieldcatalog-checkbox = 'X'.

      WHEN 'F1_PMATA'.
        wa_fieldcatalog-reptext    = 'Artículo Padre (KIT)'.
        wa_fieldcatalog-scrtext_l  = 'KIT'.
        wa_fieldcatalog-scrtext_m  = 'KIT'.
        wa_fieldcatalog-scrtext_s  = 'KIT'.
      WHEN 'F1_CANTC'.
        wa_fieldcatalog-reptext    = 'Cantidad Componente'.
        wa_fieldcatalog-scrtext_l  = 'CANTC'.
        wa_fieldcatalog-scrtext_m  = 'CANTC'.
        wa_fieldcatalog-scrtext_s  = 'CANTC'.
      WHEN 'F1_EISBE'.
        wa_fieldcatalog-reptext    = 'F1_EISBE-Stock seguridad'.
        wa_fieldcatalog-scrtext_l  = 'F1_EISBE'.
        wa_fieldcatalog-scrtext_m  = 'F1_EISBE'.
        wa_fieldcatalog-scrtext_s  = 'F1_EISBE'.
      WHEN 'F1_MFRGR'.
        wa_fieldcatalog-reptext    = 'F1_MFRGR-Clasificación energética'.
        wa_fieldcatalog-scrtext_l  = 'F1_MFRGR'.
        wa_fieldcatalog-scrtext_m  = 'F1_MFRGR'.
        wa_fieldcatalog-scrtext_s  = 'F1_MFRGR'.
      WHEN 'F3_LIFNR'.
        wa_fieldcatalog-reptext    = 'Proveedor'.
        wa_fieldcatalog-scrtext_l  = 'LIFNR'.
        wa_fieldcatalog-scrtext_m  = 'LIFNR'.
        wa_fieldcatalog-scrtext_s  = 'LIFNR'.
      WHEN 'F3_NETPR'.
        wa_fieldcatalog-reptext    = 'Valor condición PB00'.
        wa_fieldcatalog-scrtext_l  = 'PB00-KBETR'.
        wa_fieldcatalog-scrtext_m  = 'PB00-KBETR'.
        wa_fieldcatalog-scrtext_s  = 'PB00-KBETR'.
      WHEN 'F3_DATAB'.
        wa_fieldcatalog-reptext    = 'Inicio validez PB00'.
        wa_fieldcatalog-scrtext_l  = 'PB00-DATAB'.
        wa_fieldcatalog-scrtext_m  = 'PB00-DATAB'.
        wa_fieldcatalog-scrtext_s  = 'PB00-DATAB'.
      WHEN 'F3_DATBI'.
        wa_fieldcatalog-reptext    = 'Fin validez PB00'.
        wa_fieldcatalog-scrtext_l  = 'PB00-DATBI'.
        wa_fieldcatalog-scrtext_m  = 'PB00-DATBI'.
        wa_fieldcatalog-scrtext_s  = 'PB00-DATBI'.
      WHEN 'F3_KONWA'.
        wa_fieldcatalog-reptext    = 'Unidad condición PB00'.
        wa_fieldcatalog-scrtext_l  = 'PB00-KONWA'.
        wa_fieldcatalog-scrtext_m  = 'PB00-KONWA'.
        wa_fieldcatalog-scrtext_s  = 'PB00-KONWA'.
      WHEN 'F3_KPEIN'.
        wa_fieldcatalog-reptext    = 'Cantidad base PB00'.
        wa_fieldcatalog-scrtext_l  = 'PB00-KPEIN'.
        wa_fieldcatalog-scrtext_m  = 'PB00-KPEIN'.
        wa_fieldcatalog-scrtext_s  = 'PB00-KPEIN'.
      WHEN 'F3_KMEIN'.
        wa_fieldcatalog-reptext    = 'Unidad medida PB00'.
        wa_fieldcatalog-scrtext_l  = 'PB00-KMEIN'.
        wa_fieldcatalog-scrtext_m  = 'PB00-KMEIN'.
        wa_fieldcatalog-scrtext_s  = 'PB00-KMEIN'.
      WHEN 'F3_NETPR_PB30'.
        wa_fieldcatalog-reptext    = 'Valor condición PB30'.
        wa_fieldcatalog-scrtext_l  = 'PB30-KBETR'.
        wa_fieldcatalog-scrtext_m  = 'PB30-KBETR'.
        wa_fieldcatalog-scrtext_s  = 'PB30-KBETR'.
      WHEN 'F3_DATAB_PB30'.
        wa_fieldcatalog-reptext    = 'Inicio validez PB30'.
        wa_fieldcatalog-scrtext_l  = 'PB30-DATAB'.
        wa_fieldcatalog-scrtext_m  = 'PB30-DATAB'.
        wa_fieldcatalog-scrtext_s  = 'PB30-DATAB'.
      WHEN 'F3_DATBI_PB30'.
        wa_fieldcatalog-reptext    = 'Fin validez PB30'.
        wa_fieldcatalog-scrtext_l  = 'PB30-DATBI'.
        wa_fieldcatalog-scrtext_m  = 'PB30-DATBI'.
        wa_fieldcatalog-scrtext_s  = 'PB30-DATBI'.
      WHEN 'F3_KONWA_PB30'.
        wa_fieldcatalog-reptext    = 'Unidad condición PB30'.
        wa_fieldcatalog-scrtext_l  = 'PB30-KONWA'.
        wa_fieldcatalog-scrtext_m  = 'PB30-KONWA'.
        wa_fieldcatalog-scrtext_s  = 'PB30-KONWA'.
      WHEN 'F3_KPEIN_PB30'.
        wa_fieldcatalog-reptext    = 'Cantidad base PB30'.
        wa_fieldcatalog-scrtext_l  = 'PB30-KPEIN'.
        wa_fieldcatalog-scrtext_m  = 'PB30-KPEIN'.
        wa_fieldcatalog-scrtext_s  = 'PB30-KPEIN'.
      WHEN 'F3_KMEIN_PB30'.
        wa_fieldcatalog-reptext    = 'Unidad medida PB30'.
        wa_fieldcatalog-scrtext_l  = 'PB30-KMEIN'.
        wa_fieldcatalog-scrtext_m  = 'PB30-KMEIN'.
        wa_fieldcatalog-scrtext_s  = 'PB30-KMEIN'.
      WHEN ''.
        wa_fieldcatalog-reptext    = ''.
        wa_fieldcatalog-scrtext_l  = ''.
        wa_fieldcatalog-scrtext_m  = ''.
        wa_fieldcatalog-scrtext_s  = ''.
    ENDCASE.

    MODIFY lit_fieldcatalog FROM wa_fieldcatalog.
  ENDLOOP.

  IF pe_estructura = 'ZRETPDT001S10'.
    LOOP AT lit_fieldcatalog INTO wa_fieldcatalog.
      IF wa_fieldcatalog-fieldname(1) = '2'.
        wa_fieldcatalog-emphasize = 'C511'..
      ELSEIF wa_fieldcatalog-fieldname(1) = '3'.
        wa_fieldcatalog-emphasize = 'C411'.
      ENDIF.

      MODIFY lit_fieldcatalog FROM wa_fieldcatalog.
    ENDLOOP.
  ENDIF.

  IF p_mon3 <> 'X' AND
     p_mon2 <> 'X'.
    wa_sort_lvc-spos = 1.
    wa_sort_lvc-fieldname = 'ORDEN'.
    wa_sort_lvc-up = 'X'.
    APPEND wa_sort_lvc TO lit_sort_lvc.


    wa_sort_lvc-spos = 2.
    wa_sort_lvc-fieldname = 'MATNR'.
    wa_sort_lvc-up = 'X'.
    APPEND wa_sort_lvc TO lit_sort_lvc.

    IF p_mon1 = 'X'.
      wa_sort_lvc-spos = 3.
      wa_sort_lvc-fieldname = 'MATNRT'.
      wa_sort_lvc-up = 'X'.
      APPEND wa_sort_lvc TO lit_sort_lvc.
    ELSE.
      wa_sort_lvc-spos = 3.
      wa_sort_lvc-fieldname = 'PASO'.
      wa_sort_lvc-up = 'X'.
      APPEND wa_sort_lvc TO lit_sort_lvc.
    ENDIF.
  ENDIF.
*  wa_sort_lvc-spos = 3.
*  wa_sort_lvc-fieldname = 'STATUS_1'.
*  wa_sort_lvc-up = 'X'.
*  APPEND wa_sort_lvc TO lit_sort_lvc.
*
*  wa_sort_lvc-spos = 4.
*  wa_sort_lvc-fieldname = 'STATUS_2'.
*  wa_sort_lvc-up = 'X'.
*  APPEND wa_sort_lvc TO lit_sort_lvc.
*
*  wa_sort_lvc-spos = 5.
*  wa_sort_lvc-fieldname = 'STATUS_3'.
*  wa_sort_lvc-up = 'X'.
*  APPEND wa_sort_lvc TO lit_sort_lvc.
*
*  wa_sort_lvc-spos = 6.
*  wa_sort_lvc-fieldname = 'STATUS_G'.
*  wa_sort_lvc-up = 'X'.
*  APPEND wa_sort_lvc TO lit_sort_lvc.
*
*  wa_sort_lvc-spos = 7.
*  wa_sort_lvc-fieldname = 'INFO'.
*  wa_sort_lvc-up = 'X'.
*  APPEND wa_sort_lvc TO lit_sort_lvc.



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK           = ' '
*     I_BYPASSING_BUFFER          =
*     I_BUFFER_ACTIVE             =
      i_callback_program          = sy-repid
      i_callback_pf_status_set    = pe_status
      i_callback_user_command     = pe_ucomm
      i_callback_top_of_page      = pe_top
      i_callback_html_top_of_page = pe_top_html
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME            =
*     i_background_id             = ''
*     I_GRID_TITLE                =
*     I_GRID_SETTINGS             =
      is_layout_lvc               = lr_layout
      it_fieldcat_lvc             = lit_fieldcatalog
*     IT_EXCLUDING                =
*     IT_SPECIAL_GROUPS_LVC       =
      it_sort_lvc                 = lit_sort_lvc
*     IT_FILTER_LVC               =
*     IT_HYPERLINK                =
*     IS_SEL_HIDE                 =
      i_default                   = 'X'
      i_save                      = 'A'
*     is_variant                  =
*     it_events                   =
*     IT_EVENT_EXIT               =
*     IS_PRINT_LVC                =
*     IS_REPREP_ID_LVC            =
*     I_SCREEN_START_COLUMN       = 0
*     I_SCREEN_START_LINE         = 0
*     I_SCREEN_END_COLUMN         = 0
*     I_SCREEN_END_LINE           = 0
*     I_HTML_HEIGHT_TOP           =
*     I_HTML_HEIGHT_END           =
*     IT_ALV_GRAPHICS             =
*     IT_EXCEPT_QINFO_LVC         =
*     IR_SALV_FULLSCREEN_ADAPTER  =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER     =
*     ES_EXIT_CAUSED_BY_USER      =
    TABLES
      t_outtab                    = <fs_tabla>
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0. ENDIF.

ENDFORM.                    " f_listar_datos

FORM f_top_of_page_html USING top TYPE REF TO cl_dd_document.
  DATA: ld_contador   TYPE int4,
        ld_texto(255),
        ld_fecha(10),
        ld_hora(8),
        BEGIN OF lit_matnr OCCURS 0,
          matnr LIKE mara-matnr,
        END OF lit_matnr.

  WRITE gd_fecha TO ld_fecha.
  WRITE gd_hora TO ld_hora.

  CALL METHOD top->add_text
    EXPORTING
      text      = 'MONITOR CARGA DE ARTICULOS POR FASES'
*     TEXT_TABLE    =
*     FIX_LINES =
      sap_style = 'HEADING'.
*     SAP_COLOR     =
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  =
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
*   CHANGING
*     DOCUMENT      =
  .

  CALL METHOD top->new_line( ).
  CALL METHOD top->new_line( ).


  LOOP AT git_listado.
    lit_matnr-matnr = git_listado-matnr.
    APPEND lit_matnr.
  ENDLOOP.

  SORT lit_matnr.
  DELETE ADJACENT DUPLICATES FROM lit_matnr.

  DESCRIBE TABLE lit_matnr LINES ld_contador.
  WRITE ld_contador TO ld_texto LEFT-JUSTIFIED.

  CALL METHOD top->add_text
    EXPORTING
      text = 'Artículos en excel: '
*     TEXT_TABLE    =
*     FIX_LINES     =
*     SAP_STYLE     =
*     SAP_COLOR     = 'LIST_POSITIVE'
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  = 'STRONG'
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
* CHANGING
*     DOCUMENT      =
    .

  CALL METHOD top->add_text
    EXPORTING
      text         = ld_texto
*     TEXT_TABLE   =
*     FIX_LINES    =
*     SAP_STYLE    =
*     SAP_COLOR    = 'LIST_POSITIVE'
*     SAP_FONTSIZE =
*     SAP_FONTSTYLE =
      sap_emphasis = 'STRONG'
*     STYLE_CLASS  =
*     A11Y_TOOLTIP =
*   CHANGING
*     DOCUMENT     =
    .

  CALL METHOD top->new_line( ).

  CLEAR ld_contador.
  LOOP AT lit_matnr.
    LOOP AT git_listado WHERE matnr = lit_matnr-matnr AND status_g = gc_minisemaforo_rojo.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      ADD 1 TO ld_contador.
    ENDIF.
  ENDLOOP.

  WRITE ld_contador TO ld_texto LEFT-JUSTIFIED.
  CONCATENATE ld_texto 'artículos' INTO ld_texto SEPARATED BY space.

  CALL METHOD top->add_text
    EXPORTING
      text = 'Artículos erroneos:'
*     TEXT_TABLE    =
*     FIX_LINES     =
*     SAP_STYLE     =
*     SAP_COLOR     = 'LIST_POSITIVE'
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  = 'STRONG'
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
* CHANGING
*     DOCUMENT      =
    .

  IF ld_contador > 0.
    CALL METHOD top->add_text
      EXPORTING
        text         = ld_texto
*       TEXT_TABLE   =
*       FIX_LINES    =
*       SAP_STYLE    =
        sap_color    = 'LIST_NEGATIVE'
*       SAP_FONTSIZE =
*       SAP_FONTSTYLE =
        sap_emphasis = 'STRONG'
*       STYLE_CLASS  =
*       A11Y_TOOLTIP =
*     CHANGING
*       DOCUMENT     =
      .
  ELSE.
    CALL METHOD top->add_text
      EXPORTING
        text         = 'Ninguno'
*       TEXT_TABLE   =
*       FIX_LINES    =
*       SAP_STYLE    =
        sap_color    = 'LIST_POSITIVE'
*       SAP_FONTSIZE =
*       SAP_FONTSTYLE =
        sap_emphasis = 'STRONG'
*       STYLE_CLASS  =
*       A11Y_TOOLTIP =
*     CHANGING
*       DOCUMENT     =
      .
  ENDIF.

  CALL METHOD top->new_line( ).
  CALL METHOD top->new_line( ).

  CONCATENATE 'NOTA: Todos los procesos de carga realizados en esta ejecución quedarán reflejados en el log de procesos bajo usuario'
              sy-uname
              ', fecha'
              ld_fecha
              'y hora'
              ld_hora
         INTO ld_texto
     SEPARATED BY space..


  CALL METHOD top->add_text
    EXPORTING
      text      = ld_texto
*     TEXT_TABLE    =
*     FIX_LINES =
*     SAP_STYLE =
      sap_color = 'LIST_POSITIVE'
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  = 'STRONG'
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
*   CHANGING
*     DOCUMENT  =
    .
*
*  CALL METHOD top->add_gap EXPORTING width = 15.
*
*  ld_texto = git_listado-filename.
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = ld_texto
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
**     SAP_COLOR     =
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = ''
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->NEW_LINE( ).
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Transacciones:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
**     SAP_COLOR     =
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->add_gap EXPORTING width = 2.
*
*  if p_r01 = 'X'.
*    CALL METHOD TOP->ADD_ICON
*     EXPORTING
*       SAP_ICON         = 'ICON_LED_RED'
**      SAP_SIZE         = 'EXTRA_LARGE'
*       SAP_STYLE        = ''
*       SAP_COLOR        = ''
**      ALTERNATIVE_TEXT =
**      TABINDEX         =
*       .
*
*    clear ld_contador.
*    loop at git_listadot where valido = gc_minisemaforo_rojo.
*      add 1 to ld_contador.
*    endloop.
*
*    write ld_contador to ld_texto LEFT-JUSTIFIED.
*    CALL METHOD TOP->ADD_TEXT
*       EXPORTING
*         TEXT          = ld_texto
**        TEXT_TABLE    =
**        FIX_LINES     =
*         SAP_STYLE     = ''
*         SAP_COLOR     = 'LIST_NEGATIVE_INT'
**        SAP_FONTSIZE  =
**        SAP_FONTSTYLE =
*         SAP_EMPHASIS  = ''
**        STYLE_CLASS   =
**        A11Y_TOOLTIP  =
**      CHANGING
**        DOCUMENT      =
*     .
*  endif.
*
*  CALL METHOD TOP->ADD_ICON
*     EXPORTING
*       SAP_ICON         = 'ICON_LED_YELLOW'
**      SAP_SIZE         = 'EXTRA_LARGE'
*       SAP_STYLE        = ''
*       SAP_COLOR        = ''
**      ALTERNATIVE_TEXT =
**      TABINDEX         =
*       .
*
*  clear ld_contador.
*  loop at git_listadot where valido = gc_minisemaforo_ambar.
*    add 1 to ld_contador.
*  endloop.
*
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_TOTAL_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_GREEN'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*  clear ld_contador.
*  loop at git_listadot where valido = gc_minisemaforo_verde.
*    add 1 to ld_contador.
*  endloop.
*
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_POSITIVE_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD top->add_gap EXPORTING width = 14.
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Total transacciones:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
*      SAP_COLOR     = ''
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  clear ld_contador.
*  loop at git_listadot.
*    add 1 to ld_contador.
*  endloop.
*
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = ''
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD top->NEW_LINE( ).
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Idocs:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
*      SAP_COLOR     = ''
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->add_gap EXPORTING width = 18.
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_RED'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*   write gd_num_idocs_51 to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_NEGATIVE_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_YELLOW'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*   write gd_num_idocs_64 to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_TOTAL_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_GREEN'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*  write gd_num_idocs_53 to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_POSITIVE_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD top->add_gap EXPORTING width = 5.
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Total idocs:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
*      SAP_COLOR     = ''
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  write gd_num_idocs to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = ''
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
ENDFORM.

FORM f_top_of_page_html_lite USING top TYPE REF TO cl_dd_document.
  DATA: ld_contador   TYPE int4,
        ld_texto(255),
        ld_fecha(10),
        ld_hora(8).

  WRITE gd_fecha TO ld_fecha.
  WRITE gd_hora TO ld_hora.

  CALL METHOD top->add_text
    EXPORTING
      text      = 'MONITOR CARGA DE ARTICULOS LITE'
*     TEXT_TABLE    =
*     FIX_LINES =
      sap_style = 'HEADING'.
*     SAP_COLOR     =
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  =
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
*   CHANGING
*     DOCUMENT      =
  .

  CALL METHOD top->new_line( ).
  CALL METHOD top->new_line( ).


  DESCRIBE TABLE git_lite LINES ld_contador.
  WRITE ld_contador TO ld_texto LEFT-JUSTIFIED.

  CALL METHOD top->add_text
    EXPORTING
      text = 'Artículos en excel: '
*     TEXT_TABLE    =
*     FIX_LINES     =
*     SAP_STYLE     =
*     SAP_COLOR     = 'LIST_POSITIVE'
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  = 'STRONG'
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
* CHANGING
*     DOCUMENT      =
    .

  CALL METHOD top->add_text
    EXPORTING
      text         = ld_texto
*     TEXT_TABLE   =
*     FIX_LINES    =
*     SAP_STYLE    =
*     SAP_COLOR    = 'LIST_POSITIVE'
*     SAP_FONTSIZE =
*     SAP_FONTSTYLE =
      sap_emphasis = 'STRONG'
*     STYLE_CLASS  =
*     A11Y_TOOLTIP =
*   CHANGING
*     DOCUMENT     =
    .

  CALL METHOD top->new_line( ).

  CLEAR ld_contador.
  LOOP AT git_lite WHERE status = gc_minisemaforo_rojo.
    ADD 1 TO ld_contador.
  ENDLOOP.

  WRITE ld_contador TO ld_texto LEFT-JUSTIFIED.
  CONCATENATE ld_texto 'artículos' INTO ld_texto SEPARATED BY space.

  CALL METHOD top->add_text
    EXPORTING
      text = 'Artículos erroneos:'
*     TEXT_TABLE    =
*     FIX_LINES     =
*     SAP_STYLE     =
*     SAP_COLOR     = 'LIST_POSITIVE'
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  = 'STRONG'
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
* CHANGING
*     DOCUMENT      =
    .

  IF ld_contador > 0.
    CALL METHOD top->add_text
      EXPORTING
        text         = ld_texto
*       TEXT_TABLE   =
*       FIX_LINES    =
*       SAP_STYLE    =
        sap_color    = 'LIST_NEGATIVE'
*       SAP_FONTSIZE =
*       SAP_FONTSTYLE =
        sap_emphasis = 'STRONG'
*       STYLE_CLASS  =
*       A11Y_TOOLTIP =
*     CHANGING
*       DOCUMENT     =
      .
  ELSE.
    CALL METHOD top->add_text
      EXPORTING
        text         = 'Ninguno'
*       TEXT_TABLE   =
*       FIX_LINES    =
*       SAP_STYLE    =
        sap_color    = 'LIST_POSITIVE'
*       SAP_FONTSIZE =
*       SAP_FONTSTYLE =
        sap_emphasis = 'STRONG'
*       STYLE_CLASS  =
*       A11Y_TOOLTIP =
*     CHANGING
*       DOCUMENT     =
      .
  ENDIF.

  CALL METHOD top->new_line( ).
  CALL METHOD top->new_line( ).

  CONCATENATE 'NOTA: Todos los procesos de carga realizados en esta ejecución quedarán reflejados en el log de procesos bajo usuario'
              sy-uname
              ', fecha'
              ld_fecha
              'y hora'
              ld_hora
         INTO ld_texto
     SEPARATED BY space..


  CALL METHOD top->add_text
    EXPORTING
      text      = ld_texto
*     TEXT_TABLE    =
*     FIX_LINES =
*     SAP_STYLE =
      sap_color = 'LIST_POSITIVE'
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  = 'STRONG'
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
*   CHANGING
*     DOCUMENT  =
    .

ENDFORM.


FORM f_top_of_page_html2 USING top TYPE REF TO cl_dd_document.
  DATA: ld_contador   TYPE int4,
        ld_texto(255),
        ld_fecha(10),
        ld_hora(8),
        BEGIN OF lit_matnr OCCURS 0,
          matnr LIKE mara-matnr,
        END OF lit_matnr.

  WRITE gd_fecha TO ld_fecha.
  WRITE gd_hora TO ld_hora.

  CALL METHOD top->add_text
    EXPORTING
      text      = 'LOG PROCESO CARGA ARTICULOS'
*     TEXT_TABLE    =
*     FIX_LINES =
      sap_style = 'HEADING'.
*     SAP_COLOR     =
*     SAP_FONTSIZE  =
*     SAP_FONTSTYLE =
*     SAP_EMPHASIS  =
*     STYLE_CLASS   =
*     A11Y_TOOLTIP  =
*   CHANGING
*     DOCUMENT      =
  .

*  CALL METHOD top->NEW_LINE( ).
*  CALL METHOD top->NEW_LINE( ).
*
*
*  loop at git_listado.
*    lit_matnr-matnr = git_listado-matnr.
*    append lit_matnr.
*  endloop.
*
*  sort lit_matnr.
*  delete ADJACENT DUPLICATES FROM lit_matnr.
*
*  DESCRIBE TABLE lit_matnr lines ld_contador.
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*
*  CALL METHOD TOP->ADD_TEXT
*  EXPORTING
*    TEXT          = 'Artículos en excel: '
**   TEXT_TABLE    =
**   FIX_LINES     =
**    SAP_STYLE     =
**    SAP_COLOR     = 'LIST_POSITIVE'
**   SAP_FONTSIZE  =
**   SAP_FONTSTYLE =
**    SAP_EMPHASIS  = 'STRONG'
**   STYLE_CLASS   =
**   A11Y_TOOLTIP  =
** CHANGING
**   DOCUMENT      =
*.
*
*    CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = ld_texto
**     TEXT_TABLE    =
**     FIX_LINES     =
**      SAP_STYLE     =
**      SAP_COLOR     = 'LIST_POSITIVE'
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->NEW_LINE( ).
*
*  clear ld_contador.
*  loop at lit_matnr.
*    loop at git_listado where matnr = lit_matnr-matnr and status_g = gc_minisemaforo_rojo.
*      exit.
*    endloop.
*
*    if sy-subrc = 0.
*      add 1 to ld_contador.
*    endif.
*  endloop.
*
*  WRITE ld_contador to ld_texto LEFT-JUSTIFIED.
*  CONCATENATE ld_texto 'artículos' into ld_texto SEPARATED BY space.
*
*  CALL METHOD TOP->ADD_TEXT
*  EXPORTING
*    TEXT          = 'Artículos erroneos:'
**   TEXT_TABLE    =
**   FIX_LINES     =
**    SAP_STYLE     =
**    SAP_COLOR     = 'LIST_POSITIVE'
**   SAP_FONTSIZE  =
**   SAP_FONTSTYLE =
**    SAP_EMPHASIS  = 'STRONG'
**   STYLE_CLASS   =
**   A11Y_TOOLTIP  =
** CHANGING
**   DOCUMENT      =
*.
*
*  if ld_contador > 0.
*    CALL METHOD TOP->ADD_TEXT
*      EXPORTING
*        TEXT          = ld_texto
**       TEXT_TABLE    =
**       FIX_LINES     =
**        SAP_STYLE     =
*         SAP_COLOR     = 'LIST_NEGATIVE'
**       SAP_FONTSIZE  =
**       SAP_FONTSTYLE =
*        SAP_EMPHASIS  = 'STRONG'
**       STYLE_CLASS   =
**       A11Y_TOOLTIP  =
**     CHANGING
**       DOCUMENT      =
*    .
*  else.
*    CALL METHOD TOP->ADD_TEXT
*      EXPORTING
*        TEXT          = 'Ninguno'
**       TEXT_TABLE    =
**       FIX_LINES     =
**        SAP_STYLE     =
*         SAP_COLOR     = 'LIST_POSITIVE'
**       SAP_FONTSIZE  =
**       SAP_FONTSTYLE =
*        SAP_EMPHASIS  = 'STRONG'
**       STYLE_CLASS   =
**       A11Y_TOOLTIP  =
**     CHANGING
**       DOCUMENT      =
*    .
*  endif.
*
*  CALL METHOD top->NEW_LINE( ).
*  CALL METHOD top->NEW_LINE( ).
*
*  CONCATENATE 'NOTA: Todos los procesos de carga realizados en esta ejecución quedarán reflejados en el log de procesos bajo usuario'
*              sy-uname
*              ', fecha'
*              ld_fecha
*              'y hora'
*              ld_hora
*         into ld_texto
*     SEPARATED BY space..
*
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = ld_texto
**     TEXT_TABLE    =
**     FIX_LINES     =
**      SAP_STYLE     =
*      SAP_COLOR     = 'LIST_POSITIVE'
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
**      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->add_gap EXPORTING width = 15.
*
*  ld_texto = git_listado-filename.
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = ld_texto
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
**     SAP_COLOR     =
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = ''
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->NEW_LINE( ).
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Transacciones:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
**     SAP_COLOR     =
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->add_gap EXPORTING width = 2.
*
*  if p_r01 = 'X'.
*    CALL METHOD TOP->ADD_ICON
*     EXPORTING
*       SAP_ICON         = 'ICON_LED_RED'
**      SAP_SIZE         = 'EXTRA_LARGE'
*       SAP_STYLE        = ''
*       SAP_COLOR        = ''
**      ALTERNATIVE_TEXT =
**      TABINDEX         =
*       .
*
*    clear ld_contador.
*    loop at git_listadot where valido = gc_minisemaforo_rojo.
*      add 1 to ld_contador.
*    endloop.
*
*    write ld_contador to ld_texto LEFT-JUSTIFIED.
*    CALL METHOD TOP->ADD_TEXT
*       EXPORTING
*         TEXT          = ld_texto
**        TEXT_TABLE    =
**        FIX_LINES     =
*         SAP_STYLE     = ''
*         SAP_COLOR     = 'LIST_NEGATIVE_INT'
**        SAP_FONTSIZE  =
**        SAP_FONTSTYLE =
*         SAP_EMPHASIS  = ''
**        STYLE_CLASS   =
**        A11Y_TOOLTIP  =
**      CHANGING
**        DOCUMENT      =
*     .
*  endif.
*
*  CALL METHOD TOP->ADD_ICON
*     EXPORTING
*       SAP_ICON         = 'ICON_LED_YELLOW'
**      SAP_SIZE         = 'EXTRA_LARGE'
*       SAP_STYLE        = ''
*       SAP_COLOR        = ''
**      ALTERNATIVE_TEXT =
**      TABINDEX         =
*       .
*
*  clear ld_contador.
*  loop at git_listadot where valido = gc_minisemaforo_ambar.
*    add 1 to ld_contador.
*  endloop.
*
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_TOTAL_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_GREEN'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*  clear ld_contador.
*  loop at git_listadot where valido = gc_minisemaforo_verde.
*    add 1 to ld_contador.
*  endloop.
*
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_POSITIVE_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD top->add_gap EXPORTING width = 14.
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Total transacciones:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
*      SAP_COLOR     = ''
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  clear ld_contador.
*  loop at git_listadot.
*    add 1 to ld_contador.
*  endloop.
*
*  write ld_contador to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = ''
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD top->NEW_LINE( ).
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Idocs:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
*      SAP_COLOR     = ''
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  CALL METHOD top->add_gap EXPORTING width = 18.
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_RED'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*   write gd_num_idocs_51 to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_NEGATIVE_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_YELLOW'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*   write gd_num_idocs_64 to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_TOTAL_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD TOP->ADD_ICON
*   EXPORTING
*     SAP_ICON         = 'ICON_LED_GREEN'
**    SAP_SIZE         = 'EXTRA_LARGE'
*     SAP_STYLE        = ''
*     SAP_COLOR        = ''
**    ALTERNATIVE_TEXT =
**    TABINDEX         =
*     .
*
*  write gd_num_idocs_53 to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = 'LIST_POSITIVE_INT'
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
*
*  CALL METHOD top->add_gap EXPORTING width = 5.
*
*  CALL METHOD TOP->ADD_TEXT
*    EXPORTING
*      TEXT          = 'Total idocs:'
**     TEXT_TABLE    =
**     FIX_LINES     =
*      SAP_STYLE     = ''
*      SAP_COLOR     = ''
**     SAP_FONTSIZE  =
**     SAP_FONTSTYLE =
*      SAP_EMPHASIS  = 'STRONG'
**     STYLE_CLASS   =
**     A11Y_TOOLTIP  =
**   CHANGING
**     DOCUMENT      =
*  .
*
*  write gd_num_idocs to ld_texto LEFT-JUSTIFIED.
*  CALL METHOD TOP->ADD_TEXT
*     EXPORTING
*       TEXT          = ld_texto
**      TEXT_TABLE    =
**      FIX_LINES     =
*       SAP_STYLE     = ''
*       SAP_COLOR     = ''
**      SAP_FONTSIZE  =
**      SAP_FONTSTYLE =
*       SAP_EMPHASIS  = ''
**      STYLE_CLASS   =
**      A11Y_TOOLTIP  =
**    CHANGING
**      DOCUMENT      =
*   .
ENDFORM.


FORM f_ucomm USING pe_ucomm   LIKE sy-ucomm
                                   rs_selfield TYPE slis_selfield.

* 0.- Declaración de variables
*===================================================================================================
  DATA : ref_grid TYPE REF TO cl_gui_alv_grid,
         l_valid  TYPE c.

* 1.- Lógica
*===================================================================================================
  rs_selfield-refresh = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

* Code to reflect the changes done in the internal table
  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.

  IF NOT ref_grid IS INITIAL.
    CALL METHOD ref_grid->check_changed_data
      IMPORTING
        e_valid = l_valid.
  ENDIF.

  IF l_valid = 'X'.
    CASE pe_ucomm.
      WHEN 'PROCESAR'.
        PERFORM f_procesar.
      WHEN '&IC1'.
        CASE rs_selfield-fieldname.
          WHEN 'MATNR'.
            IF p_mon1 = 'X'.
*             Carga de articulos Full
              READ TABLE git_listado INDEX rs_selfield-tabindex.

              SELECT SINGLE matnr
                FROM mara
                INTO git_listado-matnr
               WHERE matnr = git_listado-matnr.

              IF sy-subrc = 0.
                SET PARAMETER ID 'MAT' FIELD git_listado-matnr.
                CALL TRANSACTION 'MM43'.
              ENDIF.
            ELSEIF p_mon2 = 'X'.
*             Log carga
              READ TABLE git_listado_log INDEX rs_selfield-tabindex.

              SELECT SINGLE matnr
                FROM mara
                INTO git_listado_log-matnr
               WHERE matnr = git_listado_log-matnr.

              IF sy-subrc = 0.
                SET PARAMETER ID 'MAT' FIELD git_listado_log-matnr.
                CALL TRANSACTION 'MM43'.
              ENDIF.
            ELSEIF p_mon3 = 'X'.
*             Carga artículos Lite
              READ TABLE git_lite INDEX rs_selfield-tabindex.

              SELECT SINGLE matnr
                FROM mara
                INTO git_lite-matnr
               WHERE matnr = git_lite-matnr.

              IF sy-subrc = 0.
                SET PARAMETER ID 'MAT' FIELD git_lite-matnr.
                CALL TRANSACTION 'MM43'.
              ENDIF.
            ENDIF.
        ENDCASE.
    ENDCASE.
  ENDIF.
ENDFORM.                    "F_UCOMM

FORM f_status USING extab TYPE slis_t_extab.
  DATA: lit_excluding LIKE sy-ucomm OCCURS 0.

  IF p_mon1 = 'X' OR p_mon3 = 'X'.
*   Si estamos en proceso de carga, ocultamos botón de precargar artículos.
    APPEND 'PRECARGAR' TO lit_excluding.
  ENDIF.

  SET PF-STATUS 'STATUS_ALV' EXCLUDING lit_excluding.
ENDFORM.                    "F_STATUS_S02

FORM f_procesar.
* 0.- Declaración de variables
*==========================================================================
  DATA: ld_pregunta     TYPE string,
        ld_respuesta(1),
        BEGIN OF lit_kits OCCURS 0,
          matnr TYPE matnr,
          compo TYPE matnr,
        END OF lit_kits,

        lf_error.

* 1.- Lógica
*==========================================================================
  IF p_mon3 = 'X'.
    LOOP AT git_lite WHERE sel = 'X'.
      EXIT.
    ENDLOOP.
  ELSE.
    LOOP AT git_listado WHERE sel = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.

  IF sy-subrc <> 0.
*   Msg: Seleccionar al menos una linea para procesar
    MESSAGE i003(zretpdt001) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF p_mon3 = 'X'.
    LOOP AT git_lite WHERE sel = 'X' AND status = gc_minisemaforo_ambar.
      EXIT.
    ENDLOOP.
  ELSE.
    LOOP AT git_listado WHERE sel = 'X' AND status_g = gc_minisemaforo_ambar.
      EXIT.
    ENDLOOP.
  ENDIF.

  IF sy-subrc <> 0.
*   Seleccionar al menos una linea procesable
    MESSAGE i004(zretpdt001) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF p_mon3 = 'X'.
  ELSE.
*   Determinar los kits que se han seleccionado
    LOOP AT git_listado WHERE sel = 'X'
                          AND status_g = gc_minisemaforo_ambar
                          AND f1_mtart = 'ZKIT'.
      CLEAR lit_kits.
      lit_kits-matnr = git_listado-matnr.
      APPEND lit_kits.
    ENDLOOP.

*   Determinar los componentes que se han seleccionado
    LOOP AT git_listado WHERE sel = 'X'
                          AND status_g = gc_minisemaforo_ambar
                          AND f1_pmata <> ''.
      CLEAR lit_kits.
      lit_kits-matnr = git_listado-f1_pmata.
      lit_kits-compo = git_listado-matnr.
      APPEND lit_kits.
    ENDLOOP.

*   Analizar kits
    lf_error = ''.

    LOOP AT lit_kits WHERE compo IS INITIAL.
      LOOP AT git_listado WHERE f1_pmata = git_listado-matnr.
        IF git_listado-status_g <> gc_minisemaforo_ambar OR
           git_listado-sel = ''.
*         MSG: Se han seleccionado kits con componentes no procesables o no seleccionados para procesar.
          MESSAGE 'Se han seleccionado kits con componentes no procesables o no seleccionados para procesar.' TYPE 'I' DISPLAY LIKE 'E'.

          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lf_error = 'X'.
      EXIT.
    ENDIF.

*   Analizar componentes
    LOOP AT lit_kits WHERE matnr IS NOT INITIAL AND compo IS NOT INITIAL.
      LOOP AT git_listado WHERE matnr = git_listado-f1_pmata.
        IF git_listado-status_g <> gc_minisemaforo_ambar OR
           git_listado-sel = ''.
*         MSG: Se han seleccionado kits con componentes no procesable o no seleccionados para procesar.
          MESSAGE 'Se han seleccionado componentes con Kit no procesable o no seleccionado para procesar.' TYPE 'I' DISPLAY LIKE 'E'.

          lf_error = 'X'.

          EXIT.
        ENDIF.
      ENDLOOP.

      IF lf_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lf_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

* Solicitamos confirmación
* Msg: ¿Lanzar proceso de creación de artículos para los artículos procesables seleccionados?
  ld_pregunta = TEXT-q01.
  PERFORM f_popup_to_confirm USING ld_pregunta CHANGING ld_respuesta.

  IF ld_respuesta = '1'.
    IF p_mon3 = 'X'.
      PERFORM f_procesar_exec_lite.
    ELSE.
      PERFORM f_procesar_exec.
    ENDIF.
  ELSE.
*   Msg: Acción cancelada
    MESSAGE i005(zretpdt001).
  ENDIF.
ENDFORM.

FORM f_popup_to_confirm USING pe_pregunta CHANGING ps_respuesta.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
*     TITLEBAR       = ' '
*     DIAGNOSE_OBJECT             = ' '
      text_question  = pe_pregunta
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer         = ps_respuesta
*   TABLES
*     PARAMETER      =
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0. ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_PANTALLA_SELECCION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validar_pantalla_seleccion CHANGING ps_error.

  IF p_mon1 = 'X'.
    IF p_file1 IS INITIAL OR
       p_file2 IS INITIAL OR
       p_file3 IS INITIAL.
*     Msg: Informar los 3 primeros ficheros de carga.
      ps_error = 'X'.
      MESSAGE s002(zretpdt001) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSEIF p_mon3 = 'X'.
    IF p_lite IS INITIAL.
*     Msg: Informar fichero de carga de artículos Lite
      ps_error = 'X'.
      MESSAGE s003(zretpdt001) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_GIT_LISTADO_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_clear_git_listado_det .
  CLEAR:"Plantilla 1
         git_listado-f1_mtart,
         git_listado-f1_attyp,
         git_listado-f1_matkl,
         git_listado-f1_maktx,
         git_listado-f1_pmata,
         git_listado-f1_cantc,
         git_listado-f1_taklv,
         git_listado-f1_taklv2,
         git_listado-f1_extwg,
         git_listado-f1_meins,
         git_listado-f1_lgnum,
         git_listado-f1_ltkze,
         git_listado-f1_ltkza,
         git_listado-f1_ekgrp,
         git_listado-f1_ladgr,
         git_listado-f1_brand_id,
         git_listado-f1_inhal,
         git_listado-f1_inhme,
         git_listado-f1_ntgew,
         git_listado-f1_gewei,
         git_listado-f1_groes,
         git_listado-f1_mstae,
         git_listado-f1_mstde,
         git_listado-f1_normt,
         git_listado-f1_bklas,
         git_listado-f1_herkl,
         git_listado-f1_herkr,
         git_listado-f1_stawn,
         git_listado-f1_loggr,
         git_listado-f1_prat1,
         git_listado-f1_prat2,
         git_listado-f1_prat3,
         git_listado-f1_prat4,
         git_listado-f1_prat5,
         git_listado-f1_sobst,
         git_listado-f1_eisbe,
         git_listado-f1_mfrgr,
         git_listado-f1_dismm,
         git_listado-f1_mstav,
         git_listado-f1_mstdv,
         git_listado-f1_scmng,
         git_listado-f1_ktgrm,
         git_listado-f1_mtpos,
         git_listado-f1_bstrf,
         git_listado-f1_zeinr,
        "Plantilla 2
         git_listado-f2_ean11,
         git_listado-f2_numtp,
         git_listado-f2_meinh,
         git_listado-f2_umrez,
         git_listado-f2_umren,
         git_listado-f2_brgew,
         git_listado-f2_gewei,
         git_listado-f2_laeng,
         git_listado-f2_breit,
         git_listado-f2_hoehe,
         git_listado-f2_meabm,
         git_listado-f2_volum,
         git_listado-f2_voleh,
        "Plantilla 3
         git_listado-f3_lifnr,
         git_listado-f3_ekorg,
         git_listado-f3_idnlf,
         git_listado-f3_ekgrp,
         git_listado-f3_minbm,
         git_listado-f3_netpr,
         git_listado-f3_datab,
         git_listado-f3_datbi,
         git_listado-f3_konwa,
         git_listado-f3_kpein,
         git_listado-f3_kmein,
         git_listado-f3_netpr_pb30,
         git_listado-f3_datab_pb30,
         git_listado-f3_datbi_pb30,
         git_listado-f3_konwa_pb30,
         git_listado-f3_kpein_pb30,
         git_listado-f3_kmein_pb30,
         git_listado-f3_bstae,
         git_listado-f3_plifz,
         git_listado-f3_norbm,
         git_listado-f3_mwskz,
         git_listado-f3_esokz,
         git_listado-f3_norbm,
         git_listado-f4_vkorg,
         git_listado-f4_vtweg,
         git_listado-f4_meinh,
         git_listado-f4_kwert,
         git_listado-f4_datab,
         git_listado-f4_datbi.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_DATOS_F1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_asignar_datos_f1 .
  git_listado-f1_mtart    = git_file1-mtart.
  git_listado-f1_attyp    = git_file1-attyp.
  git_listado-f1_matkl    = git_file1-matkl.
  git_listado-f1_maktx    = git_file1-maktx.
  git_listado-f1_pmata    = git_file1-pmata.
  git_listado-f1_cantc    = git_file1-cantc.
  git_listado-f1_taklv    = git_file1-taklv.
  git_listado-f1_taklv2    = git_file1-taklv2.
  git_listado-f1_extwg    = git_file1-extwg.
  git_listado-f1_meins    = git_file1-meins.
  git_listado-f1_lgnum    = git_file1-lgnum.
  git_listado-f1_ltkze    = git_file1-ltkze.
  git_listado-f1_ltkza    = git_file1-ltkza.
  git_listado-f1_inhal    = git_file1-inhal.
  git_listado-f1_inhme    = git_file1-inhme.
  git_listado-f1_ekgrp    = git_file1-ekgrp.
  git_listado-f1_ladgr    = git_file1-ladgr.
  git_listado-f1_brand_id = git_file1-brand_id.
  git_listado-f1_ntgew    = git_file1-ntgew.
  git_listado-f1_gewei    = git_file1-gewei.
  git_listado-f1_groes    = git_file1-groes.
  git_listado-f1_mstae    = git_file1-mstae.
  git_listado-f1_mstde    = git_file1-mstde.
  git_listado-f1_normt    = git_file1-normt.
  git_listado-f1_bklas    = git_file1-bklas.
  git_listado-f1_herkl    = git_file1-herkl.
  git_listado-f1_herkr    = git_file1-herkr.
  git_listado-f1_stawn    = git_file1-stawn.
  git_listado-f1_loggr    = git_file1-loggr.
  git_listado-f1_prat1    = git_file1-prat1.
  git_listado-f1_prat2    = git_file1-prat2.
  git_listado-f1_prat3    = git_file1-prat3.
  git_listado-f1_prat4    = git_file1-prat4.
  git_listado-f1_prat5    = git_file1-prat5.
  git_listado-f1_sobst    = git_file1-sobst.
  git_listado-f1_eisbe    = git_file1-eisbe.
  git_listado-f1_mfrgr    = git_file1-mfrgr.
  git_listado-f1_dismm    = git_file1-dismm.
  git_listado-f1_mstav    = git_file1-mstav.
  git_listado-f1_mstdv    = git_file1-mstdv.
  git_listado-f1_scmng    = git_file1-scmng.
  git_listado-f1_ktgrm    = git_file1-ktgrm.
  git_listado-f1_mtpos    = git_file1-mtpos.
  git_listado-f1_bstrf    = git_file1-bstrf.
  git_listado-f1_zeinr    = git_file1-zeinr.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_DATOS_F1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_asignar_datos_f2 .
  git_listado-f2_ean11  = git_file2-ean11.
  git_listado-f2_numtp  = git_file2-numtp.
  git_listado-f2_meinh  = git_file2-meinh.
  git_listado-f2_umrez  = git_file2-umrez.
  git_listado-f2_umren  = git_file2-umren.
  git_listado-f2_brgew  = git_file2-brgew.
  git_listado-f2_gewei  = git_file2-gewei.
  git_listado-f2_laeng  = git_file2-laeng.
  git_listado-f2_breit  = git_file2-breit.
  git_listado-f2_hoehe  = git_file2-hoehe.
  git_listado-f2_meabm  = git_file2-meabm.
  git_listado-f2_volum  = git_file2-volum.
  git_listado-f2_voleh  = git_file2-voleh.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_DATOS_F1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_asignar_datos_f3 .
  git_listado-f3_lifnr      = git_file3-lifnr.
  git_listado-f3_ekorg      = git_file3-ekorg.
  git_listado-f3_idnlf      = git_file3-idnlf.
  git_listado-f3_netpr      = git_file3-netpr.
  git_listado-f3_datab      = git_file3-datab.
  git_listado-f3_konwa      = git_file3-konwa.
  git_listado-f3_kpein      = git_file3-kpein.
  git_listado-f3_kmein      = git_file3-kmein.
  git_listado-f3_bstae      = git_file3-bstae.
  git_listado-f3_minbm      = git_file3-minbm.
  git_listado-f3_ekgrp      = git_file3-ekgrp.
  git_listado-f3_datbi      = git_file3-datbi.
  git_listado-f3_plifz      = git_file3-plifz.
  git_listado-f3_norbm      = git_file3-norbm.
  git_listado-f3_mwskz      = git_file3-mwskz.
  git_listado-f3_esokz      = git_file3-esokz.
  git_listado-f3_netpr_pb30 = git_file3-netpr_pb30.
  git_listado-f3_datab_pb30 = git_file3-datab_pb30.
  git_listado-f3_datbi_pb30 = git_file3-datbi_pb30.
  git_listado-f3_konwa_pb30 = git_file3-konwa_pb30.
  git_listado-f3_kpein_pb30 = git_file3-kpein_pb30.
  git_listado-f3_kmein_pb30 = git_file3-kmein_pb30.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CUNIT_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_FILE1_MEINS  text
*      <--P_GIT_FILE1_MEINS  text
*----------------------------------------------------------------------*
FORM f_cunit_input  USING    pe_meins
                    CHANGING ps_meins
                             ps_error.

  CLEAR ps_error.

  IF pe_meins IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = pe_meins
*       LANGUAGE       = SY-LANGU
      IMPORTING
        output         = ps_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      ps_error = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_procesar_exec .
* 0.- Declaración de variables
*==========================================================================
  DATA: BEGIN OF lit_matnr OCCURS 0,
          matnr LIKE mara-matnr,
        END OF lit_matnr,
        lf_error(1),
        ld_catalogar_ok          TYPE xflag,
        ld_docnum                TYPE edi_docnum,
        ld_index_listado         LIKE sy-tabix,
        ld_cont                  TYPE numc10,
        lit_mensajes             LIKE bdcmsgcoll          OCCURS 0 WITH HEADER LINE,
        lit_clientdata           LIKE bapie1marart        OCCURS 0 WITH HEADER LINE,
        lit_clientdatax          LIKE bapie1marartx       OCCURS 0 WITH HEADER LINE,
        lit_unitsofmeasure       LIKE bapie1marmrt        OCCURS 0 WITH HEADER LINE,
        lit_unitsofmeasurex      LIKE bapie1marmrtx       OCCURS 0 WITH HEADER LINE,
        lit_unitofmeasuretexts   LIKE bapie1mamtrt        OCCURS 0 WITH HEADER LINE,
        lit_salesdata            LIKE bapie1mvkert        OCCURS 0 WITH HEADER LINE,
        lit_salesdatax           LIKE bapie1mvkertx       OCCURS 0 WITH HEADER LINE,
        lit_plantdata            LIKE bapie1marcrt        OCCURS 0 WITH HEADER LINE,
        lit_plantdatax           LIKE bapie1marcrtx       OCCURS 0 WITH HEADER LINE,
        lit_warehousenumberdata  LIKE bapie1mlgnrt        OCCURS 0 WITH HEADER LINE,
        lit_warehousenumberdatax LIKE bapie1mlgnrtx       OCCURS 0 WITH HEADER LINE,
        lit_taxclassifications   LIKE bapie1mlanrt        OCCURS 0 WITH HEADER LINE,
        lit_storagelocationdata  LIKE bapie1mardrt        OCCURS 0 WITH HEADER LINE,
        lit_storagelocationdatax LIKE bapie1mardrtx       OCCURS 0 WITH HEADER LINE,
        lit_materialdescription  LIKE bapie1maktrt       OCCURS 0 WITH HEADER LINE,
        lit_addnclientdata       LIKE bapie1maw1rt       OCCURS 0 WITH HEADER LINE,
        lit_addnclientdatax      LIKE bapie1maw1rtx      OCCURS 0 WITH HEADER LINE,
        lit_internationalartnos  LIKE bapie1meanrt       OCCURS 0 WITH HEADER LINE,
        lit_storagetypedata      LIKE bapie1mlgtrt       OCCURS 0 WITH HEADER LINE,
        lit_storagetypedatax     LIKE bapie1mlgtrtx      OCCURS 0 WITH HEADER LINE,
        lit_valuationdata        LIKE bapie1mbewrt       OCCURS 0 WITH HEADER LINE,
        lit_valuationdatax       LIKE bapie1mbewrtx      OCCURS 0 WITH HEADER LINE,
        lit_zretpdtt02           LIKE zretpdtt02         OCCURS 0 WITH HEADER LINE,
        lit_recipientparameters  LIKE bapi_wrpl_import   OCCURS 0 WITH HEADER LINE,
        lit_recipientparametersx LIKE bapi_wrpl_importx  OCCURS 0 WITH HEADER LINE,
        lit_return_planification LIKE bapiret2           OCCURS 0 WITH HEADER LINE,
        lr_headdata              LIKE bapie1mathead,
        lf_error_reg_info(1),
        lf_error_pvps(1),
        lr_return                LIKE bapireturn1,
        wa_zretpdt001t01         LIKE zretpdt001t01.


* 1.- Lógica
*==========================================================================
*>Inicializar log de carga
  REFRESH git_log_carga.
  CLEAR zretpdt001s08.

*>Obtener los distintos artículos seleccionados en el monitor aptos para carga
  LOOP AT git_listado WHERE sel = 'X' AND status_g = gc_minisemaforo_ambar.
    READ TABLE lit_matnr WITH KEY matnr = git_listado-matnr TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      lit_matnr-matnr = git_listado-matnr.
      APPEND lit_matnr.
    ENDIF.
  ENDLOOP.

*>Procesar carga de artículos
  LOOP AT lit_matnr.
*   Inicializaciones
    CLEAR: lr_headdata,
           lr_return,
           ld_catalogar_ok,
           ld_docnum,
           lf_error_reg_info,
           lf_error_pvps,
           ld_index_listado,
           lit_mensajes,
           lit_clientdata,
           lit_clientdatax,
           lit_unitsofmeasure,
           lit_unitsofmeasurex,
           lit_unitofmeasuretexts,
           lit_salesdata,
           lit_salesdatax,
           lit_plantdata,
           lit_plantdatax,
           lit_warehousenumberdata,
           lit_warehousenumberdatax,
           lit_taxclassifications,
           lit_storagelocationdata,
           lit_storagelocationdatax,
           lit_materialdescription,
           lit_addnclientdata,
           lit_addnclientdatax,
           lit_internationalartnos,
           lit_storagetypedata,
           lit_storagetypedatax,
           lit_valuationdata,
           lit_valuationdatax,
           lit_recipientparameters,
           lit_recipientparametersx.

    REFRESH: lit_mensajes,
             lit_clientdata,
             lit_clientdatax,
             lit_unitsofmeasure,
             lit_unitsofmeasurex,
             lit_unitofmeasuretexts,
             lit_salesdata,
             lit_salesdatax,
             lit_plantdata,
             lit_plantdatax,
             lit_warehousenumberdata,
             lit_warehousenumberdatax,
             lit_taxclassifications,
             lit_storagelocationdata,
             lit_storagelocationdatax,
             lit_materialdescription,
             lit_addnclientdata,
             lit_addnclientdatax,
             lit_internationalartnos,
             lit_storagetypedata,
             lit_storagetypedatax,
             lit_valuationdata,
             lit_valuationdatax,
             lit_recipientparameters,
             lit_recipientparametersx.

*   Borramos log del articulo, por si este se ha procesado previamente
    DELETE FROM zretpdt001t01
          WHERE matnr = lit_matnr-matnr.

    COMMIT WORK AND WAIT.

*   Cargar datos básicos del artículo
    LOOP AT git_file1 WHERE matnr = lit_matnr-matnr.
      EXIT.
    ENDLOOP.

*   Datos de cabecera
    PERFORM f_fill_00_datos_iniciales CHANGING lr_headdata.

*   Datos básicos
    PERFORM f_fill_01_datos_basicos TABLES lit_clientdata lit_clientdatax.

*   Ampliación datos básicos
    PERFORM f_fill_02_amp_datos_basicos TABLES lit_addnclientdata lit_addnclientdatax.

*   Denominación artículo
    PERFORM f_fill_03_denom_articulo TABLES lit_materialdescription.

*   Datos de unidad de medida y EAN
    PERFORM f_fill_04_eans_y_ums TABLES lit_unitsofmeasure lit_unitsofmeasurex lit_unitofmeasuretexts lit_internationalartnos.

*   Datos de area de ventas
    PERFORM f_fill_05_datos_area_ventas TABLES lit_salesdata lit_salesdatax.

*   Datos de centro y almacén
    PERFORM f_fill_06_datos_centros TABLES lit_plantdata lit_plantdatax lit_storagelocationdata lit_storagelocationdatax.

*   Datos de clasificación fiscal
    PERFORM f_fill_07_clasificacion_fiscal TABLES lit_taxclassifications.

*   Datos almacén WM
    PERFORM f_fill_08_datos_almacen_wm TABLES lit_warehousenumberdata lit_warehousenumberdatax.

*   Datos planificación
    PERFORM f_fill_09_datos_planificacion TABLES lit_recipientparameters lit_recipientparametersx.

*   Creamos artículo
    CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
      EXPORTING
        headdata             = lr_headdata
      IMPORTING
        return               = lr_return
      TABLES
*       VARIANTSKEYS         =
*       CHARACTERISTICVALUE  =
*       CHARACTERISTICVALUEX =
        clientdata           = lit_clientdata
        clientdatax          = lit_clientdatax
*       CLIENTEXT            =
*       CLIENTEXTX           =
        addnlclientdata      = lit_addnclientdata
        addnlclientdatax     = lit_addnclientdatax
        materialdescription  = lit_materialdescription
        plantdata            = lit_plantdata
        plantdatax           = lit_plantdatax
*       PLANTEXT             =
*       PLANTEXTX            =
*       FORECASTPARAMETERS   =
*       FORECASTPARAMETERSX  =
*       FORECASTVALUES       =
*       TOTALCONSUMPTION     =
*       UNPLNDCONSUMPTION    =
*       PLANNINGDATA         =
*       PLANNINGDATAX        =
        storagelocationdata  = lit_storagelocationdata
        storagelocationdatax = lit_storagelocationdatax
*       STORAGELOCATIONEXT   =
*       STORAGELOCATIONEXTX  =
        unitsofmeasure       = lit_unitsofmeasure
        unitsofmeasurex      = lit_unitsofmeasurex
        unitofmeasuretexts   = lit_unitofmeasuretexts
        internationalartnos  = lit_internationalartnos
*       VENDOREAN            =
*       LAYOUTMODULEASSGMT   =
*       LAYOUTMODULEASSGMTX  =
        taxclassifications   = lit_taxclassifications
        valuationdata        = lit_valuationdata
        valuationdatax       = lit_valuationdatax
*       VALUATIONEXT         =
*       VALUATIONEXTX        =
        warehousenumberdata  = lit_warehousenumberdata
        warehousenumberdatax = lit_warehousenumberdatax
*       WAREHOUSENUMBEREXT   =
*       WAREHOUSENUMBEREXTX  =
        storagetypedata      = lit_storagetypedata
        storagetypedatax     = lit_storagetypedatax
*       STORAGETYPEEXT       =
*       STORAGETYPEEXTX      =
        salesdata            = lit_salesdata
        salesdatax           = lit_salesdatax
*       SALESEXT             =
*       SALESEXTX            =
*       POSDATA              =
*       POSDATAX             =
*       POSEXT               =
*       POSEXTX              =
*       MATERIALLONGTEXT     =
*       PLANTKEYS            =
*       STORAGELOCATIONKEYS  =
*       DISTRCHAINKEYS       =
*       WAREHOUSENOKEYS      =
*       STORAGETYPEKEYS      =
*       VALUATIONTYPEKEYS    =
      .

*   Hacemos Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'
*     IMPORTING
*       RETURN        =
      .

    IF lr_return-type = 'E'.
*     Si articulo no creado...

*     Registramos mensaje de error en el ALV y modificamos el status a rojo
      LOOP AT git_listado WHERE matnr = lit_matnr-matnr.
        ld_index_listado = sy-tabix.

        MESSAGE ID lr_return-id TYPE lr_return-type NUMBER lr_return-number WITH lr_return-message_v1 lr_return-message_v2 lr_return-message_v3 lr_return-message_v4 INTO git_listado-info.

        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado INDEX ld_index_listado.
      ENDLOOP.

*     Registramos pasos subsiguientes como error
      PERFORM f_registrar_paso USING '01' lit_matnr-matnr gc_minisemaforo_rojo 1 'Crear artículo'          git_listado-info.
      PERFORM f_registrar_paso USING '02' lit_matnr-matnr gc_minisemaforo_rojo 1 'Catalogar artículo'      gc_paso_no_realizado.
      PERFORM f_registrar_paso USING '03' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta registro info'      gc_paso_no_realizado.
      PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta PVPs'               gc_paso_no_realizado.

*     Registramos error en log resumen
      PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_rojo '01' 'Crear artículo'.

*     Incrementar contador articulos erroneos
      ADD 1 TO zretpdt001s08-art_error.
    ELSE.
*     Si articulo creado correctamente...

*     Registramos paso 01 como OK
      PERFORM f_registrar_paso USING '01' lit_matnr-matnr gc_minisemaforo_verde 1 'Crear artículo'  'Artículo creado correctamente'.

*     Actualizamos campos ZZ
      PERFORM f_act_campos_zz_full.

*     Actualizamos descripción del articulo (60 caracteres) en texto datos básicos
      PERFORM f_act_datos_basicos_articulo.

*     Actualizamos datos de planificacion
      PERFORM f_update_planification_data TABLES lit_recipientparameters lit_recipientparametersx lit_return_planification CHANGING lf_error.

*     Catalogamos
      PERFORM f_catalogar TABLES lit_mensajes USING git_file1-matnr CHANGING ld_catalogar_ok ld_docnum.

      IF ld_catalogar_ok = 'X'.
*       Si artículo catalogado correctamente...

*       Registramos paso 02 como OK
        PERFORM f_registrar_paso USING '02' lit_matnr-matnr gc_minisemaforo_verde 1 'Catalogar artículo'          'Articulo catalogado correctamente'.

*       Creamos registros info del artículo...
        ld_cont = 1.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> APRADAS @ 10.07.2024 08:24:20 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*       Forzar creación del registro info para el proveedor CD01.
        clear git_file3.
        git_file3-MATNR = lit_matnr-matnr.
        git_file3-LIFNR = 'CD01'.
        git_file3-EKORG = 'MIRO'.
*        git_file3-IDNLF = .
        git_file3-EKGRP = '100'.
*        git_file3-MINBM = .
*        git_file3-NETPR = .
*        git_file3-DATAB = .
*        git_file3-DATBI = .
*        git_file3-KONWA = .
*        git_file3-KPEIN = .
*        git_file3-KMEIN = .
*        git_file3-NETPR_PB30 = .
*        git_file3-DATAB_PB30 = .
*        git_file3-DATBI_PB30 = .
*        git_file3-KONWA_PB30 = .
*        git_file3-KPEIN_PB30 = .
*        git_file3-KMEIN_PB30 = .
*        git_file3-BSTAE = .
*        git_file3-PLIFZ = .
*        git_file3-NORBM = .
*        git_file3-MWSKZ = .
*        git_file3-ESOKZ = .
        append git_file3.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< APRADAS @ 10.07.2024 08:24:20 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        LOOP AT git_file3 WHERE matnr = lit_matnr-matnr.
          ADD 1 TO ld_cont.

          PERFORM f_alta_registro_info CHANGING lf_error.

          IF lf_error = 'X'.
            lf_error_reg_info = 'X'.

*           Registrar subpaso 03 como error
*           Msg: Error alta registro info proveedor &.
            MESSAGE s011(zretpdt001) WITH git_file3-lifnr INTO wa_zretpdt001t01-info.
            PERFORM f_registrar_paso USING '03' lit_matnr-matnr gc_minisemaforo_rojo ld_cont 'Alta registro info' wa_zretpdt001t01-info.
          ELSE.
*           Registrar subpaso 03 como OK

*           Msg: Registro info proveedor & creado correctamente.
            MESSAGE s010(zretpdt001) WITH git_file3-lifnr INTO wa_zretpdt001t01-info.
            PERFORM f_registrar_paso USING '03' lit_matnr-matnr gc_minisemaforo_verde ld_cont 'Alta registro info' wa_zretpdt001t01-info.
          ENDIF.
        ENDLOOP.

        IF lf_error_reg_info = space.
*         Si se han dado de alta correctamente todos los registros info...

*         Registrar paso 03 como OK
          PERFORM f_registrar_paso USING '03' lit_matnr-matnr gc_minisemaforo_verde 1 'Alta registro info' 'Registros info creados correctamente'.

          ld_cont = 1.
          LOOP AT git_file4 WHERE matnr = lit_matnr-matnr.
            ADD 1 TO ld_cont.
            PERFORM f_alta_pvps CHANGING lf_error ld_docnum.

            IF lf_error = 'X'.
              lf_error_pvps = 'X'.
*             Registrar subpaso 04 como error
*             Msg: Error alta PVP para unidad &
              MESSAGE s012(zretpdt001) WITH git_file4-meinh INTO wa_zretpdt001t01-info.
              PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_rojo ld_cont 'Alta PVPs'                   wa_zretpdt001t01-info.
            ELSE.
*             Registrar subpaso 04 como OK
*             Msg: PVP unidad & registrado correctamente.
              MESSAGE s013(zretpdt001) WITH git_file4-meinh INTO wa_zretpdt001t01-info.
              PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_verde ld_cont 'Alta PVPs'                   wa_zretpdt001t01-info.
            ENDIF.
          ENDLOOP.

          IF lf_error_pvps = 'X'.
            LOOP AT git_listado WHERE matnr = lit_matnr-matnr.
              git_listado-info = 'Artículo creado, catalogado y con reg.info pero error al cargar al menos 1 precio.'.
              git_listado-status_g = gc_minisemaforo_rojo.

              MODIFY git_listado.
            ENDLOOP.

*           Registramos paso 04 como error
            PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta PVPs'                   git_listado-info.

*           Registramos en log resumen
            PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_rojo '04' 'Alta PVPs'.

*           Incrementamos contador articulos erróneos
            ADD 1 TO zretpdt001s08-art_error.
          ELSE.
*           Registramos paso 04 como OK
            PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_verde 1 'Alta PVPs'                   'PVP''s creados correctamente'.

            IF git_file1-mtart = 'ZKIT'.
              PERFORM f_procesar_exec_05_kits CHANGING lf_error.

              IF lf_error = 'X' OR
                 lf_error = 'N'.
                LOOP AT git_listado WHERE matnr = lit_matnr-matnr.
                  git_listado-info = 'Error PASO 05: Alta componentes en Kit de ventas.'.
                  git_listado-status_g = gc_minisemaforo_rojo.

                  MODIFY git_listado.
                ENDLOOP.

                IF lf_error = 'X'.
                  PERFORM f_registrar_paso USING '05' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta Componentes KIT' 'Error al actualizar componentes en el KIT'.
                  PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_rojo '05' 'Alta Componentes KIT'.
                ELSEIF lf_error = 'N'.
                  PERFORM f_registrar_paso USING '05' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta Componentes KIT' 'Kit sin componentes.'.
                  PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_rojo '05' 'Alta Componentes KIT.'.
                ENDIF.
              ELSE.
                PERFORM f_registrar_paso USING '05' lit_matnr-matnr gc_minisemaforo_verde 1 'Alta Componentes KIT' 'OK'.
                PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_verde '06' 'Fin creación OK'.

              ENDIF.
            ELSE.
*             Registramos en log resumen como OK
              PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_verde '06' 'Fin creación OK'.

*             Incrementar contador articulos OK
              ADD 1 TO zretpdt001s08-art_ok.
            ENDIF.
          ENDIF.
        ELSE.
          LOOP AT git_listado WHERE matnr = lit_matnr-matnr.
*           Msg: Artículo creado, catalogado pero no dado de alta en al menos 1 proveedor.
            MESSAGE s007(zretpdt001) INTO git_listado-info.
            git_listado-status_g = gc_minisemaforo_rojo.

            MODIFY git_listado.
          ENDLOOP.

*         Registramos pasos 03 y 04 como error
          PERFORM f_registrar_paso USING '03' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta registro info'          git_listado-info.
          PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta PVPs'                   gc_paso_no_realizado.

*         Registramos en log resumen
          PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_rojo '03' 'Alta registro info'.

*         Incrementar contador artículos erróneos
          ADD 1 TO zretpdt001s08-art_error.
        ENDIF.
      ELSE.
*       Si artículo no catalogado...

*       Registramos mensaje de error en el ALV y modificamos el status a rojo
        LOOP AT git_listado WHERE matnr = lit_matnr-matnr.
          IF ld_docnum IS INITIAL.
*           Msg: Artículo creado pero no catalogado (ningun idoc creado)
            MESSAGE s006(zretpdt001) INTO git_listado-info.
          ELSE.
*           Msg: Artículo creado pero no catalogado (Idoc LIKOND: &)
            MESSAGE s009(zretpdt001) WITH ld_docnum INTO git_listado-info.
          ENDIF.
          git_listado-status_g = gc_minisemaforo_rojo.

          MODIFY git_listado.
        ENDLOOP.

*       Registramos pasos 02, 03 y 04 como error
        PERFORM f_registrar_paso USING '02' lit_matnr-matnr gc_minisemaforo_rojo 1 'Catalogar artículo'          git_listado-info.
        PERFORM f_registrar_paso USING '03' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta registro info'          gc_paso_no_realizado.
        PERFORM f_registrar_paso USING '04' lit_matnr-matnr gc_minisemaforo_rojo 1 'Alta PVPs'                   gc_paso_no_realizado.

*       Registramos error en log resumen
        PERFORM f_registrar_resumen USING git_file1-matnr git_file1-maktx gc_minisemaforo_rojo '02' 'Catalogar artículo'.

*       Incrementar contador artículos erroneos
        ADD 1 TO zretpdt001s08-art_error.
      ENDIF.
    ENDIF.

    SELECT SINGLE matnr
      FROM zretpdt001t01
      INTO lit_matnr-matnr
     WHERE matnr = lit_matnr-matnr
       AND pasos = gc_minisemaforo_rojo.

    IF sy-subrc <> 0.
      LOOP AT git_listado WHERE matnr = lit_matnr-matnr.
        git_listado-status_g = gc_minisemaforo_verde.
        git_listado-info     = 'Artículo creado correctamente'.

        MODIFY git_listado.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

  CALL SCREEN 0100 STARTING AT 10 1.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_procesar_exec_lite.
* 0.- Declaración de variables
*==========================================================================
  DATA: lf_error(1),
        ld_catalogar_ok          TYPE xflag,
        ld_docnum                TYPE edi_docnum,
        ld_index_listado         LIKE sy-tabix,
        ld_cont                  TYPE numc10,
        lit_mensajes             LIKE bdcmsgcoll          OCCURS 0 WITH HEADER LINE,
        lit_clientdata           LIKE bapie1marart        OCCURS 0 WITH HEADER LINE,
        lit_clientdatax          LIKE bapie1marartx       OCCURS 0 WITH HEADER LINE,
        lit_unitsofmeasure       LIKE bapie1marmrt        OCCURS 0 WITH HEADER LINE,
        lit_unitsofmeasurex      LIKE bapie1marmrtx       OCCURS 0 WITH HEADER LINE,
        lit_unitofmeasuretexts   LIKE bapie1mamtrt        OCCURS 0 WITH HEADER LINE,
        lit_salesdata            LIKE bapie1mvkert        OCCURS 0 WITH HEADER LINE,
        lit_salesdatax           LIKE bapie1mvkertx       OCCURS 0 WITH HEADER LINE,
        lit_plantdata            LIKE bapie1marcrt        OCCURS 0 WITH HEADER LINE,
        lit_plantdatax           LIKE bapie1marcrtx       OCCURS 0 WITH HEADER LINE,
        lit_warehousenumberdata  LIKE bapie1mlgnrt        OCCURS 0 WITH HEADER LINE,
        lit_warehousenumberdatax LIKE bapie1mlgnrtx       OCCURS 0 WITH HEADER LINE,
        lit_taxclassifications   LIKE bapie1mlanrt        OCCURS 0 WITH HEADER LINE,
        lit_storagelocationdata  LIKE bapie1mardrt        OCCURS 0 WITH HEADER LINE,
        lit_storagelocationdatax LIKE bapie1mardrtx       OCCURS 0 WITH HEADER LINE,
        lit_materialdescription  LIKE bapie1maktrt       OCCURS 0 WITH HEADER LINE,
        lit_addnclientdata       LIKE bapie1maw1rt       OCCURS 0 WITH HEADER LINE,
        lit_addnclientdatax      LIKE bapie1maw1rtx      OCCURS 0 WITH HEADER LINE,
        lit_internationalartnos  LIKE bapie1meanrt       OCCURS 0 WITH HEADER LINE,
        lit_storagetypedata      LIKE bapie1mlgtrt       OCCURS 0 WITH HEADER LINE,
        lit_storagetypedatax     LIKE bapie1mlgtrtx      OCCURS 0 WITH HEADER LINE,
        lit_valuationdata        LIKE bapie1mbewrt       OCCURS 0 WITH HEADER LINE,
        lit_valuationdatax       LIKE bapie1mbewrtx      OCCURS 0 WITH HEADER LINE,
        lit_zretpdtt02           LIKE zretpdtt02         OCCURS 0 WITH HEADER LINE,
        lr_headdata              LIKE bapie1mathead,
        lf_error_reg_info(1),
        lf_error_pvps(1),
        lr_return                LIKE bapireturn1,
        wa_zretpdt001t01         LIKE zretpdt001t01.


* 1.- Lógica
*==========================================================================
* Inicializamos log de carga
  REFRESH git_log_carga.
  CLEAR zretpdt001s08.

  LOOP AT git_lite WHERE status = gc_minisemaforo_ambar AND sel = 'X'.
*   Inicializaciones
    CLEAR: lr_headdata,
           lr_return,
           ld_catalogar_ok,
           ld_docnum,
           lf_error_reg_info,
           lf_error_pvps,
           ld_index_listado.

    REFRESH: lit_mensajes,
             lit_clientdata,
             lit_clientdatax,
             lit_unitsofmeasure,
             lit_unitsofmeasurex,
             lit_unitofmeasuretexts,
             lit_salesdata,
             lit_salesdatax,
             lit_plantdata,
             lit_plantdatax,
             lit_warehousenumberdata,
             lit_warehousenumberdatax,
             lit_taxclassifications,
             lit_storagelocationdata,
             lit_storagelocationdatax,
             lit_materialdescription,
             lit_addnclientdata,
             lit_addnclientdatax,
             lit_internationalartnos,
             lit_storagetypedata,
             lit_storagetypedatax,
             lit_valuationdata,
             lit_valuationdatax.

*   Obtener codigo de articulo
    PERFORM f_get_matnr_range CHANGING git_lite-matnr.

*   Datos de cabecera
    PERFORM f_fill_datos_iniciales_lite CHANGING lr_headdata.

*   Datos básicos
    PERFORM f_fill_datos_basicos_lite TABLES lit_clientdata lit_clientdatax.

*   Ampliación datos básicos
    PERFORM f_fill_ampliacion_datosb_lite TABLES lit_addnclientdata lit_addnclientdatax.

*   Denominación artículo
    PERFORM f_fill_denom_articulo_lite TABLES lit_materialdescription.

*   Datos de unidad de medida y EAN
    PERFORM f_fill_eans_y_ums_lite TABLES lit_unitsofmeasure lit_unitsofmeasurex lit_unitofmeasuretexts lit_internationalartnos.

*   Datos de area de ventas
    PERFORM f_fill_datos_area_ventas_lite TABLES lit_salesdata lit_salesdatax.

*   Datos de centro y almacén
    PERFORM f_fill_datos_centros_lite TABLES lit_plantdata lit_plantdatax lit_storagelocationdata lit_storagelocationdatax.

*   Datos de clasificación fiscal
    PERFORM f_fill_clasif_fiscal_lite TABLES lit_taxclassifications.

*   Datos almacén WM
    PERFORM f_fill_datos_almacen_wm_lite TABLES lit_warehousenumberdata lit_warehousenumberdatax.

*   Creamos artículo
    CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
      EXPORTING
        headdata             = lr_headdata
      IMPORTING
        return               = lr_return
      TABLES
*       VARIANTSKEYS         =
*       CHARACTERISTICVALUE  =
*       CHARACTERISTICVALUEX =
        clientdata           = lit_clientdata
        clientdatax          = lit_clientdatax
*       CLIENTEXT            =
*       CLIENTEXTX           =
        addnlclientdata      = lit_addnclientdata
        addnlclientdatax     = lit_addnclientdatax
        materialdescription  = lit_materialdescription
        plantdata            = lit_plantdata
        plantdatax           = lit_plantdatax
*       PLANTEXT             =
*       PLANTEXTX            =
*       FORECASTPARAMETERS   =
*       FORECASTPARAMETERSX  =
*       FORECASTVALUES       =
*       TOTALCONSUMPTION     =
*       UNPLNDCONSUMPTION    =
*       PLANNINGDATA         =
*       PLANNINGDATAX        =
        storagelocationdata  = lit_storagelocationdata
        storagelocationdatax = lit_storagelocationdatax
*       STORAGELOCATIONEXT   =
*       STORAGELOCATIONEXTX  =
        unitsofmeasure       = lit_unitsofmeasure
        unitsofmeasurex      = lit_unitsofmeasurex
        unitofmeasuretexts   = lit_unitofmeasuretexts
        internationalartnos  = lit_internationalartnos
*       VENDOREAN            =
*       LAYOUTMODULEASSGMT   =
*       LAYOUTMODULEASSGMTX  =
        taxclassifications   = lit_taxclassifications
        valuationdata        = lit_valuationdata
        valuationdatax       = lit_valuationdatax
*       VALUATIONEXT         =
*       VALUATIONEXTX        =
        warehousenumberdata  = lit_warehousenumberdata
        warehousenumberdatax = lit_warehousenumberdatax
*       WAREHOUSENUMBEREXT   =
*       WAREHOUSENUMBEREXTX  =
        storagetypedata      = lit_storagetypedata
        storagetypedatax     = lit_storagetypedatax
*       STORAGETYPEEXT       =
*       STORAGETYPEEXTX      =
        salesdata            = lit_salesdata
        salesdatax           = lit_salesdatax
*       SALESEXT             =
*       SALESEXTX            =
*       POSDATA              =
*       POSDATAX             =
*       POSEXT               =
*       POSEXTX              =
*       MATERIALLONGTEXT     =
*       PLANTKEYS            =
*       STORAGELOCATIONKEYS  =
*       DISTRCHAINKEYS       =
*       WAREHOUSENOKEYS      =
*       STORAGETYPEKEYS      =
*       VALUATIONTYPEKEYS    =
      .

*   Hacemos Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'
*     IMPORTING
*       RETURN        =
      .

    IF lr_return-type = 'E'.
*     Si articulo no creado...

*     Incrementar contador artículos erroneos
      ADD 1 TO zretpdt001s08-art_error.

*     Asignar mensaje de error en el monitor
      MESSAGE ID lr_return-id
            TYPE lr_return-type
          NUMBER lr_return-number
            WITH lr_return-message_v1
                 lr_return-message_v2
                 lr_return-message_v3
                 lr_return-message_v4
            INTO git_lite-info.

      git_lite-status = gc_minisemaforo_rojo.

      MODIFY git_lite.

*     Registrar paso 1 como error
      PERFORM f_registrar_paso USING '01' git_lite-matnr gc_minisemaforo_rojo 1 'Crear artículo'      git_listado-info.

*     Registrar paso 2 como error
      PERFORM f_registrar_paso USING '02' git_lite-matnr gc_minisemaforo_rojo 1 'Catalogar artículo'  gc_paso_no_realizado.

*     Registrar paso 3 como error
      PERFORM f_registrar_paso USING '03' git_lite-matnr gc_minisemaforo_rojo 1 'Alta registro info'  gc_paso_no_realizado.

*     Registrar paso 4 como error
      PERFORM f_registrar_paso USING '04' git_lite-matnr gc_minisemaforo_rojo 1 'Alta Margen'  gc_paso_no_realizado.

*     Añadir resultado proceso al log resumen
      CLEAR gr_log_carga.
      gr_log_carga-matnr   = git_lite-matnr.
      gr_log_carga-matnrt  = git_lite-maktx.
      gr_log_carga-status  = gc_minisemaforo_rojo.
      gr_log_carga-paso    = '01'.
      gr_log_carga-pasot   = 'Crear artículo'.
      APPEND gr_log_carga TO git_log_carga.
    ELSE.
*     Si articulo creado correctamente...

*     Registrar paso 1 como OK
      PERFORM f_registrar_paso USING '01' git_lite-matnr gc_minisemaforo_verde 1 'Crear artículo'  'Artículo creado correctamente'.

*     Actualizamos campos ZZ
      PERFORM f_act_campos_zz_lite.

*     Actualizamos textos UM
      PERFORM f_actualizar_textos_um_lite.

*     Catalogamos
      CALL FUNCTION 'Z_CATALOGAR_SINGLE'
        EXPORTING
          pe_matnr        = git_lite-matnr
          pe_datab        = sy-datum
          pe_datbi        = '99991231'
          pe_catsurt      = 'Z'
        IMPORTING
          ps_catalogar_ok = ld_catalogar_ok
          ps_docnum       = ld_docnum
        TABLES
          it_return       = lit_mensajes.
*          it_tiendas      = .

      IF ld_catalogar_ok = 'X'.
*       Si artículo catalogado correctamente...

*       Registrar paso 2 como OK
        PERFORM f_registrar_paso USING '02' git_lite-matnr gc_minisemaforo_verde 1 'Catalogar artículo'  'Artículo catalogado correctamente'.

*       Creamos registros info del artículo...
        PERFORM f_alta_registro_info_lite CHANGING lf_error.

        IF lf_error = 'X'.
*         Incrementar contador artículos erroneos
          ADD 1 TO zretpdt001s08-art_error.

          lf_error_reg_info = 'X'.

*         Registrar paso 3 como ERROR
*         Msg: Error alta registro info proveedor &.
          MESSAGE s011(zretpdt001) WITH git_lite-3lifnr INTO wa_zretpdt001t01-info.
          PERFORM f_registrar_paso USING '03' git_lite-matnr gc_minisemaforo_rojo 1 'Alta registro info' wa_zretpdt001t01-info.

*         Registrar paso 4 erroneo
          PERFORM f_registrar_paso USING '04' git_lite-matnr gc_minisemaforo_rojo 1 'Alta Margen'  gc_paso_no_realizado.

*         Registramos en log resumen
          CLEAR gr_log_carga.
          gr_log_carga-matnr   = git_lite-matnr.
          gr_log_carga-matnrt  = git_lite-maktx.
          gr_log_carga-status  = gc_minisemaforo_rojo.
          gr_log_carga-paso    = '03'.
          gr_log_carga-pasot   = 'Alta registro info'.
          APPEND gr_log_carga TO git_log_carga.

        ELSE.
*         Registramos paso 3 como OK
*         Msg: Registro info proveedor & creado correctamente.
          MESSAGE s010(zretpdt001) WITH git_lite-3lifnr INTO wa_zretpdt001t01-info.
          PERFORM f_registrar_paso USING '03' git_lite-matnr gc_minisemaforo_verde 1 'Alta registro info' wa_zretpdt001t01-info.

*         Alta margen
          IF git_lite-2aufs IS NOT INITIAL.
            PERFORM f_alta_aufs CHANGING lf_error ld_docnum.

            IF lf_error = 'X'.
*             Registrar paso 4 erroneo
              PERFORM f_registrar_paso USING '04' git_lite-matnr gc_minisemaforo_rojo 1 'Alta Margen' 'Error al grabar margen'.

*             Registramos en log resumen
              CLEAR gr_log_carga.
              gr_log_carga-matnr   = git_lite-matnr.
              gr_log_carga-matnrt  = git_lite-maktx.
              gr_log_carga-status  = gc_minisemaforo_rojo.
              gr_log_carga-paso    = '04'.
              gr_log_carga-pasot   = 'Alta margen'.
              APPEND gr_log_carga TO git_log_carga.

            ELSE.
*             Registrar paso 4 OK
              PERFORM f_registrar_paso USING '04' git_lite-matnr gc_minisemaforo_verde 1 'Alta Margen' 'Margen grabado correctamente'.

*             Registramos en log resumen
              CLEAR gr_log_carga.
              gr_log_carga-matnr   = git_lite-matnr.
              gr_log_carga-matnrt  = git_lite-maktx.
              gr_log_carga-status  = gc_minisemaforo_verde.
              gr_log_carga-paso    = '05'.
              gr_log_carga-pasot   = 'Fin creación OK'.
              APPEND gr_log_carga TO git_log_carga.

*             Incrementar contador artículos erroneos
              ADD 1 TO zretpdt001s08-art_ok.
            ENDIF.
          ELSE.
*           Registramos en log resumen
            CLEAR gr_log_carga.
            gr_log_carga-matnr   = git_lite-matnr.
            gr_log_carga-matnrt  = git_lite-maktx.
            gr_log_carga-status  = gc_minisemaforo_verde.
            gr_log_carga-paso    = '05'.
            gr_log_carga-pasot   = 'Fin creación OK'.
            APPEND gr_log_carga TO git_log_carga.

*           Incrementar contador artículos erroneos
            ADD 1 TO zretpdt001s08-art_ok.
          ENDIF.
        ENDIF.
      ELSE.
*       Si artículo no catalogado...

*       Incremantar contador de articulos erroneos
        ADD 1 TO zretpdt001s08-art_error.

*       Registramos mensaje de error en el ALV y modificamos el status a rojo
        IF ld_docnum IS INITIAL.
*         Msg: Artículo creado pero no catalogado (ningun idoc creado)
          MESSAGE s006(zretpdt001) INTO git_lite-info.
        ELSE.
*         Msg: Artículo creado pero no catalogado (Idoc LIKOND: &)
          MESSAGE s009(zretpdt001) WITH ld_docnum INTO git_lite-info.
        ENDIF.
        git_lite-status = gc_minisemaforo_rojo.

        MODIFY git_lite.

*       Registramos en BBDD entrada de log de proceso
        PERFORM f_registrar_paso USING '02' git_lite-matnr gc_minisemaforo_rojo 1 'Catalogar artículo' 'Error al catalogar el artículo'.

*       Registrar paso 3 como error
        PERFORM f_registrar_paso USING '03' git_lite-matnr gc_minisemaforo_rojo 1 'Alta registro info'  gc_paso_no_realizado.

*       Registrar paso 4 como error
        PERFORM f_registrar_paso USING '04' git_lite-matnr gc_minisemaforo_rojo 1 'Alta Margen'  gc_paso_no_realizado.

*       Registramos en log resumen
        CLEAR gr_log_carga.
        gr_log_carga-matnr   = git_lite-matnr.
        gr_log_carga-matnrt  = git_lite-maktx.
        gr_log_carga-status  = gc_minisemaforo_rojo.
        gr_log_carga-paso    = '02'.
        gr_log_carga-pasot   = 'Catalogar artículo'.
        APPEND gr_log_carga TO git_log_carga.
      ENDIF.
    ENDIF.

    SELECT SINGLE matnr
      FROM zretpdt001t01
      INTO git_lite-matnr
     WHERE matnr = git_lite-matnr
       AND pasos = gc_minisemaforo_rojo.

    IF sy-subrc <> 0.
      git_lite-status = gc_minisemaforo_verde.
      git_lite-info   = 'Artículo creado correctamente'.

      MODIFY git_lite.
    ENDIF.
  ENDLOOP.

  CALL SCREEN 0100 STARTING AT 10 1.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alta_registro_info  CHANGING ps_error.

* 0.- Declaración de variables
*=======================================================================
  DATA: wa_adrc              LIKE adrc,
        wa_lfa1              LIKE lfa1,
        ld_infnr             LIKE eine-infnr,
        wa_eine              LIKE eine,
        ld_cantidad(12),
        ld_unidad(5),
        lf_tiene_reg_info,
        lf_tiene_precio_neto,
        ld_precio(16),
        ld_datab(10),
        ld_mode(1)           VALUE 'N'.


  DATA: wa_eina          TYPE mewieina,
        wa_einax         TYPE mewieinax,
        wa_eine2         TYPE mewieine,
        wa_einex         TYPE mewieinex,
        it_cond_validity LIKE mewivalidity  OCCURS 0 WITH HEADER LINE,
        it_condition     LIKE mewicondition OCCURS 0 WITH HEADER LINE,
        it_return        LIKE bapireturn    OCCURS 0 WITH HEADER LINE.

* 1.- Logica
*=======================================================================
  CLEAR ps_error.

* Datos de proveedor
  IF git_file3-lifnr IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1
      INTO wa_lfa1
     WHERE lifnr = git_file3-lifnr.

    SELECT SINGLE *
      FROM adrc
      INTO wa_adrc
     WHERE addrnumber = wa_lfa1-adrnr.
  ENDIF.

* Miramos si el articulo tiene registro info ya creado en la organizacion
* de compras
  SELECT SINGLE eina~infnr
    FROM eina JOIN eine ON eina~infnr = eine~infnr
    INTO ld_infnr
   WHERE eina~matnr = git_file1-matnr
     AND eina~lifnr = git_file3-lifnr
     AND eine~ekorg = git_file3-ekorg.

  IF sy-subrc = 0.
*   Si tiene registro info => Activamos Flag
    lf_tiene_reg_info = 'X'.

*   Miramos si tiene precio neto
    SELECT SINGLE *
      FROM eine
      INTO wa_eine
     WHERE infnr = ld_infnr.

    IF sy-subrc = 0 AND wa_eine-netpr IS NOT INITIAL.
*     Si tiene precio neto informado => Activamos flag
      lf_tiene_precio_neto = 'X'.
    ENDIF.
  ENDIF.


  CLEAR: wa_eina, wa_einax, wa_eine2, wa_einex, it_cond_validity, it_condition, it_return.
  REFRESH: it_cond_validity, it_condition, it_return.

*  EINA
  wa_eina-material  = git_file3-matnr.
  wa_eina-vendor    = git_file3-lifnr.
  wa_eina-vend_mat  = git_file3-idnlf.
  wa_eina-norm_vend = 'X'.
*  EINAX
  wa_einax-material  = 'X'.
  wa_einax-vendor    = 'X'.
  wa_einax-vend_mat  = 'X'.
  if wa_eina-vendor <> 'CD01'.                                                                       "@APRADAS-10.07.2024 10:15:26
    wa_einax-norm_vend = 'X'.
  endif.
*  EINE
  wa_eine2-purch_org   = git_file3-ekorg.
  wa_eine2-info_type   = git_file3-esokz.
  wa_eine2-pur_group   = git_file3-ekgrp.
  wa_eine2-currency    = git_file3-konwa.
*  wa_eine2-net_price   = git_file3-netpr.
  wa_eine2-price_unit  = 1.
  wa_eine2-conv_num1   = 1.
  wa_eine2-conv_den1   = 1.
  wa_eine2-conf_ctrl   = git_file3-bstae.
  wa_eine2-min_po_qty  = git_file3-minbm.
  wa_eine2-plnd_delry  = git_file3-plifz.
  wa_eine2-nrm_po_qty  = git_file3-norbm.
  IF git_file3-mwskz IS NOT INITIAL.
    wa_eine2-tax_code    = git_file3-mwskz.
  ENDIF.

*  EINEX
  wa_einex-purch_org   = 'X'.
  wa_einex-info_type   = 'X'.
  wa_einex-pur_group   = 'X'.
  wa_einex-currency    = 'X'.
  wa_einex-nrm_po_qty  = 'X'.
  wa_einex-plnd_delry  = 'X'.
*  wa_einex-net_price   = 'X'.
  wa_einex-price_unit  = 'X'.
  wa_einex-conv_num1   = 'X'.
  wa_einex-conv_den1   = 'X'.
  wa_einex-conf_ctrl   = 'X'.
  wa_einex-min_po_qty  = 'X'.
  wa_einex-plnd_delry  = 'X'.
  wa_einex-nrm_po_qty  = 'X'.
  IF git_file3-mwskz IS NOT INITIAL.
    wa_einex-tax_code    = 'X'.
  ENDIF.

* Condición PB00
  IF git_file3-netpr IS NOT INITIAL.
    CLEAR it_cond_validity.
    it_cond_validity-serial_id  = '1'.
    it_cond_validity-base_uom   = git_file3-kmein.
    PERFORM f_conv_exit_cunit_output USING it_cond_validity-base_uom CHANGING it_cond_validity-base_uom.
    it_cond_validity-valid_from = git_file3-datab.
    it_cond_validity-valid_to   = git_file3-datbi.
    APPEND it_cond_validity.

    CLEAR it_condition.
    it_condition-serial_id      = '1'.
    it_condition-cond_type      = 'PB00'.
    it_condition-cond_value     = git_file3-netpr.
    it_condition-currency       = git_file3-konwa.
    it_condition-cond_p_unt     = git_file3-kpein.
    it_condition-cond_unit      = git_file3-kmein.
    it_condition-numerator      = 1.
    it_condition-denominator    = 1.
    APPEND it_condition.
  ENDIF.

* Condición PB30
  IF git_file3-netpr_pb30 IS NOT INITIAL.
    CLEAR it_cond_validity.
    it_cond_validity-serial_id  = '2'.
    it_cond_validity-base_uom   = git_file3-kmein_pb30.
    PERFORM f_conv_exit_cunit_output USING it_cond_validity-base_uom CHANGING it_cond_validity-base_uom.
    it_cond_validity-valid_from = git_file3-datab_pb30.
    it_cond_validity-valid_to   = git_file3-datbi_pb30.
    APPEND it_cond_validity.

    CLEAR it_condition.
    it_condition-serial_id      = '2'.
    it_condition-cond_type      = 'PB30'.
    it_condition-cond_value     = git_file3-netpr_pb30.
    it_condition-currency       = git_file3-konwa_pb30.
    it_condition-cond_p_unt     = git_file3-kpein_pb30.
    it_condition-cond_unit      = git_file3-kmein_pb30.
    it_condition-numerator      = 1.
    it_condition-denominator    = 1.
    APPEND it_condition.
  ENDIF.

* Llamada a la función de alta de registro info
  CALL FUNCTION 'ME_INFORECORD_MAINTAIN'
    EXPORTING
      i_eina        = wa_eina
      i_einax       = wa_einax
      i_eine        = wa_eine2
      i_einex       = wa_einex
*     TESTRUN       =
*   IMPORTING
*     E_EINA        =
*     E_EINE        =
    TABLES
*     TXT_LINES     =
      cond_validity = it_cond_validity
      condition     = it_condition
*     COND_SCALE_VALUE       =
*     COND_SCALE_QUAN        =
      return        = it_return.

* Commit
  COMMIT WORK AND WAIT.

  CALL FUNCTION 'DB_COMMIT'
*   EXPORTING
*     IV_DEFAULT       = ABAP_FALSE
    .

  CALL FUNCTION 'DEQUEUE_ALL'
*     EXPORTING
*       _SYNCHRON       = ' '
    .


  LOOP AT it_return WHERE type = 'E'.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    ps_error = 'X'.
  ENDIF.

ENDFORM.                    " F_ALTA_REGISTRO_INFO

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alta_registro_info_lite  CHANGING ps_error.
* 0.- Declaración de variables
*=======================================================================
  DATA: wa_adrc         LIKE adrc,
        wa_lfa1         LIKE lfa1,
        ld_infnr        LIKE eine-infnr,
        wa_eine         LIKE eine,
        ld_cantidad(12),
        ld_unidad(5),
        ld_precio(16),
        ld_datab(10),
        ld_mode(1)      VALUE 'N'.

  DATA: wa_eina          TYPE mewieina,
        wa_einax         TYPE mewieinax,
        wa_eine2         TYPE mewieine,
        wa_einex         TYPE mewieinex,
        it_cond_validity LIKE mewivalidity  OCCURS 0 WITH HEADER LINE,
        it_condition     LIKE mewicondition OCCURS 0 WITH HEADER LINE,
        it_return        LIKE bapireturn    OCCURS 0 WITH HEADER LINE.

* 1.- Logica
*=======================================================================
  CLEAR ps_error.

* Datos de proveedor
  IF git_lite-3lifnr IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1
      INTO wa_lfa1
     WHERE lifnr = git_lite-3lifnr.

    SELECT SINGLE *
      FROM adrc
      INTO wa_adrc
     WHERE addrnumber = wa_lfa1-adrnr.
  ENDIF.

  CLEAR: wa_eina, wa_einax, wa_eine2, wa_einex, it_cond_validity, it_condition, it_return.
  REFRESH: it_cond_validity, it_condition, it_return.

* EINA
  wa_eina-material  = git_lite-matnr.
  wa_eina-vendor    = git_lite-3lifnr.
  wa_eina-vend_mat  = git_lite-3lifnr.
  wa_eina-norm_vend = 'X'.

* EINAX
  wa_einax-material  = 'X'.
  wa_einax-vendor    = 'X'.
  wa_einax-vend_mat  = 'X'.
  wa_einax-norm_vend = 'X'.

* EINE
  wa_eine2-purch_org   = git_lite-3ekorg.
  wa_eine2-info_type   = '0'.
  wa_eine2-pur_group   = '001'.
  wa_eine2-currency    = git_lite-3konwa.
  wa_eine2-nrm_po_qty  = 1.
  wa_eine2-plnd_delry  = 5.
  wa_eine2-pur_group   = '001'.
  wa_eine2-net_price   = git_lite-3netpr.
  wa_eine2-price_unit  = 1.
  wa_eine2-conv_num1   = 1.
  wa_eine2-conv_den1   = 1.
  wa_eine2-conf_ctrl   = gc_def_bstae.

* EINEX
  wa_einex-purch_org   = 'X'.
  wa_einex-info_type   = 'X'.
  wa_einex-pur_group   = 'X'.
  wa_einex-currency    = 'X'.
  wa_einex-nrm_po_qty  = 'X'.
  wa_einex-plnd_delry  = 'X'.
  wa_einex-pur_group   = 'X'.
  wa_einex-net_price   = 'X'.
  wa_einex-price_unit  = 'X'.
  wa_einex-conv_num1   = 'X'.
  wa_einex-conv_den1   = 'X'.
  wa_einex-conf_ctrl   = 'X'.

* COND_VALIDITY
  it_cond_validity-base_uom   = git_lite-3kmein.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = it_cond_validity-base_uom
*     LANGUAGE       = SY-LANGU
    IMPORTING
*     LONG_TEXT      =
      output         = it_cond_validity-base_uom
*     SHORT_TEXT     =
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  it_cond_validity-valid_from = sy-datlo.
  it_cond_validity-valid_to   = '99991231'.
  APPEND it_cond_validity.

* CONDITION
  it_condition-cond_type   = 'PB00'.
  it_condition-cond_value  = git_lite-3netpr.
  it_condition-currency    = git_lite-3konwa.
  it_condition-cond_p_unt  = git_lite-3kpein.
  it_condition-cond_unit   = git_lite-3kmein.
  it_condition-numerator   = 1.
  it_condition-denominator = 1.
  APPEND it_condition.

  CALL FUNCTION 'ME_INFORECORD_MAINTAIN'
    EXPORTING
      i_eina        = wa_eina
      i_einax       = wa_einax
      i_eine        = wa_eine2
      i_einex       = wa_einex
*     TESTRUN       =
*   IMPORTING
*     E_EINA        =
*     E_EINE        =
    TABLES
*     TXT_LINES     =
      cond_validity = it_cond_validity
      condition     = it_condition
*     COND_SCALE_VALUE       =
*     COND_SCALE_QUAN        =
      return        = it_return.
*  }   REPLACE

  LOOP AT it_return WHERE type = 'E'.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    ps_error = 'X'.
  ENDIF.
ENDFORM.                    " F_ALTA_REGISTRO_INFO

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_INICIALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LR_HEADDATA  text
*----------------------------------------------------------------------*
FORM f_fill_00_datos_iniciales  CHANGING lr_headdata LIKE bapie1mathead..
  lr_headdata-function    = '009'.
  lr_headdata-material    = git_file1-matnr.                               "Código de material
  lr_headdata-matl_type   = git_file1-mtart.                               "Tipo de material
  lr_headdata-matl_group  = git_file1-matkl.                               "Grupo de articulos
  lr_headdata-matl_cat    = git_file1-attyp.                               "Categoría del articulo
  lr_headdata-list_view   = 'X'.                                           "Vista de surtido
  lr_headdata-basic_view  = 'X'.                                           "Vista de datos basicos
  lr_headdata-sales_view  = 'X'.                                           "Vista de ventas
  lr_headdata-logdc_view  = 'X'.                                           "Vista Centro Distribución
  lr_headdata-logst_view  = 'X'.                                           "Vista tienda
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_INICIALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LR_HEADDATA  text
*----------------------------------------------------------------------*
FORM f_fill_datos_iniciales_lite  CHANGING lr_headdata LIKE bapie1mathead.
  CLEAR lr_headdata.
  lr_headdata-function    = '009'.
  lr_headdata-material    = git_lite-matnr.                                "Código de material
  lr_headdata-matl_type   = git_lite-mtart.                                "Tipo de material
  lr_headdata-matl_group  = git_lite-matkl.                                "Grupo de articulos
  lr_headdata-matl_cat    = git_lite-attyp.                                "Categoría del articulo
  lr_headdata-list_view   = 'X'.                                           "Vista de surtido
  lr_headdata-basic_view  = 'X'.                                           "Vista de datos basicos
  lr_headdata-sales_view  = 'X'.                                           "Vista de ventas
  lr_headdata-logdc_view  = 'X'.                                           "Vista Centro Distribución
  lr_headdata-logst_view  = 'X'.                                           "Vista tienda
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_BASICOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_CLIENTDATA  text
*      -->P_LIT_CLIENTDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_01_datos_basicos  TABLES   lit_clientdata  STRUCTURE bapie1marart
                                    lit_clientdatax STRUCTURE bapie1marartx.

*>Inicializaciones
  CLEAR: lit_clientdata,
         lit_clientdatax.

*>Registrar datos básicos
  lit_clientdata-function        = '009'.
  lit_clientdatax-function       = '009'.

  IF git_file1-matnr IS NOT INITIAL.
    lit_clientdata-material       = git_file1-matnr.                                                "Artículo
    lit_clientdatax-material      = git_file1-matnr.
  ENDIF.

  IF git_file1-meins IS NOT INITIAL.
    lit_clientdata-base_uom       = git_file1-meins.                                                "UMB
    lit_clientdatax-base_uom      = 'X'.
  ENDIF.

  IF git_file1-taklv IS NOT INITIAL.
    lit_clientdata-tax_class      = git_file1-taklv.                                                "Indicador Impuestos
    lit_clientdatax-tax_class     = 'X'.
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'TRANS_GRP'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_clientdata-trans_grp      = git_val_def_int-valor2.                                         "Grupo transporte
    lit_clientdatax-trans_grp     = 'X'.
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'ITEM_CAT'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_clientdata-item_cat       = git_val_def_int-valor2.                                         "Grupo de tipos de posición general
    lit_clientdatax-item_cat      = 'X'.
  ENDIF.

  IF git_file1-extwg IS NOT INITIAL.                                                                "Grupo de artículos externo
    lit_clientdata-extmatlgrp     = git_file1-extwg.
    lit_clientdatax-extmatlgrp    = 'X'.
  ENDIF.

  IF git_file1-inhal IS NOT INITIAL.
    lit_clientdata-net_cont       = git_file1-inhal.                                                "Contenido Neto
    lit_clientdatax-net_cont      = 'X'.
  ENDIF.

  IF git_file1-inhme IS NOT INITIAL.
    lit_clientdata-cont_unit      = git_file1-inhme.                                                "Unidad contenido
    lit_clientdatax-cont_unit     = 'X'.
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'QUAL_DIK'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_clientdata-qual_dik       = git_val_def_int-valor2.                                         "El artículo es susceptible de bonificación en especie
    lit_clientdatax-qual_dik      = 'X'.
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'VAR_ORD_UN'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_clientdata-var_ord_un	    = git_val_def_int-valor2.                                         "Unidad variable de medida de pedido activa
    lit_clientdatax-var_ord_un    = 'X'.
  ENDIF.

  IF git_file1-brand_id IS NOT INITIAL.
    lit_clientdata-brand_id       = git_file1-brand_id.                                             "Marca
    lit_clientdatax-brand_id      = 'X'.
  ENDIF.

  IF git_file1-ntgew IS NOT INITIAL.
    lit_clientdata-net_weight     = git_file1-ntgew.                                               "Peso Neto
    lit_clientdatax-net_weight    = 'X'.
  ENDIF.

  IF git_file1-groes IS NOT INITIAL.
    lit_clientdata-size_dim       = git_file1-groes.                                                "Tamaño/Dimensión
    lit_clientdatax-size_dim      = 'X'.
  ENDIF.

  IF git_file1-mstae IS NOT INITIAL.
    lit_clientdata-pur_status     = git_file1-mstae.                                                "Status artículo para todos los centros
    lit_clientdatax-pur_status    = 'X'.
  ENDIF.

  IF git_file1-mstde IS NOT INITIAL.
    lit_clientdata-pvalidfrom     = git_file1-mstde.                                                "Status Valido de
    lit_clientdatax-pvalidfrom    = 'X'.
  ENDIF.

  IF git_file1-normt IS NOT INITIAL.
    lit_clientdata-std_descr      = git_file1-normt.                                                "Denominación de estándar (p.ej.DIN)
    lit_clientdatax-std_descr     = 'X'.
  ENDIF.

  IF git_file1-mstdv IS NOT INITIAL.                                                                "Fecha inicio validez status artículo p.todas cadenas distr.
    lit_clientdata-svalidfrom     = git_file1-mstdv.
    lit_clientdatax-svalidfrom    = 'X'.
  ENDIF.

  IF git_file1-mstav IS NOT INITIAL.                                                                "Status de artículo común a todas las cadenas de distribución
    lit_clientdata-sal_status     = git_file1-mstav.
    lit_clientdatax-sal_status    = 'X'.
  ENDIF.

  IF git_file1-mtpos IS NOT INITIAL.
    lit_clientdata-item_cat      = git_file1-mtpos.                                                 "Grupo de tipos de posición del maestro de artículo
    lit_clientdatax-item_cat     = 'X'.
  ENDIF.

  IF git_file1-zeinr IS NOT INITIAL.
    lit_clientdata-document      = git_file1-zeinr.                                                 "Número de documento (sin sistema de gestión de documentos)
    lit_clientdatax-document     = 'X'.
  ENDIF.

  APPEND: lit_clientdata,
          lit_clientdatax.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_BASICOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_CLIENTDATA  text
*      -->P_LIT_CLIENTDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_datos_basicos_lite  TABLES   lit_clientdata  STRUCTURE bapie1marart
                                         lit_clientdatax STRUCTURE bapie1marartx.

  CLEAR lit_clientdata.
  lit_clientdata-function       = '009'.
  lit_clientdata-material       = git_lite-matnr.                          "Código de artículo
  lit_clientdata-base_uom       = git_lite-meins.                          "UMB
*   lit_clientdata-net_weight     = ps_listado-db_ntgew.                   "Peso Neto
  lit_clientdata-tax_class      = git_lite-taklv.                          "Indicador Impuestos
*   lit_clientdata-size_dim       = ld_dimensiones.                        "Medidas
*   lit_clientdata-std_descr      = ps_listado-info_lifnr.                 "Nº Proveedor
*   lit_clientdata-brand_id       = ps_listado-db_brand_id.
  lit_clientdata-trans_grp      = '0001'.                                  "Grupo transporte
  lit_clientdata-item_cat       = 'NORM'.
  lit_clientdata-trans_grp      = '0001'.
  lit_clientdata-extmatlgrp     = git_lite-extwg.
*   IF ps_listado-db_mstde IS NOT INITIAL.
*     lit_clientdata-pur_status     = gc_mstae.
*     lit_clientdata-pvalidfrom     = ps_listado-db_mstde.
*   ENDIF.
  lit_clientdata-net_cont       = git_lite-inhal.                          "Contenido Neto
  lit_clientdata-cont_unit      = git_lite-inhme.                          "Unidad contenido
  lit_clientdata-qual_dik       = '1'.                                     "Susceptible bonificación en especie
  lit_clientdata-var_ord_un	    = '1'.                                     "Unidad variable de medida de pedido activa
  APPEND lit_clientdata.

  CLEAR lit_clientdatax.
  lit_clientdatax-function       = '009'.
* IF ps_listado-db_mstde IS NOT INITIAL.
*   lit_clientdatax-pur_status     = 'X'.
*   lit_clientdatax-pvalidfrom     = 'X'.
* ENDIF.
  lit_clientdatax-material       = git_lite-matnr.
  lit_clientdatax-base_uom       = 'X'.
* lit_clientdatax-net_weight     = 'X'.
  lit_clientdatax-tax_class      = 'X'.
* lit_clientdatax-size_dim       = 'X'.
* lit_clientdatax-std_descr      = 'X'.
  lit_clientdatax-trans_grp      = 'X'.
  lit_clientdatax-item_cat       = 'X'.
  lit_clientdatax-trans_grp      = 'X'.
* lit_clientdatax-brand_id       = 'X'.
  lit_clientdatax-extmatlgrp     = 'X'.
  lit_clientdatax-net_cont       = 'X'.
  lit_clientdatax-cont_unit      = 'X'.
  lit_clientdatax-qual_dik       = 'X'.
  lit_clientdatax-var_ord_un     = 'X'.
  APPEND lit_clientdatax.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DENOM_ARTICULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_MATERIALDESCRIPTION  text
*----------------------------------------------------------------------*
FORM f_fill_03_denom_articulo  TABLES   lit_materialdescription STRUCTURE bapie1maktrt.
  lit_materialdescription-function  = '009'.
  lit_materialdescription-material  = git_file1-matnr.                                              "Artículo
  lit_materialdescription-langu     = 'S'.
  lit_materialdescription-matl_desc = git_file1-maktx(40).                                              "Texto breve del articulo
  APPEND lit_materialdescription.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_FILL_DENOM_ARTICULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_MATERIALDESCRIPTION  text
*----------------------------------------------------------------------*
FORM f_fill_denom_articulo_lite TABLES   lit_materialdescription STRUCTURE bapie1maktrt.
  CLEAR lit_materialdescription.
  lit_materialdescription-function  = '009'.                               "Texto breve del articulo
  lit_materialdescription-material  = git_lite-matnr.
  lit_materialdescription-langu     = 'S'.
  lit_materialdescription-matl_desc = git_lite-maktx.
  APPEND lit_materialdescription.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_EANS_Y_UMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_UNITSOFMEASURE  text
*      -->P_LIT_UNITSOFMEASUREX  text
*----------------------------------------------------------------------*
FORM f_fill_04_eans_y_ums  TABLES   lit_unitsofmeasure      STRUCTURE bapie1marmrt
                                 lit_unitsofmeasurex     STRUCTURE bapie1marmrtx
                                 lit_unitofmeasuretexts  STRUCTURE bapie1mamtrt
                                 lit_internationalartnos STRUCTURE bapie1meanrt.

* 0.- Declaración de variables
*===================================================================================================
  DATA: BEGIN OF lit_matnr_meinh OCCURS 0,
          matnr LIKE mara-matnr,
          meinh LIKE mean-meinh,
        END OF lit_matnr_meinh.

* 1.- Lógica
*===================================================================================================
  LOOP AT git_file2 WHERE matnr = git_file1-matnr.
    READ TABLE lit_matnr_meinh WITH KEY matnr = git_file2-matnr meinh = git_file2-meinh.

    IF sy-subrc <> 0.
*     Registrar chivato Artículo-Unidad
      lit_matnr_meinh-matnr = git_file2-matnr.
      lit_matnr_meinh-meinh = git_file2-meinh.
      APPEND lit_matnr_meinh.

*>>>>>Registrar datos

*     Inicializaciones
      CLEAR: lit_unitsofmeasure,
             lit_unitsofmeasurex.

      lit_unitsofmeasure-function    = '009'.
      lit_unitsofmeasurex-function   = '009'.

      IF git_file2-matnr IS NOT INITIAL.                                                            "Artículo
        lit_unitsofmeasure-material    = git_file2-matnr.
        lit_unitsofmeasurex-material   = git_file2-matnr.
      ENDIF.

      IF git_file2-meinh IS NOT INITIAL.
        lit_unitsofmeasure-alt_unit    = git_file2-meinh.                                           "UMB
        lit_unitsofmeasurex-alt_unit   = git_file2-meinh.
      ENDIF.

      IF git_file2-ean11 IS NOT INITIAL.
        lit_unitsofmeasure-ean_upc     = git_file2-ean11.                                           "EAN11
        lit_unitsofmeasurex-ean_upc     = 'X'.
      ENDIF.

      IF git_file2-numtp IS NOT INITIAL.
        lit_unitsofmeasure-ean_cat     = git_file2-numtp.                                           "Tipo de EAN
        lit_unitsofmeasurex-ean_cat     = 'X'.
      ENDIF.

      IF git_file2-umrez IS NOT INITIAL.
        lit_unitsofmeasure-numerator   = git_file2-umrez.                                           "Factor conversion sobre UMB
        lit_unitsofmeasurex-numerator   = 'X'.
      ENDIF.

      IF git_file2-umren IS NOT INITIAL.
        lit_unitsofmeasure-denominatr  = git_file2-umren.                                           "Factor conversion sobre UMA
        lit_unitsofmeasurex-denominatr  = 'X'.
      ENDIF.

      IF git_file2-brgew IS NOT INITIAL.
        lit_unitsofmeasure-gross_wt   = git_file2-brgew.                                            "Peso Bruto
        lit_unitsofmeasurex-gross_wt   = 'X'.
      ENDIF.

      IF git_file2-gewei IS NOT INITIAL.
        lit_unitsofmeasure-unit_of_wt = git_file2-gewei.                                            "Unidad peso
        lit_unitsofmeasurex-unit_of_wt = 'X'.
      ENDIF.

      IF git_file2-laeng IS NOT INITIAL.
        lit_unitsofmeasure-length     = git_file2-laeng.                                            "Longitud
        lit_unitsofmeasurex-length     = 'X'.
      ENDIF.

      IF git_file2-breit IS NOT INITIAL.
        lit_unitsofmeasure-width      = git_file2-breit.                                            "Ancho
        lit_unitsofmeasurex-width      = 'X'.
      ENDIF.

      IF git_file2-hoehe IS NOT INITIAL.
        lit_unitsofmeasure-height     = git_file2-hoehe.                                            "Altura
        lit_unitsofmeasurex-height     = 'X'.
      ENDIF.

      IF git_file2-meabm IS NOT INITIAL.
        lit_unitsofmeasure-unit_dim   = git_file2-meabm.                                            "Unidad dimensión
        lit_unitsofmeasurex-unit_dim  = 'X'.
      ENDIF.

      IF git_file2-volum IS NOT INITIAL.
        lit_unitsofmeasure-volume      = git_file2-volum.                                           "Volumen
        lit_unitsofmeasurex-volume     = 'X'.
      ENDIF.

      IF git_file2-voleh IS NOT INITIAL.
        lit_unitsofmeasure-volumeunit  = git_file2-voleh.                                           "Unidad de volumen
        lit_unitsofmeasurex-volumeunit = 'X'.
      ENDIF.

*>>>>>Insertar datos
      APPEND: lit_unitsofmeasure,
              lit_unitsofmeasurex.
    ELSE.
*     Si ya se ha cargado un ean para el articulo, el resto se cargan como adicionales
      CLEAR lit_internationalartnos.

      lit_internationalartnos-function = '009'.
      IF git_file2-matnr IS NOT INITIAL.
        lit_internationalartnos-material = git_file2-matnr.
      ENDIF.

      IF git_file2-meinh IS NOT INITIAL.
        lit_internationalartnos-unit     = git_file2-meinh.                  "UMB
      ENDIF.

      IF git_file2-ean11 IS NOT INITIAL.
        lit_internationalartnos-ean_upc  = git_file2-ean11.                  "EAN UPC
      ENDIF.

      IF git_file2-numtp IS NOT INITIAL.
        lit_internationalartnos-ean_cat  = git_file2-numtp.
      ENDIF.

      APPEND lit_internationalartnos.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_EANS_Y_UMS
*&---------------------------------------------------------------------*
* Registra los datos de unidad de medida del articulo
*----------------------------------------------------------------------*
*      -->P_LIT_UNITSOFMEASURE  text
*      -->P_LIT_UNITSOFMEASUREX  text
*----------------------------------------------------------------------*
FORM f_fill_eans_y_ums_lite  TABLES   lit_unitsofmeasure      STRUCTURE bapie1marmrt
                                      lit_unitsofmeasurex     STRUCTURE bapie1marmrtx
                                      lit_unitofmeasuretexts  STRUCTURE bapie1mamtrt
                                      lit_internationalartnos STRUCTURE bapie1meanrt.

*   UM Datos básicos
  CLEAR lit_unitsofmeasure.
  lit_unitsofmeasure-function    = '009'.
  lit_unitsofmeasure-material    = git_lite-matnr.
  lit_unitsofmeasure-alt_unit    = git_lite-meins.                      "UMB
  IF git_lite-ean11 <> gc_ean_20000. "APRADAS-30.05.2018
    lit_unitsofmeasure-ean_upc     = git_lite-ean11.                      "EAN11
  ELSE.
    lit_unitsofmeasure-ean_cat     = 'IE'.                                "Tipo de EAN
  ENDIF.
  lit_unitsofmeasure-numerator   = 1.                      "Factor conversion sobre UMB
  lit_unitsofmeasure-denominatr  = 1.                      "Factor conversion sobre UMA
*   lit_unitsofmeasure-volume     = ps_listado-db_volum.                   "Volumen
*   lit_unitsofmeasure-volumeunit = ps_listado-db_voleh.                   "Unidad de volumen
*   lit_unitsofmeasure-gross_wt   = ps_listado-db_brgew.                   "Peso Bruto
*   IF ps_listado-db_gewei IS NOT INITIAL.
*     lit_unitsofmeasure-unit_of_wt = ps_listado-db_gewei.                 "Unidad peso
*   ENDIF.
*   lit_unitsofmeasure-length     = ps_listado-db_laeng.                   "Longitud
*   lit_unitsofmeasure-width      = ps_listado-db_breit.                   "Ancho
*   lit_unitsofmeasure-height     = ps_listado-db_hoehe.                   "Altura
*   lit_unitsofmeasure-unit_dim   = ps_listado-db_meabm.                   "Unidad dimensión
  APPEND lit_unitsofmeasure.

  CLEAR lit_unitsofmeasurex.
  lit_unitsofmeasurex-function    = '009'.
  lit_unitsofmeasurex-material    = git_lite-matnr.
  lit_unitsofmeasurex-alt_unit    = git_lite-meins.
  lit_unitsofmeasurex-ean_upc     = 'X'.
  IF git_lite-ean11 = gc_ean_20000. "APRADAS-30.05.2018
    lit_unitsofmeasurex-ean_cat     = 'X'.
  ENDIF.
  lit_unitsofmeasurex-numerator   = 'X'.
  lit_unitsofmeasurex-denominatr  = 'X'.
*   lit_unitsofmeasurex-volume     = 'X'.
*   lit_unitsofmeasurex-volumeunit = 'X'.
*   lit_unitsofmeasurex-gross_wt   = 'X'.
*   IF ps_listado-db_gewei IS NOT INITIAL.
*     lit_unitsofmeasurex-unit_of_wt = 'X'.
*   ENDIF.
*   lit_unitsofmeasurex-length     = 'X'.
*   lit_unitsofmeasurex-width      = 'X'.
*   lit_unitsofmeasurex-height     = 'X'.
*   lit_unitsofmeasurex-unit_dim   = 'X'.
  APPEND lit_unitsofmeasurex.

*   UM Datos de venta (UN alternativa 1)
  IF git_lite-2meinh IS NOT INITIAL.
    CLEAR lit_unitsofmeasure.
    lit_unitsofmeasure-function    = '009'.
    lit_unitsofmeasure-material    = git_lite-matnr.
    lit_unitsofmeasure-alt_unit    = git_lite-2meinh.                      "UMB
*     APRADAS-Inicio-30.04.2018 08:42:02
    IF git_lite-2ean11 <> gc_ean_20000. "APRADAS-30.05.2018
      lit_unitsofmeasure-ean_upc     = git_lite-2ean11.                    "EAN11
    ELSE.
      lit_unitsofmeasure-ean_cat     = 'IE'.                                "Tipo de EAN
    ENDIF.
*     APRADAS-Fin-30.04.2018 08:42:02
*      lit_unitsofmeasure-ean_cat     = git_lite-numtp.                      "Tipo de EAN
    lit_unitsofmeasure-numerator   = git_lite-2umrez.                      "Factor conversion sobre UMB
    lit_unitsofmeasure-denominatr  = git_lite-2umren.                      "Factor conversion sobre UMA
*     lit_unitsofmeasure-volume     = ps_listado-db_volum.                   "Volumen
*     lit_unitsofmeasure-volumeunit = ps_listado-db_voleh.                   "Unidad de volumen
*     lit_unitsofmeasure-gross_wt   = ps_listado-db_brgew.                   "Peso Bruto
*     IF ps_listado-db_gewei IS NOT INITIAL.
*       lit_unitsofmeasure-unit_of_wt = ps_listado-db_gewei.                 "Unidad peso
*     ENDIF.
*     lit_unitsofmeasure-length     = ps_listado-db_laeng.                   "Longitud
*     lit_unitsofmeasure-width      = ps_listado-db_breit.                   "Ancho
*     lit_unitsofmeasure-height     = ps_listado-db_hoehe.                   "Altura
*     lit_unitsofmeasure-unit_dim   = ps_listado-db_meabm.                   "Unidad dimensión
    APPEND lit_unitsofmeasure.

    CLEAR lit_unitsofmeasurex.
    lit_unitsofmeasurex-function    = '009'.
    lit_unitsofmeasurex-material    = git_lite-matnr.
    lit_unitsofmeasurex-alt_unit    = git_lite-2meinh.
    lit_unitsofmeasurex-ean_upc     = 'X'.
*     APRADAS-Inicio-30.04.2018 08:42:02
    IF git_lite-2ean11 = gc_ean_20000. "APRADAS-30.05.2018
      lit_unitsofmeasurex-ean_cat     = 'X'.                                "Tipo de EAN
    ENDIF.
*     APRADAS-Fin-30.04.2018 08:42:02
*      lit_unitsofmeasurex-ean_cat     = 'X'.
    lit_unitsofmeasurex-numerator   = 'X'.
    lit_unitsofmeasurex-denominatr  = 'X'.
*     lit_unitsofmeasurex-volume     = 'X'.
*     lit_unitsofmeasurex-volumeunit = 'X'.
*     lit_unitsofmeasurex-gross_wt   = 'X'.
*     IF ps_listado-db_gewei IS NOT INITIAL.
*       lit_unitsofmeasurex-unit_of_wt = 'X'.
*     ENDIF.
*     lit_unitsofmeasurex-length     = 'X'.
*     lit_unitsofmeasurex-width      = 'X'.
*     lit_unitsofmeasurex-height     = 'X'.
*     lit_unitsofmeasurex-unit_dim   = 'X'.
    APPEND lit_unitsofmeasurex.
  ENDIF.

*   UM Datos de compras (UM Alternativa 3)
  IF git_lite-3meinh IS NOT INITIAL.
    CLEAR lit_unitsofmeasure.
    lit_unitsofmeasure-function    = '009'.
    lit_unitsofmeasure-material    = git_lite-matnr.
    lit_unitsofmeasure-alt_unit    = git_lite-3meinh.                      "UMB
    IF git_lite-3ean11 <> gc_ean_20000. "APRADAS-30.05.2018
      lit_unitsofmeasure-ean_upc     = git_lite-3ean11.                    "EAN11
    ELSE.
      lit_unitsofmeasure-ean_cat     = 'IE'.
    ENDIF.
*      lit_unitsofmeasure-ean_cat     = git_lite-numtp.                      "Tipo de EAN
    lit_unitsofmeasure-numerator   = git_lite-3umrez.                      "Factor conversion sobre UMB
    lit_unitsofmeasure-denominatr  = git_lite-3umren.                      "Factor conversion sobre UMA
*     lit_unitsofmeasure-volume     = ps_listado-db_volum.                   "Volumen
*     lit_unitsofmeasure-volumeunit = ps_listado-db_voleh.                   "Unidad de volumen
*     lit_unitsofmeasure-gross_wt   = ps_listado-db_brgew.                   "Peso Bruto
*     IF ps_listado-db_gewei IS NOT INITIAL.
*       lit_unitsofmeasure-unit_of_wt = ps_listado-db_gewei.                 "Unidad peso
*     ENDIF.
*     lit_unitsofmeasure-length     = ps_listado-db_laeng.                   "Longitud
*     lit_unitsofmeasure-width      = ps_listado-db_breit.                   "Ancho
*     lit_unitsofmeasure-height     = ps_listado-db_hoehe.                   "Altura
*     lit_unitsofmeasure-unit_dim   = ps_listado-db_meabm.                   "Unidad dimensión
    APPEND lit_unitsofmeasure.

    CLEAR lit_unitsofmeasurex.
    lit_unitsofmeasurex-function    = '009'.
    lit_unitsofmeasurex-material    = git_lite-matnr.
    lit_unitsofmeasurex-alt_unit    = git_lite-3meinh.
    lit_unitsofmeasurex-ean_upc     = 'X'.
*     APRADAS-Inicio-30.04.2018 08:42:02
    IF git_lite-3ean11 = gc_ean_20000. "APRADAS-30.05.2018
      lit_unitsofmeasurex-ean_cat     = 'X'.                                "Tipo de EAN
    ENDIF.
*     APRADAS-Fin-30.04.2018 08:42:02
*      lit_unitsofmeasurex-ean_cat     = 'X'.
    lit_unitsofmeasurex-numerator   = 'X'.
    lit_unitsofmeasurex-denominatr  = 'X'.
*     lit_unitsofmeasurex-volume     = 'X'.
*     lit_unitsofmeasurex-volumeunit = 'X'.
*     lit_unitsofmeasurex-gross_wt   = 'X'.
*     IF ps_listado-db_gewei IS NOT INITIAL.
*       lit_unitsofmeasurex-unit_of_wt = 'X'.
*     ENDIF.
*     lit_unitsofmeasurex-length     = 'X'.
*     lit_unitsofmeasurex-width      = 'X'.
*     lit_unitsofmeasurex-height     = 'X'.
*     lit_unitsofmeasurex-unit_dim   = 'X'.
    APPEND lit_unitsofmeasurex.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_AREA_VENTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_SALESDATA  text
*      -->P_LIT_SALESDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_05_datos_area_ventas  TABLES   lit_salesdata  STRUCTURE bapie1mvkert
                                           lit_salesdatax STRUCTURE bapie1mvkertx.

  DATA: ld_valor LIKE zretpdt001t02-valor,
        ld_vkorg_vtweg(10)..

  LOOP AT git_alta_areas_venta.
*   Inicializar datos
    CLEAR: lit_salesdata,
           lit_salesdatax.

*   Registrar datos
    lit_salesdata-function      = '009'.
    lit_salesdatax-function     = '009'.

    lit_salesdata-material      = git_file1-matnr.                                                  "Artículo
    lit_salesdatax-material     = git_file1-matnr.

    lit_salesdata-sales_org     = git_alta_areas_venta-valor.                                       "Org. de ventas
    lit_salesdatax-sales_org    = git_alta_areas_venta-valor.

    lit_salesdata-distr_chan    = git_alta_areas_venta-valor2.                                      "Canal
    lit_salesdatax-distr_chan   = git_alta_areas_venta-valor2.

    lit_salesdata-prod_att_1    = git_file1-prat1.                                                  "Exclusividad
    lit_salesdatax-prod_att_1   = 'X'.

    lit_salesdata-prod_att_2    = git_file1-prat2.                                                  "Outlet
    lit_salesdatax-prod_att_2   = 'X'.

    lit_salesdata-prod_att_3    = git_file1-prat3.                                                  "No sube a Puntronic
    lit_salesdatax-prod_att_3   = 'X'.

    lit_salesdata-prod_att_4    = git_file1-prat4.                                                  "No sube a la Web
    lit_salesdatax-prod_att_4   = 'X'.

    lit_salesdata-prod_att_5    = git_file1-prat5.                                                  "No sube al Portal
    lit_salesdatax-prod_att_5   = 'X'.

    IF git_file1-mtpos IS NOT INITIAL.
      lit_salesdata-item_cat      = git_file1-mtpos.                                                "Grupo de tipos de posición del maestro de artículo
      lit_salesdatax-item_cat     = 'X'.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> APRADAS - 04.06.2024 17:04:09 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Inicio
*     Sustituir MTPOS del excel por el de sustitucion
      CONCATENATE git_alta_areas_venta-valor
                  git_alta_areas_venta-valor2
             into ld_vkorg_vtweg.

      select single descr
        from ZRETPDT001T02
        into lit_salesdata-item_cat
       where param  = 'MTPOS_CONV'
         and valor  = ld_vkorg_vtweg
         and valor2 = lit_salesdata-item_cat.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> APRADAS - 04.06.2024 17:04:09 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<Fin
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'CASH_DISC'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_salesdata-cash_disc     = git_val_def_int-valor2.                                         "Indicador: Derecho a descuentos
      lit_salesdatax-cash_disc    = 'X'.
    ENDIF.

    IF git_file1-ktgrm IS NOT INITIAL.
      lit_salesdata-acct_assgt     = git_file1-ktgrm.                                               "Grupo de imputación para artículo
      lit_salesdatax-acct_assgt    = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'MATL_GRP_2'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_salesdata-matl_grp_2     = git_val_def_int-valor2.                                        "Grupo material 2
      lit_salesdatax-matl_grp_2    = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'DELYG_PLNT'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.                                      "Centro suministrador
      lit_salesdata-delyg_plnt    = git_val_def_int-valor2.
      lit_salesdatax-delyg_plnt    = 'X'.
    ENDIF.


    CONCATENATE 'UNIT_GROUP' lit_salesdata-sales_org lit_salesdata-distr_chan INTO ld_valor.
    READ TABLE git_val_def_int WITH KEY valor = ld_valor.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.                                      "Grupo de unidades de medida
      lit_salesdata-unit_group    = git_val_def_int-valor2.
      lit_salesdatax-unit_group    = 'X'.
    ENDIF.

    IF git_file1-scmng IS NOT INITIAL.
      lit_salesdata-dely_unit = git_file1-scmng.
      lit_salesdatax-dely_unit = 'X'.
    ENDIF.



*>>>Añadir datos
    APPEND: lit_salesdata,
            lit_salesdatax.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_AREA_VENTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_SALESDATA  text
*      -->P_LIT_SALESDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_datos_area_ventas_lite TABLES   lit_salesdata  STRUCTURE bapie1mvkert
                                        lit_salesdatax STRUCTURE bapie1mvkertx.

  CLEAR lit_salesdata.
  lit_salesdata-function      = '009'.
  lit_salesdata-material      = git_lite-matnr.
  lit_salesdata-sales_org     = 'A008'.                                 "Org. de ventas
  lit_salesdata-distr_chan    = '10'.                                    "Canal
*    lit_salesdata-cash_disc     = 'X'.                                     "Indicador: Derecho a descuentos
  lit_salesdata-item_cat      = 'NORM'.                                  "Grupo de tipos de posición del maestro de artículo
**    lit_salesdata-acct_assgt    = ps_listado-dv_ktgrm.                     "Grupo de imputación para artículo
*    lit_salesdata-matl_grp_2    = ps_listado-dv_mvgr2.                     "Grupo material 2
  APPEND lit_salesdata.

  CLEAR lit_salesdatax.
  lit_salesdatax-function      = '009'.
  lit_salesdatax-material      = git_lite-matnr.
  lit_salesdatax-sales_org     = 'A008'.
  lit_salesdatax-distr_chan    = '10'.
*    lit_salesdatax-cash_disc     = 'X'.
  lit_salesdatax-item_cat      = 'X'.
*    lit_salesdatax-acct_assgt    = 'X'.
*    lit_salesdatax-delyg_plnt    = 'X'.
*    lit_salesdatax-matl_grp_2    = 'X'.
  APPEND lit_salesdatax.


* Replicamos al resto de areas de ventas
  lit_salesdata-function      = '009'.
  lit_salesdata-sales_org     = 'A005'.
  lit_salesdata-distr_chan    = '10'.
  APPEND lit_salesdata.

  lit_salesdatax-function      = '009'.
  lit_salesdatax-sales_org     = 'A005'.
  lit_salesdatax-distr_chan    = '10'.
  APPEND lit_salesdatax.


  lit_salesdata-function      = '009'.
  lit_salesdata-sales_org     = 'A005'.
  lit_salesdata-distr_chan    = '30'.
  APPEND lit_salesdata.

  lit_salesdatax-function      = '009'.
  lit_salesdatax-sales_org     = 'A005'.
  lit_salesdatax-distr_chan    = '30'.
  APPEND lit_salesdatax.

  lit_salesdata-function      = '009'.
  lit_salesdata-sales_org     = 'A008'.
  lit_salesdata-distr_chan    = '20'.
  APPEND lit_salesdata.

  lit_salesdatax-function      = '009'.
  lit_salesdatax-sales_org     = 'A008'.
  lit_salesdatax-distr_chan    = '20'.
  APPEND lit_salesdatax.

  lit_salesdata-function      = '009'.
  lit_salesdata-sales_org     = 'A008'.
  lit_salesdata-distr_chan    = '30'.
  APPEND lit_salesdata.

  lit_salesdatax-function      = '009'.
  lit_salesdatax-sales_org     = 'A008'.
  lit_salesdatax-distr_chan    = '30'.
  APPEND lit_salesdatax.
ENDFORM.

*===================================================================================================
*&      Form  F_FILL_DATOS_CENTROS
*===================================================================================================
* Rellena los datos de centros y almacenes a crear con la BAPI_MATERIAL_MAINTAINDATA_RT, a partir
* de los valores parametrizados en la ZRETPDT001T02
*===================================================================================================
FORM f_fill_06_datos_centros  TABLES    lit_plantdata            STRUCTURE  bapie1marcrt
                                        lit_plantdatax           STRUCTURE  bapie1marcrtx
                                        lit_storagelocationdata  STRUCTURE  bapie1mardrt
                                        lit_storagelocationdatax STRUCTURE bapie1mardrtx.


* 1.- Lógica
*===================================================================================================
  LOOP AT git_centros_alta.
    CLEAR: lit_plantdata,
           lit_plantdatax.


    lit_plantdata-function   = '009'.
    lit_plantdatax-function = '009'.

    lit_plantdata-material   = git_file1-matnr.
    lit_plantdatax-material  = git_file1-matnr.

    lit_plantdata-plant      = git_centros_alta-valor.
    lit_plantdatax-plant     = git_centros_alta-valor.


    IF git_file1-dismm IS NOT INITIAL.
      lit_plantdata-mrp_type    = git_file1-dismm.                                                  "Característica de planificación de necesidades
      lit_plantdatax-mrp_type   = 'X'.
    ENDIF.

    IF ( git_file1-mtart = 'ZMER' OR git_file1-mtart = 'ZKIT' ) AND
       ( git_file1-mtpos = 'NORM' OR git_file1-mtpos = 'LINB' OR git_file1-mtpos = 'BANS') AND
       git_centros_alta-valor(1) = 'C'.
*     Si estamos creando un centro modelo y el tipo de articulo ZMER/ZKIT y tipo de posicion NORM, LINB o BANS
*     buscamos parametrización asociada
      READ TABLE git_val_def_int WITH KEY valor = 'AVAILCHECK_CDMO_NORM'.

      IF sy-subrc = 0 AND
         git_val_def_int-valor2 IS NOT INITIAL.
*       Si existe parametrización con valor, se la asignamos
        lit_plantdata-availcheck  = git_val_def_int-valor2.                                         "Grupo de verificación p.verificación de disponibilidad
        lit_plantdatax-availcheck = 'X'.
      ENDIF.
    ELSE.
*     Sino, determinamos parametrización asociada en base a si lo que estamos creando es un centro o
*     tienda modelo
      IF git_centros_alta-valor(1) = 'C'.
*       Si estamos creando un centro modelo
        READ TABLE git_val_def_int WITH KEY valor = 'AVAILCHECK_CDMO'.

        IF sy-subrc = 0 AND
           git_val_def_int-valor2 IS NOT INITIAL.
          lit_plantdata-availcheck  = git_val_def_int-valor2.                                       "Grupo de verificación p.verificación de disponibilidad
          lit_plantdatax-availcheck = 'X'.
        ENDIF.
      ELSEIF git_centros_alta-valor(1) = 'T'.
*       Si estamos creando una tienda modelo
        READ TABLE git_val_def_int WITH KEY valor = 'AVAILCHECK_TDMO'.

        IF sy-subrc = 0 AND
           git_val_def_int-valor2 IS NOT INITIAL.
          lit_plantdata-availcheck  = git_val_def_int-valor2.                                       "Grupo de verificación p.verificación de disponibilidad
          lit_plantdatax-availcheck = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF git_file1-ladgr IS NOT INITIAL.
      lit_plantdata-loadinggrp  = git_file1-ladgr.                                                  "Grupo de carga
      lit_plantdatax-loadinggrp = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'NEG_STOCKS'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-neg_stocks  = git_val_def_int-valor2.                                           "Permitir Stocks negativos
      lit_plantdatax-neg_stocks = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'MRPPROFILE'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-mrpprofile  = git_val_def_int-valor2.                                           "Artículo: Perfil de planificación de necesidades
      lit_plantdatax-mrpprofile = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'PROC_TYPE'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-proc_type = git_val_def_int-valor2.                                             "Clase de aprovisionamiento
      lit_plantdatax-proc_type = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'REPLENTIME'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-replentime = git_val_def_int-valor2.                                            "Tiempo global de reaprovisionamiento (días laborables)
      lit_plantdatax-replentime = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'AUTO_P_ORD'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      IF ( git_centros_alta-valor = 'CDMO' or git_centros_alta-valor = 'TDMC' ) AND ( git_file1-mtart = 'ZMER' or git_file1-mtart = 'ZKIT' )."FRAMOS - EVO515 - 26102022 - Ajustamos para añadir el pincho a los ZKIT
        lit_plantdata-auto_p_ord = git_val_def_int-valor2.                                            "Indicador: pedido automático permitido
        lit_plantdatax-auto_p_ord = 'X'.
      ENDIF.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'PERIOD_IND'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-period_ind = git_val_def_int-valor2.                                            "Indicador de período
      lit_plantdatax-period_ind = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'MRP_CTRLER'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-mrp_ctrler = git_val_def_int-valor2.                                            "Planificador de necesidades temporal
      lit_plantdatax-mrp_ctrler = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'LOTSIZEKEY'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-lotsizekey = git_val_def_int-valor2.                                            "Tamaño de lote de planificación de necesidades
      lit_plantdatax-lotsizekey = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'REORDER_PT'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-reorder_pt = git_val_def_int-valor2.                                            "Punto de pedido
      lit_plantdatax-reorder_pt = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'PLND_DELRY'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-plnd_delry = git_val_def_int-valor2.                                            "Plazo de entrega previsto
      lit_plantdatax-plnd_delry = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'SUP_SOURCE'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-sup_source = git_val_def_int-valor2.                                            "Fuente de aprovisionamiento
      lit_plantdatax-sup_source = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'PUR_GROUP'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-pur_group = git_val_def_int-valor2.                                             "Grupo de compras
      lit_plantdatax-pur_group = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'SLOC_EXPRC'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-sloc_exprc = git_val_def_int-valor2.                                            "Almacén
      lit_plantdatax-sloc_exprc = 'X'.
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'DISTR_PROF'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_plantdata-distr_prof = git_val_def_int-valor2.                                            "Perfil de distribución de artículo en centro
      lit_plantdatax-distr_prof = 'X'.
    ENDIF.

    IF git_file1-loggr IS NOT INITIAL.                                                              "Valor etiquetado energético
      lit_plantdata-handlg_grp = git_file1-loggr.
      lit_plantdatax-handlg_grp = 'X'.
    ENDIF.

    IF git_centros_alta-valor = 'CDMO' AND                                                          "Valor de redondeo de la cantidad a pedir
       git_file1-bstrf IS NOT INITIAL.
      lit_plantdata-round_val = git_file1-bstrf.
      lit_plantdatax-round_val = 'X'.
    ENDIF.

    IF git_centros_alta-valor = 'CDMO' AND                                                          "Stock de seguridad
       git_file1-eisbe IS NOT INITIAL.
      lit_plantdata-safety_stk = git_file1-eisbe.
      lit_plantdatax-safety_stk = 'X'.
    ENDIF.

    IF git_file1-mfrgr IS NOT INITIAL.                                                              "Clasificación energética
      lit_plantdata-matfrgtgrp = git_file1-mfrgr.
      lit_plantdatax-matfrgtgrp = 'X'.
    ENDIF.

    APPEND: lit_plantdata,
            lit_plantdatax.

    LOOP AT git_almacenes_alta WHERE valor = git_centros_alta-valor.
      CLEAR: lit_storagelocationdata,
             lit_storagelocationdatax.

      lit_storagelocationdata-function  = '009'.
      lit_storagelocationdatax-function = '009'.

      lit_storagelocationdata-material  = git_file1-matnr.
      lit_storagelocationdatax-material = git_file1-matnr.

      lit_storagelocationdata-plant     = git_almacenes_alta-valor.
      lit_storagelocationdatax-plant    = git_almacenes_alta-valor.

      lit_storagelocationdata-stge_loc  = git_almacenes_alta-valor2.
      lit_storagelocationdatax-stge_loc = git_almacenes_alta-valor2.

      APPEND: lit_storagelocationdata,
              lit_storagelocationdatax.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_CENTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_PLANTDATA  text
*      -->P_LIT_PLANTDATAX  text
*      -->P_LIT_STORAGELOCATIONDATA  text
*      -->P_LIT_STORAGELOCATIONDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_datos_centros_lite  TABLES   lit_plantdata            STRUCTURE  bapie1marcrt
                                         lit_plantdatax           STRUCTURE  bapie1marcrtx
                                         lit_storagelocationdata  STRUCTURE  bapie1mardrt
                                         lit_storagelocationdatax STRUCTURE bapie1mardrtx.

  LOOP AT git_centros_alta.
    CLEAR lit_plantdata.
    lit_plantdata-function   = '009'.
    lit_plantdata-material   = git_lite-matnr.
    lit_plantdata-plant      = git_centros_alta-valor.
    lit_plantdata-mrp_type   = 'ND'.                                       "Característica de planificación de necesidades
    lit_plantdata-availcheck = 'KP'.                                       "Grupo de verificación p.verificación de disponibilidad
*   lit_plantdata-proc_type  = 'F'.
*   lit_plantdata-MRPPROFILE = ps_listado-cdmo_dispr.
*   lit_plantdata-replentime = ps_listado-cdmo_wzeit.                      "Tiempo global de reaprovisionamiento (días laborables)
*   lit_plantdata-auto_p_ord = 'X'.                                        "Indicador: pedido automático permitido
*   lit_plantdata-period_ind = ps_listado-cdmo_perkz.                      "Indicador de período
*   lit_plantdata-mrp_ctrler = ps_listado-cdmo_dispo.                      "Planificador de necesidades temporal
*   lit_plantdata-lotsizekey = ps_listado-cdmo_disls.                      "Tamaño de lote de planificación de necesidades
*   lit_plantdata-reorder_pt = ps_listado-cdmo_minbe.                      "Punto de pedido
*   lit_plantdata-plnd_delry = ps_listado-cdmo_plifz.                      "Plazo de entrega previsto
*   lit_plantdata-sup_source = ps_listado-cdmo_bwscl.                      "Fuente de aprovisionamiento
*   lit_plantdata-pur_group  = gc_ekgrp.                                   "Grupo de compras
*   lit_plantdata-sloc_exprc = gc_cdmo_lgort.                              "Almacén
    LOOP AT git_centros_val_alta WHERE param = 'ALTA_CENTROS_CARGA_VAL_NEG_STOCKS'
                                   AND valor = git_centros_alta-valor.
      lit_plantdata-neg_stocks = git_centros_val_alta-valor2.            "Permitir Stocks negativos
    ENDLOOP.

*   lit_plantdata-distr_prof = '001'.
    APPEND lit_plantdata.

    CLEAR lit_plantdatax.
    lit_plantdatax-function = '009'.
    lit_plantdatax-material = git_lite-matnr.
    lit_plantdatax-plant    = git_centros_alta-valor.
    lit_plantdatax-mrp_type   = 'X'.
    lit_plantdatax-availcheck = 'X'.
*   lit_plantdatax-MRPPROFILE = 'X'.
*   lit_plantdatax-proc_type = 'X'.
*   lit_plantdatax-replentime = 'X'.
*   lit_plantdatax-auto_p_ord = 'X'.
*   lit_plantdatax-period_ind = 'X'.
*   lit_plantdatax-mrp_ctrler = 'X'.
*   lit_plantdatax-lotsizekey = 'X'.
*   lit_plantdatax-reorder_pt = 'X'.
*   lit_plantdatax-plnd_delry = 'X'.
*   lit_plantdatax-sup_source = 'X'.
*   lit_plantdatax-pur_group  = 'X'.
*   lit_plantdatax-sloc_exprc = 'X'.
*   lit_plantdatax-distr_prof = 'X'.
    LOOP AT git_centros_val_alta WHERE param = 'ALTA_CENTROS_CARGA_VAL_NEG_STOCKS'
                                   AND valor = git_centros_alta-valor.
      lit_plantdatax-neg_stocks = 'X'.
    ENDLOOP.
    APPEND lit_plantdatax.

    LOOP AT git_almacenes_alta WHERE valor = git_centros_alta-valor.
      CLEAR lit_storagelocationdata.
      lit_storagelocationdata-function = '009'.
      lit_storagelocationdata-material = git_lite-matnr.
      lit_storagelocationdata-plant    = git_almacenes_alta-valor.
      lit_storagelocationdata-stge_loc = git_almacenes_alta-valor2.
      APPEND lit_storagelocationdata.

      CLEAR lit_storagelocationdatax.
      lit_storagelocationdatax-function = '009'.
      lit_storagelocationdatax-material = git_lite-matnr.
      lit_storagelocationdatax-plant    = git_almacenes_alta-valor.
      lit_storagelocationdatax-stge_loc = git_almacenes_alta-valor2.
      APPEND lit_storagelocationdatax.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_CLASIFICACION_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_TAXCLASSIFICATIONS  text
*----------------------------------------------------------------------*
FORM f_fill_07_clasificacion_fiscal  TABLES   lit_taxclassifications STRUCTURE bapie1mlanrt .
*>Inicializar datos
  CLEAR lit_taxclassifications.

*>Registrar datos
  lit_taxclassifications-function    = '009'.
  lit_taxclassifications-material    = git_file1-matnr.                                             "Artículo

  READ TABLE git_val_def_int WITH KEY valor = 'DEPCOUNTRY'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_taxclassifications-depcountry  = git_val_def_int-valor2.                                    "País suministrador (país del cual se envía la mercancía)"
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'TAX_TYPE_1'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_taxclassifications-tax_type_1  = git_val_def_int-valor2.                                    "Tipo de impuesto (IVA, federal, etc.)
  ENDIF.

  IF git_file1-taklv IS NOT INITIAL.
    lit_taxclassifications-taxclass_1  = git_file1-taklv.                                           "Clasificación fiscal para el artículo
  ENDIF.

*>Añadir datos
  APPEND lit_taxclassifications.

*>APRADAS-31.03.2021 16:21:02-Inicio
* Si han informado indicador de impuestos 2 lo asignamos en la creación
  IF git_file1-taklv2 IS NOT INITIAL.

    CLEAR lit_taxclassifications.
    lit_taxclassifications-function    = '009'.
    lit_taxclassifications-material    = git_file1-matnr.                                           "Artículo

    READ TABLE git_val_def_int WITH KEY valor = 'DEPCOUNTRY_2'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_taxclassifications-depcountry  = git_val_def_int-valor2.                                  "País suministrador (país del cual se envía la mercancía)"
    ENDIF.

    READ TABLE git_val_def_int WITH KEY valor = 'TAX_TYPE_1_2'.
    IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
      lit_taxclassifications-tax_type_1  = git_val_def_int-valor2.                                  "Tipo de impuesto (IVA, federal, etc.)
    ENDIF.

    lit_taxclassifications-taxclass_1  = git_file1-taklv2.                                          "Clasificación fiscal para el artículo

    APPEND lit_taxclassifications.
  ENDIF.
*<APRADAS-31.03.2021 16:21:02-Fin
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_CLASIFICACION_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_TAXCLASSIFICATIONS  text
*----------------------------------------------------------------------*
FORM f_fill_clasif_fiscal_lite TABLES   lit_taxclassifications STRUCTURE bapie1mlanrt .
  CLEAR lit_taxclassifications.
  lit_taxclassifications-function    = '009'.
  lit_taxclassifications-material    = git_lite-matnr.
  lit_taxclassifications-depcountry  = 'ES'.
  lit_taxclassifications-tax_type_1  = 'MWST'.
  lit_taxclassifications-taxclass_1  = git_lite-taklv.
  APPEND lit_taxclassifications.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_ALMACEN_WM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_WAREHOUSENUMBERDATA  text
*----------------------------------------------------------------------*
FORM f_fill_08_datos_almacen_wm  TABLES   lit_warehousenumberdata STRUCTURE bapie1mlgnrt
                                          lit_warehousenumberdatax STRUCTURE bapie1mlgnrtx.

*>Si en la plantilla no se ha especificado número de almacén, no se carga nada
  IF git_file1-lgnum IS INITIAL.
    EXIT.
  ENDIF.

*>Inicializar datos
  CLEAR: lit_warehousenumberdata,
         lit_warehousenumberdatax.

*>Registrar datos
  lit_warehousenumberdata-function   = '009'.
  lit_warehousenumberdatax-function  = '009'.

  lit_warehousenumberdata-material   = git_file1-matnr.                                             "Artículo
  lit_warehousenumberdatax-material  = git_file1-matnr.

  lit_warehousenumberdata-whse_no    = git_file1-lgnum.                                             "Número de almacén
  lit_warehousenumberdatax-whse_no   = git_file1-lgnum.

  IF git_file1-ltkze IS NOT INITIAL.
    lit_warehousenumberdata-placement  = git_file1-ltkze.                                           "Indicador tipo de almacén para la entrada en stock
    lit_warehousenumberdatax-placement = 'X'.
  ENDIF.

  IF git_file1-ltkza IS NOT INITIAL.
    lit_warehousenumberdata-withdrawal = git_file1-ltkza.                                           "Indicador tipo de almacén para la salida de stock
    lit_warehousenumberdatax-withdrawal = 'X'.
  ENDIF.

*>Añadir datos
  APPEND: lit_warehousenumberdata,
          lit_warehousenumberdatax.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATOS_ALMACEN_WM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_WAREHOUSENUMBERDATA  text
*----------------------------------------------------------------------*
FORM f_fill_datos_almacen_wm_lite  TABLES   lit_warehousenumberdata STRUCTURE bapie1mlgnrt
                                            lit_warehousenumberdatax STRUCTURE bapie1mlgnrtx.
  CLEAR lit_warehousenumberdata.
  lit_warehousenumberdata-function   = '009'.
  lit_warehousenumberdata-material   = git_lite-matnr.
  lit_warehousenumberdata-whse_no    = gc_def_lgnum.
*  lit_warehousenumberdata-placement  = gc_def_ltkze.
*  lit_warehousenumberdata-withdrawal = gc_def_ltkza.
  APPEND lit_warehousenumberdata.

  CLEAR lit_warehousenumberdatax.
  lit_warehousenumberdatax-function   = '009'.
  lit_warehousenumberdatax-material   = git_lite-matnr.
  lit_warehousenumberdatax-whse_no    = gc_def_lgnum.
*  lit_warehousenumberdatax-placement  = 'X'.
*  lit_warehousenumberdatax-withdrawal = 'X'.
  APPEND lit_warehousenumberdatax.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_CMATN1_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_FILE3_MATNR  text
*      <--P_GIT_FILE3_MATNR  text
*----------------------------------------------------------------------*
FORM f_cmatn1_input  USING    pe_matnr
                     CHANGING ps_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = pe_matnr
    IMPORTING
      output       = ps_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0. ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_APLIACION_DATOS_BASICOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_ADDNCLIENTDATA  text
*      -->P_LIT_ADDNCLIENTDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_02_amp_datos_basicos  TABLES   lit_addnclientdata STRUCTURE bapie1maw1rt
                                        lit_addnclientdatax STRUCTURE bapie1maw1rtx.

*>Inicializaciones
  CLEAR: lit_addnclientdata,
         lit_addnclientdatax.

*>Registrar datos
  lit_addnclientdata-function   = '009'.
  lit_addnclientdatax-function  = '009'.

  IF git_file1-matnr IS NOT INITIAL.
    lit_addnclientdata-material   = git_file1-matnr.                                                "Artículo
    lit_addnclientdatax-material  = git_file1-matnr.
  ENDIF.

  IF git_file1-ladgr IS NOT INITIAL.
    lit_addnclientdata-loadinggrp = git_file1-ladgr.                                                "Grupo de carga
    lit_addnclientdatax-loadinggrp  = 'X'.
  ENDIF.

  IF git_file1-ekgrp IS NOT INITIAL.
    lit_addnclientdata-pur_group  = git_file1-ekgrp.                                                "Grupo de compras
    lit_addnclientdatax-pur_group    = 'X'.
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'LI_PROC_ST'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_addnclientdata-li_proc_st   = git_val_def_int-valor2.                                       "Procedimiento catalogación p.tienda u otros tipos de surtido
    lit_addnclientdatax-li_proc_st  = 'X'. "APRADAS-19.04.2016
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'LI_PROC_DC'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_addnclientdata-li_proc_dc   = git_val_def_int-valor2.                                       "Procedimiento catalogación p.ce.distribución/tipos surtido
    lit_addnclientdatax-li_proc_dc  = 'X'. "APRADAS-19.04.2016
  ENDIF.

  READ TABLE git_val_def_int WITH KEY valor = 'REPL_LIST'.
  IF sy-subrc = 0 AND git_val_def_int-valor2 IS NOT INITIAL.
    lit_addnclientdata-repl_list = git_val_def_int-valor2.                                          "Clase de lista de surtido
    lit_addnclientdatax-repl_list = 'X'.
  ENDIF.

  lit_addnclientdata-list_st_fr  	= sy-datum.                                                       "Fecha hasta la que se ha efectuado listado en la filial
  lit_addnclientdatax-list_st_fr  = 'X'.

  lit_addnclientdata-list_st_to  	= '99991231'.                                                     "Fecha hasta la cual se lista en la filial
  lit_addnclientdatax-list_st_to  = 'X'.

  lit_addnclientdata-sell_st_fr   = sy-datum.                                                       "Fecha a partir de la que se efectúa listado en centro distr.
  lit_addnclientdatax-sell_st_fr  = 'X'.

  lit_addnclientdata-sell_st_to   = '99991231'.                                                     "Fecha hasta la que se visualiza lista alm.ctral./ctro.distr.
  lit_addnclientdatax-sell_st_to  = 'X'.

  IF git_file1-bklas IS NOT INITIAL.
    lit_addnclientdata-val_class  = git_file1-bklas.                                                "Categoría de valoración
    lit_addnclientdatax-val_class  = 'X'.
  ENDIF.

  IF git_file1-herkl IS NOT INITIAL.
    lit_addnclientdata-countryori = git_file1-herkl.                                                "País de origen del artículo (origen CCI)
    lit_addnclientdatax-countryori = 'X'.
  ENDIF.

  IF git_file1-herkr IS NOT INITIAL.
    lit_addnclientdata-regionorig = git_file1-herkr.                                                "Región de origen del artículo (origen Cámara de Comercio)
    lit_addnclientdatax-regionorig = 'X'.
  ENDIF.

  IF git_file1-stawn IS NOT INITIAL.
    lit_addnclientdata-comm_code  = git_file1-stawn.                                                "Número estadístico de mercancía
    lit_addnclientdatax-comm_code  = 'X'.
  ENDIF.

*>Insertar datos
  APPEND: lit_addnclientdata,
          lit_addnclientdatax.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILL_APLIACION_DATOS_BASICOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_ADDNCLIENTDATA  text
*      -->P_LIT_ADDNCLIENTDATAX  text
*----------------------------------------------------------------------*
FORM f_fill_ampliacion_datosb_lite  TABLES   lit_addnclientdata STRUCTURE bapie1maw1rt
                                              lit_addnclientdatax STRUCTURE bapie1maw1rtx.

  CLEAR lit_addnclientdata.
  lit_addnclientdata-function   = '009'.
  lit_addnclientdata-material   = git_lite-matnr.
  lit_addnclientdata-loadinggrp = '0001'.                                  "Grupo de carga
*  lit_addnclientdata-val_class  = gc_wbkla.
*  lit_addnclientdata-countryori = ps_listado-db_wherl.
*  lit_addnclientdata-regionorig = ps_listado-db_wherr.
*  lit_addnclientdata-comm_code  = ps_listado-db_wstaw.
  lit_addnclientdata-li_proc_st = '02'.                                    "Procedimiento catalogación p.tienda u otros tipos de surtido
  lit_addnclientdata-li_proc_dc = '02'.                                    "Procedimiento catalogación p.ce.distribución/tipos surtido
  lit_addnclientdata-list_st_fr	= sy-datum.
  lit_addnclientdata-list_st_to	= '99991231'.
  lit_addnclientdata-sell_st_fr = sy-datum.
  lit_addnclientdata-sell_st_to = '99991231'.
  APPEND lit_addnclientdata.

  CLEAR lit_addnclientdatax.
  lit_addnclientdatax-function    = '009'.
  lit_addnclientdatax-material    = git_lite-matnr.
  lit_addnclientdatax-loadinggrp  = 'X'.
*  lit_addnclientdatax-val_class  = 'X'.
*  lit_addnclientdatax-countryori = 'X'.
*  lit_addnclientdatax-regionorig = 'X'.
*  lit_addnclientdatax-comm_code  = 'X'.
  lit_addnclientdatax-li_proc_st  = 'X'. "APRADAS-19.04.2016
  lit_addnclientdatax-li_proc_dc  = 'X'. "APRADAS-19.04.2016
  lit_addnclientdatax-list_st_fr  = 'X'.
  lit_addnclientdatax-list_st_to  = 'X'.
  lit_addnclientdatax-sell_st_fr  = 'X'.
  lit_addnclientdatax-sell_st_to  = 'X'.
  APPEND lit_addnclientdatax.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_data .
* Fecha y hora para grabación en log
  gd_fecha = sy-datum.
  gd_hora  = sy-uzeit.

* Centros a dar de alta en carga full y lite
  SELECT *
    FROM zretpdt001t02
    INTO TABLE git_centros_alta
   WHERE param = 'ALTA_CENTROS_CARGA'.

* Valores fijos centros a dar de alta
  SELECT *
    FROM zretpdt001t02
    INTO TABLE git_centros_val_alta
   WHERE param LIKE 'ALTA_CENTROS_CARGA_VAL%'.

* Almacenes a dar de alta en cada una de las tiendas en carga full y lite
  SELECT *
    FROM zretpdt001t02
    INTO TABLE git_almacenes_alta
   WHERE param = 'ALTA_ALMACENES_CARGA'.

* Valores fijos de campos asignados internamente
  SELECT *
    FROM zretpdt001t02
    INTO TABLE git_val_def_int
   WHERE param = 'VALOR_DEFECTO_INTERNO'.

* Areas de venta en las que dar de alta el artículo
  SELECT *
    FROM zretpdt001t02
    INTO TABLE git_alta_areas_venta
   WHERE param = 'ALTA_AREAS_VENTA'.

* Valores por defecto excels
  SELECT *
    FROM zretpdt001t02
    INTO TABLE git_valores_def
   WHERE param = 'VALOR_DEFECTO'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_at_selection_screen_output .
  IF p_mon2 = 'X'.  "Log de procesos
    LOOP AT SCREEN.
      IF screen-name CS 'P_FILE' OR
         screen-name CS 'P_LITE' OR
         screen-name CS 'P_OPC'.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF p_mon3 = 'X'. "Carga reducida
    LOOP AT SCREEN.
      IF screen-name CS 'P_FILE' OR
         screen-name CS 'P_OPC' OR
         screen-name CS 'S_MATNR' OR
         screen-name CS 'S_USER' OR
         screen-name CS 'S_FECHA' OR
         screen-name CS 'S_HORA'.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name CS 'S_MATNR' OR
         screen-name CS 'S_USER' OR
         screen-name CS 'S_FECHA' OR
         screen-name CS 'P_LITE' OR
         screen-name CS 'S_HORA'.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name CS 'P_MON3'.
      screen-input = 0.
      screen-invisible = 1.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECCIONAR_DATOS_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleccionar_datos_log .
  SELECT *
    FROM zretpdt001t01
    INTO CORRESPONDING FIELDS OF TABLE git_listado_log
   WHERE matnr   IN s_matnr
     AND usuario IN s_user
     AND fecha   IN s_fecha
     AND hora    IN s_hora.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_DATOS_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_asignar_datos_f4 .
  git_listado-f4_vkorg = git_file4-vkorg.
  git_listado-f4_vtweg = git_file4-vtweg.
  git_listado-f4_meinh = git_file4-meinh.
  git_listado-f4_kwert = git_file4-kwert.
  git_listado-f4_konwa = git_file4-konwa.
  git_listado-f4_kpein = git_file4-kpein.
  git_listado-f4_kmein = git_file4-kmein.
  git_listado-f4_datab = git_file4-datab.
  git_listado-f4_datbi = git_file4-datbi.
ENDFORM.


*===================================================================================================
*&      Form  F_ALTA_PVPS
*===================================================================================================
* Rutina para grabar el PVP del artículo mediante un idoc COND_A
*===================================================================================================
FORM f_alta_pvps  CHANGING ps_error
                           ps_docnum.
* 0.- Declaración de variables
*===================================================================================================
  DATA: lit_idoc_containers LIKE edidd OCCURS 0 WITH HEADER LINE,
        lr_e1bp_wrpl_import TYPE e1bp_wrpl_import,
        lr_e1bpe1mathead    TYPE e1bpe1mathead,
        lr_e1bpe1marcrt     TYPE e1bpe1marcrt,
        lr_e1bpe1marcrtx    TYPE e1bpe1marcrtx,
        lr_idoc_control_new LIKE edidc,
        lr_idoc_control     LIKE edidc,
        lf_error            LIKE sy-subrc,
        ld_kunnr_tienda     TYPE kunnr,
        ld_segnum           TYPE idocdsgnum,
        ld_segnum_cab       TYPE idocdsgnum,
        ld_identifier       LIKE edidc-docnum,
        lr_e1komg           LIKE e1komg,
        lr_e1konh           LIKE e1konh,
        lr_e1konp           LIKE e1konp,
        ld_status           LIKE edidc-status,
        ld_kbetr            TYPE p DECIMALS 4,
        ld_kbrue            TYPE p DECIMALS 4.

* 1.- Logica
*===================================================================================================
*>Abrir Idoc
  PERFORM f_idoc_abrir USING    'ZRETFG006_IDOC_CONDA'  ''  CHANGING lr_idoc_control ld_identifier lf_error.

*>Segmento-E1KOMG
  lr_e1komg-kvewe       = 'A'.
  lr_e1komg-kotabnr     = '073'.
  lr_e1komg-kappl       = 'V'.
  lr_e1komg-kschl       = 'ZVKR'.
  CONCATENATE git_file4-vkorg git_file4-vtweg git_file4-matnr INTO lr_e1komg-vakey RESPECTING BLANKS.
  lr_e1komg-vkorg       = git_file4-vkorg.
  lr_e1komg-vtweg       = git_file4-vtweg.
  lr_e1komg-matnr       = git_file4-matnr.
  lr_e1komg-vrkme       = git_file4-meinh.
  PERFORM f_unit_of_measure_sap_to_iso USING lr_e1komg-vrkme CHANGING lr_e1komg-vrkme.
  lr_e1komg-evrtp = '00000'.
  lr_e1komg-posnr = '000000'.
  lr_e1komg-anzsn = '0000000000'.
  lr_e1komg-vakey_long = lr_e1komg-vakey.

  ADD 1 TO ld_segnum.
  lit_idoc_containers-segnam  = 'E1KOMG'.
  lit_idoc_containers-sdata   = lr_e1komg.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  APPEND lit_idoc_containers.

  ld_segnum_cab = ld_segnum.

*>Segmento-E1KONH
  lr_e1konh-datab = git_file4-datab.
  lr_e1konh-datbi = git_file4-datbi.

  ADD 1 TO ld_segnum.
  lit_idoc_containers-segnam  = 'E1KONH'.
  lit_idoc_containers-sdata   = lr_e1konh.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  lit_idoc_containers-psgnum  = ld_segnum_cab.
  APPEND lit_idoc_containers.
  ld_segnum_cab = ld_segnum.

*>Segmento-E1KONP
  lr_e1konp-kschl = 'VKP0'.
  lr_e1konp-stfkz = 'A'.
  lr_e1konp-krech = 'C'.
  lr_e1konp-kbetr = git_file4-kwert.
  lr_e1konp-konwa = git_file4-konwa.
  lr_e1konp-kpein = git_file4-kpein.
  lr_e1konp-kmein = git_file4-kmein.
  PERFORM f_unit_of_measure_sap_to_iso USING lr_e1konp-kmein CHANGING lr_e1konp-kmein.
  lr_e1konp-kwaeh = 'EUR'.
  lr_e1konp-zaehk_ind = '01'.

  ADD 1 TO ld_segnum.
  lit_idoc_containers-segnam  = 'E1KONP'.
  lit_idoc_containers-sdata   = lr_e1konp.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  lit_idoc_containers-psgnum  = ld_segnum_cab.
  APPEND lit_idoc_containers.


*>Añadir Segmentos
  PERFORM f_idoc_add_segmentos TABLES lit_idoc_containers USING  ld_identifier  CHANGING lf_error.

*>Cerrar Idoc
  PERFORM f_idoc_cerrar USING    ld_identifier  CHANGING lr_idoc_control_new  lf_error.

*>Cambiar status del idoc a 64
  PERFORM f_idoc_cambiar_status USING lr_idoc_control_new-docnum  '64' CHANGING lf_error.

*>Forzar Commit
  COMMIT WORK AND WAIT.

  CALL FUNCTION 'DEQUEUE_ALL'
*     EXPORTING
*       _SYNCHRON       = ' '
    .

  ps_docnum = lr_idoc_control_new-docnum.

  SUBMIT rbdapp01
    WITH docnum BETWEEN ps_docnum AND space
    WITH p_output = space
     AND RETURN.

  COMMIT WORK AND WAIT.

  CLEAR ld_status.
  SELECT SINGLE status
    FROM edidc
    INTO ld_status
   WHERE docnum = ps_docnum.

  IF ld_status <> '53'.
    ps_error = 'X'.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_ALTA_PVPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LF_ERROR  text
*      <--P_LD_DOCNUM  text
*----------------------------------------------------------------------*
FORM f_alta_aufs  CHANGING ps_error
                           ps_docnum.

* 0.- Declaración de variables
*=======================================================================
  DATA: lit_idoc_containers LIKE edidd OCCURS 0 WITH HEADER LINE,
        lr_e1bp_wrpl_import TYPE e1bp_wrpl_import,
        lr_e1bpe1mathead    TYPE e1bpe1mathead,
        lr_e1bpe1marcrt     TYPE e1bpe1marcrt,
        lr_e1bpe1marcrtx    TYPE e1bpe1marcrtx,
        lr_idoc_control_new LIKE edidc,
        lr_idoc_control     LIKE edidc,
        lf_error            LIKE sy-subrc,
        ld_kunnr_tienda     TYPE kunnr,
        ld_segnum           TYPE idocdsgnum,
        ld_segnum_cab       TYPE idocdsgnum,
        ld_identifier       LIKE edidc-docnum,
        lr_e1komg           LIKE e1komg,
        lr_e1konh           LIKE e1konh,
        lr_e1konp           LIKE e1konp,
        ld_status           LIKE edidc-status,
        ld_kbetr            TYPE p DECIMALS 4,
        ld_kbrue            TYPE p DECIMALS 4.


* 1.- Logica
*=======================================================================
* Abrimos IDOC
  PERFORM f_idoc_abrir USING    'ZRETFG006_IDOC_CONDA'
                                ''
                       CHANGING lr_idoc_control
                                ld_identifier
                                lf_error.

* E1KOMG
  lr_e1komg-kvewe       = 'A'.
  lr_e1komg-kotabnr     = '904'.
  lr_e1komg-kappl       = 'V'.
  lr_e1komg-kschl       = 'AUFS'.
  lr_e1komg-vakey       = git_lite-matnr.
  lr_e1komg-matnr       = git_lite-matnr.

  lr_e1komg-evrtp = '00000'.
  lr_e1komg-posnr = '000000'.
  lr_e1komg-anzsn = '0000000000'.
  lr_e1komg-vakey_long = lr_e1komg-vakey.

  ADD 1 TO ld_segnum.
  lit_idoc_containers-segnam  = 'E1KOMG'.
  lit_idoc_containers-sdata   = lr_e1komg.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  APPEND lit_idoc_containers.

  ld_segnum_cab = ld_segnum.

* Segmento E1KONH
  lr_e1konh-datab = sy-datlo.
  lr_e1konh-datbi = '99991231'.

  ADD 1 TO ld_segnum.
  lit_idoc_containers-segnam  = 'E1KONH'.
  lit_idoc_containers-sdata   = lr_e1konh.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  lit_idoc_containers-psgnum  = ld_segnum_cab.
  APPEND lit_idoc_containers.
  ld_segnum_cab = ld_segnum.

* Segmento E1KONP
  lr_e1konp-kschl = 'AUFS'.
  lr_e1konp-stfkz = 'A'.
  lr_e1konp-krech = 'A'.
  lr_e1konp-kbetr = git_lite-2aufs.
  lr_e1konp-konwa = '%'.
  lr_e1konp-kpein = 0.
  lr_e1konp-kmein = ''.
  lr_e1konp-kwaeh = ''.
  lr_e1konp-zaehk_ind = '01'.

  ADD 1 TO ld_segnum.
  lit_idoc_containers-segnam  = 'E1KONP'.
  lit_idoc_containers-sdata   = lr_e1konp.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  lit_idoc_containers-psgnum  = ld_segnum_cab.
  APPEND lit_idoc_containers.


* Añadimos segmentos
  PERFORM f_idoc_add_segmentos TABLES lit_idoc_containers
                               USING  ld_identifier
                               CHANGING lf_error.

* Cerramos IDOC
  PERFORM f_idoc_cerrar USING    ld_identifier
                        CHANGING lr_idoc_control_new
                                 lf_error.

* Cambiamos STATUS al idoc
  PERFORM f_idoc_cambiar_status USING lr_idoc_control_new-docnum
                                      '64'
                              CHANGING lf_error.

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'DEQUEUE_ALL'
*     EXPORTING
*       _SYNCHRON       = ' '
    .

  ps_docnum = lr_idoc_control_new-docnum.

  SUBMIT rbdapp01
    WITH docnum BETWEEN ps_docnum AND space
    WITH p_output = space
     AND RETURN.

  COMMIT WORK AND WAIT.

  CLEAR ld_status.
  SELECT SINGLE status
    FROM edidc
    INTO ld_status
   WHERE docnum = ps_docnum.

  IF ld_status <> '53'.
    ps_error = 'X'.
  ENDIF.
ENDFORM.


FORM f_idoc_abrir  USING pe_operacion
                         pe_tienda
                   CHANGING ps_lr_idoc_control STRUCTURE edidc
                            ps_identifier
                            ps_error.

* 0.- Declaracion de variables
*=======================================================================
  DATA: lit_hardcodes LIKE zhardcodes OCCURS 0 WITH HEADER LINE.

* 1.- Logica
*=======================================================================
* Inicializamos parámetros de salida
  CLEAR: ps_lr_idoc_control,
         ps_identifier,
         ps_error.

* Recuperamos los datos de configuración del IDOC a abrir
  SELECT *
    FROM zhardcodes
    INTO TABLE lit_hardcodes
   WHERE programa = pe_operacion.

  LOOP AT lit_hardcodes.
    CASE lit_hardcodes-param.
      WHEN 'DOCREL'.
        ps_lr_idoc_control-docrel = lit_hardcodes-valor.
      WHEN 'DIRECT'.
        ps_lr_idoc_control-direct = lit_hardcodes-valor.
      WHEN 'RCVPOR'.
        ps_lr_idoc_control-rcvpor = lit_hardcodes-valor.
      WHEN 'RCVPRT'.
        ps_lr_idoc_control-rcvprt = lit_hardcodes-valor.
      WHEN 'RCVPRN'.
        ps_lr_idoc_control-rcvprn = lit_hardcodes-valor.
      WHEN 'STDVRS'.
        ps_lr_idoc_control-stdvrs = lit_hardcodes-valor.
      WHEN 'STDMES'.
        ps_lr_idoc_control-stdmes = lit_hardcodes-valor.
      WHEN 'TEST'.
        ps_lr_idoc_control-test   = lit_hardcodes-valor.
      WHEN 'SNDPOR'.
        ps_lr_idoc_control-sndpor = lit_hardcodes-valor.
      WHEN 'SNDPRT'.
        ps_lr_idoc_control-sndprt = lit_hardcodes-valor.
      WHEN 'SNDPRN'.
        IF lit_hardcodes-valor = '<<TIENDA>>'.
          ps_lr_idoc_control-sndprn = pe_tienda.
        ELSE.
          ps_lr_idoc_control-sndprn = lit_hardcodes-valor.
        ENDIF.
      WHEN 'MESTYP'.
        ps_lr_idoc_control-mestyp = lit_hardcodes-valor.
      WHEN 'IDOCTP'.
        ps_lr_idoc_control-idoctp = lit_hardcodes-valor.
      WHEN 'SNDLAD'.
        ps_lr_idoc_control-sndlad = lit_hardcodes-valor.
      WHEN 'MESCOD'.
        ps_lr_idoc_control-mescod = lit_hardcodes-valor.
      WHEN 'MESFCT'.
        ps_lr_idoc_control-mesfct = lit_hardcodes-valor.
    ENDCASE.
  ENDLOOP.

  CONCATENATE sy-datum
              sy-uzeit
         INTO ps_lr_idoc_control-serial.

* Abrir creacion del IDOC
  CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_CREATE'
    EXPORTING
      idoc_control         = ps_lr_idoc_control
*     PI_RFC_MULTI_CP      = '    '
    IMPORTING
      identifier           = ps_identifier
    EXCEPTIONS
      other_fields_invalid = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    ps_error = sy-subrc.
  ENDIF.


ENDFORM.                    " F_IDOC_ABRIR

*&---------------------------------------------------------------------*
*&      Form  f_idoc_add_segmentos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PE_IT_IDOC_CONTAINERS  text
*      -->PE_IDENTIFIER          text
*      -->PS_ERROR               text
*----------------------------------------------------------------------*
FORM f_idoc_add_segmentos TABLES   pe_it_idoc_containers STRUCTURE edidd
                          USING    pe_identifier
                          CHANGING ps_error.

  CLEAR ps_error.

  CALL FUNCTION 'EDI_SEGMENTS_ADD_BLOCK'
    EXPORTING
      identifier                    = pe_identifier
    TABLES
      idoc_containers               = pe_it_idoc_containers
    EXCEPTIONS
      identifier_invalid            = 1
      idoc_containers_empty         = 2
      parameter_error               = 3
      segment_number_not_sequential = 4
      OTHERS                        = 5.

  IF sy-subrc <> 0.
    ps_error = sy-subrc.
  ENDIF.

ENDFORM.                    "f_add_segmentos

*&---------------------------------------------------------------------*
*&      Form  f_idoc_cerrar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PE_IDENTIFIER        text
*      -->PS_IDOC_CONTROL_NEW  text
*      -->PS_ERROR             text
*----------------------------------------------------------------------*
FORM f_idoc_cerrar USING    pe_identifier
                   CHANGING ps_idoc_control_new LIKE edidc
                            ps_error.

  CLEAR ps_error.

  CALL FUNCTION 'EDI_DOCUMENT_CLOSE_CREATE'
    EXPORTING
      identifier          = pe_identifier
*     NO_DEQUEUE          = ' '
*     SYN_ACTIVE          = ' '
    IMPORTING
      idoc_control        = ps_idoc_control_new
*     SYNTAX_RETURN       =
    EXCEPTIONS
      document_not_open   = 1
      document_no_key     = 2
      failure_in_db_write = 3
      parameter_error     = 4
      OTHERS              = 5.
  IF sy-subrc <> 0.
    ps_error = sy-subrc.
  ENDIF.

ENDFORM.                    " F_IDOC_CERRAR


*&---------------------------------------------------------------------*
*&      Form  f_idoc_cambiar_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PE_NUMIDOC text
*      -->PE_STATUS  text
*      -->PS_ERROR   text
*----------------------------------------------------------------------*
FORM f_idoc_cambiar_status  USING    pe_numidoc
                                     pe_status
                            CHANGING ps_error.
* 0.- Declaracion de variables
*=======================================================================
  DATA: lt_edids TYPE TABLE OF bdidocstat WITH HEADER LINE.

* 1.- Logica
*=======================================================================
  CLEAR ps_error.

  CLEAR lt_edids.
  lt_edids-docnum   = pe_numidoc.
  lt_edids-status   = pe_status.
  lt_edids-uname    = sy-uname.
  lt_edids-repid    = sy-repid.
  lt_edids-tid      = sy-tcode.
  APPEND lt_edids.

  CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
    EXPORTING
      idoc_number               = pe_numidoc
    TABLES
      idoc_status               = lt_edids[]
    EXCEPTIONS
      idoc_foreign_lock         = 1
      idoc_not_found            = 2
      idoc_status_records_empty = 3
      idoc_status_invalid       = 4
      db_error                  = 5
      OTHERS                    = 6.

  IF sy-subrc <> 0.
    ps_error = sy-subrc.
  ENDIF.

ENDFORM.                    " F_CAMBIAR_STATUS_IDOC

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_command_0100 .
  DATA: ld_okcode LIKE sy-ucomm.

  ld_okcode = gd_okcode_0100.

  CLEAR: gd_okcode_0100,
         sy-ucomm.

  CASE ld_okcode.
    WHEN 'ACEPTAR'.
      PERFORM f_free_alv USING gr_grid_01 gr_container_01.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_9106_free_alv_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_free_alv USING pe_alv        TYPE REF TO cl_gui_alv_grid
                      pe_container  TYPE REF TO cl_gui_custom_container.

  IF pe_alv IS NOT INITIAL.
    CALL METHOD pe_alv->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    FREE pe_alv.
  ENDIF.


  IF pe_container IS NOT INITIAL.

    CALL METHOD pe_container->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    FREE pe_container.
  ENDIF.

ENDFORM.                    "f_9000_free_files_detail



*&---------------------------------------------------------------------*
*&      Form  F_GET_MATNR_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GIT_LITE_MATNR  text
*----------------------------------------------------------------------*
FORM f_get_matnr_range  CHANGING ps_matnr.
* 0.- Declaración de variables
*==========================================================================

* 1.- Lógica
*==========================================================================
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '02'
      object                  = 'MATERIALNR'
*     QUANTITY                = '1'
*     SUBOBJECT               = ' '
*     TOYEAR                  = '0000'
*     IGNORE_BUFFER           = ' '
    IMPORTING
      number                  = ps_matnr
*     QUANTITY                =
*     RETURNCODE              =
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_REGISTRAR_PASO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8070   text
*      -->P_GIT_LITE_MATNR  text
*      -->P_GC_MINISEMAFORO_ROJO  text
*      -->P_1      text
*      -->P_8074   text
*----------------------------------------------------------------------*
FORM f_registrar_paso  USING    pe_paso
                                pe_matnr
                                pe_pasos
                                pe_pason
                                pe_pasot
                                pe_info.

  DATA: wa_zretpdt001t01 LIKE zretpdt001t01.

  wa_zretpdt001t01-matnr    = pe_matnr.
  wa_zretpdt001t01-usuario  = sy-uname.
  wa_zretpdt001t01-fecha    = gd_fecha.
  wa_zretpdt001t01-hora     = gd_hora.
  wa_zretpdt001t01-paso     = pe_paso.
  wa_zretpdt001t01-pasos    = pe_pasos.
  wa_zretpdt001t01-pason    = pe_pason.
  wa_zretpdt001t01-pasot    = pe_pasot.
  wa_zretpdt001t01-info     = pe_info.
  INSERT zretpdt001t01 FROM wa_zretpdt001t01.

  COMMIT WORK AND WAIT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ACT_CAMPOS_ZZ_LITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_act_campos_zz_lite .

  UPDATE mara
     SET zzmodelo = git_lite-zzmodelo
         zzanexo1 = git_lite-zzanexo1
         zzanexo2 = git_lite-zzanexo2
         zzgrad   = git_lite-zzgrad
         zzdepbal = git_lite-zzdepbal
         zzgrubal = git_lite-zzgrubal
         zzplubal = git_lite-zzplubal
         zzcadbal = git_lite-zzcadbal
         zzmodprec = git_lite-zzmodprec
         zzimping  = git_lite-zzimping
         zzdescp   = git_lite-zzdescp
   WHERE matnr = git_lite-matnr.

  COMMIT WORK AND WAIT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZAR_TEXTOS_UM_LITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_actualizar_textos_um_lite .
  DATA: lit_zretpdtt02 LIKE zretpdtt02 OCCURS 0 WITH HEADER LINE.

* Grabamos textos y codigos de formato del articulo
  REFRESH lit_zretpdtt02.

  CLEAR lit_zretpdtt02.
  lit_zretpdtt02-matnr         = git_lite-matnr.
  lit_zretpdtt02-meinh         = git_lite-meins.
  lit_zretpdtt02-maktm_ean     = git_lite-maktm_ean.
  lit_zretpdtt02-maktm_gondola = git_lite-maktm_gondola.
  lit_zretpdtt02-codformato    = git_lite-zzcodformato.
  APPEND lit_zretpdtt02.

  IF git_lite-2ean11 IS NOT INITIAL.
    CLEAR lit_zretpdtt02.
    lit_zretpdtt02-matnr         = git_lite-matnr.
    lit_zretpdtt02-meinh         = git_lite-2meinh.
    lit_zretpdtt02-maktm_ean     = git_lite-2maktm_ean.
    lit_zretpdtt02-maktm_gondola = git_lite-2maktm_gondola.
    lit_zretpdtt02-codformato    = git_lite-2zzcodformato.
    APPEND lit_zretpdtt02.
  ENDIF.

  IF git_lite-3ean11 IS NOT INITIAL.
    CLEAR lit_zretpdtt02.
    lit_zretpdtt02-matnr         = git_lite-matnr.
    lit_zretpdtt02-meinh         = git_lite-3meinh.
    lit_zretpdtt02-maktm_ean     = git_lite-3maktm_ean.
    lit_zretpdtt02-maktm_gondola = git_lite-3maktm_gondola.
    lit_zretpdtt02-codformato    = git_lite-3zzcodformato.
    APPEND lit_zretpdtt02.
  ENDIF.

  MODIFY zretpdtt02 FROM TABLE lit_zretpdtt02.

  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ACT_CAMPOS_ZZ_FULL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_act_campos_zz_full .
  LOOP AT git_listado WHERE matnr = git_file1-matnr.
    EXIT.
  ENDLOOP.

*  update mara
*     set zzmodelo = git_file1-zzmodelo
*         zzanexo1 = git_file1-zzanexo1
*         zzanexo2 = git_file1-zzanexo2
*         zzgrad   = git_file1-zzgrad
*         zzdescp  = git_listado-f1_zzdescp
*   where matnr = git_file1-matnr.

  COMMIT WORK AND WAIT.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_REGISTRAR_RESUMEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_FILE1_MATNR  text
*      -->P_GIT_FILE1_MAKTX  text
*      -->P_GC_MINISEMAFORO_ROJO  text
*      -->P_6791   text
*      -->P_6792   text
*----------------------------------------------------------------------*
FORM f_registrar_resumen  USING    pe_matnr
                                   pe_maktx
                                   pe_status
                                   pe_paso
                                   pe_pasot.

  CLEAR gr_log_carga.
  gr_log_carga-matnr   = pe_matnr.
  gr_log_carga-matnrt  = pe_maktx.
  gr_log_carga-status  = pe_status.
  gr_log_carga-paso    = pe_paso.
  gr_log_carga-pasot   = pe_pasot.
  APPEND gr_log_carga TO git_log_carga.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CATALOGAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_MENSAJES
*&      --> GIT_FILE1_MATNR
*&      <-- LD_CATALOGAR_OK
*&      <-- LD_DOCNUM
*&---------------------------------------------------------------------*
FORM f_catalogar  TABLES   it_mensajes STRUCTURE bdcmsgcoll
                  USING    pe_matnr
                  CHANGING ps_catalogar_ok
                           ps_docnum.

  REFRESH it_mensajes.
  CLEAR: ps_catalogar_ok,
         ps_docnum.

  TRANSLATE pe_matnr TO UPPER CASE.

  CALL FUNCTION 'Z_CATALOGAR_SINGLE'
    EXPORTING
      pe_matnr        = pe_matnr
      pe_datab        = sy-datlo
      pe_datbi        = '99991231'
      pe_catsurt      = 'Z'
    IMPORTING
      ps_catalogar_ok = ps_catalogar_ok
      ps_docnum       = ps_docnum
    TABLES
      it_return       = it_mensajes.
*      it_tiendas      = .

  ps_catalogar_ok = 'X'.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICAR_EAN_EXISTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_LITE_EAN11  text
*      <--P_LF_EAN_EXISTE  text
*----------------------------------------------------------------------*
FORM f_verificar_ean_existe  USING    pe_ean11
                             CHANGING ps_ean_existe.

* 0.- Declaración de variables
*==========================================================================
  DATA: ld_ean11 LIKE mean-ean11.

* 1.- Lógica
*==========================================================================
* Inicializaciones
  CLEAR ps_ean_existe.

  IF pe_ean11(1) = '0'.
    ld_ean11 = pe_ean11.

    DO.
      SELECT SINGLE ean11
        FROM mean
        INTO ld_ean11
       WHERE ean11 = ld_ean11.

      IF sy-subrc = 0.
        ps_ean_existe = 'X'.

        EXIT.
      ELSE.
        IF ld_ean11(1) <> '0'.
          EXIT.
        ENDIF.
        ld_ean11 = ld_ean11+1.
      ENDIF.
    ENDDO.
  ELSE.
    SELECT SINGLE ean11
      FROM mean
      INTO pe_ean11
     WHERE ean11 = pe_ean11.

    IF sy-subrc = 0.
      ps_ean_existe = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CONFECCIONAR_LISTADO_FULL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_confeccionar_listado_full TABLES lit_matnr STRUCTURE zretpdt001s11.
*>Ordenar contenido importado de los ficheros
  SORT: git_file1 BY matnr ASCENDING,
        git_file2 BY matnr ASCENDING meinh ASCENDING,
        git_file3 BY matnr ASCENDING lifnr ASCENDING,
        git_file4 BY matnr ASCENDING meinh ASCENDING.

*>Quedarnos con los distintos articulos contenidos en los 4 excels
  LOOP AT git_file1.
    lit_matnr-matnr = git_file1-matnr.
    APPEND lit_matnr.
  ENDLOOP.

  LOOP AT git_file2.
    lit_matnr-matnr = git_file2-matnr.
    APPEND lit_matnr.
  ENDLOOP.

  LOOP AT git_file3.
    lit_matnr-matnr = git_file3-matnr.
    APPEND lit_matnr.
  ENDLOOP.

  LOOP AT git_file4.
    lit_matnr-matnr = git_file4-matnr.
    APPEND lit_matnr.
  ENDLOOP.

  SORT lit_matnr.
  DELETE ADJACENT DUPLICATES FROM lit_matnr.

* Registrar cada artículo en el monitor
  loop_at lit_matnr.
*   Para cada articulo registrado en los excels...
  CLEAR git_listado.
  git_listado-matnr = lit_matnr-matnr.

*   Miramos si el articulo está en el excel de datos basicos
  LOOP AT git_file1 WHERE matnr = lit_matnr-matnr.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    git_listado-matnrt = git_file1-maktx.
    git_listado-status_1 = gc_minisemaforo_verde.
  ELSE.
    git_listado-status_1  = gc_minisemaforo_ambar.
  ENDIF.

*   Miramos si el articulo está en el excel de UM/EAN's
  LOOP AT git_file2 WHERE matnr = lit_matnr-matnr.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    git_listado-matnrt = git_file1-maktx.
    git_listado-status_2 = gc_minisemaforo_verde.
  ELSE.
    git_listado-status_2  = gc_minisemaforo_ambar.
  ENDIF.

*   Miramos si el articulo está en el excel de registro info
  LOOP AT git_file3 WHERE matnr = lit_matnr-matnr.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    git_listado-matnrt = git_file1-maktx.
    git_listado-status_3 = gc_minisemaforo_verde.
  ELSE.
    git_listado-status_3  = gc_minisemaforo_ambar.
  ENDIF.

*   Miramos si el artículo está en el excel de precios de venta
  LOOP AT git_file4 WHERE matnr = lit_matnr-matnr.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    git_listado-matnrt = git_file1-maktx.
    git_listado-status_4 = gc_minisemaforo_verde.
  ELSE.
    git_listado-status_4  = gc_minisemaforo_ambar.
  ENDIF.

  APPEND git_listado.
  endloop_at 'Confeccionando monitor' '' '' '' ''.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_VALIDACIONES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_VALIDACIONES
*&---------------------------------------------------------------------*
FORM f_get_validaciones  TABLES   lit_validaciones STRUCTURE zretpdt001t04.

  SELECT *
    FROM zretpdt001t04
    INTO TABLE lit_validaciones.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_VALOR_DEF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_VALORES_DEF
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_get_valor_def  TABLES   lit_valores_def STRUCTURE zretpdt001t02
                       USING   pe_campo
                    CHANGING   ps_valor_def.

  CLEAR ps_valor_def.

  LOOP AT lit_valores_def WHERE valor = pe_campo.
    ps_valor_def = lit_valores_def-valor2.

    EXIT.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CARGAR_FICHEROS_FULL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_VALORES_DEF
*&      --> LIT_MATNR_ERROR_UM
*&---------------------------------------------------------------------*
FORM f_cargar_ficheros_full  TABLES   lit_valores_def STRUCTURE zretpdt001t02
                                      lit_matnr_error_um STRUCTURE zretpdt001s11
                            CHANGING  ps_error.
* 0.- Declaración de variables
*===================================================================================================
  DATA: ld_mensaje  TYPE string,
        lf_error(1),
        lit_raw     TYPE truxs_t_text_data,
        lit_file3   LIKE zretpdt001s03 OCCURS 0 WITH HEADER LINE.

* 1.- Lógica
*===================================================================================================
  IF p_file1 IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = lit_raw
        i_filename           = p_file1
      TABLES
        i_tab_converted_data = git_file1
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO ld_mensaje.

      MESSAGE ld_mensaje TYPE 'I'.

      ps_error = 'X'.

      EXIT.
    ENDIF.

    LOOP AT git_file1.
*     Convertir codigo de artículo a formato interno
      PERFORM f_cmatn1_input USING git_file1-matnr CHANGING git_file1-matnr.

*     Convertir unidad medida base a formato interno
      PERFORM f_cunit_input  USING git_file1-meins CHANGING git_file1-meins lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file1-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Convertir UM contenido a formato interno
      PERFORM f_cunit_input  USING git_file1-inhme CHANGING git_file1-inhme lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file1-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Convertir Unidad de peso a formato interno
      PERFORM f_cunit_input  USING git_file1-gewei CHANGING git_file1-gewei lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file1-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Obtener valores por defecto
      IF git_file1-lgnum IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'LGNUM' CHANGING git_file1-lgnum.
      ENDIF.

      IF git_file1-ltkze IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'LTKZE' CHANGING git_file1-ltkze.
      ENDIF.

      IF git_file1-ltkza IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'LTKZA' CHANGING git_file1-ltkza.
      ENDIF.

      IF git_file1-ladgr IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'LADGR' CHANGING git_file1-ladgr.
      ENDIF.

      MODIFY git_file1.
    ENDLOOP.
  ENDIF.

  IF p_file2 IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = lit_raw
        i_filename           = p_file2
      TABLES
        i_tab_converted_data = git_file2
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO ld_mensaje.

      MESSAGE ld_mensaje TYPE 'I'.

      ps_error = 'X'.

      EXIT.
    ENDIF.

    LOOP AT git_file2.
*     Convertir código de artículo a formato interno
      PERFORM f_cmatn1_input USING git_file2-matnr CHANGING git_file2-matnr.

*     Convertir Unidad del EAN a formato interno
      PERFORM f_cunit_input USING git_file2-meinh CHANGING git_file2-meinh lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file2-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Convertir unidad de peso a formato interno
      PERFORM f_cunit_input USING git_file2-gewei CHANGING git_file2-gewei lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file2-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Convertir unidad de volumen a formato interno
      PERFORM f_cunit_input USING git_file2-voleh CHANGING git_file2-voleh lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file2-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Convertir unidad de longitud a formato interno
      PERFORM f_cunit_input USING git_file2-meabm CHANGING git_file2-meabm lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file2-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

      MODIFY git_file2.
    ENDLOOP.
  ENDIF.

*>Cargar excel con datos de registro info
  IF p_file3 IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = lit_raw
        i_filename           = p_file3
      TABLES
        i_tab_converted_data = lit_file3
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO ld_mensaje.

      MESSAGE ld_mensaje TYPE 'I'.

      ps_error = 'X'.

      EXIT.
    ENDIF.

    LOOP AT lit_file3.
      MOVE-CORRESPONDING lit_file3 TO git_file3.

      PERFORM f_cmatn1_input USING git_file3-matnr CHANGING git_file3-matnr.

      PERFORM f_cunit_input  USING git_file3-kmein CHANGING git_file3-kmein lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file3-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

      PERFORM f_cunit_input  USING git_file3-kmein_pb30 CHANGING git_file3-kmein_pb30 lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file3-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '.' IN lit_file3-netpr2 WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN lit_file3-netpr2 WITH '.'.

      IF git_file3-konwa = 'EUR4' OR
         git_file3-konwa = 'USD4'.
        git_file3-netpr = lit_file3-netpr2 * 100.
      ELSE.
        git_file3-netpr = lit_file3-netpr2.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '.' IN lit_file3-netpr2_pb30 WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN lit_file3-netpr2_pb30 WITH '.'.

      IF git_file3-konwa_pb30 = 'EUR4' OR
         git_file3-konwa_pb30 = 'USD4'.
        git_file3-netpr_pb30 = lit_file3-netpr2_pb30 * 100.
      ELSE.
        git_file3-netpr_pb30 = lit_file3-netpr2_pb30.
      ENDIF.


*     Obtener campos por defecto
      IF git_file3-bstae IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'BSTAE' CHANGING git_file3-bstae.
      ENDIF.

      IF git_file3-datbi IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'DATBI_REGINFO' CHANGING git_file3-datbi.
      ENDIF.

      IF git_file3-datbi_pb30 IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'DATBI_REGINFO' CHANGING git_file3-datbi_pb30.
      ENDIF.

      IF git_file3-esokz IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'ESOKZ' CHANGING git_file3-esokz.
      ENDIF.

      IF git_file3-konwa IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KONWA' CHANGING git_file3-konwa.
      ENDIF.

      IF git_file3-kmein IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KMEIN' CHANGING git_file3-kmein.
      ENDIF.

      IF git_file3-kpein IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KPEIN' CHANGING git_file3-kpein.
      ENDIF.

      IF git_file3-konwa_pb30 IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KONWA' CHANGING git_file3-konwa_pb30.
      ENDIF.

      IF git_file3-kmein_pb30 IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KMEIN' CHANGING git_file3-kmein_pb30.
      ENDIF.

      IF git_file3-kpein_pb30 IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KPEIN' CHANGING git_file3-kpein_pb30.
      ENDIF.

      APPEND git_file3.
    ENDLOOP.
  ENDIF.

  IF p_file4 IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = lit_raw
        i_filename           = p_file4
      TABLES
        i_tab_converted_data = git_file4
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO ld_mensaje.

      MESSAGE ld_mensaje TYPE 'I'.

      ps_error = 'X'.

      EXIT.
    ENDIF.

    LOOP AT git_file4.
      PERFORM f_cmatn1_input USING git_file4-matnr CHANGING git_file4-matnr.

      PERFORM f_cunit_input  USING git_file4-meinh CHANGING git_file4-meinh lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file4-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

      PERFORM f_cunit_input  USING git_file4-kmein CHANGING git_file4-kmein lf_error.
      IF lf_error = 'X'.
        lit_matnr_error_um-matnr = git_file4-matnr.
        APPEND lit_matnr_error_um.
      ENDIF.

*     Obtener valores por defecto
      IF git_file4-konwa IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KONWA' CHANGING git_file4-konwa.
      ENDIF.

      IF git_file4-kmein IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KMEIN' CHANGING git_file4-kmein.
      ENDIF.

      IF git_file4-kpein IS INITIAL.
        PERFORM f_get_valor_def TABLES lit_valores_def USING 'KPEIN' CHANGING git_file4-kpein.
      ENDIF.


      MODIFY git_file4.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM f_start_of_selection.
* Validar Pantalla Selección
  PERFORM f_validar_pantalla_seleccion CHANGING gf_error.

  IF gf_error = space.
*   Validación OK...

*   Cargar parametrizaciones proceso de carga
    PERFORM f_init_data.

*   Ejecutar proceso
    IF p_mon1 = 'X'.                                                                                "Carga Plantillas Full
      PERFORM f_cargar_ficheros CHANGING gf_error.
    ELSEIF p_mon3 = 'X'.                                                                            "Carga Plantillas Lite
      PERFORM f_cargar_ficheros_lite CHANGING gf_error.
    ELSE.                                                                                           "Log Proceso Carga
      PERFORM f_seleccionar_datos_log.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_END_OF_SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_end_of_selection .
  IF gf_error = space.
    IF p_mon1 = 'X'. "Carga completa
      IF git_listado[] IS NOT INITIAL.
        PERFORM f_listar_datos USING 'ZRETPDT001S04'
                                     'F_STATUS'
                                     'F_UCOMM'
                                     ''
                                     'F_TOP_OF_PAGE_HTML'
                                     'GIT_LISTADO[]'
                                     ''
                                     'X'.
      ELSE.
*          Msg: No se determinaron artículos para cargar.
        MESSAGE i001(zretpdt001).
      ENDIF.
    ELSEIF p_mon3 = 'X'. "Carga/Precarga reducida
      IF git_lite[] IS NOT INITIAL.
        PERFORM f_listar_datos USING 'ZRETPDT001S10'
                                     'F_STATUS'
                                     'F_UCOMM'
                                     ''
                                     'F_TOP_OF_PAGE_HTML_LITE'
                                     'GIT_LITE[]'
                                     ''
                                     'X'.
      ELSE.
*              Msg: No se determinaron artículos para cargar.
        MESSAGE i001(zretpdt001).
      ENDIF.
    ELSE. "Log
      IF git_listado_log[] IS NOT INITIAL.
        PERFORM f_listar_datos USING 'ZRETPDT001S06'
                                     ''
                                     ''
                                     ''
                                     'F_TOP_OF_PAGE_HTML2'
                                     'GIT_LISTADO_LOG[]'
                                     ''
                                     ''.
      ELSE.
*         Msg: No se determinaron artículos para cargar.
        MESSAGE i001(zretpdt001).
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_NUMBER_GET_NEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LD_PRECA
*&---------------------------------------------------------------------*
FORM f_number_get_next  CHANGING ps_preca.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZZPRECA'
*     QUANTITY                = '1'
*     SUBOBJECT               = ' '
*     TOYEAR                  = '0000'
*     IGNORE_BUFFER           = ' '
    IMPORTING
      number                  = ps_preca
*     QUANTITY                =
*     RETURNCODE              =
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0. ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_user_command_0200 .
  DATA: ld_okcode LIKE sy-ucomm.

  ld_okcode = gd_okcode_0200.

  CASE ld_okcode.
    WHEN 'ACEPTAR'.
      gf_aceptar_0200 = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCELAR'.
      gf_aceptar_0200 = ''.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CARGAR_FICHEROS_DETALLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_cargar_ficheros_detalle .
  DATA: ld_index LIKE sy-tabix.

  LOOP AT git_file1.
    LOOP AT git_listado WHERE matnr = git_file1-matnr AND f1_mtart IS INITIAL.
      ld_index = sy-tabix.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      PERFORM f_asignar_datos_f1.
      MODIFY git_listado INDEX ld_index.
    ELSE.
      LOOP AT git_listado WHERE matnr = git_file1-matnr.
        ld_index = sy-tabix.
        EXIT.
      ENDLOOP.

      PERFORM f_clear_git_listado_det.

      PERFORM f_asignar_datos_f1.

      APPEND git_listado.
    ENDIF.
  ENDLOOP.


  LOOP AT git_file2.
    LOOP AT git_listado WHERE matnr = git_file2-matnr AND f2_ean11 IS INITIAL.
      ld_index = sy-tabix.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      PERFORM f_asignar_datos_f2.
      MODIFY git_listado INDEX ld_index.
    ELSE.
      LOOP AT git_listado WHERE matnr = git_file2-matnr.
        ld_index = sy-tabix.
        EXIT.
      ENDLOOP.

      PERFORM f_clear_git_listado_det.

      PERFORM f_asignar_datos_f2.

      APPEND git_listado.
    ENDIF.
  ENDLOOP.

  LOOP AT git_file3.
    LOOP AT git_listado WHERE matnr = git_file3-matnr AND f3_lifnr IS INITIAL.
      ld_index = sy-tabix.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      PERFORM f_asignar_datos_f3.
      MODIFY git_listado INDEX ld_index.
    ELSE.
      LOOP AT git_listado WHERE matnr = git_file3-matnr.
        ld_index = sy-tabix.
        EXIT.
      ENDLOOP.

      PERFORM f_clear_git_listado_det.

      PERFORM f_asignar_datos_f3.

      APPEND git_listado.
    ENDIF.
  ENDLOOP.

  LOOP AT git_file4.
    LOOP AT git_listado WHERE matnr = git_file4-matnr AND f4_vkorg IS INITIAL.
      ld_index = sy-tabix.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      PERFORM f_asignar_datos_f4.
      MODIFY git_listado INDEX ld_index.
    ELSE.
      LOOP AT git_listado WHERE matnr = git_file4-matnr.
        ld_index = sy-tabix.
        EXIT.
      ENDLOOP.

      PERFORM f_clear_git_listado_det.

      PERFORM f_asignar_datos_f4.

      APPEND git_listado.
    ENDIF.
  ENDLOOP.
ENDFORM.

*===================================================================================================
* Form F_VALIDACION_FULL_ART_145
*===================================================================================================
* Validación 1.45
*
*===================================================================================================
FORM f_validacion_full_art_145  TABLES   lit_validaciones STRUCTURE zretpdt001t04
                                USING    ld_index_listado
                                CHANGING lf_error.

  CLEAR lf_error.

  LOOP AT git_file1 WHERE matnr = git_listado-matnr.
    EXIT.
  ENDLOOP.

  IF git_file1-loggr IS NOT INITIAL.
    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.45' activa = 'X'.

    IF sy-subrc = 0.
      SELECT SINGLE loggr
        FROM tlog
        INTO git_file1-loggr
       WHERE loggr = git_file1-loggr.

      IF sy-subrc <> 0.
        lf_error = 'X'.

*       Msg: Datos básicos: Etiquetado energético & no válido.
        MESSAGE s066(zretpdt001) WITH git_file1-loggr INTO git_listado-info.
        git_listado-status_g = gc_minisemaforo_rojo.

        MODIFY git_listado INDEX ld_index_listado.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*===================================================================================================
* Form F_VALIDACION_FULL_ART_147
*===================================================================================================
* Validación 1.47
*
*===================================================================================================
FORM f_validacion_full_art_147  TABLES   lit_validaciones STRUCTURE zretpdt001t04
                                USING    ld_index_listado
                                CHANGING lf_error.

  CLEAR lf_error.

  LOOP AT git_file1 WHERE matnr = git_listado-matnr.
    EXIT.
  ENDLOOP.

  IF ( git_file1-mstae IS NOT INITIAL AND git_file1-mstde IS INITIAL ) OR
     ( git_file1-mstae IS INITIAL AND git_file1-mstde IS NOT INITIAL ).

    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.47' activa = 'X'.

    IF sy-subrc = 0.
      lf_error = 'X'.

*     Msg: Datos básicos: StatMatTodosCe y su validez deben informarse a la vez.
      MESSAGE s069(zretpdt001) WITH git_file1-loggr INTO git_listado-info.
      git_listado-status_g = gc_minisemaforo_rojo.

      MODIFY git_listado INDEX ld_index_listado.
    ENDIF.
  ENDIF.
ENDFORM.

*===================================================================================================
* Form F_VALIDACION_FULL_ART_148
*===================================================================================================
* Validación 1.48
*
*===================================================================================================
FORM f_validacion_full_art_148  TABLES   lit_validaciones STRUCTURE zretpdt001t04
                                USING    ld_index_listado
                                CHANGING lf_error.

  CLEAR lf_error.

  LOOP AT git_file1 WHERE matnr = git_listado-matnr.
    EXIT.
  ENDLOOP.

  IF ( git_file1-mstav IS NOT INITIAL AND git_file1-mstdv IS INITIAL ) OR
     ( git_file1-mstav IS INITIAL AND git_file1-mstdv IS NOT INITIAL ).

    READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.48' activa = 'X'.

    IF sy-subrc = 0.
      lf_error = 'X'.

*     Msg: Datos básicos: Stat.todas CDis y su validez deben informarse a la vez.
      MESSAGE s070(zretpdt001) WITH git_file1-loggr INTO git_listado-info.
      git_listado-status_g = gc_minisemaforo_rojo.

      MODIFY git_listado INDEX ld_index_listado.
    ENDIF.
  ENDIF.
ENDFORM.

*===================================================================================================
* Form F_VALIDACION_FULL_ART_304
*===================================================================================================
* Validación 3.04
*
* Se verifica que en el excel de registros info se hayan informado completamente los valores del
* PB00 y/o PB30
*===================================================================================================
FORM f_validacion_full_art_304  TABLES   lit_validaciones STRUCTURE zretpdt001t04
                                USING    ld_index_listado
                                         wa_file3         LIKE git_file3
                                CHANGING lf_error.

* 0.- Declaración de variables
*===================================================================================================
  DATA: wa_file1   LIKE git_file1,
        lf_pb00_ok,
        lf_pb30_ok.

* 1.- Lógica
*===================================================================================================
* Inicializar retorno
  CLEAR lf_error.

* Verificar que la validación está activa
  READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '3.04' activa = 'X'.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* Validar si el PB00 se ha informado correctamente
  IF wa_file3-netpr IS NOT INITIAL AND
     wa_file3-datab IS NOT INITIAL.
    lf_pb00_ok = 'X'.
  ENDIF.

* Validar si el PB30 se ha informado correctamente
  IF wa_file3-netpr_pb30 IS NOT INITIAL AND
     wa_file3-datab_pb30 IS NOT INITIAL.
    lf_pb30_ok = 'X'.
  ENDIF.

* Si tanto PB00 como PB30 son OK => Fin
  IF lf_pb00_ok = 'X' AND lf_pb30_ok = 'X'.
    EXIT.
  ENDIF.

* Si no se ha informado correctamente ninguno de los 2 => Error
  IF lf_pb00_ok = '' AND lf_pb30_ok = ''.
*     Msg: Registro Info: Datos de precio de compra PB00/PB30 incompletos
    git_listado-info     = 'Registro Info: Datos de precio de compra PB00/PB30 incompletos'.
    git_listado-status_g = gc_minisemaforo_rojo.

    MODIFY git_listado INDEX ld_index_listado.

    EXIT.
  ENDIF.

* Si PB00 no es OK y han informado algun dato en sus columnas => Error
  IF lf_pb00_ok = ''.
    IF wa_file3-netpr IS NOT INITIAL OR
       wa_file3-datab IS NOT INITIAL.
*     Msg: Registro Info: Datos de precio de compra PB00 incompletos
      git_listado-info     = 'Registro Info: Datos de precio de compra PB00 incompletos'.
      git_listado-status_g = gc_minisemaforo_rojo.

      MODIFY git_listado INDEX ld_index_listado.

      EXIT.
    ENDIF.
  ENDIF.

* Si PB30 no es OK y han informado algun dato en sus columnas => Error
  IF lf_pb30_ok = ''.
    IF wa_file3-netpr_pb30 IS NOT INITIAL OR
       wa_file3-datab_pb30 IS NOT INITIAL.
*     Msg: Registro Info: Datos de precio de compra PB30 incompletos
      git_listado-info     = 'Registro Info: Datos de precio de compra PB30 incompletos'.
      git_listado-status_g = gc_minisemaforo_rojo.

      MODIFY git_listado INDEX ld_index_listado.

      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

*===================================================================================================
* Form F_VALIDACION_FULL_ART_146
*===================================================================================================
* Validación 1.46
*
* Se verifica la integridad de los Kits y sus componentes
*===================================================================================================
FORM f_validacion_full_art_146  TABLES   lit_validaciones STRUCTURE zretpdt001t04
                                USING    ld_index_listado
                                CHANGING lf_error.

* 0.- Declaración de variables
*===================================================================================================
  DATA: wa_file1 LIKE git_file1.

* 1.- Lógica
*===================================================================================================
* Inicializar retorno
  CLEAR lf_error.

* Verificar que la validación está activa
  READ TABLE lit_validaciones WITH KEY tipocarga = 'F' validacion = '1.46' activa = 'X'.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* Cargar datos basicos del artículo en estudio
  LOOP AT git_file1 WHERE matnr = git_listado-matnr.
    EXIT.
  ENDLOOP.

  IF git_file1-mtart = 'ZKIT' AND git_file1-attyp <> '10'.
    lf_error = 'X'.

    git_listado-status_g = gc_minisemaforo_rojo.
    git_listado-info     = 'DB: Para tipo artículo ZKIT la categoria de artículo debe ser 10'.

    MODIFY git_listado INDEX ld_index_listado.

    EXIT.
  ELSEIF git_file1-mtart <> 'ZKIT' AND git_file1-attyp <> '00'.
    lf_error = 'X'.

    git_listado-status_g = gc_minisemaforo_rojo.
    git_listado-info     = 'DB: Para tipo art. dif. a ZKIT la categ. artículo debe ser 00'.

    MODIFY git_listado INDEX ld_index_listado.

    EXIT.
  ELSEIF git_file1-pmata IS NOT INITIAL.
    IF git_file1-mtart = 'ZKIT'.
*     Msg: DB: KIT con artículo padre asignado.
      git_listado-info     = 'DB: KIT con artículo padre asignado.'.
      git_listado-status_g = gc_minisemaforo_rojo.

      MODIFY git_listado INDEX ld_index_listado.

      EXIT.
    ENDIF.

    LOOP AT git_file1 INTO wa_file1 WHERE matnr = git_file1-pmata AND mtart = 'ZKIT' AND attyp = '10'.
      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      lf_error = 'X'.

*     Msg: DB: Artículo padre & no existe en el excel como ZKIT-10.
      MESSAGE s067(zretpdt001) WITH git_file1-pmata INTO git_listado-info.

      git_listado-status_g = gc_minisemaforo_rojo.

      MODIFY git_listado INDEX ld_index_listado.

      EXIT.
    ENDIF.

    IF git_file1-cantc IS INITIAL.
      lf_error = 'X'.

      git_listado-status_g = gc_minisemaforo_rojo.
      git_listado-info     = 'DB: No se ha especificado cantidad del componente.'.

      MODIFY git_listado INDEX ld_index_listado.
    ENDIF.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SAVE_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_LINES_ING_CA
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_save_text  TABLES   lit_lines STRUCTURE tline
                  USING    pe_tdobject
                           pe_tdname
                           pe_tdid
                           pe_tdspras.

  DATA: lr_header          LIKE thead.

  lr_header-tdobject = pe_tdobject.
  lr_header-tdname   = pe_tdname.
  lr_header-tdid     = pe_tdid.
  lr_header-tdspras  = pe_tdspras.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
*     CLIENT   = SY-MANDT
      header   = lr_header
*     INSERT   = ' '
*     SAVEMODE_DIRECT       = ' '
*     OWNER_SPECIFIED       = ' '
*     LOCAL_CAT             = ' '
*   IMPORTING
*     FUNCTION =
*     NEWHEADER             =
    TABLES
      lines    = lit_lines
    EXCEPTIONS
      id       = 1
      language = 2
      name     = 3
      object   = 4
      OTHERS   = 5.

  IF sy-subrc <> 0. ENDIF.

  CALL FUNCTION 'COMMIT_TEXT'
*   EXPORTING
*     OBJECT                = '*'
*     NAME                  = '*'
*     ID                    = '*'
*     LANGUAGE              = '*'
*     SAVEMODE_DIRECT       = ' '
*     KEEP                  = ' '
*     LOCAL_CAT             = ' '
*   IMPORTING
*     COMMIT_COUNT          =
*   TABLES
*     T_OBJECT              =
*     T_NAME                =
*     T_ID                  =
*     T_LANGUAGE            =
    .

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'DB_COMMIT'
*   EXPORTING
*     IV_DEFAULT       = ABAP_FALSE
    .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ACT_DATOS_BASICOS_ARTICULO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_act_datos_basicos_articulo .
  DATA: lit_lines   LIKE tline OCCURS 0 WITH HEADER LINE.

  lit_lines-tdformat = '*'.
  lit_lines-tdline   = git_file1-maktx.

  APPEND lit_lines.

  PERFORM f_save_text TABLES lit_lines USING 'MATERIAL' git_file1-matnr 'GRUN' 'S'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_FILL_09_DATOS_PLANIFICACION
*&---------------------------------------------------------------------*
*& Dentro del proceso de carga del articulo, en esta rutina se rellenan los datos de las tablas de
*  la bapi de planificación del artículo
*&---------------------------------------------------------------------*
*&      --> LIT_RECIPIENTPARAMETERS
*&      --> LIT_RECIPIENTPARAMETERSX
*&---------------------------------------------------------------------*
FORM f_fill_09_datos_planificacion  TABLES   lit_recipientparameters STRUCTURE bapi_wrpl_import
                                             lit_recipientparametersx STRUCTURE bapi_wrpl_importx.

* Solo actualizamos datos de planificación si en el excel se han informado datos
  IF git_file1-dismm IS NOT INITIAL OR
     git_file1-sobst IS NOT INITIAL.
    LOOP AT git_centros_alta.
      CLEAR: lit_recipientparameters,
             lit_recipientparametersx.

      lit_recipientparameters-recipient     = git_centros_alta-valor.                                 "Centro
      lit_recipientparametersx-recipient    = git_centros_alta-valor.

      lit_recipientparameters-material      = git_file1-matnr.                                        "Artículo
      lit_recipientparametersx-material     = 'X'.

      IF git_file1-dismm IS NOT INITIAL.                                                              "Característica de planificación de necesidades
        lit_recipientparameters-mrp_type      = git_file1-dismm.
        lit_recipientparametersx-mrp_type     = 'X'.
      ENDIF.

      IF git_file1-sobst IS NOT INITIAL.
        lit_recipientparameters-target_stock      = git_file1-sobst.                                  "Stock objetivo
        lit_recipientparametersx-target_stock     = 'X'.
      ENDIF.

      APPEND: lit_recipientparameters,
              lit_recipientparametersx.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_UPDATE_PLANIFICATION_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_update_planification_data  TABLES lit_recipientparameters  STRUCTURE bapi_wrpl_import
                                         lit_recipientparametersx STRUCTURE bapi_wrpl_importx
                                         lit_return               STRUCTURE bapiret2
                                CHANGING ps_error.


  REFRESH lit_return.

  ps_error = ''.

  IF lit_recipientparameters[] IS INITIAL.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_RTMAT_RPL_SAVEREPLICAMULT'
    TABLES
      recipientparameters  = lit_recipientparameters
      recipientparametersx = lit_recipientparametersx
      return               = lit_return.

* Hacemos commit
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'
*   IMPORTING
*     RETURN        =
    .

  READ TABLE lit_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.

  IF sy-subrc = 0.
    ps_error = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CONV_EXIT_CUNIT_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_COND_VALIDITY_BASE_UOM
*&      <-- IT_COND_VALIDITY_BASE_UOM
*&---------------------------------------------------------------------*
FORM f_conv_exit_cunit_output  USING    pe_meins
                               CHANGING ps_meins.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = pe_meins
*     LANGUAGE       = SY-LANGU
    IMPORTING
*     LONG_TEXT      =
      output         = ps_meins
*     SHORT_TEXT     =
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0. ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_UNIT_OF_MEASURE_SAP_TO_ISO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_E1KOMG_VRKME
*&      <-- LR_E1KOMG_VRKME
*&---------------------------------------------------------------------*
FORM f_unit_of_measure_sap_to_iso  USING    pe_vrkme
                                   CHANGING ps_vrkme.
  CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
    EXPORTING
      sap_code    = pe_vrkme
    IMPORTING
      iso_code    = ps_vrkme
    EXCEPTIONS
      not_found   = 1
      no_iso_code = 2
      OTHERS      = 3.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PROCESAR_EXEC_05_KITS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LF_ERROR
*&---------------------------------------------------------------------*
FORM f_procesar_exec_05_kits  CHANGING ps_error.
* 0.- Declaración de variables
*===================================================================================================
  DATA: BEGIN OF lit_componentes OCCURS 0,
          componente TYPE matnr,
          cantidad   TYPE int4,
        END OF lit_componentes,

        ld_cantidad(10),

        wa_file1        LIKE git_file1,

        ld_mode(1)      VALUE 'N'.

* 1.- Lógica
*===================================================================================================
* Determinar los componentes del kit
  LOOP AT git_file1 INTO wa_file1 WHERE pmata = git_file1-matnr.
    lit_componentes-componente = wa_file1-matnr.
    lit_componentes-cantidad   = wa_file1-cantc.

    APPEND lit_componentes.
  ENDLOOP.

  IF lit_componentes[] IS INITIAL.
    ps_error = 'N'.
    EXIT.
  ENDIF.

* Asignar componentes
  CLEAR ps_error.

  REFRESH: bdcdata,
           messtab.

* MM42: Pantalla inicial
  PERFORM bdc_dynpro      USING 'SAPLMGMW' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'.
  PERFORM bdc_field       USING 'RMMW1-MATNR'
                                git_file1-matnr.
  PERFORM bdc_field       USING 'RMMW1-EKORG'
                                ''.
  PERFORM bdc_field       USING 'RMMW1-LIFNR'
                                ''.
  PERFORM bdc_field       USING 'RMMW1-LTSNR'
                                ''.
  PERFORM bdc_field       USING 'RMMW1-VKORG'
                                ''.
  PERFORM bdc_field       USING 'RMMW1-VTWEG'
                                ''.
  PERFORM bdc_field       USING 'RMMW1-VZWRK'
                                ''.
  PERFORM bdc_field       USING 'RMMW1-FIWRK'
                                ''.
  PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(01)'
                                'X'.

* MM42: Datos básicos => Navegar a asignación de componentes
  PERFORM bdc_dynpro      USING 'SAPLMGMW' '4008'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PB47'.

  LOOP AT lit_componentes.
    WRITE lit_componentes-cantidad TO ld_cantidad LEFT-JUSTIFIED.

    PERFORM bdc_dynpro      USING 'SAPLWST1' '0100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'WSTR_DYNP-IDNRK(02)'
                                  lit_componentes-componente.

    PERFORM bdc_field       USING 'WSTR_DYNP-CPQTY(02)'
                                  ld_cantidad.

    PERFORM bdc_dynpro      USING 'SAPLWST1' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'WSTR_DYNP-IDNRK(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=P+'.
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPLWST1' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                               '=BACK'.

  PERFORM bdc_dynpro      USING 'SAPLMGMW' '4008'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BABA'.
  PERFORM bdc_dynpro      USING 'SAPLSPO1' '0300'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=YES'.

  DO 10 TIMES.
    REFRESH messtab.

    CALL TRANSACTION 'MM42' USING bdcdata MESSAGES INTO messtab MODE ld_mode.

    COMMIT WORK AND WAIT.

    LOOP AT messtab WHERE msgid = 'M3' AND msgnr = '801'.
      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      ps_error = 'X'.

      WAIT UP TO 1 SECONDS.
    ELSE.
      ps_error = ''.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.
