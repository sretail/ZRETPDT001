FUNCTION z_catalogar_single.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(PE_MATNR) TYPE  MATNR
*"     REFERENCE(PE_DATAB) TYPE  DATAB
*"     REFERENCE(PE_DATBI) TYPE  DATBI
*"     REFERENCE(PE_CATSURT) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(PS_CATALOGAR_OK) TYPE  XFLAG
*"     REFERENCE(PS_DOCNUM) TYPE  EDI_DOCNUM
*"  TABLES
*"      IT_RETURN STRUCTURE  BDCMSGCOLL
*"      IT_TIENDAS STRUCTURE  ZSTIENDAS OPTIONAL
*"----------------------------------------------------------------------
* 0.- Declaracion de variables
*-----------------------------------------------------------------------
  DATA:"Segmento solo con articulo
        lr_e1matnr LIKE e1matnr,
       "Codigo articulo anterior
        ld_matnr_ant LIKE mara-matnr,
       "Condiciones de catalogación
        lr_e1wlk1m LIKE e1wlk1m,
       "Contador para el numero del segmento del IDOC a crear
        ld_segnum           TYPE idocdsgnum,
       "Identificador del numero de segmento de cabecera
        ld_segnum_cab       TYPE idocdsgnum,
       "Datos de control del IDOC a crear
        lr_idoc_control     LIKE edidc,
        lr_idoc_control_new LIKE edidc,
       "Identificador del IDOC abierto
        ld_identifier       LIKE edidc-docnum,
       "Indicador nuevo articulo == nuevo IDOC
        lf_nuevo_articulo(1),
       "Tabla interna con los segmentos del IDOC a crear
        lit_idoc_containers LIKE edidd OCCURS 0 WITH HEADER LINE,
       "Tabla para actualizacion del status del IDOC
        lt_edids            TYPE TABLE OF bdidocstat WITH HEADER LINE,
       "Variables de datos parametrizados en ZHARDCODES
        ld_docrel           TYPE edi_docrel,
        ld_rcvpor           TYPE edi_rcvpor,
        ld_rcvprn TYPE edi_rcvprn,
        ld_sndpor TYPE edi_sndpor,
        ld_sndprn TYPE edi_sndprn,
       "Grupo de articulos
        ld_matkl  like mara-matkl,
        lit_wrs6  like wrs6 OCCURS 0 WITH HEADER LINE,
        lit_wrsz  like wrsz OCCURS 0 WITH HEADER LINE.

  ranges: lran_asort for wrs1-asort.

* 0.- Inicializaciones
*-----------------------------------------------------------------------
* Inicializaciones
  clear ps_docnum.
  clear ps_catalogar_ok.
  clear ps_docnum.

* Recuperamos datos parametrizados en la ZHARDCODES
  SELECT SINGLE valor
    FROM zhardcodes
    INTO ld_docrel
   WHERE programa = 'Z_CATALOGAR_SINGLE'
     AND param    = 'DOCREL'.

  SELECT SINGLE valor
    FROM zhardcodes
    INTO ld_rcvpor
   WHERE programa = 'Z_CATALOGAR_SINGLE'
     AND param    = 'RCVPOR'.

  SELECT SINGLE valor
    FROM zhardcodes
    INTO ld_rcvprn
   WHERE programa = 'Z_CATALOGAR_SINGLE'
     AND param    = 'RCVPRN'.

  SELECT SINGLE valor
    FROM zhardcodes
    INTO ld_sndpor
   WHERE programa = 'Z_CATALOGAR_SINGLE'
     AND param    = 'SNDPOR'.

  SELECT SINGLE valor
    FROM zhardcodes
    INTO ld_sndprn
   WHERE programa = 'Z_CATALOGAR_SINGLE'
     AND param    = 'SNDPRN'.

* APRADAS-Inicio- 17.03.2016
  if pe_catsurt = 'X'.
*   Si catalogación por surtidos del grupo de articulos...

*   Recuperamos surtidos de tipo C
    lran_asort-sign = 'I'.
    lran_asort-option = 'EQ'.
    select asort
      from wrs1
      into lran_asort-low
     where sotyp = 'C'.
      append lran_asort.
    endselect.

*   Obtener grupo de articulos del articulo
    select single matkl
      from mara
      into ld_matkl
     where matnr = pe_matnr.

*   Recuperamos los surtidos asociados a ese grupo de articulos
    select *
      from wrs6
      into table lit_wrs6
     where matkl = ld_matkl
       and asort in lran_asort.

    if sy-subrc <> 0.
*     Si para el grupo de articulos no encontramos ningun surtido, no catalogamos
*     pero devolvemos como si hubieramos catalogado ok
      ps_catalogar_ok = 'X'.
      exit.
    endif.
  endif.
* APRADAS-Fin   - 17.03.2016

* DATOS DE CONTROL DEL IDOC
*----------------------------------------------------------------------
  lr_idoc_control-docrel = ld_docrel.
  lr_idoc_control-direct = '2'.
  lr_idoc_control-rcvpor = ld_rcvpor.
  lr_idoc_control-rcvprt = 'KU'.
  lr_idoc_control-rcvprn = ld_rcvprn.
  lr_idoc_control-sndpor = ld_sndpor.
  lr_idoc_control-sndprt = 'KU'.
  lr_idoc_control-sndprn = ld_sndprn.
  lr_idoc_control-mestyp = 'LIKOND'.
  lr_idoc_control-idoctp = 'LIKOND01'.

* Abrir creación del IDOC
*-----------------------------------------------------------------------
  CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_CREATE'
    EXPORTING
      idoc_control         = lr_idoc_control
    IMPORTING
      identifier           = ld_identifier
    EXCEPTIONS
      other_fields_invalid = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
  ENDIF.


  lr_e1matnr-matnr = pe_matnr.

  ADD 1 TO ld_segnum.

  lit_idoc_containers-segnam  = 'E1MATNR'.
  lit_idoc_containers-sdata   = lr_e1matnr.
  lit_idoc_containers-docnum  = ld_identifier.
  lit_idoc_containers-segnum  = ld_segnum.
  ld_segnum_cab = ld_segnum.
  APPEND lit_idoc_containers.

* APRADAS-Inicio- 17.03.2016
  if pe_catsurt = 'X'.
*   Si catalogación por surtido

    LOOP AT lit_wrs6.
      lr_e1wlk1m-msgfn = '009'.
      lr_e1wlk1m-filia = lit_wrs6-asort.
      lr_e1wlk1m-datbi = '99991231'.
      lr_e1wlk1m-datab = sy-datum.

      lr_e1wlk1m-strnr = pe_matnr.

      ADD 1 TO ld_segnum.

      lit_idoc_containers-segnam  = 'E1WLK1M'.
      lit_idoc_containers-sdata   = lr_e1wlk1m.
      lit_idoc_containers-docnum  = ld_identifier.
      lit_idoc_containers-segnum  = ld_segnum.
      lit_idoc_containers-psgnum  = ld_segnum_cab.
      APPEND lit_idoc_containers.
    ENDLOOP.

  elseif pe_catsurt = 'Z'.
    lr_e1wlk1m-msgfn = '009'.
    lr_e1wlk1m-filia = 'ZGENERAL'.
    lr_e1wlk1m-datbi = '99991231'.
    lr_e1wlk1m-datab = sy-datum.

    lr_e1wlk1m-strnr = pe_matnr.

    ADD 1 TO ld_segnum.

    lit_idoc_containers-segnam  = 'E1WLK1M'.
    lit_idoc_containers-sdata   = lr_e1wlk1m.
    lit_idoc_containers-docnum  = ld_identifier.
    lit_idoc_containers-segnum  = ld_segnum.
    lit_idoc_containers-psgnum  = ld_segnum_cab.
    APPEND lit_idoc_containers.
  else.
* APRADAS-Fin   - 17.03.2016
*   Si catalogación por tienda...
    LOOP AT it_tiendas.
      lr_e1wlk1m-msgfn = '009'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = it_tiendas-werks
        IMPORTING
          output = lr_e1wlk1m-filia.

      lr_e1wlk1m-datbi = '99991231'.
      lr_e1wlk1m-datab = sy-datum.

      lr_e1wlk1m-strnr = pe_matnr.

      ADD 1 TO ld_segnum.

      lit_idoc_containers-segnam  = 'E1WLK1M'.
      lit_idoc_containers-sdata   = lr_e1wlk1m.
      lit_idoc_containers-docnum  = ld_identifier.
      lit_idoc_containers-segnum  = ld_segnum.
      lit_idoc_containers-psgnum  = ld_segnum_cab.
      APPEND lit_idoc_containers.
    ENDLOOP.
  endif. "APRADAS-17.03.2016

* Añadir segmentos al IDOC
*----------------------------------------------------------------------*
  CALL FUNCTION 'EDI_SEGMENTS_ADD_BLOCK'
    EXPORTING
      identifier                    = ld_identifier
    TABLES
      idoc_containers               = lit_idoc_containers
    EXCEPTIONS
      identifier_invalid            = 1
      idoc_containers_empty         = 2
      parameter_error               = 3
      segment_number_not_sequential = 4
      OTHERS                        = 5.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  Finalizar creación del IDOC
*----------------------------------------------------------------------*
  CALL FUNCTION 'EDI_DOCUMENT_CLOSE_CREATE'
    EXPORTING
      identifier                = ld_identifier
* NO_DEQUEUE                = ' '
* SYN_ACTIVE                = ' '
   IMPORTING
     idoc_control              = lr_idoc_control_new
* SYNTAX_RETURN             =
   EXCEPTIONS
     document_not_open         = 1
     document_no_key           = 2
     failure_in_db_write       = 3
     parameter_error           = 4
     OTHERS                    = 5
            .
  IF sy-subrc <> 0.
*MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.






*   Cambiar el status del IDOC de 50 a 64
*----------------------------------------------------------------------*
  CLEAR lt_edids.
  lt_edids-docnum   = lr_idoc_control_new-docnum.
  lt_edids-status   = '64'.
  lt_edids-uname    = sy-uname.
  lt_edids-repid    = sy-repid.
  lt_edids-tid      = sy-tcode.
  APPEND lt_edids.

  CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
    EXPORTING
      idoc_number               = lr_idoc_control_new-docnum
    TABLES
      idoc_status               = lt_edids[]
    EXCEPTIONS
      idoc_foreign_lock         = 1
      idoc_not_found            = 2
      idoc_status_records_empty = 3
      idoc_status_invalid       = 4
      db_error                  = 5
      OTHERS                    = 6.


  commit work and WAIT.

  CALL FUNCTION 'DEQUEUE_ALL'
*   EXPORTING
*     _SYNCHRON       = ' '
            .


  SUBMIT RBDAPP01
    with DOCNUM BETWEEN lr_idoc_control_new-docnum and space
    with P_OUTPUT = space
    and return.

  ps_catalogar_ok = 'X'.

ENDFUNCTION.
