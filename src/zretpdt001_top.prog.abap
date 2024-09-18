*&---------------------------------------------------------------------*
*&  Include           ZRETPDT001_TOP
*&---------------------------------------------------------------------*
*==========================================================================
* DICCIONARIO DE DATOS
*==========================================================================
TABLES: mara,
        zretpdt001s08,
        ZRETPDT001s12,
        ZAGRUPACIONES.

*==========================================================================
* CONSTANTES
*==========================================================================
CONSTANTS: gc_icono_tickets         TYPE icon_d      VALUE '@O5@',
           gc_icono_detalle_ticket  TYPE icon_d      VALUE '@16@',
           gc_minisemaforo_verde    TYPE icon_d      VALUE '@5B@',
           gc_minisemaforo_ambar    TYPE icon_d      VALUE '@5D@',
           gc_minisemaforo_rojo     TYPE icon_d      VALUE '@5C@',
           gc_minisemaforo_inactivo TYPE icon_d      VALUE '@BZ@',
           gc_icono_inbox           TYPE icon_d      VALUE '@BP@',
           gc_def_bstae             LIKE eine-bstae  VALUE 'Z004',
           gc_def_lgnum             type lgnum       VALUE '010',
           gc_ean_20000             TYPE int4        VALUE  2000,
           gc_paso_no_realizado     TYPE bapi_msg    VALUE 'Paso no realizado por errores anteriores',
           gc_carga_full            type char01      value 'F',
           gc_carga_lite            type char01      value 'L'.

*==========================================================================
* DEFINICIONES GLOBALES
*==========================================================================
DATA:
     "Plantilla 1: Datos básicos
      git_file1            LIKE zretpdt001s01 OCCURS 0 WITH HEADER LINE,
     "Plantilla 2: EANs y UMs
      git_file2            LIKE zretpdt001s02 OCCURS 0 WITH HEADER LINE,
     "Plantilla 3: Registro Info
      git_file3            LIKE zretpdt001s05 OCCURS 0 WITH HEADER LINE,
     "Plantilla 4: Precios de venta
      git_file4            LIKE zretpdt001s07 OCCURS 0 WITH HEADER LINE,
     "Plantilla Reducida
      git_lite             LIKE zretpdt001s10 OCCURS 0 WITH HEADER LINE,
     "Monitor
      git_listado          LIKE zretpdt001s04 OCCURS 0 WITH HEADER LINE,
      git_listado_log      LIKE zretpdt001s06 OCCURS 0 WITH HEADER LINE,
      git_almacenes_alta   LIKE zretpdt001t02 OCCURS 0 WITH HEADER LINE,
      git_centros_alta     LIKE zretpdt001t02 OCCURS 0 WITH HEADER LINE,
      git_centros_val_alta LIKE zretpdt001t02 OCCURS 0 WITH HEADER LINE,
      git_val_def_int      LIKE zretpdt001t02 OCCURS 0 WITH HEADER LINE,
      git_alta_areas_venta like zretpdt001t02 OCCURS 0 WITH HEADER LINE,
     "Valores por defecto en campos del excel
      git_valores_def  like ZRETPDT001t02 occurs 0 WITH HEADER LINE,
      bdcdata              LIKE bdcdata       OCCURS 0 WITH HEADER LINE,
      messtab              LIKE bdcmsgcoll    OCCURS 0 WITH HEADER LINE,
      gd_fecha             LIKE sy-datum,
      gd_hora              LIKE sy-uzeit,
      gf_error(1),
      git_log_carga        LIKE zretpdt001s09 OCCURS 0,
      gr_log_carga         LIKE zretpdt001s09,
      gd_okcode_0100       LIKE sy-ucomm,
      gd_okcode_0200       LIKE sy-ucomm,
      gf_aceptar_0200      type char1,
      gr_grid_01           TYPE REF TO cl_gui_alv_grid,
      gr_container_01      TYPE REF TO cl_gui_custom_container,
      git_fieldcatalog_01  TYPE lvc_t_fcat,
      gr_layout_01         TYPE lvc_s_layo.
