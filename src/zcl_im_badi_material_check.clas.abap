class ZCL_IM_BADI_MATERIAL_CHECK definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_MATERIAL_CHECK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BADI_MATERIAL_CHECK IMPLEMENTATION.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_MARA_MEINS.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_PMATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA.
  endmethod.


method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA_RETAIL.
* 0.- Declaración de variables
*==========================================================================
  data: ld_puntero_1(100) value '(SAPLZRETPDTT02)GF_CAMBIOS_TEXTOS',
        ld_puntero_2(100) value '(SAPLZRETPDTT02)GIT_MATNR_DESC[]',
        wa_matnr_desc     type  ZRETPDTT02S01,
        lit_ZRETPDTt02    type  STANDARD TABLE OF ZRETPDTt02,
        wa_ZRETPDTt02     type  ZRETPDTt02,
        wa_mean           type  meani.

  FIELD-SYMBOLS: <fs_cambios_textos> type char01,
                 <fs_matnr_Desc> type STANDARD TABLE.

* 1.- Lógica
*==========================================================================

***************************************************************************
* Usuario:      APRADAS@23.01.2018
* Descripción:
*   Actualización textos del articulo
*   NOTA: Este bloque debe ir al final del todo
*==========================================================================
* Inicio
*==========================================================================
  if rt_errdat[] is initial.
    assign (ld_puntero_1) to <fs_cambios_textos>.

    if <fs_cambios_textos> is ASSIGNED.
      if <fs_cambios_textos> = 'X'.
        assign (ld_puntero_2) to <fs_matnr_Desc>.

        if <fs_matnr_Desc> is ASSIGNED.
          loop at <fs_matnr_Desc> into wa_matnr_desc.
            MOVE-CORRESPONDING wa_matnr_desc to wa_ZRETPDTt02.
            append wa_ZRETPDTt02 to lit_ZRETPDTt02.
          endloop.

          MODIFY ZRETPDTt02 from table lit_ZRETPDTt02.
        endif.
      endif.
    endif.

*   Insertar entradas para nuevas unidades de medida
    if wmara-matnr is not initial.
      refresh lit_ZRETPDTt02.
      loop at tmean_me_tab into wa_mean.
        select single matnr
          from ZRETPDTt02
          into wmara-matnr
         where matnr = wmara-matnr
           and meinh = wa_mean-meinh.

        if sy-subrc <> 0.
          clear lit_ZRETPDTt02.
          wa_ZRETPDTt02-matnr = wmara-matnr.
          wa_ZRETPDTt02-meinh = wa_mean-meinh.

          append wa_ZRETPDTt02 to lit_ZRETPDTt02.
        endif.
      endloop.

      MODIFY ZRETPDTt02 from table lit_ZRETPDTt02.

    endif.

*   APRADAS-Inicio-14.06.2018 09:44:55
*   Eliminar entradas de unidades de medida que ya no tenga el articulo

*   Recuperamos todas las unidades de formato
    refresh lit_ZRETPDTt02.
    select *
      from ZRETPDTt02
      into table lit_ZRETPDTt02
     where matnr = wmara-matnr.

    loop at lit_ZRETPDTt02 into wa_ZRETPDTt02.
*     Para cada unidad de formato

*     Miramos si la unidad de formato la sigue teniendo el articulo
      read table tmean_me_tab with key meinh = wa_ZRETPDTt02-meinh TRANSPORTING no FIELDS.

      if sy-subrc <> 0.
*       Si no la tiene, la eliminamos
        delete from ZRETPDTt02
              where matnr = wa_ZRETPDTt02-matnr
                and meinh = wa_ZRETPDTt02-meinh.
      endif.
    endloop.
*   APRADAS-Fin-14.06.2018 09:44:55
  endif.
*==========================================================================
* Fin
*==========================================================================

endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_MASS_MARC_DATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~FRE_SUPPRESS_MARC_CHECK.
  endmethod.
ENDCLASS.
