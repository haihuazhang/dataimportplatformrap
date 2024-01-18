CLASS lhc_configuration DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR Configuration
        RESULT result.
*      zzPrintPDFAction FOR MODIFY
*        IMPORTING keys FOR ACTION Configuration~zzPrintPDFAction RESULT result.
ENDCLASS.

CLASS lhc_configuration IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
*  METHOD zzPrintPDFAction.
*    DATA(lv_input_key) = keys[ 1 ]-%key.
*
*
*    SELECT SINGLE template FROM zzi_zt_dtimp_conf WHERE Object = 'ZZTESTADS'
*      INTO @DATA(lv_xdp).
*
*    "Get Data
*    DATA(lo_fdp_util) = cl_fp_fdp_services=>get_instance( 'ZDSAG_BILLING_SRV_DEF' ).
*    DATA(lt_keys)     = lo_fdp_util->get_keys( ).
*    lt_keys[ name = 'ID' ]-value = '1'.
*
*    TRY.
*        DATA(lv_xml) = lo_fdp_util->read_to_xml( lt_keys ).
*      CATCH cx_fp_fdp_error INTO DATA(lx_fdp).
*        "handle exception
*        DATA(lv_message) = lx_fdp->get_longtext(  ).
*    ENDTRY.
**    out->write( 'Service data retrieved' ).
*
**    cl_fp_ads_util=>render_pdf( EXPORTING iv_locale = 'en_US'
**                                          iv_xdp_layout = lv_xdp
**                                          iv_xml_data = lv_xml
**                                IMPORTING ev_pdf = DATA(lv_pdf)      ).
*    cl_fp_ads_util=>render_4_pq(
*      EXPORTING
*        iv_locale       = 'en_US'
*        iv_pq_name      = 'TEST'
*        iv_xml_data     = lv_xml
*        iv_xdp_layout   = lv_xdp
**        is_options      = VALUE #(
**          trace_level = 0 "Use 0 in production environment
**        )
*      IMPORTING
*        ev_trace_string = DATA(lv_trace)
*        ev_pdl          = DATA(lv_pdf)
*    ).

*    DATA(lv_print_itemid) = cl_print_queue_utils=>create_queue_itemid(  ).
*
*    cl_print_queue_utils=>create_queue_item_by_data(
*        EXPORTING
*            iv_qname = 'TEST'
*            iv_print_data = lv_pdf
*            iv_name_of_main_doc = 'DSAG DEMO Output'
*            iv_itemid = lv_print_itemid
*        IMPORTING
*            ev_err_msg = DATA(LV_err_msg)
*        RECEIVING
*            rv_itemid = DATA(lv_itemid)
*    ).
*    IF lv_err_msg IS NOT INITIAL.
*      APPEND VALUE #(  uuid = lv_input_key-uuid
*               %msg = new_message(
*               id       = '00'
*               number   = 000
*               severity = if_abap_behv_message=>severity-error
*               v1       = lv_err_msg
*               )
*             )
*        TO reported-configuration.
*    ENDIF.
*    lt_entity[ 1 ]-FileName = 'Test.pdf'.
*    lt_entity[ 1 ]-MimeType = 'application/pdf'.
*    lt_entity[ 1 ]-attachment = lv_pdf.
*    DATA : create_printing TYPE TABLE FOR CREATE zi_zt_prt_record.
*
*    types : BEGIN OF ts_key,
*            ID TYPE string,
*            END OF ts_key.
*    DATA : ls_key TYPE ts_key,
*           lv_key_json TYPE string.
*
*    ls_key-id = '1'.
*
*    /ui2/cl_json=>serialize(
*        EXPORTING
*            data = ls_key
*        RECEIVING
*            r_json = lv_key_json
*     ).
*
*
*    create_printing = VALUE #( ( TemplateUUID = '296C71771B641EDE9CF5C5B9772609BA' IsExternalProvidedData = abap_false ProvidedKeys = lv_key_json ) ).
*
*
*
*    MODIFY ENTITIES OF zi_zt_prt_record
*        ENTITY Record
*        CREATE FIELDS ( TemplateUUID IsExternalProvidedData ProvidedKeys ) with create_printing
*        MAPPED DATA(mapped_records)
*        REPORTED DATA(reported_records)
*        FAILED DATA(failed_records).
*
**    COMMIT ENTITIES.
*
*    result = VALUE #( for record in mapped_records-record ( %param = VALUE #( url = record-uuid ) ) ).
*    "
**
*
**    DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static(  ).
**    result = VALUE #( ( %tky-%key-uuid = lv_uuid uuid = lv_uuid %param = VALUE #( FileName = 'Test.pdf' MimeType = 'application/pdf' attachment = lv_pdf id = lv_uuid ) ) ).
*
*  ENDMETHOD.

ENDCLASS.
