CLASS zzcl_dtimp_process_generic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ms_configuration TYPE STRUCTURE FOR READ RESULT zzi_zt_dtimp_conf.
    DATA: ms_file TYPE STRUCTURE FOR READ RESULT zzr_zt_dtimp_files.
    DATA: uuid TYPE sysuuid_x16.
    DATA: mo_table TYPE REF TO data.
    DATA: out TYPE REF TO if_oo_adt_classrun_out.
    DATA: application_log TYPE REF TO if_bali_log .

    METHODS init_application_log.
    METHODS get_file_content IMPORTING p_uuid TYPE sysuuid_x16.
    METHODS get_batch_import_configuration IMPORTING p_uuid TYPE sysuuid_x16.
    METHODS get_uuid IMPORTING it_parameters TYPE if_apj_dt_exec_object=>tt_templ_val .
    METHODS get_data_from_xlsx.
    METHODS add_text_to_app_log_or_console IMPORTING i_text TYPE cl_bali_free_text_setter=>ty_text
                                                     i_type TYPE cl_bali_free_text_setter=>ty_severity OPTIONAL
                                           RAISING   cx_bali_runtime.
    METHODS process_logic.
    METHODS save_job_info.
ENDCLASS.



CLASS zzcl_dtimp_process_generic IMPLEMENTATION.


  METHOD add_text_to_app_log_or_console.
    TRY.
        IF sy-batch = abap_true.

          DATA(application_log_free_text) = cl_bali_free_text_setter=>create(
                                 severity = COND #( WHEN i_type IS NOT INITIAL
                                                    THEN i_type
                                                    ELSE if_bali_constants=>c_severity_status )
                                 text     = i_text ).

          application_log_free_text->set_detail_level( detail_level = '1' ).
          application_log->add_item( item = application_log_free_text ).
          cl_bali_log_db=>get_instance( )->save_log( log = application_log
                                                     assign_to_current_appl_job = abap_true ).

        ELSE.
*          out->write( |sy-batch = abap_false | ).
          out->write( i_text ).
        ENDIF.
      CATCH cx_bali_runtime INTO DATA(lx_bali_runtime).
    ENDTRY.
  ENDMETHOD.


  METHOD get_batch_import_configuration.
    READ ENTITY zzi_zt_dtimp_conf ALL FIELDS WITH VALUE #( (  %key-uuid = ms_file-uuidconf ) )
        RESULT FINAL(lt_configuration).
    ms_configuration = lt_configuration[ 1 ].
  ENDMETHOD.


  METHOD get_data_from_xlsx.


    TRY.
        "create internal table for corresponding table type
        CREATE DATA mo_table TYPE TABLE OF (ms_configuration-structname).

        " read xlsx object
        DATA(lo_document) = xco_cp_xlsx=>document->for_file_content( ms_file-attachment ).
        DATA(lo_worksheet) = lo_document->read_access(  )->get_workbook(  )->worksheet->for_name( CONV string( ms_configuration-sheetname ) ).
        DATA(lv_sheet_exists) = lo_worksheet->exists(  ).
        IF lv_sheet_exists = abap_false.
          TRY.
              add_text_to_app_log_or_console( i_text = |Excel sheet ms_configuration-Sheetname does not exist in the data file|
                                              i_type = if_bali_constants=>c_severity_error ).
            CATCH cx_bali_runtime.
              "handle exception
          ENDTRY.
          RETURN.
        ENDIF.

        DATA(lo_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
        )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( ms_configuration-startline )
        )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( ms_configuration-startcolumn )
        )->get_pattern(  ).

        lo_worksheet->select( lo_pattern )->row_stream(  )->operation->write_to( mo_table )->execute(  ).
      CATCH cx_sy_create_data_error INTO DATA(lx_sy_create_data_error).
        TRY.
            add_text_to_app_log_or_console( i_text = |Data structure of Import Object not found, please contact Administrator|
                                            i_type = if_bali_constants=>c_severity_error ).
          CATCH cx_bali_runtime.
            "handle exception
        ENDTRY.
*        return.
*        RAISE EXCEPTION TYPE cx_bali_runtime.
    ENDTRY.





  ENDMETHOD.


  METHOD get_file_content.
    READ ENTITY zzr_zt_dtimp_files ALL FIELDS WITH VALUE #( (  %key-uuid = p_uuid ) )
        RESULT FINAL(lt_file).
    ms_file = lt_file[ 1 ].
  ENDMETHOD.


  METHOD get_uuid.
    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'P_ID'.
          uuid = ls_parameter-low.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_apj_dt_exec_object~get_parameters.
    " Return the supported selection parameters here
    et_parameter_def = VALUE #(
      ( selname = 'P_ID'    kind = if_apj_dt_exec_object=>parameter datatype = 'X' length = 16 param_text = 'UUID of stored file' changeable_ind = abap_true )
    ).
  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.



    " get uuid
    get_uuid( it_parameters ).

    "create log handle
    init_application_log(  ).

    " save job info to ZZC_ZT_DTIMP_FILES
    save_job_info(  ).

    TRY.
        cl_system_uuid=>convert_uuid_x16_static( EXPORTING uuid = uuid IMPORTING uuid_c36 = DATA(lv_uuid_c36)  ).
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.
    TRY.
        add_text_to_app_log_or_console( |process batch import uuid { lv_uuid_c36 }| ).
      CATCH cx_bali_runtime.
        "handle exception
    ENDTRY.


    IF uuid IS INITIAL.
      TRY.
          add_text_to_app_log_or_console( i_text = |record not found for uuid { lv_uuid_c36 }|
                                          i_type = if_bali_constants=>c_severity_error ).
        CATCH cx_bali_runtime.
          "handle exception
      ENDTRY.
      RETURN.
    ENDIF.




    " get file content
    get_file_content( uuid ).
    TRY.
        add_text_to_app_log_or_console( |file name: { ms_file-filename }| ).
      CATCH cx_bali_runtime.
        "handle exception
    ENDTRY.

    IF ms_file IS INITIAL.
      TRY.
          add_text_to_app_log_or_console( i_text = |record not found for uuid { lv_uuid_c36 }|
                                          i_type = if_bali_constants=>c_severity_error ).
        CATCH cx_bali_runtime.
          "handle exception
      ENDTRY.
      RETURN.
    ENDIF.

    IF ms_file-attachment IS INITIAL.
      TRY.
          add_text_to_app_log_or_console( i_text = |File not found|
                                          i_type = if_bali_constants=>c_severity_error ).
        CATCH cx_bali_runtime.
          "handle exception
      ENDTRY.
      RETURN.
    ENDIF.

    " get configuration
    get_batch_import_configuration( uuid ).
    TRY.
        add_text_to_app_log_or_console( |import object: { ms_configuration-objectname }| ).
      CATCH cx_bali_runtime.
        "handle exception
    ENDTRY.

    " read excel
    IF ms_configuration IS INITIAL.
      TRY.
          add_text_to_app_log_or_console( i_text = |configuration not found for this batch import record |
                                          i_type = if_bali_constants=>c_severity_error ).
        CATCH cx_bali_runtime.
          "handle exception
      ENDTRY.
      RETURN.
    ENDIF.
    get_data_from_xlsx(  ).

    " call function module
    IF mo_table IS  INITIAL.
      RETURN.
    ENDIF.
    process_logic(  ).



  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    me->out = out.

    DATA  et_parameters TYPE if_apj_rt_exec_object=>tt_templ_val  .

    et_parameters = VALUE #(
        ( selname = 'P_ID'
          kind = if_apj_dt_exec_object=>parameter
          sign = 'I'
          option = 'EQ'
          low = 'E1AC8CCB08981EDEA8DD9164372E8ACB' )
      ).

    TRY.

        if_apj_rt_exec_object~execute( it_parameters = et_parameters ).
        out->write( |Finished| ).

      CATCH cx_root INTO DATA(job_scheduling_exception).
        out->write( |Exception has occured: { job_scheduling_exception->get_text(  ) }| ).
    ENDTRY.
  ENDMETHOD.


  METHOD init_application_log.
    DATA : external_id TYPE c LENGTH 100.

    external_id = uuid.
*    cl_bali_log=>
    TRY.
        application_log = cl_bali_log=>create_with_header(
                               header = cl_bali_header_setter=>create( object = 'ZZ_ALO_DATAIMPORT'
                                                                       subobject = 'ZZ_ALO_TEXT_SUB'
                                                                       external_id = external_id ) ).
      CATCH cx_bali_runtime.
        "handle exception
    ENDTRY.
  ENDMETHOD.


  METHOD process_logic.
    DATA : ptab      TYPE abap_func_parmbind_tab,
           lo_data_e TYPE REF TO data.
    FIELD-SYMBOLS : <fs_t_e> TYPE STANDARD TABLE.

    ptab = VALUE #( ( name  = 'IO_DATA'
                  kind  = abap_func_exporting
                  value = REF #( mo_table ) )
                ( name  = 'IV_STRUC'
                  kind  = abap_func_exporting
                  value = REF #( ms_configuration-structname ) )
                ( name  = 'EO_DATA'
                  kind  = abap_func_importing
                  value = REF #( lo_data_e ) ) ).

    CALL FUNCTION ms_configuration-fmname PARAMETER-TABLE ptab.

    TRY.
        ASSIGN lo_data_e->* TO <fs_t_e>.
      CATCH cx_bali_runtime.
        "handle exception
    ENDTRY.


    " save log
    DATA(lv_has_error) = abap_false.
    LOOP AT <fs_t_e> ASSIGNING FIELD-SYMBOL(<fs_s_e>).
      TRY.
          add_text_to_app_log_or_console( i_text = |data line: { sy-tabix }, result: { <fs_s_e>-('type') }, message: {  <fs_s_e>-('message') }|
                                          i_type = COND #( WHEN <fs_s_e>-('type') = if_bali_constants=>c_severity_error
                                                           THEN if_bali_constants=>c_severity_warning
                                                           ELSE <fs_s_e>-('type') ) ).
          IF <fs_s_e>-('type') = if_bali_constants=>c_severity_error.
            lv_has_error = abap_true.
          ENDIF.
*          add_text_to_app_log_or_console( |{ <fs_s_e>-('OrderID') }   { <fs_s_e>-('DeliveryDate') }  { <fs_s_e>-('OrderQuantity') }| ).
        CATCH cx_bali_runtime.
          "handle exception
      ENDTRY.
    ENDLOOP.

    IF lv_has_error = abap_true.
      TRY.
          add_text_to_app_log_or_console( i_text = |Batch import processing contains errors|
                                          i_type = if_bali_constants=>c_severity_error ).
        CATCH cx_bali_runtime.
          "handle exception
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD save_job_info.
    IF sy-batch = abap_true.
      DATA(log_handle) = application_log->get_handle( ).
      DATA: jobname   TYPE cl_apj_rt_api=>ty_jobname.
      DATA: jobcount  TYPE cl_apj_rt_api=>ty_jobcount.
      DATA: catalog   TYPE cl_apj_rt_api=>ty_catalog_name.
      DATA: template  TYPE cl_apj_rt_api=>ty_template_name.
      TRY.
          cl_apj_rt_api=>get_job_runtime_info(
                              IMPORTING
                                ev_jobname        = jobname
                                ev_jobcount       = jobcount
                                ev_catalog_name   = catalog
                                ev_template_name  = template ).
        CATCH cx_apj_rt.
          "handle exception
      ENDTRY.

      MODIFY ENTITY zzr_zt_dtimp_files UPDATE FIELDS ( jobcount jobname loghandle ) WITH VALUE #( ( jobcount = jobcount jobname = jobname loghandle = log_handle uuid = uuid ) ).
      COMMIT ENTITIES.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
