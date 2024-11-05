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
    TYPES : BEGIN OF ts_sheet_content,
              uuid                  TYPE sysuuid_x16,
              SheetName             TYPE string,
              sheetnameup           TYPE string,
              RootNode              TYPE abap_boolean,
              StartLine             TYPE int4,
              StartColumn           TYPE c LENGTH 4,
              HasFieldnameLine      TYPE zzedtimphasfieldline,
              HasDescLine           TYPE zzedtimphasdescline,
              sheet                 TYPE REF TO if_xco_xlsx_ra_worksheet,
              data_table            TYPE REF TO data,
              data_table_with_child TYPE REF TO data,
              handle                TYPE REF TO cl_abap_tabledescr,
              handle_with_child     TYPE REF TO cl_abap_tabledescr,
            END OF ts_sheet_content,
            tt_sheet_content TYPE TABLE OF ts_sheet_content.
    DATA: ms_configuration TYPE STRUCTURE FOR READ RESULT zzi_zt_dtimp_conf\\Configuration,
          mt_structure     TYPE TABLE FOR READ RESULT zzi_zt_dtimp_conf\\Structure,
          mt_fields        TYPE TABLE FOR READ RESULT zzi_zt_dtimp_conf\\Field.

    DATA: ms_file TYPE STRUCTURE FOR READ RESULT zzr_zt_dtimp_files.

    DATA : mt_message_process TYPE TABLE FOR CREATE zzr_zt_dtimp_files\\Files\_Messages.
    DATA: mt_import_item TYPE TABLE FOR CREATE zzr_zt_dtimp_files\\Files\_ImportData.

    DATA: uuid TYPE sysuuid_x16.
    DATA: mo_table TYPE REF TO data.
    DATA: out TYPE REF TO if_oo_adt_classrun_out.
    DATA: application_log TYPE REF TO if_bali_log .

    DATA: mt_sheet_content TYPE tt_sheet_content.

    METHODS init_application_log
      RAISING
        cx_bali_runtime.
    METHODS get_file_content IMPORTING p_uuid TYPE sysuuid_x16
                             RAISING
*                                       cx_bali_runtime
                                       cx_uuid_error
                                       zzcx_dtimp_exception.
    METHODS get_batch_import_configuration IMPORTING p_uuid TYPE sysuuid_x16
                                           RAISING
                                                     zzcx_dtimp_exception.
*                                           RAISING
*                                                     cx_bali_runtime.
    METHODS get_uuid IMPORTING it_parameters TYPE if_apj_dt_exec_object=>tt_templ_val .
    METHODS get_data_from_xlsx
      RAISING
*        cx_bali_runtime
        cx_sy_create_data_error.
    METHODS get_sheets_from_xlsx
      RAISING
*        cx_bali_runtime
        cx_parameter_invalid_range
        cx_sy_struct_creation
        cx_sy_table_creation .

*    METHODS build_data_hierarchically.

    METHODS add_text_to_app_log_or_console IMPORTING i_text TYPE cl_bali_free_text_setter=>ty_text
                                                     i_type TYPE cl_bali_free_text_setter=>ty_severity OPTIONAL.
*                                           RAISING   cx_bali_runtime.
    METHODS add_except_to_log_or_console IMPORTING ix_exception TYPE REF TO cx_root.
*                                         RAISING   cx_bali_runtime..


    METHODS process_logic
      RAISING
        cx_uuid_error.
    METHODS save_messages.
    METHODS save_job_info
      RAISING
        cx_apj_rt.
    METHODS save_process_status
      IMPORTING
        i_type TYPE symsgty.

    METHODS create_element_handle
      IMPORTING
                i_type                TYPE c
                i_length              TYPE i
                i_decimal             TYPE i
      RETURNING VALUE(ro_element_ref) TYPE REF TO cl_abap_datadescr
      RAISING   cx_parameter_invalid_range.
    METHODS build_hierarchical_data_model.
    METHODS append_child_model_as_node
      CHANGING
        current_sheet_config TYPE ts_sheet_content.
    METHODS append_child_data_as_node
      IMPORTING
        where_conditions      TYPE string_table
      CHANGING
        current_sheet_content TYPE ts_sheet_content.
    METHODS save_data
      RAISING
        zzcx_dtimp_exception.

ENDCLASS.



CLASS ZZCL_DTIMP_PROCESS_GENERIC IMPLEMENTATION.


  METHOD add_except_to_log_or_console.
    TRY.
*        IF I_type = if_bali_constants=>c_severity_error.
        save_process_status(
           i_type = if_bali_constants=>c_severity_error
         ).



*        ENDIF.
        IF sy-batch = abap_true.

*          DATA(application_log_free_text) = cl_bali_free_text_setter=>create(
*                                 severity = COND #( WHEN i_type IS NOT INITIAL
*                                                    THEN i_type
*                                                    ELSE if_bali_constants=>c_severity_status )
*                                 text     = i_text ).
          DATA(l_ref) = cl_bali_exception_setter=>create( severity = if_bali_constants=>c_severity_error
                                                           exception = ix_exception ).

          l_ref->set_detail_level( detail_level = '1' ).
          application_log->add_item( item = l_ref ).
          cl_bali_log_db=>get_instance( )->save_log( log = application_log
                                                     assign_to_current_appl_job = abap_true ).

        ELSE.
*          out->write( |sy-batch = abap_false | ).
          out->write( ix_exception->get_longtext(  ) ).
        ENDIF.
      CATCH cx_bali_runtime INTO DATA(lx_bali_runtime).
    ENDTRY.


  ENDMETHOD.


  METHOD add_text_to_app_log_or_console.
    TRY.
        IF I_type = if_bali_constants=>c_severity_error.
          save_process_status(
             i_type = i_type
           ).



        ENDIF.
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


  METHOD append_child_data_as_node.
    DATA : where_conditions_child TYPE TABLE OF string.
    FIELD-SYMBOLS : <ft_table> TYPE STANDARD TABLE.


*    CREATE DATA current_sheet_content-data_table_with_child TYPE HANDLE current_sheet_content-handle_with_child.
*    current_sheet_content-data_table_with_child->* = CORRESPONDING #( current_sheet_content-data_table->* ).

    LOOP AT current_sheet_content-data_table_with_child->* ASSIGNING FIELD-SYMBOL(<fs_current_line>) WHERE (where_conditions).

      "Get Child Node Field
      LOOP AT mt_sheet_content ASSIGNING FIELD-SYMBOL(<sheet_content>) WHERE sheetnameup = current_sheet_content-sheetname.


        ASSIGN COMPONENT <sheet_content>-sheetname OF STRUCTURE <fs_current_line> TO <ft_table>.
        IF sy-subrc = 0.
          CLEAR: where_conditions_child.
*          loop at mt_fields ASSIGNING FIELD-SYMBOL(<fs_ff>) WHERE UUIDStruc = <sheet_content>-uuid.
*            append |{ <fs_ff>-FieldName } = '{ <fs_current_line>-(<fs_ff>-ForeignField) }' | to where_conditions_child.
*          ENDLOOP.

          where_conditions_child = VALUE #(
            FOR foreign_field IN mt_fields WHERE ( UUIDStruc = <sheet_content>-uuid
                                                   AND
                                                   IsForeignField = abap_true )
            ( |{ foreign_field-FieldName } = '{ <fs_current_line>-(foreign_field-ForeignField) }' |  )
            ( |AND| )
          ).
          IF lines( where_conditions_child ) > 1.
            DELETE where_conditions_child INDEX lines( where_conditions_child ).
          ENDIF.

          append_child_data_as_node(
            EXPORTING
                where_conditions = where_conditions_child
            CHANGING
                current_sheet_content = <sheet_content>
          ).

*        <fs_current_line>-(<sheet_content>-sheetname) = <sheet_content>-data_table_with_child->*[ (where_conditions_child) ].
          LOOP AT <sheet_content>-data_table_with_child->* ASSIGNING FIELD-SYMBOL(<fs_child_line>) WHERE (where_conditions_child).
*            <fs_current_line>-(<sheet_content>-sheetname) = VALUE #( base <fs_current_line>-(<sheet_content>-sheetname) ( <fs_child_line> ) ).
            APPEND <fs_child_line> TO <ft_table> .
          ENDLOOP.
*        <fs_current_line>-(<sheet_content>-sheetname) = FILTER #( <sheet_content>-data_table_with_child->* WHERE (where_conditions_child)  ).

        ENDIF.


      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD append_child_model_as_node.
    "Get Structure Type Handle of current sheet
    DATA : lo_current_structure TYPE REF TO cl_abap_structdescr.
    lo_current_structure ?=  current_sheet_config-handle->get_table_line_type(  ).

    " Add Child Sheet Table type as child node of current sheet structure type
    DATA(lt_components) = lo_current_structure->get_components(  ).
    LOOP AT mt_sheet_content ASSIGNING FIELD-SYMBOL(<sheet_content>) WHERE sheetnameup = current_sheet_config-sheetname.
      append_child_model_as_node(
        CHANGING
            current_sheet_config = <sheet_content> ).
      APPEND VALUE cl_abap_structdescr=>component(  name = <sheet_content>-sheetname type = <sheet_content>-handle_with_child  ) TO lt_components.
    ENDLOOP.

    " replace original table type
    DATA(table_ref) = cl_abap_tabledescr=>get(
        p_line_type  = cl_abap_structdescr=>get( lt_components )
        p_table_kind = cl_abap_tabledescr=>tablekind_std ).
    current_sheet_config-handle_with_child = table_ref.

    CREATE DATA current_sheet_config-data_table_with_child TYPE HANDLE current_sheet_config-handle_with_child.
    current_sheet_config-data_table_with_child->* = CORRESPONDING #( current_sheet_config-data_table->* ).

  ENDMETHOD.


  METHOD build_hierarchical_data_model.
    DATA : ls_root_content      TYPE ts_sheet_content.


    ASSIGN mt_sheet_content[ rootnode = abap_true ] TO FIELD-SYMBOL(<root_sheet_content>).

    " Build hierarchical model and init data
    add_text_to_app_log_or_console( |Building hierarchical model and init data.| ).
    append_child_model_as_node(
        CHANGING
            current_sheet_config = <root_sheet_content> ).

    " Build hierarchical data table
    add_text_to_app_log_or_console( |Building hierarchical data table.| ).
    append_child_data_as_node(
        EXPORTING
            where_conditions = VALUE #(  )
        CHANGING
            current_sheet_content = <root_sheet_content> ).


  ENDMETHOD.


  METHOD create_element_handle.
    CASE i_type.
      WHEN 'C'.
        RETURN cl_abap_elemdescr=>get_c( i_length ).
      WHEN 'P'.
        RETURN cl_abap_elemdescr=>get_p( p_length = i_length p_decimals = i_decimal ).
      WHEN 'N'.
        RETURN cl_abap_elemdescr=>get_n( i_length ).
      WHEN 'D'.
        RETURN cl_abap_elemdescr=>get_d(  ).
      WHEN 'S'.
        RETURN cl_abap_elemdescr=>get_string(  ).
    ENDCASE.

*    ( name = 'A' type = cl_abap_elemdescr=>get_string( ) )
*          ( name = 'B' type = cl_abap_elemdescr=>get_i( ) )
*          ( name = 'C' type = cl_abap_elemdescr=>get_c( 5 ) )
*          ( name = 'D' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'LAND1' ) ) )
  ENDMETHOD.


  METHOD get_batch_import_configuration.

    READ ENTITIES OF zzi_zt_dtimp_conf
    ENTITY Configuration ALL FIELDS WITH VALUE #( (  %key-uuid = ms_file-uuidconf ) )
        RESULT FINAL(lt_configuration)
        ENTITY Configuration BY \_Structures ALL FIELDS WITH VALUE #( ( %key-uuid = ms_file-UuidConf ) )
        RESULT FINAL(lt_structure).

    READ ENTITIES OF zzi_zt_dtimp_conf
    ENTITY Structure  BY \_Fields ALL FIELDS WITH CORRESPONDING #( lt_structure )
    RESULT FINAL(lt_fields).

    ms_configuration = lt_configuration[ 1 ].
    mt_structure = lt_structure.
    mt_fields = lt_fields.

    SORT mt_fields BY Sequence.

*    TRY.
    add_text_to_app_log_or_console( |import object: { ms_configuration-objectname }| ).
*      CATCH cx_bali_runtime.
    "handle exception
*    ENDTRY.

    " read excel
    IF ms_configuration IS INITIAL.
*      TRY.
*      add_text_to_app_log_or_console( i_text = |configuration not found for this batch import record |
*                                      i_type = if_bali_constants=>c_severity_error ).
*        CATCH cx_bali_runtime.
      "handle exception
*      ENDTRY.
      RAISE EXCEPTION TYPE zzcx_dtimp_exception
        EXPORTING
          textid = VALUE #(
              msgid = '00'
              msgno = '000'
              attr1 = |configuration not found for this batch import record |
          ).
*      RETURN.
    ENDIF.

    IF mt_structure IS INITIAL.
*      TRY.
*      add_text_to_app_log_or_console( i_text = |structure configuration not found for this batch import record |
*                                      i_type = if_bali_constants=>c_severity_error ).
*        CATCH cx_bali_runtime.
      "handle exception
*      ENDTRY.
      RAISE EXCEPTION TYPE zzcx_dtimp_exception
        EXPORTING
          textid = VALUE #(
              msgid = '00'
              msgno = '000'
              attr1 = |structure configuration not found for this batch import record |
          ).
*      RETURN.
    ENDIF.

    IF mt_fields IS INITIAL.
*      TRY.
*      add_text_to_app_log_or_console( i_text = |field configuration not found for this batch import record |
*                                      i_type = if_bali_constants=>c_severity_error ).
*        CATCH cx_bali_runtime.
      "handle exception
*      ENDTRY.
      RAISE EXCEPTION TYPE zzcx_dtimp_exception
        EXPORTING
          textid = VALUE #(
              msgid = '00'
              msgno = '000'
              attr1 = |field configuration not found for this batch import record |
          ).
*      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_data_from_xlsx.


*    TRY.
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
*      CATCH cx_sy_create_data_error INTO DATA(lx_sy_create_data_error).
*        TRY.
    add_text_to_app_log_or_console( i_text = |Data structure of Import Object not found, please contact Administrator|
                                    i_type = if_bali_constants=>c_severity_error ).
*          CATCH cx_bali_runtime.
    "handle exception
*        ENDTRY.
*        return.
*        RAISE EXCEPTION TYPE cx_bali_runtime.
*    ENDTRY.





  ENDMETHOD.


  METHOD get_file_content.
*    IF uuid IS INITIAL.
**      TRY.
**      add_text_to_app_log_or_console( i_text = |record not found for uuid { uuid }|
**                                      i_type = if_bali_constants=>c_severity_error ).
**        CATCH cx_bali_runtime.
*      "handle exception
**      ENDTRY.
*       RAISE EXCEPTION TYPE zzcx_dtimp_exception
*        EXPORTING
*            textid = VALUE #(
*                msgid = '00'
*                msgno = '000'
*                attr1 = |record not found for uuid { uuid }|
*            ).
*
**      RETURN.
*    ENDIF.
*    TRY.
    cl_system_uuid=>convert_uuid_x16_static( EXPORTING uuid = uuid IMPORTING uuid_c36 = DATA(lv_uuid_c36)  ).
*      CATCH cx_uuid_error.
    "handle exception
*    ENDTRY.
*    TRY.
    add_text_to_app_log_or_console( |process batch import uuid { lv_uuid_c36 }| ).
*      CATCH cx_bali_runtime.
    "handle exception
*    ENDTRY.

    READ ENTITY zzr_zt_dtimp_files ALL FIELDS WITH VALUE #( (  %key-uuid = p_uuid ) )
        RESULT FINAL(lt_file).
    ms_file = lt_file[ 1 ].

    IF ms_file IS INITIAL.
*      TRY.
*      add_text_to_app_log_or_console( i_text = |record not found for uuid { lv_uuid_c36 }|
*                                      i_type = if_bali_constants=>c_severity_error ).
*        CATCH cx_bali_runtime.
      "handle exception
*      ENDTRY.
      RAISE EXCEPTION TYPE zzcx_dtimp_exception
        EXPORTING
          textid = VALUE #(
              msgid = '00'
              msgno = '000'
              attr1 = |record not found for uuid { lv_uuid_c36 }|
          ).
*      RETURN.
    ENDIF.

*    TRY.
    add_text_to_app_log_or_console( |file name: { ms_file-filename }| ).
*      CATCH cx_bali_runtime.
    "handle exception
*    ENDTRY.



    IF ms_file-attachment IS INITIAL.
*      TRY.
*      add_text_to_app_log_or_console( i_text = |File not found|
*                                      i_type = if_bali_constants=>c_severity_error ).
*        CATCH cx_bali_runtime.
      "handle exception
*      ENDTRY.
      RAISE EXCEPTION TYPE zzcx_dtimp_exception
        EXPORTING
          textid = VALUE #(
              msgid = '00'
              msgno = '000'
              attr1 = |File not found|
          ).
*      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_sheets_from_xlsx.

    add_text_to_app_log_or_console( |Processing excel sheets.| ).

    " read xlsx object
    DATA(lo_document) = xco_cp_xlsx=>document->for_file_content( ms_file-Attachment ).

    DATA ls_sheet_content TYPE ts_sheet_content.
    LOOP AT mt_structure ASSIGNING FIELD-SYMBOL(<fs_conf_excel_s>).
      CLEAR ls_sheet_content.
      ls_sheet_content = CORRESPONDING #( <fs_conf_excel_s> ).

      ls_sheet_content-sheetname = <fs_conf_excel_s>-SheetName.
      ls_sheet_content-sheet = lo_document->read_access(  )->get_workbook(  )->worksheet->for_name( ls_sheet_content-sheetname ).
*      DATA(lv_sheet_exists) = ls_sheet_content-sheet->exists(  ).
      IF ls_sheet_content-sheet->exists(  ) = abap_false.

        add_text_to_app_log_or_console( i_text = |Excel sheet { ls_sheet_content-sheetname } does not exist in the data file|
                                         i_type = if_bali_constants=>c_severity_warning ).
        CONTINUE.
      ENDIF.

      "Get Data from Excel
      "create internal table from fields configuration
      ls_sheet_content-handle = cl_abap_tabledescr=>get(
        p_line_type  = cl_abap_structdescr=>get(
           VALUE #( FOR field IN mt_fields WHERE ( UUIDStruc = <fs_conf_excel_s>-uuid )
            ( name = field-FieldName
              type = create_element_handle(
                        i_type = field-FieldType1
                        i_length = field-FieldLength
                        i_decimal = field-FieldDecimal
                     )
            )
           )
         )
        p_table_kind = cl_abap_tabledescr=>tablekind_std ).



      CREATE DATA ls_sheet_content-data_table TYPE HANDLE ls_sheet_content-handle.


      DATA(lo_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
      )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( <fs_conf_excel_s>-StartLine )
      )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( <fs_conf_excel_s>-StartColumn )
      )->get_pattern(  ).

      ls_sheet_content-sheet->select( lo_pattern )->row_stream(  )->operation->write_to( ls_sheet_content-data_table )->execute(  ).
      APPEND ls_sheet_content TO mt_sheet_content.

    ENDLOOP.

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
    TRY.
        init_application_log(  ).
      CATCH cx_bali_runtime INTO DATA(lx_init_application_log).
        add_except_to_log_or_console(
            ix_exception = lx_init_application_log
        ).
        RETURN.
        "handle exception
    ENDTRY.

    " save job info to ZZC_ZT_DTIMP_FILES
    TRY.
        save_job_info(  ).
      CATCH cx_apj_rt INTO DATA(lx_save_job_info).
        add_except_to_log_or_console(
         ix_exception = lx_save_job_info
     ).
        RETURN.
        "handle exception
    ENDTRY.

    " get file content
    TRY.
        get_file_content( uuid ).
      CATCH cx_uuid_error  zzcx_dtimp_exception INTO DATA(lx_get_file_content).
        "handle exception
        add_except_to_log_or_console(
            ix_exception = lx_get_file_content
        ).
        RETURN.
    ENDTRY.

    TRY.
        " get configuration
        get_batch_import_configuration( uuid ).
      CATCH  zzcx_dtimp_exception INTO DATA(lx_get_batchimport_conf).
        "handle exception
        add_except_to_log_or_console(
            ix_exception = lx_get_file_content
        ).
        RETURN.
    ENDTRY.


*    get_data_from_xlsx(  ).

    TRY.
        get_sheets_from_xlsx(  ).
      CATCH cx_bali_runtime cx_parameter_invalid_range cx_sy_struct_creation cx_sy_table_creation INTO DATA(lx_sheet_reader_exception).
        add_except_to_log_or_console(
          ix_exception = lx_sheet_reader_exception
        ).
        RETURN.
    ENDTRY.

    build_hierarchical_data_model(  ).

    " save data to data table
    TRY.
        save_data(  ).
      CATCH zzcx_dtimp_exception INTO DATA(lx_save_data).
        add_except_to_log_or_console(
            ix_exception = lx_save_data
        ).
        RETURN.
        "handle exception
    ENDTRY.


    " process custom logic

    TRY.
        process_logic(  ).
      CATCH cx_uuid_error INTO DATA(lx_process_logic).
        add_except_to_log_or_console(
            ix_exception = lx_process_logic
        ).
        RETURN.
        "handle exception
    ENDTRY.

    save_messages(  ).


  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    me->out = out.

    DATA  et_parameters TYPE if_apj_rt_exec_object=>tt_templ_val  .

    et_parameters = VALUE #(
        ( selname = 'P_ID'
          kind = if_apj_dt_exec_object=>parameter
          sign = 'I'
          option = 'EQ'
          low = '5D6C37D8F3541EEFA5D2F3CAC90E28F3' )
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
*    TRY.
    application_log = cl_bali_log=>create_with_header(
                           header = cl_bali_header_setter=>create( object = 'ZZ_ALO_DATAIMPORT'
                                                                   subobject = 'ZZ_ALO_TEXT_SUB'
                                                                   external_id = external_id ) ).
*      CATCH cx_bali_runtime.
*        "handle exception
*    ENDTRY.
  ENDMETHOD.


  METHOD process_logic.
    DATA : ptab      TYPE abap_func_parmbind_tab,
           lo_data_e TYPE REF TO data.
    FIELD-SYMBOLS : <fs_t_e> TYPE STANDARD TABLE.


    DATA : lo_process_class      TYPE REF TO zzif_process_data,

           lt_message            TYPE zzt_dmp_data_list,
           ls_message_for_create TYPE STRUCTURE FOR CREATE zzr_zt_dtimp_files\\Files\_Messages.

    add_text_to_app_log_or_console( |Processing custom logic.| ).
    CREATE OBJECT lo_process_class TYPE (ms_configuration-classname).
    lo_process_class->process(
         EXPORTING
             io_data = mt_sheet_content[ rootnode = abap_true ]-data_table_with_child
*             iv_structure = ms_configuration-Structname
             io_data_handle = mt_sheet_content[ rootnode = abap_true ]-handle_with_child
         IMPORTING
             eo_data = lo_data_e
             et_message = lt_message

     ).

    " process message

    LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<fs_message_h>).

      LOOP AT <fs_message_h>-message_list ASSIGNING FIELD-SYMBOL(<fs_message>).
*        TRY.
        ls_message_for_create = VALUE #(
*                            uuid = mt_version_item[ Line = <fs_item>-line ]-uuid
                       uuid = uuid
      %target = VALUE #( (
*                                   DataJson = ls_data-data_json
                            %cid = cl_system_uuid=>create_uuid_c22_static(  )
                            line = <fs_message_h>-line
                            type = <fs_message>-type
                            id = <fs_message>-id
                            msgnumber = <fs_message>-number
                            message = <fs_message>-message
                            messagev1 = <fs_message>-message_v1
                            messagev2 = <fs_message>-message_v2
                            messagev3 = <fs_message>-message_v3
                            messagev4 = <fs_message>-message_v4

                            %control = VALUE #(
                               line = if_abap_behv=>mk-on
                               type = if_abap_behv=>mk-on
                               id = if_abap_behv=>mk-on
                               msgnumber = if_abap_behv=>mk-on
                               message = if_abap_behv=>mk-on
                               messagev1 = if_abap_behv=>mk-on
                               messagev2 = if_abap_behv=>mk-on
                               messagev3 = if_abap_behv=>mk-on
                               messagev4 = if_abap_behv=>mk-on
                             )
                         ) )
    ).
*          CATCH cx_uuid_error

        "handle exception
*        ENDTRY.
        APPEND ls_message_for_create TO mt_message_process.
      ENDLOOP.
    ENDLOOP.






    " save log
*    DATA(lv_has_error) = abap_false.
*    LOOP AT <fs_t_e> ASSIGNING FIELD-SYMBOL(<fs_s_e>).
*      TRY.
*          add_text_to_app_log_or_console( i_text = |data line: { sy-tabix }, result: { <fs_s_e>-('type') }, message: {  <fs_s_e>-('message') }|
*                                          i_type = COND #( WHEN <fs_s_e>-('type') = if_bali_constants=>c_severity_error
*                                                           THEN if_bali_constants=>c_severity_warning
*                                                           ELSE <fs_s_e>-('type') ) ).
*          IF <fs_s_e>-('type') = if_bali_constants=>c_severity_error.
*            lv_has_error = abap_true.
*          ENDIF.
**          add_text_to_app_log_or_console( |{ <fs_s_e>-('OrderID') }   { <fs_s_e>-('DeliveryDate') }  { <fs_s_e>-('OrderQuantity') }| ).
*        CATCH cx_bali_runtime.
*          "handle exception
*      ENDTRY.
*    ENDLOOP.
*
*    IF lv_has_error = abap_true.
*      TRY.
*          add_text_to_app_log_or_console( i_text = |Batch import processing contains errors|
*                                          i_type = if_bali_constants=>c_severity_error ).
*        CATCH cx_bali_runtime.
*          "handle exception
*      ENDTRY.
*    ENDIF.
  ENDMETHOD.


  METHOD save_data.
    DATA : ls_import_item       TYPE STRUCTURE FOR CREATE zzr_zt_dtimp_files\\Files\_ImportData.
    DATA : lv_line TYPE i.
    FIELD-SYMBOLS : <ft_table> TYPE STANDARD TABLE.

    add_text_to_app_log_or_console( |Saving original data.| ).
    ASSIGN mt_sheet_content[ rootnode = abap_true ]-data_table_with_child->* TO <ft_table>.
    IF sy-subrc = 0.
      IF lines( <ft_table> ) > 0.

        LOOP AT <ft_table> ASSIGNING FIELD-SYMBOL(<fs_line>).
          lv_line = sy-tabix.
          ls_import_item = VALUE #(
                                     uuid = ms_file-uuid
                                     %target = VALUE #( (
                                         DataJson = /ui2/cl_json=>serialize( data = <fs_line> )
                                         Line = lv_line
                                         %cid = lv_line
                                         %control = VALUE #(
                                            Line = if_abap_behv=>mk-on
                                            DataJson = if_abap_behv=>mk-on
                                          )
                                      ) )

                                     ).
          APPEND ls_import_item TO mt_import_item.
        ENDLOOP.

        IF mt_import_item IS NOT INITIAL.
          MODIFY ENTITIES OF zzr_zt_dtimp_files
              ENTITY Files
              CREATE BY \_ImportData
              FROM mt_import_item
              MAPPED DATA(ls_mapped)
              FAILED DATA(ls_failed)
              REPORTED DATA(ls_reported).
          IF ls_failed-importdata IS NOT INITIAL.
*            TRY.
*                add_text_to_app_log_or_console( i_text = CONV text200(  ls_reported-importitem[ 1 ]-%msg->if_message~get_longtext(  ) )
*                                         i_type = if_bali_constants=>c_severity_warning ).
*              CATCH cx_bali_runtime.
*            ENDTRY.
            IF lines( ls_reported-importdata ) > 0.
              RAISE EXCEPTION TYPE zzcx_dtimp_exception
                EXPORTING
                  textid = VALUE #(
                      msgid = '00'
                      msgno = '000'
                      attr1 = ls_reported-importdata[ 1 ]-%msg->if_message~get_longtext(  )
                   ).
            ELSE.
              RAISE EXCEPTION TYPE zzcx_dtimp_exception
                EXPORTING
                  textid = VALUE #(
                      msgid = '00'
                      msgno = '000'
                      attr1 = | Error when saving data: { ls_failed-importdata[ 1 ]-%fail-cause } |
                   ).
            ENDIF.

          ELSE.
            COMMIT ENTITIES.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.



  ENDMETHOD.


  METHOD save_job_info.
    IF sy-batch = abap_true.
      DATA(log_handle) = application_log->get_handle( ).
      DATA: jobname   TYPE cl_apj_rt_api=>ty_jobname.
      DATA: jobcount  TYPE cl_apj_rt_api=>ty_jobcount.
      DATA: catalog   TYPE cl_apj_rt_api=>ty_catalog_name.
      DATA: template  TYPE cl_apj_rt_api=>ty_template_name.
*      TRY.
      cl_apj_rt_api=>get_job_runtime_info(
                          IMPORTING
                            ev_jobname        = jobname
                            ev_jobcount       = jobcount
                            ev_catalog_name   = catalog
                            ev_template_name  = template ).
*        CATCH cx_apj_rt.
      "handle exception
*      ENDTRY.

      MODIFY ENTITY zzr_zt_dtimp_files UPDATE FIELDS ( jobcount jobname loghandle ) WITH VALUE #( ( jobcount = jobcount jobname = jobname loghandle = log_handle uuid = uuid ) ).
      COMMIT ENTITIES.
    ENDIF.
  ENDMETHOD.


  METHOD save_messages.
    add_text_to_app_log_or_console( |Saving messages.| ).
    IF ms_file IS NOT INITIAL.
      MODIFY ENTITIES OF zzr_zt_dtimp_files
          ENTITY Files
          UPDATE FIELDS ( Status )
          WITH VALUE #( ( Status = 'S'
                          %control-Status = if_abap_behv=>mk-on
                          %key-uuid = uuid
                           ) )
          CREATE BY \_Messages
          FROM mt_message_process
          MAPPED DATA(ls_mapped)
          FAILED DATA(ls_failed)
          REPORTED DATA(ls_reported).
      IF ls_failed-message IS NOT INITIAL.
*        TRY.
        add_text_to_app_log_or_console( i_text = CONV cl_bali_free_text_setter=>ty_text(  ls_reported-message[ 1 ]-%msg->if_message~get_longtext(  ) )
                                 i_type = if_bali_constants=>c_severity_warning ).
*          CATCH cx_bali_runtime.
*        ENDTRY.
      ELSE.
        COMMIT ENTITIES.
*        TRY.
        add_text_to_app_log_or_console( i_text = | Check Navigation Messages of this record. |
                                        ).
*          CATCH cx_bali_runtime.
        "handle exception
*        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save_process_status.
    MODIFY ENTITIES OF zzr_zt_dtimp_files
        ENTITY Files
        UPDATE FIELDS ( Status )
        WITH VALUE #( ( Status =  i_type
                        %control-Status = if_abap_behv=>mk-on
                        %key-uuid = uuid
                         ) )
        MAPPED DATA(ls_mapped)
        FAILED DATA(ls_failed)
        REPORTED DATA(ls_reported).
    IF ls_failed-files IS NOT INITIAL.
*      TRY.
      add_text_to_app_log_or_console( i_text = CONV cl_bali_free_text_setter=>ty_text(  ls_reported-files[ 1 ]-%msg->if_message~get_longtext(  ) )
                               i_type = if_bali_constants=>c_severity_warning ).
*        CATCH cx_bali_runtime.
*      ENDTRY.
    ELSE.
      COMMIT ENTITIES.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
