CLASS lhc_field DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS determineTemplatebyField FOR DETERMINE ON SAVE
      IMPORTING keys FOR Field~determineTemplatebyField.
    METHODS checkFields FOR VALIDATE ON SAVE
      IMPORTING keys FOR Field~checkFields.

ENDCLASS.

CLASS lhc_field IMPLEMENTATION.

  METHOD determineTemplatebyField.
    DATA : lv_line   TYPE i,
           lv_column TYPE c.
    " Read keys of configuration by structure
    READ ENTITIES OF zzi_zt_dtimp_conf
        IN LOCAL MODE
            ENTITY Field BY \_Conf ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(lt_files).

    " Read Sheet Configuration and Field Configuration
    READ ENTITIES OF zzi_zt_dtimp_conf
     IN LOCAL MODE
*        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_files)

         ENTITY Configuration BY \_Structures
             ALL FIELDS WITH CORRESPONDING #( lt_files )
      RESULT DATA(lt_structures).

    IF lines( lt_structures ) > 0.
      READ ENTITIES OF zzi_zt_dtimp_conf
          IN LOCAL MODE
              ENTITY Structure BY \_Fields
                  ALL FIELDS WITH CORRESPONDING #( lt_structures )
       RESULT DATA(lt_fields).
      IF lines( lt_fields ) > 0.
        SORT lt_fields BY Sequence.
      ENDIF.
    ENDIF.


    DATA : lv_sheet_count TYPE i.

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_file>).
      " Construction of Excel
      DATA(lo_write_access) = xco_cp_xlsx=>document->empty( )->write_access( ).

      lv_sheet_count = 0.
      LOOP AT lt_structures ASSIGNING FIELD-SYMBOL(<fs_structure>) WHERE UUIDConf = <fs_file>-uuid.
        lv_sheet_count += 1.
        " Create new sheet or change sheet name when in first sheet
        IF lv_sheet_count = 1.
          IF lo_write_access->get_workbook(  )->worksheet->at_position( 1 )->exists(  ) EQ abap_true.
            DATA(lo_worksheet) = lo_write_access->get_workbook(  )->worksheet->at_position( 1 )->set_name( CONV string( <fs_structure>-SheetName ) ).
          ELSE.
            lo_worksheet = lo_write_access->get_workbook( )->add_new_sheet( CONV string( <fs_structure>-SheetName ) ).
          ENDIF.
        ELSE.
          lo_worksheet = lo_write_access->get_workbook( )->add_new_sheet( CONV string( <fs_structure>-SheetName ) ).
        ENDIF.



        " Build Field Name and Description Table.
        lv_line = 1.
        IF <fs_structure>-HasFieldnameLine = abap_true.
          lv_column = <fs_structure>-StartColumn.
          DATA(lo_cursor) = lo_worksheet->cursor(
            io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( lv_column )
            io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_line )
          ).
          LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE UUIDStruc = <fs_structure>-uuid.
            "


            " Write Field Name
            lo_cursor->get_cell( )->value->write_from( <fs_field>-FieldName ).

            " Add Column
            lo_cursor->move_right(  ).
*            lv_column = zzcl_odata_utils=>get_alphabet(
*              EXPORTING
*                  iv_alphabet = lv_column
*                  iv_number = 1
*             ).
          ENDLOOP.
        ENDIF.

        lv_line += 1.
        IF <fs_structure>-HasDescLine = abap_true.
          lv_column = <fs_structure>-StartColumn.
          lo_cursor = lo_worksheet->cursor(
            io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( lv_column )
            io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_line )
          ).
          LOOP AT lt_fields ASSIGNING <fs_field> WHERE UUIDStruc = <fs_structure>-uuid.


            " Write Field Description
            lo_cursor->get_cell( )->value->write_from( <fs_field>-FieldDescription ).

            " Add Column
            lo_cursor->move_right(  ).

          ENDLOOP.
        ENDIF.


      ENDLOOP.
      <fs_file>-Template = lo_write_access->get_file_content(  ).
      <fs_file>-FileName = |{ <fs_file>-Object }_Template.xlsx|.
      <fs_file>-Mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
      <fs_file>-MimeTypeForTemplate = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
    ENDLOOP.


    MODIFY ENTITIES OF zzi_zt_dtimp_conf
        IN LOCAL MODE
            ENTITY Configuration UPDATE FIELDS ( Template FileName Mimetype MimeTypeForTemplate )
            WITH VALUE #( FOR conf IN lt_files (
                %tky = conf-%tky
                Template = conf-Template
                FileName = conf-FileName
                Mimetype = conf-Mimetype
                MimeTypeForTemplate = conf-MimeTypeForTemplate
            ) )
        REPORTED DATA(ls_reported)
        FAILED DATA(ls_failed).
  ENDMETHOD.

  METHOD checkFields.
    " Check Mandatory Fields
    DATA permission_request TYPE STRUCTURE FOR PERMISSIONS REQUEST zzr_zt_dtimp_field.
    DATA(description_permission_request) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( REF #( permission_request-%field ) ) ).
    DATA(components_permission_request) = description_permission_request->get_components(  ).

    DATA reported_field LIKE LINE OF reported-field.



    LOOP AT components_permission_request INTO DATA(component_permission_request).
      permission_request-%field-(component_permission_request-name) = if_abap_behv=>mk-on.
    ENDLOOP.

*    GET PERMISSIONS ONLY GLOBAL FEATURES ENTITY zhdr_dmp_t_import REQUEST permission_request
*        RESULT DATA(permission_result).

    " Get current field values
    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
    ENTITY Field
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(fields).
    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).


      APPEND VALUE #( %tky  = <field>-%tky
          %state_area = if_abap_behv=>state_area_all ) TO reported-field.

      GET PERMISSIONS ONLY FEATURES ENTITY zzr_zt_dtimp_field
                FROM VALUE #( ( %tky = <field>-%tky ) )
                REQUEST permission_request
                RESULT DATA(permission_result_instance)
                FAILED DATA(failed_permission_result)
                REPORTED DATA(reported_permission_result).

      LOOP AT components_permission_request INTO component_permission_request.

        "permission result for instances (field ( features : instance ) MandFieldInstfeat;) is stored in an internal table.
        "So we have to retrieve the information for the current entity
        "whereas the global information (field ( mandatory ) MandFieldBdef;) is stored in a structure
        IF ( permission_result_instance-instances[ uuid = <field>-uuid ]-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory OR
             permission_result_instance-global-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory ) AND
             <field>-(component_permission_request-name) IS INITIAL.

          APPEND VALUE #( %tky = <field>-%tky ) TO failed-field.

          "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
          "add the value via assigning value to the field of a structure

          CLEAR reported_field.
          reported_field-%tky = <field>-%tky.
          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
          reported_field-%msg = new_message( id       = '00'
                                                         number   = 000
                                                         severity = if_abap_behv_message=>severity-error
                                                         v1       = |{ component_permission_request-name }|
*                                                         v2       = | with key: { <data>-uuid } is required | ).
                                                         v2       = |is required.| ).
          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <field>-%tky-%is_draft
                                                                    uuid = <field>-UUIDConf )
                                            structure =        VALUE #( %is_draft = <field>-%tky-%is_draft
                                                                    uuid = <field>-UUIDStruc )
                                             ).
          reported_field-%state_area = 'VAL_CONFIGURATION'.
          APPEND reported_field  TO reported-field.

        ENDIF.
      ENDLOOP.

      " Check Field Sheet Name of Parent



    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_structure DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS:
*    checkRootNodeOnSave FOR VALIDATE ON SAVE
*      IMPORTING keys FOR Structure~checkRootNodeOnSave,
      determineTemplatebyStructure FOR DETERMINE ON SAVE
        IMPORTING keys FOR Structure~determineTemplatebyStructure,
*      checkFields FOR VALIDATE ON SAVE
*        IMPORTING keys FOR Structure~checkFields,
*      checkSubFields FOR VALIDATE ON SAVE
*        IMPORTING keys FOR Structure~checkSubFields,
      checkStructure FOR VALIDATE ON SAVE
        IMPORTING keys FOR Structure~checkStructure,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR Structure RESULT result.

ENDCLASS.

CLASS lhc_structure IMPLEMENTATION.

*  METHOD checkRootNodeOnSave.
*    DATA : reported_conf LIKE LINE OF reported-configuration.
*
*
*    " Read keys of configuration by structure
*    READ ENTITIES OF zzi_zt_dtimp_conf
*        IN LOCAL MODE
*            ENTITY Structure BY \_Conf ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_files).
*
*    APPEND VALUE #( %tky  = lt_files[ 1 ]-%tky
*        %state_area = if_abap_behv=>state_area_all ) TO reported-configuration.
*
*    " Read Sheet Configuration and Field Configuration
*    READ ENTITIES OF zzi_zt_dtimp_conf
*     IN LOCAL MODE
**        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
**        RESULT DATA(lt_files)
*
*         ENTITY Configuration BY \_Structures
*             ALL FIELDS WITH CORRESPONDING #( lt_files )
*      RESULT DATA(lt_structures).
*    IF xsdbool( line_exists( lt_structures[ RootNode = abap_true ] ) ) = abap_false .
*      APPEND VALUE #( %tky = lt_files[ 1 ]-%tky ) TO failed-configuration.
*
*      "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
*      "add the value via assigning value to the field of a structure
*
*      CLEAR reported_conf.
*      reported_conf-%tky = lt_files[ 1 ]-%tky.
**          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
*      reported_conf-%msg = new_message( id       = '00'
*                                                     number   = 000
*                                                     severity = if_abap_behv_message=>severity-error
*                                                     v1       = |There should be at least one root node for|
*                                                     v2       = |Excel sheets.| ).
*      reported_conf-%state_area = 'VAL_CONFIGURATION'.
*      APPEND reported_conf  TO reported-configuration.
*    ENDIF.
*
**    if line_exists( lt_structures[ RootNode = abap_false SheetNameUp = space ] ).
**
**    ENDIF.
*
*
*  ENDMETHOD.

  METHOD determineTemplatebyStructure.
    DATA : lv_line   TYPE i,
           lv_column TYPE c.
    " Read keys of configuration by structure
    READ ENTITIES OF zzi_zt_dtimp_conf
        IN LOCAL MODE
            ENTITY Structure BY \_Conf ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(lt_files).

    " Read Sheet Configuration and Field Configuration
    READ ENTITIES OF zzi_zt_dtimp_conf
     IN LOCAL MODE
*        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_files)

         ENTITY Configuration BY \_Structures
             ALL FIELDS WITH CORRESPONDING #( lt_files )
      RESULT DATA(lt_structures).

    IF lines( lt_structures ) > 0.
      READ ENTITIES OF zzi_zt_dtimp_conf
          IN LOCAL MODE
              ENTITY Structure BY \_Fields
                  ALL FIELDS WITH CORRESPONDING #( lt_structures )
       RESULT DATA(lt_fields).
      IF lines( lt_fields ) > 0.
        SORT lt_fields BY Sequence.
      ENDIF.
    ENDIF.


    DATA : lv_sheet_count TYPE i.

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_file>).
      " Construction of Excel
      DATA(lo_write_access) = xco_cp_xlsx=>document->empty( )->write_access( ).

      lv_sheet_count = 0.
      LOOP AT lt_structures ASSIGNING FIELD-SYMBOL(<fs_structure>) WHERE UUIDConf = <fs_file>-uuid.
        lv_sheet_count += 1.
        " Create new sheet or change sheet name when in first sheet
        IF lv_sheet_count = 1.
          IF lo_write_access->get_workbook(  )->worksheet->at_position( 1 )->exists(  ) EQ abap_true.
            DATA(lo_worksheet) = lo_write_access->get_workbook(  )->worksheet->at_position( 1 )->set_name( CONV string( <fs_structure>-SheetName ) ).
          ELSE.
            lo_worksheet = lo_write_access->get_workbook( )->add_new_sheet( CONV string( <fs_structure>-SheetName ) ).
          ENDIF.
        ELSE.
          lo_worksheet = lo_write_access->get_workbook( )->add_new_sheet( CONV string( <fs_structure>-SheetName ) ).
        ENDIF.



        " Build Field Name and Description Table.
        lv_line = 1.
        IF <fs_structure>-HasFieldnameLine = abap_true.
          lv_column = <fs_structure>-StartColumn.
          DATA(lo_cursor) = lo_worksheet->cursor(
            io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( lv_column )
            io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_line )
          ).
          LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE UUIDStruc = <fs_structure>-uuid.
            "


            " Write Field Name
            lo_cursor->get_cell( )->value->write_from( <fs_field>-FieldName ).

            " Add Column
            lo_cursor->move_right(  ).
*            lv_column = zzcl_odata_utils=>get_alphabet(
*              EXPORTING
*                  iv_alphabet = lv_column
*                  iv_number = 1
*             ).
          ENDLOOP.
        ENDIF.

        lv_line += 1.
        IF <fs_structure>-HasDescLine = abap_true.
          lv_column = <fs_structure>-StartColumn.
          lo_cursor = lo_worksheet->cursor(
            io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( lv_column )
            io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( lv_line )
          ).
          LOOP AT lt_fields ASSIGNING <fs_field> WHERE UUIDStruc = <fs_structure>-uuid.


            " Write Field Description
            lo_cursor->get_cell( )->value->write_from( <fs_field>-FieldDescription ).

            " Add Column
            lo_cursor->move_right(  ).

          ENDLOOP.
        ENDIF.


      ENDLOOP.
      <fs_file>-Template = lo_write_access->get_file_content(  ).
      <fs_file>-FileName = |{ <fs_file>-Object }_Template.xlsx|.
      <fs_file>-Mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
      <fs_file>-MimeTypeForTemplate = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
    ENDLOOP.


    MODIFY ENTITIES OF zzi_zt_dtimp_conf
        IN LOCAL MODE
            ENTITY Configuration UPDATE FIELDS ( Template FileName Mimetype MimeTypeForTemplate )
            WITH VALUE #( FOR conf IN lt_files (
                %tky = conf-%tky
                Template = conf-Template
                FileName = conf-FileName
                Mimetype = conf-Mimetype
                MimeTypeForTemplate = conf-MimeTypeForTemplate
            ) )
        REPORTED DATA(ls_reported)
        FAILED DATA(ls_failed).



  ENDMETHOD.

*  METHOD checkFields.
*    " Check Mandatory Fields
*    DATA permission_request TYPE STRUCTURE FOR PERMISSIONS REQUEST zzr_zt_dtimp_struc.
*    DATA(description_permission_request) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( REF #( permission_request-%field ) ) ).
*    DATA(components_permission_request) = description_permission_request->get_components(  ).
*
*    DATA reported_field LIKE LINE OF reported-structure.
*
*
*
*    LOOP AT components_permission_request INTO DATA(component_permission_request).
*      permission_request-%field-(component_permission_request-name) = if_abap_behv=>mk-on.
*    ENDLOOP.
*
*    GET PERMISSIONS ONLY GLOBAL FEATURES ENTITY zhdr_dmp_t_import REQUEST permission_request
*        RESULT DATA(permission_result).
*
*    " Get current field values
*    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
*    ENTITY Structure
*      ALL FIELDS
*      WITH CORRESPONDING #( keys )
*      RESULT DATA(structures).
*
*    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
*        ENTITY Configuration BY \_Structures
*        ALL FIELDS WITH VALUE #( FOR structure IN structures ( %tky-%is_draft = structure-%is_draft
*                                                   %tky-uuid = structure-UUIDConf ) )
*        RESULT DATA(lt_all_structure).
*
*    LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>).
*      APPEND VALUE #( %tky        = <structure>-%tky
*              %state_area = if_abap_behv=>state_area_all ) TO reported-structure.
*
*      GET PERMISSIONS ONLY FEATURES ENTITY zzr_zt_dtimp_struc
*                FROM VALUE #( ( %tky = <structure>-%tky ) )
*                REQUEST permission_request
*                RESULT DATA(permission_result_instance)
*                FAILED DATA(failed_permission_result)
*                REPORTED DATA(reported_permission_result).
*
*      LOOP AT components_permission_request INTO component_permission_request.
*
*        "permission result for instances (field ( features : instance ) MandFieldInstfeat;) is stored in an internal table.
*        "So we have to retrieve the information for the current entity
*        "whereas the global information (field ( mandatory ) MandFieldBdef;) is stored in a structure
*        IF ( permission_result_instance-instances[ uuid = <structure>-uuid ]-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory OR
*             permission_result_instance-global-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory ) AND
*             <structure>-(component_permission_request-name) IS INITIAL.
*
*          APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.
*
*          "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
*          "add the value via assigning value to the field of a structure
*
*          CLEAR reported_field.
*          reported_field-%tky = <structure>-%tky.
*          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
*          reported_field-%msg = new_message( id       = '00'
*                                                         number   = 000
*                                                         severity = if_abap_behv_message=>severity-error
*                                                         v1       = |{ component_permission_request-name }|
*                                                         v2       = |is required.| ).
*          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
*                                                                    uuid = <structure>-UUIDConf ) ).
*          reported_field-%state_area = 'VAL_STRUCTURE'.
*          APPEND reported_field  TO reported-structure.
*
*        ENDIF.
*      ENDLOOP.
*
*      " Check Field Sheet Name of Parent
*      IF <structure>-RootNode = abap_false AND <structure>-SheetNameUp IS INITIAL.
*        APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.
*
*
*        CLEAR reported_field.
*        reported_field-%tky = <structure>-%tky.
*        reported_field-%tky = VALUE #(
*            %is_draft = if_abap_behv=>mk-on
*            uuid = <structure>-uuid
*         ).
*
*        reported_field-%element-sheetnameup = if_abap_behv=>mk-on.
*        reported_field-%msg = new_message( id       = '00'
*                                                       number   = 000
*                                                       severity = if_abap_behv_message=>severity-error
*                                                       v1       = |Sheet Name of Parent is required|
*                                                       v2       = |for child sheet.| ).
*        reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
*                                                                  uuid = <structure>-UUIDConf ) ).
*        reported_field-%state_area = 'VAL_STRUCTURE'.
*        APPEND reported_field  TO reported-structure.
*
*      ENDIF.
*
*      IF <structure>-RootNode = abap_false AND <structure>-SheetNameUp IS NOT INITIAL.
*        IF xsdbool( line_exists( lt_all_structure[ SheetName = <structure>-SheetNameUp ] ) ) = abap_false .
*          APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.
*
*
*          CLEAR reported_field.
*          reported_field-%tky = <structure>-%tky.
*          reported_field-%element-sheetnameup = if_abap_behv=>mk-on.
*          reported_field-%msg = new_message( id       = '00'
*                                                         number   = 000
*                                                         severity = if_abap_behv_message=>severity-error
*                                                         v1       = |Could not find configuration|
*                                                         v2       = |from Sheet Name of Parent.| ).
*          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
*                                                                    uuid = <structure>-UUIDConf ) ).
*          reported_field-%state_area = 'VAL_STRUCTURE'.
*          APPEND reported_field  TO reported-structure.
*        ENDIF.
*
*        IF <structure>-SheetName = <structure>-SheetNameUp.
*          APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.
*
*
*          CLEAR reported_field.
*          reported_field-%tky = <structure>-%tky.
*          reported_field-%element-sheetnameup = if_abap_behv=>mk-on.
*          reported_field-%msg = new_message( id       = '00'
*                                                         number   = 000
*                                                         severity = if_abap_behv_message=>severity-error
*                                                         v1       = |Setting sheet name of parent to itself|
*                                                         v2       = |is not permitted.| ).
*          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
*                                                                    uuid = <structure>-UUIDConf ) ).
*          reported_field-%state_area = 'VAL_STRUCTURE'.
*          APPEND reported_field  TO reported-structure.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.
*
*
*  ENDMETHOD.
*
*  METHOD checkSubFields.
*    " Check At least one key fields
*    DATA : reported_struc LIKE LINE OF reported-structure.
*
*
*
*    LOOP AT keys ASSIGNING FIELD-SYMBOL(<struct_key>).
*
*      APPEND VALUE #( %tky  = <struct_key>-%tky
*          %state_area = if_abap_behv=>state_area_all ) TO reported-structure.
*
*      " Read Sheet Configuration and Field Configuration
*      READ ENTITIES OF zzi_zt_dtimp_conf
*       IN LOCAL MODE
*        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_files)
*           ENTITY Structure FIELDS ( UUIDConf SheetName ) WITH VALUE #(  (
*                CORRESPONDING #( <struct_key> )
*                ) )
*          RESULT DATA(lt_struc)
*
*           ENTITY Structure BY \_Fields
*               ALL FIELDS WITH VALUE #(  (
*                CORRESPONDING #( <struct_key> )
*                ) )
*
*        RESULT DATA(lt_fields).
*      IF xsdbool( line_exists( lt_fields[ IsKeyField = abap_true ] ) ) = abap_false .
*        APPEND VALUE #( %tky = <struct_key>-%tky ) TO failed-structure.
*
*        "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
*        "add the value via assigning value to the field of a structure
*
*        CLEAR reported_struc.
*        reported_struc-%tky = <struct_key>-%tky.
*          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
*        reported_struc-%msg = new_message( id       = '00'
*                                                       number   = 000
*                                                       severity = if_abap_behv_message=>severity-error
*                                                       v1       = |There should be at least one key field for |
*                                                       v2 = | sheet '{ lt_struc[ 1 ]-SheetName }' | ).
*        reported_struc-%path =  VALUE #( configuration = VALUE #( %is_draft = lt_struc[ 1 ]-%tky-%is_draft
*                                                          uuid = lt_struc[ 1 ]-UUIDConf ) ).
*        reported_struc-%state_area = 'VAL_STRUCTURE'.
*        APPEND reported_struc  TO reported-structure.
*      ENDIF.
*    ENDLOOP.
*
*
*  ENDMETHOD.



  METHOD checkStructure.
    " Check Mandatory Fields
    DATA permission_request TYPE STRUCTURE FOR PERMISSIONS REQUEST zzr_zt_dtimp_struc.
    DATA(description_permission_request) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( REF #( permission_request-%field ) ) ).
    DATA(components_permission_request) = description_permission_request->get_components(  ).

    DATA reported_field LIKE LINE OF reported-structure.



    LOOP AT components_permission_request INTO DATA(component_permission_request).
      permission_request-%field-(component_permission_request-name) = if_abap_behv=>mk-on.
    ENDLOOP.

*    GET PERMISSIONS ONLY GLOBAL FEATURES ENTITY zhdr_dmp_t_import REQUEST permission_request
*        RESULT DATA(permission_result).

    " Get current field values
    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
    ENTITY Structure
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(structures).

    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
        ENTITY Configuration BY \_Structures
        ALL FIELDS WITH VALUE #( FOR structure IN structures ( %tky-%is_draft = structure-%is_draft
                                                   %tky-uuid = structure-UUIDConf ) )
        RESULT DATA(lt_all_structure).

    LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>).
      APPEND VALUE #( %tky        = <structure>-%tky
              %state_area = if_abap_behv=>state_area_all ) TO reported-structure.

      GET PERMISSIONS ONLY FEATURES ENTITY zzr_zt_dtimp_struc
                FROM VALUE #( ( %tky = <structure>-%tky ) )
                REQUEST permission_request
                RESULT DATA(permission_result_instance)
                FAILED DATA(failed_permission_result)
                REPORTED DATA(reported_permission_result).

      LOOP AT components_permission_request INTO component_permission_request.

        "permission result for instances (field ( features : instance ) MandFieldInstfeat;) is stored in an internal table.
        "So we have to retrieve the information for the current entity
        "whereas the global information (field ( mandatory ) MandFieldBdef;) is stored in a structure
        IF ( permission_result_instance-instances[ uuid = <structure>-uuid ]-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory OR
             permission_result_instance-global-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory ) AND
             <structure>-(component_permission_request-name) IS INITIAL.

          APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.

          "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
          "add the value via assigning value to the field of a structure

          CLEAR reported_field.
          reported_field-%tky = <structure>-%tky.
          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
          reported_field-%msg = new_message( id       = '00'
                                                         number   = 000
                                                         severity = if_abap_behv_message=>severity-error
                                                         v1       = |{ component_permission_request-name }|
                                                         v2       = |is required.| ).
          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
                                                                    uuid = <structure>-UUIDConf ) ).
          reported_field-%state_area = 'VAL_STRUCTURE'.
          APPEND reported_field  TO reported-structure.

        ENDIF.
      ENDLOOP.

      " Check Field Sheet Name of Parent
      IF <structure>-RootNode = abap_false AND <structure>-SheetNameUp IS INITIAL.
        APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.


        CLEAR reported_field.
        reported_field-%tky = <structure>-%tky.
*        reported_field-%tky = VALUE #(
*            %is_draft = if_abap_behv=>mk-on
*            uuid = <structure>-uuid
*         ).

        reported_field-%element-sheetnameup = if_abap_behv=>mk-on.
        reported_field-%msg = new_message( id       = '00'
                                                       number   = 000
                                                       severity = if_abap_behv_message=>severity-error
                                                       v1       = |Sheet Name of Parent is required|
                                                       v2       = |for child sheet.| ).
        reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
                                                                  uuid = <structure>-UUIDConf ) ).
        reported_field-%state_area = 'VAL_STRUCTURE'.
        APPEND reported_field  TO reported-structure.

      ENDIF.

      IF <structure>-RootNode = abap_false AND <structure>-SheetNameUp IS NOT INITIAL.
        IF xsdbool( line_exists( lt_all_structure[ SheetName = <structure>-SheetNameUp ] ) ) = abap_false .
          APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.


          CLEAR reported_field.
          reported_field-%tky = <structure>-%tky.
          reported_field-%element-sheetnameup = if_abap_behv=>mk-on.
          reported_field-%msg = new_message( id       = '00'
                                                         number   = 000
                                                         severity = if_abap_behv_message=>severity-error
                                                         v1       = |Could not find configuration|
                                                         v2       = |from Sheet Name of Parent.| ).
          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
                                                                    uuid = <structure>-UUIDConf ) ).
          reported_field-%state_area = 'VAL_STRUCTURE'.
          APPEND reported_field  TO reported-structure.
        ENDIF.

        IF <structure>-SheetName = <structure>-SheetNameUp.
          APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.


          CLEAR reported_field.
          reported_field-%tky = <structure>-%tky.
          reported_field-%element-sheetnameup = if_abap_behv=>mk-on.
          reported_field-%msg = new_message( id       = '00'
                                                         number   = 000
                                                         severity = if_abap_behv_message=>severity-error
                                                         v1       = |Setting sheet name of parent to itself|
                                                         v2       = |is not permitted.| ).
          reported_field-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
                                                                    uuid = <structure>-UUIDConf ) ).
          reported_field-%state_area = 'VAL_STRUCTURE'.
          APPEND reported_field  TO reported-structure.
        ENDIF.
      ENDIF.

    ENDLOOP.


    " Check At least one key fields
    DATA : reported_struc LIKE LINE OF reported-structure.



    LOOP AT structures ASSIGNING <structure>.

*      APPEND VALUE #( %tky  = <structure>-%tky
*          %state_area = if_abap_behv=>state_area_all ) TO reported-structure.

      " Read Sheet Configuration and Field Configuration
      READ ENTITIES OF zzi_zt_dtimp_conf
       IN LOCAL MODE
*        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_files)
*           ENTITY Structure FIELDS ( UUIDConf SheetName ) WITH VALUE #(  (
*                CORRESPONDING #( <struct_key> )
*                ) )
*          RESULT DATA(lt_struc)

           ENTITY Structure BY \_Fields
               ALL FIELDS WITH VALUE #(  (
                CORRESPONDING #( <structure> )
                ) )

        RESULT DATA(lt_fields).
      IF xsdbool( line_exists( lt_fields[ IsKeyField = abap_true ] ) ) = abap_false .
        APPEND VALUE #( %tky = <structure>-%tky ) TO failed-structure.

        "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
        "add the value via assigning value to the field of a structure

        CLEAR reported_struc.
        reported_struc-%tky = <structure>-%tky.
*          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
        reported_struc-%msg = new_message( id       = '00'
                                                       number   = 000
                                                       severity = if_abap_behv_message=>severity-error
                                                       v1       = |There should be at least one key field for |
                                                       v2 = | sheet '{ <structure>-SheetName }' | ).
        reported_struc-%path =  VALUE #( configuration = VALUE #( %is_draft = <structure>-%tky-%is_draft
                                                          uuid = <structure>-UUIDConf ) ).
        reported_struc-%state_area = 'VAL_STRUCTURE'.
        APPEND reported_struc  TO reported-structure.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
    ENTITY Structure
    FIELDS ( RootNode )
    WITH CORRESPONDING #( keys )
    RESULT DATA(structures)
    FAILED failed.
    result = VALUE #( FOR structure IN structures
           ( %tky                           = structure-%tky
*                 %delete = COND #( WHEN file-JobName IS NOT INITIAL
*                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                 %update = COND #( WHEN file-JobName IS NOT INITIAL
*                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                 %action-Edit = COND #( WHEN file-JobName IS NOT INITIAL
*                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
              %field-SheetNameUp = COND #( WHEN structure-RootNode = abap_false THEN  if_abap_behv=>fc-f-mandatory ELSE if_abap_behv=>fc-f-unrestricted )
          ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lhc_configuration DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR Configuration
        RESULT result,
*      checkFields FOR VALIDATE ON SAVE
*        IMPORTING keys FOR Configuration~checkFields,
*      checkRootNodeOnSave FOR VALIDATE ON SAVE
*        IMPORTING keys FOR Configuration~checkRootNodeOnSave,
      checkConfiguration FOR VALIDATE ON SAVE
        IMPORTING keys FOR Configuration~checkConfiguration.


*      zzPrintPDFAction FOR MODIFY
*        IMPORTING keys FOR ACTION Configuration~zzPrintPDFAction RESULT result.
ENDCLASS.

CLASS lhc_configuration IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.


*  METHOD checkFields.
*    " Check Mandatory Fields
*    DATA permission_request TYPE STRUCTURE FOR PERMISSIONS REQUEST zzi_zt_dtimp_conf.
*    DATA(description_permission_request) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( REF #( permission_request-%field ) ) ).
*    DATA(components_permission_request) = description_permission_request->get_components(  ).
*
*    DATA reported_field LIKE LINE OF reported-configuration.
*
*
*
*    LOOP AT components_permission_request INTO DATA(component_permission_request).
*      permission_request-%field-(component_permission_request-name) = if_abap_behv=>mk-on.
*    ENDLOOP.
*
**    GET PERMISSIONS ONLY GLOBAL FEATURES ENTITY zhdr_dmp_t_import REQUEST permission_request
**        RESULT DATA(permission_result).
*
*    " Get current field values
*    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
*    ENTITY Configuration
*      ALL FIELDS
*      WITH CORRESPONDING #( keys )
*      RESULT DATA(configurations).
*    LOOP AT configurations ASSIGNING FIELD-SYMBOL(<configuration>).
*      APPEND VALUE #( %tky        = <configuration>-%tky
*                  %state_area = if_abap_behv=>state_area_all ) TO reported-configuration.
**      LOOP AT components_permission_request INTO component_permission_request.
**        IF permission_result-global-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory
**          AND <data>-(component_permission_request-name) IS INITIAL.
**          APPEND VALUE #( %tky = <data>-%tky  ) TO failed-import.
***          failed-zr_tcs016[ 1 ]-%fail-cause-unspecific
**
**          CLEAR reported_field.
**          reported_field-%tky = <data>-%tky.
**          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
***          reported_field-%path = VALUE #( imports-%is_draft = <data>-%is_draft
***                                          imports-%key-uuid = <data>-uuid
***                                          ).
**          reported_field-%state_area = component_permission_request-name.
**
**          reported_field-%msg = new_message( id       = '00'
**                                                         number   = 000
**                                                         severity = if_abap_behv_message=>severity-error
**                                                         v1       = |{ component_permission_request-name }|
***                                                         v2       = | with key: { <data>-uuid } is required | ).
**                                                         v2       = | is required | ).
**
**          APPEND reported_field TO reported-import .
**        ENDIF.
**      ENDLOOP.
*
*
*
*      GET PERMISSIONS ONLY FEATURES ENTITY zzi_zt_dtimp_conf
*                FROM VALUE #( ( %tky = <configuration>-%tky ) )
*                REQUEST permission_request
*                RESULT DATA(permission_result_instance)
*                FAILED DATA(failed_permission_result)
*                REPORTED DATA(reported_permission_result).
*
*      LOOP AT components_permission_request INTO component_permission_request.
*
*        "permission result for instances (field ( features : instance ) MandFieldInstfeat;) is stored in an internal table.
*        "So we have to retrieve the information for the current entity
*        "whereas the global information (field ( mandatory ) MandFieldBdef;) is stored in a structure
*        IF ( permission_result_instance-instances[ uuid = <configuration>-uuid ]-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory OR
*             permission_result_instance-global-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory ) AND
*             <configuration>-(component_permission_request-name) IS INITIAL.
*
*          APPEND VALUE #( %tky = <configuration>-%tky ) TO failed-configuration.
*
*          "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
*          "add the value via assigning value to the field of a structure
*
*          CLEAR reported_field.
*          reported_field-%tky = <configuration>-%tky.
*          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
*          reported_field-%msg = new_message( id       = '00'
*                                                         number   = 000
*                                                         severity = if_abap_behv_message=>severity-error
*                                                         v1       = |{ component_permission_request-name }|
*                                                         v2       = |is required.|
*                                                         ).
*          reported_field-%state_area = 'VAL_CONFIGURATION'.
*          reported_field-%action-Prepare = if_abap_behv=>mk-on.
*          APPEND reported_field  TO reported-configuration.
*
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*  ENDMETHOD.
*
*  METHOD checkRootNodeOnSave.
*    DATA : reported_conf LIKE LINE OF reported-configuration.
*
*
*    " Read keys of configuration by structure
**    READ ENTITIES OF zzi_zt_dtimp_conf
**        IN LOCAL MODE
**            ENTITY Structure BY \_Conf ALL FIELDS WITH CORRESPONDING #( keys )
**        RESULT DATA(lt_files).
*
*    APPEND VALUE #( %tky  = keys[ 1 ]-%tky
*        %state_area = if_abap_behv=>state_area_all ) TO reported-configuration.
*
*    " Read Sheet Configuration and Field Configuration
*    READ ENTITIES OF zzi_zt_dtimp_conf
*     IN LOCAL MODE
**        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
**        RESULT DATA(lt_files)
*
*         ENTITY Configuration BY \_Structures
*             ALL FIELDS WITH CORRESPONDING #( keys )
*      RESULT DATA(lt_structures).
*    IF xsdbool( line_exists( lt_structures[ RootNode = abap_true ] ) ) = abap_false .
*      APPEND VALUE #( %tky = keys[ 1 ]-%tky ) TO failed-configuration.
*
*      "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
*      "add the value via assigning value to the field of a structure
*
*      CLEAR reported_conf.
*      reported_conf-%tky = keys[ 1 ]-%tky.
**          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
*      reported_conf-%msg = new_message( id       = '00'
*                                                     number   = 000
*                                                     severity = if_abap_behv_message=>severity-error
*                                                     v1       = |There should be at least one root node for|
*                                                     v2       = |Excel sheets.| ).
*      reported_conf-%state_area = 'VAL_CONFIGURATION'.
*      APPEND reported_conf  TO reported-configuration.
*    ENDIF.
*
*  ENDMETHOD.

  METHOD checkConfiguration.

    " Check Mandatory Fields
    DATA permission_request TYPE STRUCTURE FOR PERMISSIONS REQUEST zzi_zt_dtimp_conf.
    DATA(description_permission_request) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( REF #( permission_request-%field ) ) ).
    DATA(components_permission_request) = description_permission_request->get_components(  ).

    DATA reported_field LIKE LINE OF reported-configuration.



    LOOP AT components_permission_request INTO DATA(component_permission_request).
      permission_request-%field-(component_permission_request-name) = if_abap_behv=>mk-on.
    ENDLOOP.

*    GET PERMISSIONS ONLY GLOBAL FEATURES ENTITY zhdr_dmp_t_import REQUEST permission_request
*        RESULT DATA(permission_result).

    " Get current field values
    READ ENTITIES OF zzi_zt_dtimp_conf IN LOCAL MODE
    ENTITY Configuration
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(configurations).
    LOOP AT configurations ASSIGNING FIELD-SYMBOL(<configuration>).
      APPEND VALUE #( %tky        = <configuration>-%tky
                  %state_area = if_abap_behv=>state_area_all ) TO reported-configuration.


      GET PERMISSIONS ONLY FEATURES ENTITY zzi_zt_dtimp_conf
                FROM VALUE #( ( %tky = <configuration>-%tky ) )
                REQUEST permission_request
                RESULT DATA(permission_result_instance)
                FAILED DATA(failed_permission_result)
                REPORTED DATA(reported_permission_result).

      LOOP AT components_permission_request INTO component_permission_request.

        "permission result for instances (field ( features : instance ) MandFieldInstfeat;) is stored in an internal table.
        "So we have to retrieve the information for the current entity
        "whereas the global information (field ( mandatory ) MandFieldBdef;) is stored in a structure
        IF ( permission_result_instance-instances[ uuid = <configuration>-uuid ]-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory OR
             permission_result_instance-global-%field-(component_permission_request-name) = if_abap_behv=>fc-f-mandatory ) AND
             <configuration>-(component_permission_request-name) IS INITIAL.

          APPEND VALUE #( %tky = <configuration>-%tky ) TO failed-configuration.

          "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
          "add the value via assigning value to the field of a structure

          CLEAR reported_field.
          reported_field-%tky = <configuration>-%tky.
          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
          reported_field-%msg = new_message( id       = '00'
                                                         number   = 000
                                                         severity = if_abap_behv_message=>severity-error
                                                         v1       = |{ component_permission_request-name }|
                                                         v2       = |is required.|
                                                         ).
          reported_field-%state_area = 'VAL_CONFIGURATION'.
          reported_field-%action-Prepare = if_abap_behv=>mk-on.
          APPEND reported_field  TO reported-configuration.

        ENDIF.
      ENDLOOP.
    ENDLOOP.


    DATA : reported_conf LIKE LINE OF reported-configuration.


    " Read Sheet Configuration and Field Configuration
    READ ENTITIES OF zzi_zt_dtimp_conf
     IN LOCAL MODE
*        ENTITY Configuration ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_files)

         ENTITY Configuration BY \_Structures
             ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_structures).
    IF xsdbool( line_exists( lt_structures[ RootNode = abap_true ] ) ) = abap_false .
      APPEND VALUE #( %tky = keys[ 1 ]-%tky ) TO failed-configuration.

      "since %element-(component_permission_request-name) = if_abap_behv=>mk-on could not be added using a VALUE statement
      "add the value via assigning value to the field of a structure

      CLEAR reported_conf.
      reported_conf-%tky = keys[ 1 ]-%tky.
*          reported_field-%element-(component_permission_request-name) = if_abap_behv=>mk-on.
      reported_conf-%msg = new_message( id       = '00'
                                                     number   = 000
                                                     severity = if_abap_behv_message=>severity-error
                                                     v1       = |There should be at least one root node for|
                                                     v2       = |Excel sheets.| ).
      reported_conf-%state_area = 'VAL_CONFIGURATION'.
      APPEND reported_conf  TO reported-configuration.
    ENDIF.



  ENDMETHOD.

ENDCLASS.
