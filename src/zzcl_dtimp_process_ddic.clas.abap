CLASS zzcl_dtimp_process_ddic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zzif_process_data .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS : Create_DOMAS IMPORTING is_data           TYPE any
                           RETURNING VALUE(ev_message) TYPE zzs_dmp_data_list,
      create_dtels IMPORTING is_data           TYPE any
                   RETURNING VALUE(ev_message) TYPE zzs_dmp_data_list,
      set_fixed_values IMPORTING it_fixed_values  TYPE STANDARD TABLE
                                 io_specification TYPE REF TO if_xco_cp_gen_doma_s_form,
      get_buildin_type IMPORTING iv_doma        TYPE any
                       RETURNING VALUE(eo_type) TYPE REF TO cl_xco_ad_built_in_type.

ENDCLASS.



CLASS ZZCL_DTIMP_PROCESS_DDIC IMPLEMENTATION.


  METHOD create_domas.
    FIELD-SYMBOLS: <ft_domas> TYPE STANDARD TABLE.

    ASSIGN COMPONENT 'TRANSPORT' OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_transport>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'PACKAGE' OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_package>).
      IF sy-subrc = 0.

        DATA(lo_env) = xco_cp_generation=>environment->dev_system(
            iv_transport = CONV sxco_transport( <fs_transport> )
        ).
        DATA(lo_put_operation) = lo_env->create_put_operation( ).

        ASSIGN COMPONENT 'DOMA' OF STRUCTURE is_data TO <ft_domas>.
        IF sy-subrc = 0.
          LOOP AT <ft_domas> ASSIGNING FIELD-SYMBOL(<fs_doma>).

            DATA(lo_specification) = lo_put_operation->for-doma->add_object( CONV sxco_ad_object_name( <fs_doma>-('NAME') )
               )->set_package( CONV sxco_package( <fs_package> )
               )->create_form_specification( ).

            lo_specification->set_format( get_buildin_type( <fs_doma> ) ).
            lo_specification->set_short_description( CONV if_xco_cp_gen_doma_s_form=>tv_short_description( <fs_doma>-('DESCRIPTION') ) ).
            lo_specification->output_characteristics->set_case_sensitive( CONV abap_boolean( <fs_doma>-('CASE_SENSITIVE') ) ).
            lo_specification->output_characteristics->set_conversion_routine( CONV if_xco_gen_doma_s_fo_outpt_chr=>tv_conversion_routine( <fs_doma>-('CONVERSION_ROUTINE') ) ).

            "Fixed Value
            FIELD-SYMBOLS: <ft_fixed_values> TYPE STANDARD TABLE.
            ASSIGN COMPONENT 'FIXED_VALUES' OF STRUCTURE <fs_doma> TO <ft_fixed_values>.
            set_fixed_values(
                it_fixed_values = <ft_fixed_values>
                io_specification = lo_specification
            ).
          ENDLOOP.
          TRY.
              DATA(lo_result) = lo_put_operation->execute(  ).
              DATA(lt_messages) = lo_result->findings->if_xco_news~get_messages(  ).
              LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<fo_message>).
                APPEND VALUE #(
                     type = <fo_message>->value-msgty
                     id = <fo_message>->value-msgid
                     number = <fo_message>->value-msgno
                     message_v1 = <fo_message>->value-msgv1
                     message_v2 = <fo_message>->value-msgv2
                     message_v3 = <fo_message>->value-msgv3
                     message_v4 = <fo_message>->value-msgv4
                     message =  <fo_message>->get_text(  )
                 ) TO ev_message-message_list.
              ENDLOOP.


            CATCH cx_xco_gen_put_exception INTO DATA(lx_put_exception).

              lt_messages =  lx_put_exception->if_xco_news~get_messages(  ).
              LOOP AT lt_messages ASSIGNING <fo_message>.
                APPEND VALUE #(
                     type = 'E'
                     id = '00'
                     number = 000
                     message =  <fo_message>->get_text(  )
                 ) TO ev_message-message_list.
              ENDLOOP.

          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.




  ENDMETHOD.


  METHOD create_dtels.
    FIELD-SYMBOLS: <ft_dtels> TYPE STANDARD TABLE.

    ASSIGN COMPONENT 'TRANSPORT' OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_transport>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'PACKAGE' OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_package>).
      IF sy-subrc = 0.

        DATA(lo_env) = xco_cp_generation=>environment->dev_system(
            iv_transport = CONV sxco_transport( <fs_transport> )
        ).
        DATA(lo_put_operation) = lo_env->create_put_operation( ).

        ASSIGN COMPONENT 'DTEL' OF STRUCTURE is_data TO <ft_dtels>.
        IF sy-subrc = 0.
          LOOP AT <ft_dtels> ASSIGNING FIELD-SYMBOL(<fs_dtel>).

            DATA(lo_specification) = lo_put_operation->for-dtel->add_object( CONV sxco_ad_object_name(  <fs_dtel>-('NAME') )
               )->set_package( CONV sxco_package( <fs_package> )
               )->create_form_specification( ).

            lo_specification->set_data_type(  xco_cp_abap_dictionary=>domain( CONV sxco_ad_object_name( <fs_dtel>-('DOMAIN') ) ) ).
            lo_specification->set_short_description( CONV if_xco_cp_gen_dtel_s_form=>tv_short_description( <fs_dtel>-('DESCRIPTION') ) ).
            lo_specification->field_label-short->set_text( CONV if_xco_gen_dtel_s_fo_fld_lbl=>tv_text( <fs_dtel>-('DESCRIPTION') ) )->set_length( 10 ).
            lo_specification->field_label-medium->set_text( CONV if_xco_gen_dtel_s_fo_fld_lbl=>tv_text( <fs_dtel>-('DESCRIPTION') ) )->set_length( 20 ).
            lo_specification->field_label-long->set_text( CONV if_xco_gen_dtel_s_fo_fld_lbl=>tv_text( <fs_dtel>-('DESCRIPTION') ) )->set_length( 40 ).
            lo_specification->field_label-heading->set_text( CONV if_xco_gen_dtel_s_fo_fld_lbl=>tv_text( <fs_dtel>-('DESCRIPTION') ) )->set_length( 55 ).

          ENDLOOP.
          TRY.
              DATA(lo_result) = lo_put_operation->execute(  ).
              DATA(lt_messages) = lo_result->findings->if_xco_news~get_messages(  ).
              LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<fo_message>).
                APPEND VALUE #(
                     type = <fo_message>->value-msgty
                     id = <fo_message>->value-msgid
                     number = <fo_message>->value-msgno
                     message_v1 = <fo_message>->value-msgv1
                     message_v2 = <fo_message>->value-msgv2
                     message_v3 = <fo_message>->value-msgv3
                     message_v4 = <fo_message>->value-msgv4
                     message =  <fo_message>->get_text(  )
                 ) TO ev_message-message_list.
              ENDLOOP.



            CATCH cx_xco_gen_put_exception INTO DATA(lx_put_exception).

              lt_messages =  lx_put_exception->if_xco_news~get_messages(  ).
              LOOP AT lt_messages ASSIGNING <fo_message>.
                APPEND VALUE #(
                     type = 'E'
                     id = '00'
                     number = 000
                     message =  <fo_message>->get_text(  )
                 ) TO ev_message-message_list.
              ENDLOOP.

          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_buildin_type.
    ASSIGN COMPONENT 'TYPE' OF STRUCTURE iv_doma TO FIELD-SYMBOL(<fs_type>).
    ASSIGN COMPONENT 'LENGTH' OF STRUCTURE iv_doma TO FIELD-SYMBOL(<fs_length>).
    ASSIGN COMPONENT 'DECIMAL' OF STRUCTURE iv_doma TO FIELD-SYMBOL(<fs_DECIMAL>).
    IF sy-subrc = 0.
      CASE <fs_type>.
        WHEN 'ACCP'.
          RETURN xco_cp_abap_dictionary=>built_in_type->accp.
        WHEN 'CLNT'.
          RETURN xco_cp_abap_dictionary=>built_in_type->clnt.
        WHEN 'CUKY'.
          RETURN xco_cp_abap_dictionary=>built_in_type->cuky.
        WHEN 'DATN'.
          RETURN xco_cp_abap_dictionary=>built_in_type->datn.
        WHEN 'DATS'.
          RETURN xco_cp_abap_dictionary=>built_in_type->dats.
        WHEN 'DECFLOAT16'.
          RETURN xco_cp_abap_dictionary=>built_in_type->decfloat16.
        WHEN 'DECFLOAT34'.
          RETURN xco_cp_abap_dictionary=>built_in_type->decfloat34.
        WHEN 'INT1'.
          RETURN xco_cp_abap_dictionary=>built_in_type->int1.
        WHEN 'INT2'.
          RETURN xco_cp_abap_dictionary=>built_in_type->int2.
        WHEN 'INT4'.
          RETURN xco_cp_abap_dictionary=>built_in_type->int4.
        WHEN 'INT8'.
          RETURN xco_cp_abap_dictionary=>built_in_type->int8.
        WHEN 'TIMN'.
          RETURN xco_cp_abap_dictionary=>built_in_type->timn.
        WHEN 'TIMS'.
          RETURN xco_cp_abap_dictionary=>built_in_type->tims.
        WHEN 'UTCLONG'.
          RETURN xco_cp_abap_dictionary=>built_in_type->utclong.
        WHEN 'CHAR'.
          RETURN xco_cp_abap_dictionary=>built_in_type->char( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
        WHEN 'CURR'.
          RETURN xco_cp_abap_dictionary=>built_in_type->curr( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> )  iv_decimals = CONV cl_xco_ad_built_in_type_f=>tv_decimals( <fs_decimal> ) ).
        WHEN 'DEC'.
          RETURN xco_cp_abap_dictionary=>built_in_type->dec( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> )  iv_decimals = CONV cl_xco_ad_built_in_type_f=>tv_decimals( <fs_decimal> ) ).
        WHEN 'NUMC'.
          RETURN xco_cp_abap_dictionary=>built_in_type->numc( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
        WHEN 'QUAN'.
          RETURN xco_cp_abap_dictionary=>built_in_type->quan( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> )  iv_decimals = CONV cl_xco_ad_built_in_type_f=>tv_decimals( <fs_decimal> ) ).
        WHEN 'RAW'.
          RETURN xco_cp_abap_dictionary=>built_in_type->raw( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
        WHEN 'RAWSTRING'.
          RETURN xco_cp_abap_dictionary=>built_in_type->rawstring( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
        WHEN 'SSTRING'.
          RETURN xco_cp_abap_dictionary=>built_in_type->sstring( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
        WHEN 'STRING'.
          RETURN xco_cp_abap_dictionary=>built_in_type->string( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
        WHEN 'UNIT'.
          RETURN xco_cp_abap_dictionary=>built_in_type->unit( iv_length = CONV cl_xco_ad_built_in_type_f=>tv_length( <fs_length> ) ).
      ENDCASE.
    ENDIF.


  ENDMETHOD.


  METHOD set_fixed_values.
    LOOP AT it_fixed_values ASSIGNING FIELD-SYMBOL(<fs_fixed_value>).
      io_specification->fixed_values->add_fixed_value( CONV if_xco_gen_doma_s_fo_fxd_value=>tv_lower_limit( <fs_fixed_value>-('VALUE') )
        )->set_description( CONV if_xco_gen_doma_s_fo_fxd_value=>tv_description( <fs_fixed_value>-('DESCRIPTION') ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zzif_process_data~process.
    DATA : ls_message TYPE zzs_dmp_data_list.

    CREATE DATA eo_data TYPE HANDLE io_data_handle.

    eo_data->* = io_data->*.

    LOOP AT io_data->* ASSIGNING FIELD-SYMBOL(<fs_data>).
      DATA(lv_tabix) = sy-tabix.

      ls_message =  me->create_domas( <fs_data> ).
      ls_message-line = lv_tabix.

      APPEND ls_message  TO et_message.

      ls_message =  me->create_dtels( <fs_data> ).
      ls_message-line = lv_tabix.

      APPEND ls_message  TO et_message.

    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
