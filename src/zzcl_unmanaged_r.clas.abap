CLASS zzcl_unmanaged_r DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_journalentry_unmanaged IMPORTING io_request  TYPE REF TO if_rap_query_request
                                                 io_response TYPE REF TO if_rap_query_response.
ENDCLASS.



CLASS zzcl_unmanaged_r IMPLEMENTATION.


  METHOD if_rap_query_provider~select.
    DATA(lv_entity_id) =  io_request->get_entity_id(  ).

    CASE lv_entity_id.

      WHEN 'ZR_S_JOURNALITEM_UNMANAGED'.
        get_journalentry_unmanaged(
            EXPORTING
                io_request = io_request
                io_response = io_response
         ).


      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.
  METHOD get_journalentry_unmanaged.

    DATA(lt_ranges) = io_request->get_filter(  )->get_as_ranges(  ).
    DATA : lr_companycode   TYPE RANGE OF bukrs,

           lt_entity TYPE TABLE OF ZR_S_JournalItem_Unmanaged.


    LOOP AT lt_ranges ASSIGNING FIELD-SYMBOL(<fs_range>).
      TRANSLATE <fs_range>-name TO UPPER CASE.
      CASE <fs_range>-name.
        WHEN 'COMPANYCODE'.
          lr_companycode = CORRESPONDING #( <fs_range>-range ).
      ENDCASE.
    ENDLOOP.

    SELECT * FROM
        I_JournalEntryItem
        WHERE Companycode IN @lr_companycode
        INTO TABLE @DATA(lt_result).

    MOVE-CORRESPONDING lt_result TO lt_entity.



    zzcl_odata_utils=>filtering( EXPORTING io_filter = io_request->get_filter(  ) CHANGING ct_data = lt_entity ).

    IF io_request->is_total_numb_of_rec_requested(  ) .
      io_response->set_total_number_of_records( lines( lt_entity ) ).
    ENDIF.

    zzcl_odata_utils=>orderby( EXPORTING it_order = io_request->get_sort_elements( )  CHANGING ct_data = lt_entity ).

    zzcl_odata_utils=>paging( EXPORTING io_paging = io_request->get_paging(  ) CHANGING ct_data = lt_entity ).

    io_response->set_data( lt_entity ).


*    io_response->set_data( lt_result_entity ).

*    io_response->set_total_number_of_records(  lines( lt_result_entity )  ).
*



  ENDMETHOD.

ENDCLASS.
