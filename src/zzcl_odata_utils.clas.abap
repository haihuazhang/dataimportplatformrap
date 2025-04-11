CLASS zzcl_odata_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_sorting_order,
        descending TYPE string VALUE 'desc',
        ascending  TYPE string VALUE 'asc',
      END OF   gcs_sorting_order .

    CLASS-METHODS paging
      IMPORTING
        !io_paging TYPE REF TO if_rap_query_paging
      CHANGING
        !ct_data   TYPE STANDARD TABLE .
    CLASS-METHODS filtering
      IMPORTING
        !io_filter TYPE REF TO if_rap_query_filter
      CHANGING
        !ct_data   TYPE STANDARD TABLE .
    CLASS-METHODS orderby
      IMPORTING
        !it_order TYPE if_rap_query_request=>tt_sort_elements
      CHANGING
        !ct_data  TYPE STANDARD TABLE .


    CLASS-METHODS get_paging
      IMPORTING
        !io_paging TYPE REF TO if_rap_query_paging
      EXPORTING
        top        TYPE i
        skip       TYPE i.

    CLASS-METHODS get_alphabet
      IMPORTING
                iv_alphabet        TYPE zzechar1
                iv_number          TYPE i
      RETURNING VALUE(ev_alphabet) TYPE zzechar1.
    CLASS-METHODS get_internal
      IMPORTING
                io_elem_ref    TYPE REF TO cl_abap_elemdescr
                iv_data        TYPE any
      RETURNING VALUE(ev_data) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_ODATA_UTILS IMPLEMENTATION.


  METHOD filtering.
    TRY.
        DATA(lt_filter) = io_filter->get_as_ranges(  ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_range). ##NO_HANDLER
        EXIT.
        "handle exception
        ##NO_HANDLER
    ENDTRY.
    FIELD-SYMBOLS: <fs_data> TYPE any,
                   <fs_fval> TYPE any.
    DATA: lv_index TYPE sy-tabix.

    LOOP AT lt_filter INTO DATA(ls_filter).
      TRANSLATE ls_filter-name TO UPPER CASE.            "#EC TRANSLANG
      LOOP AT ct_data ASSIGNING <fs_data>.
        lv_index = sy-tabix.
        ASSIGN COMPONENT ls_filter-name OF STRUCTURE <fs_data> TO <fs_fval>.
        CHECK sy-subrc EQ 0.
        IF <fs_fval> NOT IN ls_filter-range.
          DELETE ct_data INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_alphabet.
*    PARAMETERS char TYPE c DEFAULT 'A'.
    DATA : cnt          TYPE i VALUE 0,
           total_length TYPE i,
           lv_abcde     TYPE c LENGTH 26 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    FIND FIRST OCCURRENCE OF iv_alphabet IN lv_abcde MATCH OFFSET cnt.
    cnt += iv_number.
    IF cnt > 25.
      ev_alphabet = iv_alphabet.
    ELSE.
      ev_alphabet = lv_abcde+cnt(1).
    ENDIF.
  ENDMETHOD.


  METHOD get_internal.
    ev_data = iv_data.
    IF io_elem_ref->edit_mask CS 'ALPHA'.

      ev_data = | { iv_data ALPHA = IN } |.

    ENDIF.

    IF io_elem_ref->get_relative_name(  ) = 'SYSUUID_X16'.

      DATA : lv_uuid_uppercase TYPE string.
      lv_uuid_uppercase = iv_data.
      TRANSLATE lv_uuid_uppercase TO UPPER CASE.
      DATA(lv_uuid_length) = strlen( lv_uuid_uppercase ).
      TRY.
          CASE lv_uuid_length.
            WHEN '16'.
              ev_data = lv_uuid_uppercase.
            WHEN '22'.
              cl_system_uuid=>convert_uuid_c22_static(
                  EXPORTING
                      uuid = CONV sysuuid_c22( lv_uuid_uppercase )
                  IMPORTING
                      uuid_x16 = DATA(lv_uuid_x16)
               ).
              ev_data = lv_uuid_X16.
            WHEN '26'.
              cl_system_uuid=>convert_uuid_c26_static(
              EXPORTING
                     uuid = CONV sysuuid_c26( lv_uuid_uppercase )
                 IMPORTING
                     uuid_x16 = lv_uuid_X16
              ).
              ev_data = lv_uuid_X16.
            WHEN '32'.
              cl_system_uuid=>convert_uuid_c32_static(
              EXPORTING
                     uuid = CONV sysuuid_c32( lv_uuid_uppercase )
                 IMPORTING
                     uuid_x16 = lv_uuid_X16
              ).
              ev_data = lv_uuid_X16.
            WHEN '36'.
              cl_system_uuid=>convert_uuid_c36_static(
                EXPORTING
                   uuid = CONV sysuuid_c36( lv_uuid_uppercase )
                IMPORTING
                    uuid_x16 = lv_uuid_X16
               ).
              ev_data = lv_uuid_X16.
          ENDCASE.
        CATCH cx_uuid_error INTO DATA(lx_uuid).
          "handle exception
*          ev_data = lv_uuid_X16.
          ev_data = iv_data.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD get_paging.
    skip =  io_paging->get_offset(  ).
    top = io_paging->get_page_size(  ).


*    DATA: lv_from TYPE i,
*          lv_to   TYPE i.
*
*    IF lv_skip IS NOT INITIAL.
*      lv_from = lv_skip + 1. "start from record
*    ELSE.
*      lv_from = 1.
*    ENDIF.
*    IF lv_top EQ if_rap_query_paging=>page_size_unlimited OR lv_top IS INITIAL.
*      lv_to = lines( ct_data ).
*    ELSE.
**          IF lv_top IS NOT INITIAL.
*      lv_to   = lv_from + lv_top - 1.
*    ENDIF.
*    IF lv_top EQ if_rap_query_paging=>page_size_unlimited.
*
*    ENDIF.


  ENDMETHOD.


  METHOD orderby.
    DATA: lt_otab  TYPE abap_sortorder_tab,
          ls_oline TYPE abap_sortorder.
    DATA: ls_order LIKE LINE OF it_order.

    LOOP AT it_order INTO ls_order.
      ls_oline-name = ls_order-element_name.
      TRANSLATE ls_oline-name TO UPPER CASE.             "#EC TRANSLANG
*      IF ls_order-descending = gcs_sorting_order-descending.
      ls_oline-descending = ls_order-descending.
*      ENDIF.
      APPEND ls_oline TO lt_otab.
      CLEAR ls_oline.
    ENDLOOP.

    SORT ct_data BY (lt_otab).
  ENDMETHOD.


  METHOD paging.
    DATA(lv_skip) =  io_paging->get_offset(  ).
    DATA(lv_top) = io_paging->get_page_size(  ).


    DATA: lv_from TYPE i,
          lv_to   TYPE i.
    DATA: lo_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_result> TYPE STANDARD TABLE,
                   <fs_rec>    TYPE any.

    CREATE DATA lo_data LIKE ct_data.
    ASSIGN lo_data->* TO <fs_result>.

    IF lv_skip IS NOT INITIAL.
      lv_from = lv_skip + 1. "start from record
    ELSE.
      lv_from = 1.
    ENDIF.
    IF lv_top EQ if_rap_query_paging=>page_size_unlimited OR lv_top IS INITIAL.
      lv_to = lines( ct_data ).
    ELSE.
*          IF lv_top IS NOT INITIAL.
      lv_to   = lv_from + lv_top - 1.
    ENDIF.

    LOOP AT ct_data ASSIGNING <fs_rec> FROM lv_from TO lv_to.
      APPEND <fs_rec> TO <fs_result>.
    ENDLOOP.

    ct_data = <fs_result>.
  ENDMETHOD.
ENDCLASS.
