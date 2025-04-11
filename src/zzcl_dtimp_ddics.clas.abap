CLASS zzcl_dtimp_ddics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_function_modules IMPORTING io_request  TYPE REF TO if_rap_query_request
                                           io_response TYPE REF TO if_rap_query_response
                                 RAISING   cx_rap_query_prov_not_impl
                                           cx_rap_query_provider.

    METHODS get_structures IMPORTING io_request  TYPE REF TO if_rap_query_request
                                     io_response TYPE REF TO if_rap_query_response
                           RAISING   cx_rap_query_prov_not_impl
                                     cx_rap_query_provider.
ENDCLASS.



CLASS ZZCL_DTIMP_DDICS IMPLEMENTATION.


  METHOD get_function_modules.

    DATA: lt_funcs TYPE TABLE OF zzr_dtimp_func.
    DATA(lo_package) = xco_cp_abap_repository=>package->for( 'ZZDATAIMPORT' ).



    DATA(lt_func_result) = xco_cp_abap_repository=>objects->fugr->all->in( lo_package )->get(  ).

    " get function modules under every function group
    lt_funcs = VALUE #( FOR result IN lt_func_result
                            FOR func IN result->function_modules->all->get(  )
                            ( FunctionModuleName = func->name FunctionModuleDesc = func->content(  )->get_short_text(  )  ) ).
*

    zzcl_odata_utils=>filtering( EXPORTING io_filter = io_request->get_filter(  ) CHANGING ct_data = lt_funcs ).

    IF io_request->is_total_numb_of_rec_requested(  ) .
      io_response->set_total_number_of_records( lines( lt_funcs ) ).
    ENDIF.

    zzcl_odata_utils=>orderby( EXPORTING it_order = io_request->get_sort_elements( )  CHANGING ct_data = lt_funcs ).

    zzcl_odata_utils=>paging( EXPORTING io_paging = io_request->get_paging(  ) CHANGING ct_data = lt_funcs ).

    io_response->set_data( lt_funcs ).


*    DATA(sort_elements) = io_request->get_sort_elements( ).
*    /iwbep/cl_mgw_data_util=>orderby( it_order = sort_elements CHANGING ct_data = lt_funcs ).


  ENDMETHOD.


  METHOD get_structures.
    DATA: lt_structs TYPE TABLE OF zzr_dtimp_struc.
    DATA(lo_package) = xco_cp_abap_repository=>package->for( 'ZZDATAIMPORT' ).
    DATA(lt_structure) = xco_cp_abap_repository=>objects->tabl->structures->all->in( lo_package )->get(  ).

    lt_structs = VALUE #( FOR structure IN lt_structure
                            ( StructureName = structure->name StructureDesc = structure->content(  )->get_short_description(  ) ) ).

    zzcl_odata_utils=>filtering( EXPORTING io_filter = io_request->get_filter(  ) CHANGING ct_data = lt_structs ).

    IF io_request->is_total_numb_of_rec_requested(  ) .
      io_response->set_total_number_of_records( lines( lt_structs ) ).
    ENDIF.

    zzcl_odata_utils=>orderby( EXPORTING it_order = io_request->get_sort_elements( )  CHANGING ct_data = lt_structs ).

    zzcl_odata_utils=>paging( EXPORTING io_paging = io_request->get_paging(  ) CHANGING ct_data = lt_structs ).

    io_response->set_data( lt_structs ).



  ENDMETHOD.


  METHOD if_rap_query_provider~select.
*    TRY.
    CASE io_request->get_entity_id( ).

      WHEN 'ZZR_DTIMP_FUNC'.
        get_function_modules( io_request = io_request io_response = io_response ).
      WHEN 'ZZR_DTIMP_STRUC'.
        get_structures( io_request = io_request io_response = io_response ).
    ENDCASE.

*      CATCH cx_rap_query_provider INTO DATA(lx_query). ##NO_HANDLER
*        ##NO_HANDLER
*    ENDTRY.
  ENDMETHOD.
ENDCLASS.
