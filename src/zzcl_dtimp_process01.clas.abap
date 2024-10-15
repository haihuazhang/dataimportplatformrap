CLASS zzcl_dtimp_process01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zzif_process_data .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zzcl_dtimp_process01 IMPLEMENTATION.


  METHOD zzif_process_data~process.
    DATA : ls_message TYPE zzs_dmp_data_list.

    CREATE DATA eo_data TYPE TABLE OF (iv_structure).

    eo_data->* = io_data->*.

    LOOP AT io_data->* ASSIGNING FIELD-SYMBOL(<fs_data>).
      ls_message =  VALUE #( line = sy-tabix
                              message_list = VALUE #(  (
                                id = 'SY'
                                number = '530'
                                type =  'S'

                               )
                               ( id = 'ZZFI'
                                 number = '003'
                                 type = 'E' )   )

      ).
      APPEND ls_message TO et_message.
    ENDLOOP.



  ENDMETHOD.
ENDCLASS.
