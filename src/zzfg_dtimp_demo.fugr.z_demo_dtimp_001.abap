FUNCTION z_demo_dtimp_001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IO_DATA) TYPE REF TO  DATA OPTIONAL
*"     VALUE(IV_STRUC) TYPE  ZZESTRUCTNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(EO_DATA) TYPE REF TO  DATA
*"----------------------------------------------------------------------
*   CHANGING
*     CH_1         TYPE ANY
*   TABLES
*     TAB_P1       LIKE structure_name
*     TAB_P2       TYPE tab_type
*   RAISING
*     CX_SY_ZERODIVIDE
*     RESUMABLE(CX_SY_ASSIGN_CAST_ERROR).
  CREATE DATA eo_data TYPE TABLE OF (iv_struc).

  eo_data->* = io_data->*.

  loop at eo_data->* ASSIGNING FIELD-SYMBOL(<line>).
    <line>-('Message') = 'Created Successfully'.
    <line>-('Type') = 'S'.
  endloop.




ENDFUNCTION.
