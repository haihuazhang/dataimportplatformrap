INTERFACE zzif_process_data
  PUBLIC .
  METHODS process IMPORTING iv_structure TYPE zzestructname
                            io_data      TYPE REF TO data
                  EXPORTING eo_data      TYPE REF TO data
                            et_message   TYPE zzt_dmp_data_list.
ENDINTERFACE.
