CLASS zzcl_get_job_status DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zzcl_get_job_status IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA jobname  TYPE cl_apj_rt_api=>ty_jobname .
    DATA jobcount   TYPE cl_apj_rt_api=>ty_jobcount  .
    DATA jobstatus  TYPE cl_apj_rt_api=>ty_job_status  .
    DATA jobstatustext  TYPE cl_apj_rt_api=>ty_job_status_text .

    DATA lt_original_data TYPE STANDARD TABLE OF zzc_zt_dtimp_files WITH DEFAULT KEY.
    lt_original_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).

      TRY.

          IF <fs_original_data>-jobname IS NOT INITIAL AND <fs_original_data>-jobcount IS NOT INITIAL.

*            cl_apj_rt_api=>get_job_status(
*              EXPORTING
*                iv_jobname  = CONV zzejobname( <fs_original_data>-jobname )
*                iv_jobcount = CONV zzejobcount( <fs_original_data>-jobcount )
*              IMPORTING
*                ev_job_status = jobstatus
*                ev_job_status_text = jobstatustext
*              ).

            DATA(ls_job_info) = cl_apj_rt_api=>get_job_details( iv_jobname  = CONV #( <fs_original_data>-jobname )
                                                                iv_jobcount = CONV #( <fs_original_data>-jobcount ) ).

            <fs_original_data>-jobstatus = ls_job_info-status.
            <fs_original_data>-jobstatustext = ls_job_info-status_text.

            CASE ls_job_info-status.
              WHEN 'F'. "Finished
                <fs_original_data>-jobstatuscriticality = 3.
              WHEN 'A'. "Aborted
                <fs_original_data>-jobstatuscriticality = 1.
              WHEN 'R'. "Running
                <fs_original_data>-jobstatuscriticality = 2.
              WHEN OTHERS.
                <fs_original_data>-jobstatuscriticality = 0.
            ENDCASE.

            <fs_original_data>-logstatus = ls_job_info-logstatus.

            CASE ls_job_info-logstatus.
              WHEN 'S'. "Finished
                <fs_original_data>-logstatustext = 'Success'.
                <fs_original_data>-logstatuscriticality = 3.
              WHEN 'E'. "Aborted
                <fs_original_data>-logstatustext = 'Error'.
                <fs_original_data>-logstatuscriticality = 1.
              WHEN OTHERS.
                <fs_original_data>-logstatustext = 'None'.
                <fs_original_data>-logstatuscriticality = 0.
            ENDCASE.

            DATA(lv_loghandle) = cl_web_http_utility=>escape_url( CONV #( <fs_original_data>-loghandle ) ).

            DATA(lv_url) = |#ApplicationJob-show?JobCatalogEntryName=&/v4_JobRunLog/%252F| &&
                           |ApplicationLogOverviewSet('{ lv_loghandle }')%20%252F| &&
                           |JobRunOverviewSet(JobName%253D'{ <fs_original_data>-jobname }'%252C| &&
                           |JobRunCount='{ <fs_original_data>-jobcount }')%20default|.

            <fs_original_data>-applicationlogurl = lv_url.
          ENDIF.

        CATCH cx_apj_rt INTO DATA(exception).

          DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

          <fs_original_data>-jobstatus = ''.
          <fs_original_data>-jobstatustext = exception->get_text(  ).
          <fs_original_data>-jobstatuscriticality = 0.

        CATCH cx_root INTO DATA(root_exception).

*          RAISE EXCEPTION TYPE zapp_cx_demo_01
*            EXPORTING
*              previous = root_exception.

      ENDTRY.

    ENDLOOP.

    ct_calculated_data = CORRESPONDING #(  lt_original_data ).
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    CONSTANTS fieldname_jobcount TYPE string VALUE 'JOBCOUNT'.
    CONSTANTS fieldname_jobname TYPE string VALUE 'JOBNAME'.

    IF iv_entity <> 'ZZC_ZT_DTIMP_FILES'.
*      RAISE EXCEPTION TYPE
*        EXPORTING
*          textid        = zapp_cx_demo_01=>generic_error
*          error_value_1 = |{ iv_entity } has no virtual elements|.
    ENDIF.

    LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_calc_element>).
      CASE <fs_calc_element>.
        WHEN 'JOBSTATUS' .
          COLLECT fieldname_jobcount INTO et_requested_orig_elements.
          COLLECT fieldname_jobname INTO et_requested_orig_elements.
*          APPEND 'JOBCOUNT' TO et_requested_orig_elements.
*          APPEND 'JOBNAME' TO et_requested_orig_elements.
        WHEN 'JOBSTATUSTEXT'.
          COLLECT fieldname_jobcount INTO et_requested_orig_elements.
          COLLECT fieldname_jobname INTO et_requested_orig_elements.
*          APPEND 'JOBCOUNT' TO et_requested_orig_elements.
*          APPEND 'JOBNAME' TO et_requested_orig_elements.
        WHEN 'JOBSTATUSCRITICALITY'.
          COLLECT fieldname_jobcount INTO et_requested_orig_elements.
          COLLECT fieldname_jobname INTO et_requested_orig_elements.
        WHEN OTHERS.
*          RAISE EXCEPTION TYPE zapp_cx_demo_01
*            EXPORTING
*              textid        = zapp_cx_demo_01=>generic_error
*              error_value_1 = |Virtual element { <fs_calc_element> } not known|.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
