REPORT zps_carga_proj.

INCLUDE zps_carga_proj_top.
INCLUDE zps_carga_proj_f01.

START-OF-SELECTION.

  PERFORM f_download_arq  USING p_file.
  IF gt_dados[] IS NOT INITIAL.
    CASE p_check.
      WHEN space.
        PERFORM f_bapi_execute.
        PERFORM f_log.

      WHEN abap_true.
        PERFORM f_test.
        PERFORM f_log.
    ENDCASE.
  ELSE.
    MESSAGE text-er1 TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
