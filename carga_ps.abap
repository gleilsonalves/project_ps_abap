REPORT zps_carga_proj.

* Tipos de estruturas
TYPES: BEGIN OF ty_arq,
         linha(800) TYPE c,
       END OF ty_arq,

       BEGIN OF ty_dados,
         prod       TYPE prps-prart,
         sba_ofer   TYPE prps-pspri,
         respons    TYPE prps-vernr,
         requernt   TYPE prps-astnr,
         port_proj  TYPE prps-usr00,
         mod_part   TYPE prps-usr01,
         mercado    TYPE prps-usr02,
         cliente    TYPE prps-usr03,
         pep(4)     TYPE c,
         inicio(10) TYPE c,
         fim(10)    TYPE c,
         def_proj   TYPE proj-pspid,
         centro     TYPE proj-werks,
         centro_lcr TYPE proj-prctr,
       END OF ty_dados,

       BEGIN OF ty_log,
         projeto     TYPE proj-pspid,
         message(50) TYPE c,
       END OF ty_log.

* Tabelas internas
DATA: gt_dados TYPE TABLE OF ty_dados,
      gt_log   TYPE TABLE OF ty_log.

* Work Areas
DATA: gs_dados TYPE ty_dados,
      gs_log   TYPE ty_log.

* Tela de seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1.

* Verificar arquivo selecionado.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_valida_arq USING p_file .

START-OF-SELECTION.

  PERFORM f_download_arq  USING p_file.
  IF gt_dados[] IS NOT INITIAL.
    PERFORM f_bapi_execute.
    PERFORM f_log.
  ELSE.
    MESSAGE text-er1 TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_ARQ
*&---------------------------------------------------------------------*
FORM f_download_arq USING p_file TYPE rlgrap-filename.

  DATA: gc_file TYPE string,
        lt_arq  TYPE TABLE OF ty_arq,
        ls_arq  TYPE ty_arq.

  DATA: lv_inicio(8) TYPE c,
        lv_fim(8)    TYPE c.

  CLEAR: gc_file, gs_dados, ls_arq.
  FREE  lt_arq.

  gc_file = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename        = gc_file
    TABLES
      data_tab        = lt_arq
    EXCEPTIONS
      file_open_error = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_arq INTO ls_arq.

    SPLIT ls_arq AT ';' INTO gs_dados-prod
                             gs_dados-sba_ofer
                             gs_dados-respons
                             gs_dados-requernt
                             gs_dados-port_proj
                             gs_dados-mod_part
                             gs_dados-mercado
                             gs_dados-cliente
                             gs_dados-pep
                             gs_dados-inicio
                             gs_dados-fim
                             gs_dados-def_proj
                             gs_dados-centro
                             gs_dados-centro_lcr.

    "Ignorar a linha de cabeçalho do arquivo
    IF sy-tabix EQ 1.
      CONTINUE.
    ENDIF.

    CONCATENATE gs_dados-inicio+6(4) gs_dados-inicio+3(2) gs_dados-inicio(2)
           INTO lv_inicio.

    CONCATENATE gs_dados-fim+6(4) gs_dados-fim+3(2) gs_dados-fim(2)
           INTO lv_fim.

    CLEAR: gs_dados-inicio, gs_dados-fim.

    gs_dados-inicio = lv_inicio.
    gs_dados-fim    = lv_fim.

    APPEND gs_dados TO gt_dados.
    CLEAR: gs_dados, lv_inicio, lv_fim.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ARQ
*&---------------------------------------------------------------------*
FORM f_valida_arq USING p_file1 TYPE rlgrap-filename.

  IF sy-batch IS INITIAL.

    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      CHANGING
        file_name     = p_file1
      EXCEPTIONS
        mask_too_long = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BAPI_EXECUTE
*&---------------------------------------------------------------------*
FORM f_bapi_execute.

  DATA: lt_cjdi   TYPE TABLE OF rcj_markl,
        lt_return TYPE STANDARD TABLE OF bapiret2.

  DATA: ls_proj     LIKE proj,
        ls_prps     LIKE prps,
        ls_prps_aux LIKE prps,
        lv_vsnmr    LIKE vskopf-vsnmr,
        ls_cjdi     LIKE LINE OF lt_cjdi,
        ls_tcn41    LIKE tcn41,
        ls_tc10     LIKE tc10.

  DATA: lv_respons    TYPE proj-vernr,
        lv_requernt   TYPE proj-astnr,
        lv_centro     TYPE proj-werks,
        lv_centro_lcr TYPE proj-prctr,
        lv_inicio     TYPE proj-plfaz,
        lv_fim        TYPE proj-plsez,
        lv_old_ucomm  TYPE sy-ucomm,
        lv_ok(1)      TYPE c.

  CONSTANTS: lc_p(1)         TYPE c VALUE 'P',
             lc_x(1)         TYPE c VALUE 'X',
             lc_wbs_stand(8) TYPE c VALUE 'DXXXX001',
             lc_change(1)    TYPE c VALUE 'V'.

  CLEAR: gs_dados, gs_log.

  LOOP AT gt_dados INTO gs_dados.

    CLEAR: ls_proj, lv_respons,
           lv_requernt, lv_centro,
           lv_centro_lcr, lv_inicio,
           lv_fim, lv_old_ucomm, lv_ok.

    lv_respons    = gs_dados-respons.
    lv_requernt   = gs_dados-requernt.
    lv_centro     = gs_dados-centro.
    lv_centro_lcr = gs_dados-centro_lcr.

*   Exportações vão ser importadas no include FCJWBFKP_EB_DEFINITION_EINBIND na ENHANCEMENT Z_ENHAN_PROJ_DEF1
    FREE MEMORY ID: 'ZRESP', 'ZREQ', 'ZCENT', 'ZCENT_LCR'.
    EXPORT lv_respons    TO MEMORY ID 'ZRESP'.
    EXPORT lv_requernt   TO MEMORY ID 'ZREQ'.
    EXPORT lv_centro     TO MEMORY ID 'ZCENT'.
    EXPORT lv_centro_lcr TO MEMORY ID 'ZCENT_LCR'.

    lv_inicio = gs_dados-inicio.
    lv_fim    = gs_dados-fim.

    CALL FUNCTION 'CJDW_CHECK_PROJ_EXIST'
      EXPORTING
        i_pspid            = gs_dados-def_proj
      EXCEPTIONS
        proj_exist         = 1
        proj_version_exist = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      CHECK sy-subrc EQ 1.

      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = text-er2.

      APPEND gs_log TO gt_log.
      CLEAR gs_log.
      CONTINUE.
    ENDIF.

    lv_old_ucomm = sy-ucomm.
    sy-ucomm     = 'GEN1'.

    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

    CALL FUNCTION 'CJWB_PROJECT_COPY'
      EXPORTING
        call_from                  = lc_p
        standard_wbs_to_be_copied  = lc_wbs_stand
        start_termin               = lv_inicio
        end_termin                 = lv_fim
        no_dialog                  = lc_x
        replace_string             = gs_dados-def_proj
      IMPORTING
        project_workarea           = ls_proj
        vsnmr_copy                 = lv_vsnmr
      EXCEPTIONS
        existing_project           = 1
        existing_project_in_paging = 2
        wrong_call                 = 3
        wbs_for_copy_not_found     = 4
        error_pspid_generate       = 5
        no_copy                    = 6
        error                      = 7
        OTHERS                     = 8.

    IF sy-subrc <> 0.

      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = 'Erro ao criar o projeto!'.
      APPEND gs_log TO gt_log.
      CLEAR gs_log.
      lv_ok = space.

    ELSE.

      CALL FUNCTION 'CJDW_GET_TREE'
        EXPORTING
          levels    = '99'
          x_outline = space
        TABLES
          elements  = lt_cjdi
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF sy-subrc IS INITIAL.
        LOOP AT lt_cjdi INTO ls_cjdi.

          CLEAR ls_prps.
          CALL FUNCTION 'CJDW_PRPS_GET'
            EXPORTING
              index     = ls_cjdi-index
            IMPORTING
              e_prps    = ls_prps
            EXCEPTIONS
              cancel    = 1
              not_found = 2
              OTHERS    = 3.

          IF sy-subrc IS INITIAL.

            CLEAR ls_prps_aux.
            MOVE-CORRESPONDING ls_prps TO ls_prps_aux.
            PERFORM f_pep_level USING ls_prps_aux gs_dados CHANGING ls_prps.

            ls_prps-werks = gs_dados-centro.
            ls_prps-prctr = gs_dados-centro_lcr.

            CALL FUNCTION 'CJDW_PRPS_MODIFY'
              EXPORTING
                beakz     = lc_change
                index     = ls_cjdi-index
                i_prps    = ls_prps
              EXCEPTIONS
                not_found = 1
                posnr     = 2
                OTHERS    = 3.

            IF sy-subrc <> 0.
            ENDIF.

          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'CJWB_CHECK_BEFORE_COMMIT'
        EXCEPTIONS
          cancel = 1.

      CALL FUNCTION 'CJDW_GET_NEW_NUMBERS'.
      CALL FUNCTION 'CJDT_GET_NEW_NUMBERS'.

      CALL FUNCTION 'BAPI_PS_PRECOMMIT'
        TABLES
          et_return = lt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      lv_ok = abap_true.
      sy-ucomm = lv_old_ucomm.

      SET SCREEN 0.

      CALL FUNCTION 'LOCATION_CORRECT'
        EXPORTING
          bild            = space
          bildgruppe      = space
          kopfgruppe      = space
          positionsgruppe = space
          programm        = space
          stackstufe      = 0
          trtyp           = space.

    ENDIF.

    IF lv_ok IS NOT INITIAL.
      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = 'Projeto Criado com Sucesso!'.
      APPEND gs_log TO gt_log.
      CLEAR gs_log.
    ELSE.
      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = 'Projeto não foi criado!'.
      APPEND gs_log TO gt_log.
      CLEAR gs_log.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PEP_LEVEL
*&---------------------------------------------------------------------*
FORM f_pep_level USING l_prps_aux l_dados CHANGING l_prps.

  DATA: ls_prps_aux LIKE prps,
        ls_dados    TYPE ty_dados,
        ls_prps     LIKE prps.

  CLEAR: ls_prps_aux, ls_dados.

  MOVE-CORRESPONDING l_prps_aux TO ls_prps_aux.
  MOVE-CORRESPONDING l_dados TO ls_dados.
  MOVE-CORRESPONDING l_prps TO ls_prps.

  IF ls_prps_aux-stufe = 1.
    REPLACE 'XXXX' IN ls_prps-posid WITH ls_dados-pep.

    ls_prps-prart = ls_dados-prod.
    ls_prps-pspri = ls_dados-sba_ofer.
    ls_prps-vernr = ls_dados-respons.
    ls_prps-astnr = ls_dados-requernt.
    ls_prps-usr00 = ls_dados-port_proj.
    ls_prps-usr01 = ls_dados-mod_part.
    ls_prps-usr02 = ls_dados-mercado.
    ls_prps-usr03 = ls_dados-cliente.
  ENDIF.

  IF ls_prps_aux-stufe = 2.
    REPLACE 'XXXX' IN ls_prps-posid WITH ls_dados-pep.

    ls_prps-vernr = ls_dados-respons.
    ls_prps-astnr = ls_dados-requernt.
    ls_prps-usr00 = ls_dados-port_proj.
    ls_prps-usr01 = ls_dados-mod_part.
    ls_prps-usr02 = ls_dados-mercado.
    ls_prps-usr03 = ls_dados-cliente.
  ENDIF.

  IF ls_prps_aux-stufe = 3.
    REPLACE 'XXXX' IN ls_prps-posid WITH ls_dados-pep.

    ls_prps-vernr = ls_dados-respons.
    ls_prps-astnr = ls_dados-requernt.
  ENDIF.

  MOVE-CORRESPONDING ls_prps TO l_prps.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_LOG
*&---------------------------------------------------------------------*
FORM f_log.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv.

  ls_fieldcat-col_pos   = 1.
  ls_fieldcat-fieldname = 'PROJETO'.
  ls_fieldcat-tabname   = 'T_LOG'.

  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-col_pos   = 2.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-tabname   = 'T_LOG'.

  APPEND ls_fieldcat TO lt_fieldcat.

  ls_layout-zebra = abap_true.
  ls_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = 'SY-REPID'
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
    TABLES
      t_outtab           = gt_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.
