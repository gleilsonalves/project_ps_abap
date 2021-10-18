*&---------------------------------------------------------------------*
*&  Include           ZPS_CARGA_PROJ_F01
*&---------------------------------------------------------------------*

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

    SPLIT ls_arq AT ';' INTO gs_dados-prod         "Produto
                             gs_dados-sba_ofer     "SBA/OFERTA
                             gs_dados-respons      "Responsável
                             gs_dados-requernt     "Requerente
                             gs_dados-port_proj    "Porte do Projeto
                             gs_dados-mod_part     "Mod. Particip.
                             gs_dados-mercado      "Mercado
                             gs_dados-cliente      "Cliente
                             gs_dados-pep          "XXXX do PEP padrão standard
                             gs_dados-pep0         "000000 do PEP padrão standard
                             gs_dados-inicio       "Data de inicio
                             gs_dados-fim          "Data de fim
                             gs_dados-def_proj     "Definição do projeto
                             gs_dados-desc         "Descrição projeto
                             gs_dados-centro       "Centro
                             gs_dados-centro_lcr.  "Centro de lucro

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
        lv_resp_name  TYPE proj-verna,
        lv_requernt   TYPE proj-astnr,
        lv_req_name   TYPE proj-astna,
        lv_centro     TYPE proj-werks,
        lv_centro_lcr TYPE proj-prctr,
        lv_inicio     TYPE proj-plfaz,
        lv_fim        TYPE proj-plsez,
        lv_old_ucomm  TYPE sy-ucomm,
        lv_ok(1)      TYPE c.

  DATA: lv_pspid TYPE proj-pspid,
        lv_vbukr TYPE proj-vbukr,
        lv_plfaz TYPE proj-plfaz,
        lv_plsez TYPE proj-plsez,
        lv_kalid TYPE proj-kalid,
        lv_zteht TYPE proj-zteht.

  CONSTANTS: lc_p(1)         TYPE c VALUE 'P',
             lc_x(1)         TYPE c VALUE 'X',
             lc_wbs_stand(8) TYPE c VALUE 'DXXXX001',
             lc_change(1)    TYPE c VALUE 'V'.

  CLEAR: gs_dados, gs_log.

  LOOP AT gt_dados INTO gs_dados.

    CLEAR: ls_proj, lv_respons, lv_requernt, lv_centro,
           lv_centro_lcr, lv_inicio, lv_resp_name, lv_req_name,
           lv_fim, lv_old_ucomm, lv_ok, lv_pspid, lv_vbukr,
           lv_plfaz, lv_plsez, lv_kalid, lv_zteht.

    lv_respons    = gs_dados-respons.
    lv_requernt   = gs_dados-requernt.
    lv_centro     = gs_dados-centro.
    lv_centro_lcr = gs_dados-centro_lcr.

    PERFORM f_search_names USING lv_respons lv_requernt
                           CHANGING lv_resp_name lv_req_name.

*   Exportações vão ser importadas no include FCJWBFKP_EB_DEFINITION_EINBIND na ENHANCEMENT Z_ENHAN_PROJ_DEF1
*   Dados reference aos campos obrigatório da criação do projeto ao chamar a bapi CJWB_PROJECT_COPY
    FREE MEMORY ID: 'ZRESP', 'ZREQ', 'ZCENT', 'ZCENT_LCR'.
    EXPORT lv_respons    TO MEMORY ID 'ZRESP'.
    EXPORT lv_resp_name  TO MEMORY ID 'ZRESP_NAME'.
    EXPORT lv_requernt   TO MEMORY ID 'ZREQ'.
    EXPORT lv_req_name   TO MEMORY ID 'ZREQ_NAME'.
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
      IF sy-subrc EQ 1.
        gs_log-icon    = '@09@'.
        gs_log-projeto = gs_dados-def_proj.
        gs_log-message = text-er2.

        APPEND gs_log TO gt_log.
        CLEAR gs_log.
        CONTINUE.
      ENDIF.
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

      gs_log-icon    = '@0A@'.
      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = text-er3.
      APPEND gs_log TO gt_log.
      CLEAR gs_log.

      lv_ok = space.
      CONTINUE.

    ELSE.

      CLEAR ls_proj-post1.

      lv_pspid = ls_proj-pspid.
      lv_vbukr = ls_proj-vbukr.
      lv_plfaz = ls_proj-plfaz.
      lv_plsez = ls_proj-plsez.
      lv_kalid = ls_proj-kalid.
      lv_zteht = 'DIA'.

      ls_proj-post1 = gs_dados-desc.

      CALL FUNCTION 'CJDW_PROJ_MODIFY'
        EXPORTING
          i_proj        = ls_proj
        EXCEPTIONS
          beakz         = 1
          pspnr         = 2
          error_message = 98
          OTHERS        = 99.

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
            PERFORM f_pep_level USING ls_prps_aux gs_dados lv_resp_name lv_req_name CHANGING ls_prps.

            IF ls_prps-stufe <> 3.
              ls_prps-werks = gs_dados-centro.
              ls_prps-prctr = gs_dados-centro_lcr.
            ENDIF.

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
      DATA(lv_status) = space.
      PERFORM f_call_cj02 USING lv_pspid lv_vbukr
                                lv_plfaz lv_plsez
                                lv_kalid lv_zteht
                          CHANGING lv_status.

      IF lv_status EQ abap_true.
        gs_log-icon    = '@08@'.
        gs_log-projeto = gs_dados-def_proj.
        gs_log-message = text-ss2.
        APPEND gs_log TO gt_log.
        CLEAR gs_log.
      ELSE.
        gs_log-icon    = '@09@'.
        gs_log-projeto = gs_dados-def_proj.
        gs_log-message = text-ss3.
        APPEND gs_log TO gt_log.
        CLEAR gs_log.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PEP_LEVEL
*&---------------------------------------------------------------------*
FORM f_pep_level USING l_prps_aux l_dados l_resp_name l_req_name CHANGING l_prps.

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
    ls_prps-verna = l_resp_name.
    ls_prps-astnr = ls_dados-requernt.
    ls_prps-astna = l_req_name.
    ls_prps-usr00 = ls_dados-port_proj.
    ls_prps-usr01 = ls_dados-mod_part.
    ls_prps-usr02 = ls_dados-mercado.
    ls_prps-usr03 = ls_dados-cliente.
  ENDIF.

  IF ls_prps_aux-stufe = 2.
    REPLACE 'XXXX'   IN ls_prps-posid WITH ls_dados-pep.
    REPLACE '000000' IN ls_prps-posid WITH ls_dados-pep0.

    ls_prps-prart = ls_dados-prod.
    ls_prps-pspri = ls_dados-sba_ofer.
    ls_prps-vernr = ls_dados-respons.
    ls_prps-verna = l_resp_name.
    ls_prps-astnr = ls_dados-requernt.
    ls_prps-astna = l_req_name.
    ls_prps-usr00 = ls_dados-port_proj.
    ls_prps-usr01 = ls_dados-mod_part.
    ls_prps-usr02 = ls_dados-mercado.
    ls_prps-usr03 = ls_dados-cliente.
  ENDIF.

  IF ls_prps_aux-stufe = 3.
    REPLACE 'XXXX'   IN ls_prps-posid WITH ls_dados-pep.
    REPLACE '000000' IN ls_prps-posid WITH ls_dados-pep0.

    ls_prps-prart = ls_dados-prod.
    ls_prps-pspri = ls_dados-sba_ofer.
    ls_prps-vernr = ls_dados-respons.
    ls_prps-verna = l_resp_name.
    ls_prps-astnr = ls_dados-requernt.
    ls_prps-astna = l_req_name.
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
  ls_fieldcat-seltext_m = text-tt3.
  ls_fieldcat-icon      = abap_true.
  ls_fieldcat-fieldname = 'ICON'.
  ls_fieldcat-tabname   = 'T_LOG'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos   = 2.
  ls_fieldcat-seltext_m = text-tt1.
  ls_fieldcat-fieldname = 'PROJETO'.
  ls_fieldcat-tabname   = 'T_LOG'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos   = 3.
  ls_fieldcat-seltext_m = text-tt2.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-tabname   = 'T_LOG'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

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

*&---------------------------------------------------------------------*
*&      Form  F_TEST
*&---------------------------------------------------------------------*
FORM f_test.

  CLEAR gs_dados.
  LOOP AT gt_dados INTO gs_dados.

    CALL FUNCTION 'CJDW_CHECK_PROJ_EXIST'
      EXPORTING
        i_pspid            = gs_dados-def_proj
      EXCEPTIONS
        proj_exist         = 1
        proj_version_exist = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      CHECK sy-subrc EQ 1.
      gs_log-icon    = '@09@'.
      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = text-er2.

      APPEND gs_log TO gt_log.
      CLEAR gs_log.
    ELSE.
      gs_log-icon    = '@08@'.
      gs_log-projeto = gs_dados-def_proj.
      gs_log-message = text-ss1.

      APPEND gs_log TO gt_log.
      CLEAR gs_log.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CALL_CJ02
*&---------------------------------------------------------------------*
FORM f_call_cj02 USING l_pspid l_vbukr l_plfaz l_plsez l_kalid l_zteht CHANGING l_status.

  DATA: lt_msg  TYPE TABLE OF bdcmsgcoll.

  DATA: lv_mode  TYPE c VALUE 'N'.

  CONCATENATE l_plfaz+6(2) l_plfaz+4(2) l_plfaz(4) INTO DATA(lv_plfaz).
  CONCATENATE l_plsez+6(2) l_plsez+4(2) l_plsez(4) INTO DATA(lv_plsez).

  CLEAR: gt_bdcdata[], lt_msg[].

  PERFORM f_batch_input USING:
        'X' 'SAPLCJWB'    '0100',
        ' ' 'BDC_CURSOR'  '*PROJ-PSPID',
        ' ' 'BDC_OKCODE'  '=MDTB',
        ' ' '*PROJ-PSPID' l_pspid,
        ' ' '*PRPS-POSID' ' '.

  PERFORM f_batch_input USING:
        'X' 'SAPLCJWB'   '0998',
        ' ' 'BDC_CURSOR' 'PROJ-PSPID',
        ' ' 'BDC_OKCODE' '=STFR',
        ' ' 'PROJ-PSPID' ' ',
        ' ' 'BDC_SUBSCR' 'SAPLCJWB                                1205SUBSCR2',
        ' ' 'PROJ-VBUKR' l_vbukr,
        ' ' 'PROJ-PLFAZ' lv_plfaz,
        ' ' 'PROJ-PLSEZ' lv_plsez,
        ' ' 'PROJ-KALID' l_kalid,
        ' ' 'PROJ-ZTEHT' l_zteht,
        ' ' 'BDC_SUBSCR' 'SAPLCJWB                                0700STATUS'.

  PERFORM f_batch_input USING:
        'X' 'SAPLCJWB'   '0998',
        ' ' 'BDC_CURSOR' 'PROJ-PSPID',
        ' ' 'BDC_OKCODE' '=BU',
        ' ' 'PROJ-PSPID' ' ',
        ' ' 'BDC_SUBSCR' 'SAPLCJWB                                1205SUBSCR2',
        ' ' 'PROJ-VBUKR' l_vbukr,
        ' ' 'PROJ-PLFAZ' lv_plfaz,
        ' ' 'PROJ-PLSEZ' lv_plsez,
        ' ' 'PROJ-KALID' l_kalid,
        ' ' 'PROJ-ZTEHT' l_zteht,
        ' ' 'BDC_SUBSCR' 'SAPLCJWB                                0700STATUS'.

  CALL TRANSACTION 'CJ02' USING gt_bdcdata
                          MODE  lv_mode
                          UPDATE 'S'
                          MESSAGES INTO lt_msg.

  READ TABLE lt_msg INTO DATA(ls_msg)
                    WITH KEY msgtyp = 'S'
                             msgnr  = '162'.

  IF sy-subrc IS INITIAL.
    l_status = abap_true.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BATCH_INPUT
*&---------------------------------------------------------------------*
FORM f_batch_input USING l_dynbegin l_name l_value.

  IF l_dynbegin = abap_true.
    MOVE: l_name      TO gs_bdcdata-program,
          l_value     TO gs_bdcdata-dynpro,
          l_dynbegin  TO gs_bdcdata-dynbegin.
    APPEND gs_bdcdata TO gt_bdcdata.
  ELSE.
    MOVE: l_name  TO gs_bdcdata-fnam,
          l_value TO gs_bdcdata-fval.
    APPEND gs_bdcdata TO gt_bdcdata.
  ENDIF.

  CLEAR gs_bdcdata.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SEARCH_NAMES
*&---------------------------------------------------------------------*
FORM f_search_names USING l_resp l_req CHANGING l_resp_name l_req_name.

  DATA: lv_resp(8) TYPE c,
        lv_req(8)  TYPE c.

  lv_resp = l_resp.
  lv_req  = l_req.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_resp
    IMPORTING
      output = lv_resp.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_req
    IMPORTING
      output = lv_req.

  l_resp = lv_resp.
  l_req  = lv_req.

  "Selecionar nome do responsavel
  SELECT vernr, verna
    FROM tcj04
    INTO TABLE @DATA(lt_tcj04)
    WHERE vernr = @l_resp.

  IF lt_tcj04[] IS NOT INITIAL.
    READ TABLE lt_tcj04 INTO DATA(ls_tcj04) INDEX 1.
    l_resp_name = ls_tcj04-verna.
  ENDIF.

  "Selecionar nome do requerente
  SELECT astnr, astna
    FROM tcj05
    INTO TABLE @DATA(lt_tcj05)
    WHERE astnr = @l_req.

  IF lt_tcj05[] IS NOT INITIAL.
    READ TABLE lt_tcj05 INTO DATA(ls_tcj05) INDEX 1.
    l_req_name = ls_tcj05-astna.
  ENDIF.

ENDFORM.
