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
         pep0(6)    TYPE c,
         inicio(10) TYPE c,
         fim(10)    TYPE c,
         def_proj   TYPE proj-pspid,
         desc       TYPE proj-post1,
         centro     TYPE proj-werks,
         centro_lcr TYPE proj-prctr,
       END OF ty_dados,

       BEGIN OF ty_log,
         icon(4)     TYPE c,
         projeto     TYPE proj-pspid,
         message(50) TYPE c,
       END OF ty_log.

* Tabelas internas
DATA: gt_dados   TYPE TABLE OF ty_dados,
      gt_log     TYPE TABLE OF ty_log,
      gt_bdcdata TYPE STANDARD TABLE OF bdcdata.

* Work Areas
DATA: gs_dados   TYPE ty_dados,
      gs_log     TYPE ty_log,
      gs_bdcdata TYPE bdcdata.

* Tela de seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY,
            p_check AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b1.

* Verificar arquivo selecionado.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_valida_arq USING p_file .
