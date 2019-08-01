*&---------------------------------------------------------------------*
*& Report  YDK_WEBS_USR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ydk_webs_usr MESSAGE-ID ydkwebs.

TABLES: sscrfields, ydk_webs_usr.

PARAMETERS: pwsusr TYPE ydk_webs_usr-wsusr OBLIGATORY.

SELECTION-SCREEN FUNCTION KEY: 1.

DATA: create  TYPE abap_bool.
DATA: view    TYPE abap_bool.
DATA: changed TYPE abap_bool.

DATA: dev_del TYPE abap_bool.

DATA: password TYPE string.
DATA: pass_changed TYPE abap_bool.

DATA: gt_dev TYPE STANDARD TABLE OF ydk_webs_dev.
FIELD-SYMBOLS <dev> LIKE LINE OF gt_dev.

DATA: alvg_dev TYPE REF TO cl_gui_alv_grid.

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_toolbar_dev                      " TOOLBAR
          FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
          e_object
          e_interactive
          sender.

    METHODS handle_user_command_dev                 " USER_COMMAND for components
          FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
          e_ucomm.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar_dev.                      " TOOLBAR
*     FOR EVENT toolbar OF cl_gui_alv_grid
* IMPORTING
*     e_object
*     e_interactive
*     sender.

    REFRESH e_object->mt_toolbar.

    FIELD-SYMBOLS <toolbar> TYPE stb_button.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <toolbar>.

    <toolbar>-function  = 'DEL'.
    <toolbar>-icon      = icon_delete.
    <toolbar>-text      = ''.
    <toolbar>-quickinfo = 'Удалить'.
  ENDMETHOD.

  METHOD handle_user_command_dev.                 " USER_COMMAND for components
*   FOR EVENT user_command OF cl_gui_alv_grid
*IMPORTING
*   e_ucomm.
    CASE e_ucomm.
      WHEN 'DEL'.
        DATA: lt_rows TYPE STANDARD TABLE OF lvc_s_row.
        FIELD-SYMBOLS: <row> LIKE LINE OF lt_rows.

        CALL METHOD alvg_dev->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.
        READ TABLE lt_rows INDEX 1 ASSIGNING <row>.
        CHECK sy-subrc = 0.

        DELETE gt_dev INDEX <row>-index.
        dev_del = abap_true.
        changed = abap_true.

        CALL METHOD alvg_dev->refresh_table_display
          EXPORTING
            is_stable = VALUE #( row = abap_true col = abap_true )
*           i_soft_refresh =
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


INITIALIZATION.
  IF sy-tcode CS '03'.
    view = abap_true.
  ELSE.
    sscrfields-functxt_01 = 'Create'.
  ENDIF.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      create = abap_true.
      sscrfields-ucomm = 'ONLI'.
  ENDCASE.

START-OF-SELECTION.
  IF create = abap_true.
    PERFORM create.
  ELSE.
    PERFORM get_data.
  ENDIF.

FORM get_data.
  SELECT SINGLE *
    FROM ydk_webs_usr
   WHERE wsusr = pwsusr.

  IF sy-subrc <> 0.
    MESSAGE s009 WITH pwsusr DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT * INTO TABLE gt_dev
    FROM ydk_webs_dev
   WHERE wsusr = pwsusr.
  SORT gt_dev BY num.

  CALL SCREEN 2000.
ENDFORM.

FORM create.
  SELECT SINGLE *
    FROM ydk_webs_usr
   WHERE wsusr = pwsusr.

  IF sy-subrc = 0.
    MESSAGE s010 WITH pwsusr DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CLEAR ydk_webs_usr.
  ydk_webs_usr-wsusr = pwsusr.

  CALL SCREEN 2000.
ENDFORM.

MODULE status_2000 OUTPUT.
  PERFORM status_2000.
ENDMODULE.                 " STATUS_2000  OUTPUT

FORM status_2000.
  DATA: title TYPE string.
  IF view = abap_true.
    title = 'View'.
  ELSE.
    IF create = abap_true.
      title = 'Create'.
    ELSE.
      title = 'Edit'.
    ENDIF.
  ENDIF.

  SET PF-STATUS 'MAIN'.
  SET TITLEBAR 'ANY' WITH title pwsusr.

  IF ydk_webs_usr-is_secondary_login = abap_false.
    LOOP AT SCREEN.
      CHECK screen-group1 = 'S'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  PERFORM create_objects_2000.
ENDFORM.

MODULE user_command_2000 INPUT.
  PERFORM user_command_2000.
ENDMODULE.                 " USER_COMMAND_2000  INPUT

FORM user_command_2000.
  DATA: answer TYPE c LENGTH 1.
  CASE sy-ucomm.
    WHEN 'BACK'.
      IF changed = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            textline1 = 'There are changes, will save?'
            titel     = 'Exit'
          IMPORTING
            answer    = answer.
        CASE answer.
          WHEN 'J'. PERFORM save.
          WHEN 'A'. RETURN.
        ENDCASE.
      ENDIF.

      CLEAR sy-subrc.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN 'REFS'.
      CLEAR: password, pass_changed.
  ENDCASE.
ENDFORM.

MODULE set_change INPUT.
  changed = abap_true.
ENDMODULE.                 " SET_CHANGE  INPUT

MODULE pass_changed INPUT.
  pass_changed = abap_true.
  changed = abap_true.
ENDMODULE.                 " PASS_CHANGED  INPUT

FORM save.
  IF dev_del = abap_true.
    SORT gt_dev BY num.
    LOOP AT gt_dev ASSIGNING <dev>.
      <dev>-num = sy-tabix.
    ENDLOOP.

    DELETE FROM ydk_webs_dev WHERE wsusr = pwsusr.
    INSERT ydk_webs_dev FROM TABLE gt_dev.
  ENDIF.

  IF create = abap_true OR pass_changed = abap_true.
    CALL FUNCTION 'YDK_WEBS_MODIFY_USER'
      EXPORTING
        usr                 = ydk_webs_usr
        password            = password
      EXCEPTIONS
        invalid_data        = 1
        pass_hash_collision = 2
        OTHERS              = 3.
  ELSE.
    MODIFY ydk_webs_usr.
    COMMIT WORK.
  ENDIF.

  CLEAR: changed, dev_del, pass_changed.

  MESSAGE 'Сохранено' TYPE 'S'.
ENDFORM.

FORM create_objects_2000.
  CHECK alvg_dev IS INITIAL.

  DATA: container TYPE REF TO cl_gui_custom_container.

  CREATE OBJECT container
    EXPORTING
      container_name = 'DEV'.

  CREATE OBJECT g_event_receiver.

  PERFORM alv_dev_prepare USING container.
ENDFORM.

FORM alv_dev_prepare USING container.
  CREATE OBJECT alvg_dev
    EXPORTING
      i_parent = container.

  DATA: variant TYPE disvariant.
  DATA: layout  TYPE lvc_s_layo.
  DATA: it_toolbar_excluding TYPE ui_functions.

  variant-report = sy-repid.
  variant-handle = 'DEV'.

  layout-cwidth_opt = abap_true.

  SET HANDLER g_event_receiver->handle_toolbar_dev      FOR alvg_dev.
  SET HANDLER g_event_receiver->handle_user_command_dev FOR alvg_dev.

  DATA: fc TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'YDK_WEBS_DEV'
    CHANGING
      ct_fieldcat            = fc
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  fc[ fieldname = 'WSUSR' ]-tech = abap_true.
  fc[ fieldname = 'NUM' ]-tech   = abap_true.

  CALL METHOD alvg_dev->set_table_for_first_display
    EXPORTING
      i_structure_name = 'YDK_WEBS_DEV'
      is_variant       = variant
      i_save           = 'A'
      i_default        = 'X' " default wa_layout allowed
      is_layout        = layout
    CHANGING
      it_outtab        = gt_dev
      it_fieldcatalog  = fc.
ENDFORM.
