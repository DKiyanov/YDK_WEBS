class YDK_CL_WEBS_UTILS definition
  public
  inheriting from YDK_CL_WEBS_ACTION
  final
  create public .

public section.

  class-methods GET_PROG_TEXTS
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
protected section.
private section.
ENDCLASS.



CLASS YDK_CL_WEBS_UTILS IMPLEMENTATION.


  METHOD get_prog_texts.
    TYPES: ty_proc_tab TYPE STANDARD TABLE OF string.

    TYPES: BEGIN OF ty_text,
             key   TYPE string,
             value TYPE string,
           END   OF ty_text.
    TYPES: ty_text_tab TYPE STANDARD TABLE OF ty_text.

    TYPES: BEGIN OF ty_handler,
             type TYPE ydk_webs_act-handler_type,
             name TYPE ydk_webs_act-handler_name,
           END   OF ty_handler.
    DATA: lt_handler TYPE STANDARD TABLE OF ty_handler.
    FIELD-SYMBOLS <handler> LIKE LINE OF lt_handler.

    DATA: itproc TYPE ty_proc_tab.
    FIELD-SYMBOLS <handler_id> LIKE LINE OF itproc.

    DATA: itret TYPE ty_text_tab.
    FIELD-SYMBOLS <ret> LIKE LINE OF itret.

    DATA: ittext TYPE STANDARD TABLE OF textpool.
    FIELD-SYMBOLS <text> LIKE LINE OF ittext.

    return_status = status_err.

    from_json( EXPORTING json = action_data CHANGING data = itproc ).

    DATA: repid TYPE sy-repid.
    DATA: class_name TYPE seoclsname.
    DATA: key TYPE string.

    LOOP AT itproc ASSIGNING <handler_id>.
      SELECT DISTINCT
        handler_type AS type
        handler_name AS name
        INTO CORRESPONDING FIELDS OF TABLE lt_handler
        FROM ydk_webs_act
       WHERE handler_id = <handler_id>.

      LOOP AT lt_handler ASSIGNING <handler>.
        IF <handler>-type IS INITIAL.
          <handler>-type = 'M'.
        ENDIF.
        IF <handler>-name IS INITIAL.
          <handler>-name = <handler_id>.
        ENDIF.
      ENDLOOP.

      SORT lt_handler.
      DELETE ADJACENT DUPLICATES FROM lt_handler.

      LOOP AT lt_handler ASSIGNING <handler>.
        CASE <handler>-type.
          WHEN 'M'. " Class
            CLEAR class_name WITH '='.
            CONCATENATE <handler>-name class_name INTO class_name.
            CONCATENATE class_name 'CP' INTO repid.
          WHEN 'S'. " Program
            repid = <handler>-name.
        ENDCASE.

        REFRESH ittext.
        READ TEXTPOOL repid INTO ittext.

        LOOP AT ittext ASSIGNING <text>.
          IF <text>-id = 'I'.
            CONCATENATE <handler_id> '|' <text>-key INTO key.
          ELSE.
            CONCATENATE <handler_id> '|' <text>-id '@' <text>-key INTO key.
          ENDIF.

          CHECK NOT line_exists( itret[ key = key ] ).

          APPEND INITIAL LINE TO itret ASSIGNING <ret>.
          <ret>-key = key.
          <ret>-value = <text>-entry.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    get_json( EXPORTING data = itret IMPORTING return_data = return_data return_status = return_status ).
  ENDMETHOD.
ENDCLASS.
