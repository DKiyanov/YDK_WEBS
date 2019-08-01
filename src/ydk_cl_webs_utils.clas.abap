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
    TYPES: BEGIN OF ty_proc,
             type TYPE string,
             id   TYPE string,
           END   OF ty_proc.
    TYPES: ty_proc_tab TYPE STANDARD TABLE OF ty_proc.

    TYPES: BEGIN OF ty_text,
             key   TYPE string,
             value TYPE string,
           END   OF ty_text.
    TYPES: ty_text_tab TYPE STANDARD TABLE OF ty_text.

    DATA: itproc TYPE ty_proc_tab.
    FIELD-SYMBOLS <proc> LIKE LINE OF itproc.

    DATA: itret TYPE ty_text_tab.
    FIELD-SYMBOLS <ret> LIKE LINE OF itret.

    DATA: ittext TYPE STANDARD TABLE OF textpool.
    FIELD-SYMBOLS <text> LIKE LINE OF ittext.

    return_status = status_err.

    from_json( EXPORTING json = action_data CHANGING data = itproc ).

    DATA: repid TYPE sy-repid.
    DATA: class_name TYPE seoclsname.

    LOOP AT itproc ASSIGNING <proc>.
      IF <proc>-type = 'CLAS'.
        CLEAR class_name WITH '='.
        CONCATENATE <proc>-id class_name INTO class_name.
        CONCATENATE class_name 'CP' INTO repid.
      ELSE.
        repid = <proc>-id.
      ENDIF.

      REFRESH ittext.
      READ TEXTPOOL repid INTO ittext.

      LOOP AT ittext ASSIGNING <text>.
        APPEND INITIAL LINE TO itret ASSIGNING <ret>.
        <ret>-value = <text>-entry.

        IF <text>-id = 'I'.
          CONCATENATE <proc>-type '&' <proc>-id '|' <text>-key INTO <ret>-key.
        ELSE.
          CONCATENATE <proc>-type '&' <proc>-id '|' <text>-id '@' <text>-key INTO <ret>-key.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    get_json( EXPORTING data = itret IMPORTING return_data = return_data return_status = return_status ).
  ENDMETHOD.
ENDCLASS.
