class YDK_CL_WEBS_FILE definition
  public
  inheriting from YDK_CL_WEBS_ACTION
  final
  create public .

public section.

  types:
    BEGIN OF ty_param,
        name  TYPE wwwparams-name,
        value TYPE wwwparams-value,
      END   OF ty_param .
  types:
    ty_param_tab TYPE STANDARD TABLE OF ty_param .
  types:
    BEGIN OF ty_file_info,
             id        TYPE wwwdata-objid,
             extension TYPE wwwparams-value,
             name      TYPE wwwparams-value,
             title     TYPE wwwdata-text,
             loaddate  TYPE wwwdata-tdate,
             loadtime  TYPE wwwdata-ttime,
             size      TYPE i,
             version   TYPE wwwparams-value,
           END   OF ty_file_info .

  class-data DEFAULT_RELID type WWWDATATAB-RELID value 'MI'. "#EC NOTEXT

  class-methods GET_PARAMETERS
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
  class-methods GET_CONTENT
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
  class-methods GET_INFO
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
protected section.
private section.
ENDCLASS.



CLASS YDK_CL_WEBS_FILE IMPLEMENTATION.


  METHOD get_content.
    DATA: key TYPE wwwdatatab.
    DATA: itmime TYPE STANDARD TABLE OF w3mime.
    DATA: cfilesize(11) TYPE c.
    DATA: file_size TYPE i.
    DATA: xstr TYPE xstring.

    key-relid = default_relid.
    key-objid = action_data.
    TRANSLATE key-objid TO UPPER CASE.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = key
      TABLES
        mime              = itmime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO return_data.
      return_status = status_err.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid  = key-relid
        objid  = key-objid
        name   = 'filesize'
      IMPORTING
        value  = cfilesize
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO return_data.
      return_status = status_err.
      RETURN.
    ENDIF.

    file_size = cfilesize.

    CALL METHOD cl_umc_ostream=>tab2xstr
      EXPORTING
        it_data  = itmime
        i_length = file_size
      IMPORTING
        e_xstr   = xstr.

    CALL METHOD cl_http_utility=>encode_x_base64
      EXPORTING
        unencoded = xstr
      RECEIVING
        encoded   = return_data.

    return_status = status_ok.
  ENDMETHOD.


  METHOD get_info.
    DATA: objid TYPE wwwparams-objid.
    DATA: itparams TYPE STANDARD TABLE OF wwwparams.
    FIELD-SYMBOLS <param> LIKE LINE OF itparams.
    DATA: ret TYPE ty_file_info.
    DATA: itf TYPE STANDARD TABLE OF string.

    objid = action_data.
    TRANSLATE objid TO UPPER CASE.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = default_relid
        objid            = objid
      TABLES
        params           = itparams
      EXCEPTIONS
        entry_not_exists = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      IF sy-subrc = 1.
        MESSAGE e020(ydkwebs) WITH objid INTO return_data.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO return_data.
      ENDIF.
      return_status = status_err.
      RETURN.
    ENDIF.

    ret-id = objid.

    LOOP AT itparams ASSIGNING <param>.
      TRANSLATE <param>-name TO UPPER CASE.
      CASE <param>-name.
        WHEN 'FILEEXTENSION'.
          IF <param>-value(1) = '.'.
            SHIFT <param>-value.
          ENDIF.
          ret-extension = <param>-value.
          TRANSLATE ret-extension TO UPPER CASE.
        WHEN 'FILENAME'.
          TRANSLATE <param>-value USING '\/'.
          SPLIT <param>-value AT '/' INTO TABLE itf.
          ret-name = itf[ lines( itf ) ].
        WHEN 'FILESIZE'.
          SHIFT <param>-value LEFT DELETING LEADING space.
          ret-size = <param>-value.
        WHEN 'VERSION'.
          ret-version = <param>-value.
      ENDCASE.
    ENDLOOP.

    SELECT SINGLE
      text  AS title
      tdate AS loaddate
      ttime AS loadtime
      INTO CORRESPONDING FIELDS OF ret
      FROM wwwdata
     WHERE relid = default_relid
       AND objid = objid
       AND srtf2 = 0.

    get_json( EXPORTING data = ret IMPORTING return_data = return_data return_status = return_status ).
  ENDMETHOD.


  METHOD get_parameters.
    DATA: objid TYPE wwwparams-objid.
    DATA: itparams TYPE STANDARD TABLE OF wwwparams.
    FIELD-SYMBOLS <param> LIKE LINE OF itparams.
    DATA: itret TYPE ty_param_tab.
    FIELD-SYMBOLS <ret> LIKE LINE OF itret.

    objid = action_data.
    TRANSLATE objid TO UPPER CASE.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = default_relid
        objid            = objid
      TABLES
        params           = itparams
      EXCEPTIONS
        entry_not_exists = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      IF sy-subrc = 1.
        MESSAGE e001(ydkwebs) with objid.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO return_data.
      ENDIF.

      return_status = status_err.
      RETURN.
    ENDIF.

    LOOP AT itparams ASSIGNING <param>.
      APPEND INITIAL LINE TO itret ASSIGNING <ret>.
      MOVE-CORRESPONDING <param> TO <ret>.
    ENDLOOP.

    APPEND INITIAL LINE TO itret ASSIGNING <ret>.
    <ret>-name = 'TITLE'.
    SELECT SINGLE text INTO <ret>-value
      FROM wwwdata
     WHERE relid = default_relid
       AND objid = objid
       AND srtf2 = 0.

    get_json( EXPORTING data = itret IMPORTING return_data = return_data return_status = return_status ).
  ENDMETHOD.
ENDCLASS.
