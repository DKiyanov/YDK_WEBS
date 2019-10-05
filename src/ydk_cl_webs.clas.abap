class YDK_CL_WEBS definition
  public
  inheriting from YDK_CL_WEBS_ACTION
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_user_info,
        wsusr              TYPE ydk_webs_usr-wsusr,
        pernr              TYPE ydk_webs_usr-pernr,
        uname              TYPE ydk_webs_usr-uname,
        is_secondary_login TYPE ydk_webs_usr-is_secondary_login,
        cmd                TYPE ydk_webs_usr-cmd,
        save_pass          TYPE ydk_webs_usr-save_pass,
        devct              TYPE ydk_webs_usr-devct,
      END   OF ty_user_info .

  class-data WUSER type TY_USER_INFO read-only .
protected section.
private section.

  types:
    ty_act_tab TYPE STANDARD TABLE OF ydk_webs_act .

  data FROM_UTF8_CONVERTER type ref to CL_ABAP_CONV_OBJ .
  data TO_UTF8_CONVERTER type ref to CL_ABAP_CONV_OBJ .
  data ITACT type TY_ACT_TAB .
  type-pools ABAP .
  data STARTING type ABAP_BOOL value ABAP_TRUE. "#EC NOTEXT
  data MYINC type I .

  methods GET_ACTION
    importing
      !HANDLER_ID type STRING
      !ACTION type STRING
    returning
      value(ACT) type ref to YDK_WEBS_ACT .
  methods CONVERT_IN
    importing
      !XSTR type XSTRING
    changing
      !STR type STRING .
  methods CONVERT_RET
    importing
      !STR type STRING
    changing
      !XRET type XSTRING .
  methods SESSION_START
    importing
      !ACTION_DATA type STRING
    returning
      value(ERR) type STRING .
  methods GET_PASS_HASH
    importing
      !STR type STRING
    returning
      value(RET) type STRING .
  methods CHANGE_PASSWORD
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
  methods USER_QUERY
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
  methods GET_PASSWORD_VALIDATION
    returning
      value(RET) type STRING .
ENDCLASS.



CLASS YDK_CL_WEBS IMPLEMENTATION.


  METHOD change_password.
    DATA: pass_hash TYPE string.

    pass_hash = get_pass_hash( action_data ).

    SELECT SINGLE pass_hash INTO pass_hash
      FROM ydk_webs_usr
     WHERE pass_hash = pass_hash.
    IF sy-subrc = 0.
      return_status = status_err.
      MESSAGE e006(ydkwebs) INTO return_data. " есть коллизия - это мало вероятно, но возможно
      RETURN.
    ENDIF.

    UPDATE ydk_webs_usr SET pass_hash = pass_hash
     WHERE wsusr = wuser-wsusr.

    COMMIT WORK.

    return_status = status_ok.
  ENDMETHOD.


  METHOD convert_in.
    IF from_utf8_converter IS INITIAL.
      CREATE OBJECT from_utf8_converter
        EXPORTING
          incode = '4110'.                " utf-8
    ENDIF.

    CALL METHOD from_utf8_converter->convert
      EXPORTING
        inbuff    = xstr
        inbufflg  = 0
        outbufflg = 0
      IMPORTING
        outbuff   = str.
  ENDMETHOD.


  METHOD convert_ret.
    IF to_utf8_converter IS INITIAL.
      CREATE OBJECT to_utf8_converter
        EXPORTING
          outcode = '4110'.                " utf-8
    ENDIF.

    CALL METHOD to_utf8_converter->convert
      EXPORTING
        inbuff    = str
        inbufflg  = 0
        outbufflg = 0
      IMPORTING
        outbuff   = xret.
  ENDMETHOD.


  METHOD get_action.
    FIELD-SYMBOLS <act> LIKE LINE OF itact.

    CLEAR act.

    READ TABLE itact ASSIGNING <act> WITH KEY handler_id = handler_id action = action BINARY SEARCH.
    IF sy-subrc = 0.
      GET REFERENCE OF <act> INTO act.
      RETURN.
    ENDIF.

    READ TABLE itact WITH KEY handler_id = handler_id BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    SELECT * APPENDING TABLE itact
      FROM ydk_webs_act
     WHERE handler_id   = handler_id.

    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO itact ASSIGNING <act>.
      <act>-handler_id = handler_id.
      SORT itact BY handler_id action.
      RETURN.
    ENDIF.

    SORT itact BY handler_id action.
    READ TABLE itact ASSIGNING <act> WITH KEY handler_id = handler_id action = action BINARY SEARCH.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    GET REFERENCE OF <act> INTO act.
  ENDMETHOD.


  METHOD get_password_validation.
    TYPES: BEGIN OF ty_validation,
             min_length TYPE i,
             digits     TYPE i,
             symbols    TYPE i,
             message    TYPE string,
           END   OF ty_validation.

    DATA: validation TYPE ty_validation.

    validation-min_length = 8.
    MESSAGE s008(ydkwebs) WITH validation-min_length INTO validation-message.

ENHANCEMENT-POINT YDK_WEBS_SYS SPOTS YDK_WEBS_PASSWORD_VALIDATION .


    get_json( EXPORTING data = validation IMPORTING return_data = ret ).
  ENDMETHOD.


  METHOD get_pass_hash.
    DATA: str2hash  TYPE string.
    DATA: hash_str TYPE string.

    CONCATENATE str '-' 'Новый-Ургал' INTO str2hash.

    CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
      EXPORTING
        data          = str2hash
      IMPORTING
        hashb64string = ret.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA: xdata TYPE xstring.

    DATA: handler_id TYPE string.
    DATA: action TYPE string.
    DATA: action_data TYPE string.

    DATA: return_status TYPE string.
    DATA: return_data TYPE string.

    DATA: act TYPE REF TO ydk_webs_act.

    DATA: xout TYPE xstring.

    handler_id   = server->request->get_header_field( 'X-YDK-HANDLER_ID' ).
    action    = server->request->get_header_field( 'X-YDK-ACTION' ).

    xdata = server->request->get_data( ).
    convert_in( EXPORTING xstr = xdata CHANGING str = action_data ).

    TRANSLATE handler_id   TO UPPER CASE.
    TRANSLATE action    TO UPPER CASE.

    DO 1 TIMES.
      IF starting = abap_true.
        IF handler_id = 'SYSTEM' AND action = 'ENTRY'.
          return_data = session_start( action_data ).
          IF return_data IS NOT INITIAL.
            return_status = status_err.
            EXIT.
          ENDIF.

          server->set_session_stateful( ).
          starting = abap_false.
          return_status = status_ok.
          EXIT.
        ENDIF.

        return_status = status_err.
        return_data = '$ErrOnSessionOpening'. "ошибка при открытии сейсии
        EXIT.
      ENDIF.

      IF handler_id = 'SYSTEM'.
        CASE action.
          WHEN 'LOGOFF'.
            server->logoff( ).
            return_status = status_ok.
          WHEN 'CHANGE_PASSWORD'.
            change_password( EXPORTING action_data = action_data IMPORTING return_status = return_status return_data = return_data ).
          WHEN 'GET_PASSWORD_VALIDATION'.
            return_data = get_password_validation( ).
            return_status = status_ok.
          WHEN 'QUERY'.
            user_query( EXPORTING action_data = action_data IMPORTING return_status = return_status return_data = return_data ).
        ENDCASE.
        EXIT.
      ENDIF.

      act = get_action( EXPORTING handler_id = handler_id action = action ).
      IF act IS INITIAL.
        return_status = status_err.
        MESSAGE e001(ydkwebs) WITH handler_id action INTO return_data. " Действие & & & не авторизовано
        EXIT.
      ENDIF.

      IF act->savelog = abap_true.
        DATA: log TYPE ydk_webs_log.

        log-wsusr      = '$' && sy-uname.
        log-cpudt      = sy-datum.
        log-cputm      = sy-uzeit.
        log-handler_id = handler_id.
        log-action     = action.

        CALL FUNCTION 'YDK_WEBS_SAVE_LOG' IN UPDATE TASK
          EXPORTING
            log = log.

        COMMIT WORK.
      ENDIF.

      DATA: handler_name TYPE ydk_webs_act-handler_name.
      handler_name = act->handler_name.
      IF handler_name IS INITIAL.
        handler_name = act->handler_id.
      ENDIF.

      return_status = '!@!'.
      DATA: exc_ref TYPE REF TO cx_sy_dyn_call_error.

      CASE act->handler_type.
        WHEN 'S'.
          PERFORM (action) IN PROGRAM (handler_name) USING action_data CHANGING return_status return_data IF FOUND.
        WHEN 'M' OR space.
          TRY.
              CALL METHOD (handler_name)=>(action)
                EXPORTING
                  action_data   = action_data
                IMPORTING
                  return_status = return_status
                  return_data   = return_data.
            CATCH cx_sy_dyn_call_error INTO exc_ref.
              return_status = status_err.
              return_data = exc_ref->get_text( ).
          ENDTRY.
      ENDCASE.

      IF return_status = '!@!'.
        return_status = status_err.
        MESSAGE e003(ydkwebs) WITH handler_id action INTO return_data. " Обработчик & & не найден
        EXIT.
      ENDIF.
    ENDDO.

    server->response->set_header_field( EXPORTING name = 'X-YDK-STATUS' value = return_status ).
    convert_ret( EXPORTING str = return_data CHANGING xret = xout ).
    server->response->set_data( data = xout ).
  ENDMETHOD.


  METHOD session_start.
    TYPES: BEGIN OF ty_query,
             deviceid       TYPE string,
             persauthstring TYPE string,
           END   OF ty_query.

    DATA: query TYPE ty_query.

    DATA: langu TYPE sy-langu.

    DATA: itusr TYPE STANDARD TABLE OF ydk_webs_usr.
    DATA: pass_hash TYPE string.
    FIELD-SYMBOLS <user> LIKE LINE OF itusr.
    DATA: user_count TYPE i.

    DATA: itdev TYPE STANDARD TABLE OF ydk_webs_dev-devid.
    DATA: dev TYPE ydk_webs_dev.

    from_json( EXPORTING json = action_data CHANGING data = query ).

    IF NOT query-persauthstring IS INITIAL.
      pass_hash = get_pass_hash( query-persauthstring ).

      SELECT * INTO TABLE itusr
        FROM ydk_webs_usr
       WHERE pass_hash = pass_hash.

      IF sy-subrc <> 0.
        MESSAGE e004(ydkwebs) INTO err. " Не верное имя пользователя или пароль
        RETURN.
      ENDIF.

      IF lines( itusr ) > 1.
        LOOP AT itusr ASSIGNING <user> WHERE wsusr = sy-uname
                                         AND uname = sy-uname
                                         AND is_secondary_login IS INITIAL.
          ADD 1 TO user_count.
        ENDLOOP.
        IF user_count > 1.
          MESSAGE e004(ydkwebs) INTO err. " Не верное имя пользователя или пароль
          RETURN.
        ENDIF.
      ELSE.
        READ TABLE itusr ASSIGNING <user> INDEX 1.
      ENDIF.

      MOVE-CORRESPONDING <user> TO wuser.
    ELSE.
      SELECT * INTO TABLE itusr
        FROM ydk_webs_usr
       WHERE wsusr = sy-uname
         AND uname = sy-uname
         AND is_secondary_login = ''.
      IF sy-subrc <> 0.
        MESSAGE e004(ydkwebs) INTO err. " Не верное имя пользователя или пароль
        RETURN.
      ENDIF.

      READ TABLE itusr ASSIGNING <user> INDEX 1.
    ENDIF.

    IF NOT <user>-devct IS INITIAL.
      SELECT devid INTO TABLE itdev
        FROM ydk_webs_dev
       WHERE wsusr = <user>-wsusr.

      IF NOT line_exists( itdev[ table_line = query-deviceid ] ).
        IF <user>-devct > lines( itdev ).
          dev-wsusr = <user>-wsusr.
          dev-num   = lines( itdev ) + 1.
          dev-devid = query-deviceid.
          INSERT ydk_webs_dev FROM dev.
          COMMIT WORK.
        ELSE.
          MESSAGE e005(ydkwebs) INTO err. " Устройство не авторизовано
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD user_query.
    return_status = status_err.

    FIELD-SYMBOLS <fld> TYPE any.
    ASSIGN COMPONENT action_data OF STRUCTURE wuser TO <fld>.
    IF sy-subrc <> 0.
      MESSAGE e007(ydkwebs) WITH action_data INTO return_data.
      RETURN.
    ENDIF.

    return_data = <fld>.

    return_status = status_ok.
  ENDMETHOD.
ENDCLASS.
