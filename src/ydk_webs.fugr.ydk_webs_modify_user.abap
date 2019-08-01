FUNCTION ydk_webs_modify_user.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(USR) TYPE  YDK_WEBS_USR
*"     VALUE(PASSWORD) TYPE  STRING OPTIONAL
*"  EXCEPTIONS
*"      INVALID_DATA
*"      PASS_HASH_COLLISION
*"----------------------------------------------------------------------

  IF usr-wsusr IS INITIAL.
    RAISE invalid_data.
  ENDIF.

  IF usr-is_secondary_login IS INITIAL.
    CLEAR password.
    usr-uname = usr-wsusr.
    usr-save_pass = space.
  ENDIF.

* Генерируем строку такую же как и мобильное устройство
  DATA: action_data TYPE string.
  CONCATENATE usr-wsusr ':' password INTO action_data.
  PERFORM get_dev_pass_hash USING action_data CHANGING action_data.

  DATA: pass_hash TYPE ydk_webs_usr-pass_hash.

  PERFORM get_pass_hash USING action_data CHANGING pass_hash.

  IF usr-is_secondary_login IS NOT INITIAL.
    SELECT SINGLE pass_hash INTO pass_hash
      FROM ydk_webs_usr
     WHERE pass_hash = pass_hash
       AND wsusr <> usr-wsusr.
    IF sy-subrc = 0.
      RAISE pass_hash_collision.
    ENDIF.
  ENDIF.

  usr-pass_hash = pass_hash.
  MODIFY ydk_webs_usr FROM usr.

  COMMIT WORK.
ENDFUNCTION.
