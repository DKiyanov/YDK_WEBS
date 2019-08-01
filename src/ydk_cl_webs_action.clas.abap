class YDK_CL_WEBS_ACTION definition
  public
  abstract
  create public .

public section.

  constants STATUS_OK type STRING value 'OK'. "#EC NOTEXT
  constants STATUS_ERR type STRING value 'ERR'. "#EC NOTEXT

  class-methods GET_JSON
    importing
      !DATA type DATA
    exporting
      !RETURN_DATA type STRING
      !RETURN_STATUS type STRING .
  class-methods FROM_JSON
    importing
      !JSON type STRING
    changing
      !DATA type DATA .
  class-methods USER
    returning
      value(USER) type ref to YDK_CL_WEBS=>TY_USER_INFO .
protected section.
private section.
ENDCLASS.



CLASS YDK_CL_WEBS_ACTION IMPLEMENTATION.


  METHOD from_json.
    ydk_cl_json=>deserialize( EXPORTING json = json CHANGING data = data ).
  ENDMETHOD.


  METHOD get_json.
    return_data = ydk_cl_json=>serialize( data = data compress = abap_false pretty_name = abap_false ).
    return_status = status_ok.
  ENDMETHOD.


  METHOD user.
    GET REFERENCE OF ydk_cl_webs=>wuser INTO user.
  ENDMETHOD.
ENDCLASS.
