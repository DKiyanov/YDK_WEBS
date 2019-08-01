FUNCTION-POOL ydk_webs.                     "MESSAGE-ID ..

* INCLUDE LYDK_WEBSD...                      " Local class definition

FORM get_pass_hash USING in_data CHANGING pass_hash.
  DATA: str2hash  TYPE string.
  DATA: hash_str TYPE string.

  CONCATENATE in_data '-' 'Новый-Ургал' INTO str2hash.

  CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
    EXPORTING
      data          = str2hash
    IMPORTING
      hashb64string = hash_str.
  pass_hash = hash_str.
ENDFORM.

FORM get_dev_pass_hash USING in_data CHANGING pass_hash.
  DATA: converter TYPE REF TO cl_abap_conv_obj.
  DATA: str2hash  TYPE string.
  DATA: xstr2hash  TYPE xstring.
  DATA: hash_str TYPE string.

  CONCATENATE in_data '-' 'Хабаровск' INTO str2hash.

  CREATE OBJECT converter
    EXPORTING
      outcode = '4110'.                " utf-8

  CALL METHOD converter->convert
    EXPORTING
      inbuff    = str2hash
      inbufflg  = 0
      outbufflg = 0
    IMPORTING
      outbuff   = xstr2hash.

  CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
    EXPORTING
      alg           = 'SHA1'
      data          = xstr2hash
    IMPORTING
      hashb64string = hash_str.

  pass_hash = hash_str.
ENDFORM.
