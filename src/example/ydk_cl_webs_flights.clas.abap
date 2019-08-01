class YDK_CL_WEBS_FLIGHTS definition
  public
  inheriting from YDK_CL_WEBS_ACTION
  final
  create public .

public section.

  class-methods GET_FLIGHTS
    importing
      !ACTION_DATA type STRING
    exporting
      !RETURN_STATUS type STRING
      !RETURN_DATA type STRING .
protected section.
private section.
ENDCLASS.



CLASS YDK_CL_WEBS_FLIGHTS IMPLEMENTATION.


  METHOD get_flights.
    TYPES: BEGIN OF ty_query,
             carrid TYPE RANGE OF sflight-carrid,
             connid TYPE RANGE OF sflight-connid,
             fldate TYPE RANGE OF sflight-fldate,
           END   OF ty_query.

    DATA: query TYPE ty_query.

    from_json( EXPORTING json = action_data CHANGING data = query ).

    DATA: lt_sflight TYPE STANDARD TABLE OF sflight.

    SELECT * INTO TABLE lt_sflight
      FROM sflight
     WHERE carrid IN query-carrid
       AND connid IN query-connid
       AND fldate IN query-fldate.

    get_json( EXPORTING data = lt_sflight IMPORTING return_status = return_status return_data = return_data ).
  ENDMETHOD.
ENDCLASS.
