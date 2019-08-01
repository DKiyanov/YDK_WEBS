class YDK_CL_JSON definition
  public
  final
  create public .

*"* public components of class YDK_CL_JSON
*"* do not include other source files here!!!
public section.
  type-pools ABAP .
  class CX_SY_CONVERSION_ERROR definition load .

  class-methods RESTORE
    importing
      !JSON type STRING
      !PRETTY_NAME type ABAP_BOOL default ABAP_FALSE
      !LENGTH type I
    changing
      !DATA type DATA optional
      !OFFSET type I default 0 .
  class-methods RESTORE_TYPE
    importing
      !JSON type STRING
      !PRETTY_NAME type ABAP_BOOL default ABAP_FALSE
      !LENGTH type I
    changing
      !DATA type DATA optional
      !OFFSET type I default 0 .
  class-methods DUMP
    importing
      !DATA type DATA
      !COMPRESS type ABAP_BOOL default ABAP_FALSE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !PRETTY_NAME type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_JSON) type STRING .
  class-methods DESERIALIZE
    importing
      !JSON type STRING
      !PRETTY_NAME type ABAP_BOOL default ABAP_FALSE
    changing
      !DATA type DATA .
  class-methods SERIALIZE
    importing
      !DATA type DATA
      !COMPRESS type ABAP_BOOL default ABAP_FALSE
      !NAME type STRING optional
      !PRETTY_NAME type ABAP_BOOL default ABAP_FALSE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
    returning
      value(R_JSON) type STRING .
  class-methods DUMP_TYPE
    importing
      !DATA type DATA
      !TYPE_DESCR type ref to CL_ABAP_ELEMDESCR
    returning
      value(R_JSON) type STRING .
  class-methods DUMP_TYPE_EX
    importing
      !DATA type DATA
    returning
      value(R_JSON) type STRING .
  class-methods PRETTY_NAME
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
  class-methods ESCAPE
    importing
      !IN type ANY
    returning
      value(OUT) type STRING .
PROTECTED SECTION.
*"* protected components of class YDK_CL_JSON
*"* do not include other source files here!!!
private section.
*"* private components of class YDK_CL_JSON
*"* do not include other source files here!!!
ENDCLASS.



CLASS YDK_CL_JSON IMPLEMENTATION.


METHOD DESERIALIZE.

  DATA: length TYPE i.

  IF json IS NOT INITIAL.
    length = NUMOFCHAR( json ).
    restore_type( EXPORTING json = json pretty_name = pretty_name length = length CHANGING data = data ).
  ENDIF.

ENDMETHOD.


METHOD DUMP.

  "Get attributes of class
  DATA: l_typedesc         TYPE REF TO cl_abap_typedescr,
        l_elem_descr       TYPE REF TO cl_abap_elemdescr,
        properties         TYPE STANDARD TABLE OF string,
        fields             TYPE STANDARD TABLE OF string,
        symbol_table       TYPE cl_abap_structdescr=>symbol_table,
        lv_prop_name       TYPE string,
        lv_itemval         TYPE string.

  FIELD-SYMBOLS: <attr>             LIKE LINE OF cl_abap_objectdescr=>attributes,
                 <line>             TYPE ANY,
                 <value>            TYPE ANY,
                 <symbol_table>     LIKE LINE OF symbol_table,
                 <table>            TYPE ANY TABLE.

  " we need here macro instead of method calls because of the performance reasons.
  " Based on SAT measurments.

  "Loop attributes of class
  CASE type_descr->kind.
    WHEN cl_abap_typedescr=>kind_ref." OBJECT

      DATA: l_classdesc TYPE REF TO cl_abap_classdescr,
             obj_ref    TYPE REF TO object.

      obj_ref ?= data.
      l_classdesc ?= cl_abap_typedescr=>describe_by_object_ref( obj_ref ).

      LOOP AT l_classdesc->attributes ASSIGNING <attr> WHERE is_constant EQ abap_false AND alias_for IS INITIAL AND
        ( is_interface EQ abap_false OR type_kind NE cl_abap_typedescr=>typekind_oref ).
        ASSIGN obj_ref->(<attr>-name) TO <value>.
        IF compress EQ abap_false OR <value> IS NOT INITIAL.
          l_typedesc = cl_abap_typedescr=>describe_by_data( <value> ).
          lv_itemval = dump( data = <value> compress = compress pretty_name = pretty_name type_descr = l_typedesc ).
          IF pretty_name EQ abap_true.
            lv_prop_name = pretty_name( <attr>-name ).
          ELSE.
            lv_prop_name = <attr>-name.
          ENDIF.
          CONCATENATE `"` lv_prop_name  `":` lv_itemval INTO lv_itemval.
          APPEND lv_itemval TO properties.
        ENDIF.
      ENDLOOP.

      CONCATENATE LINES OF properties INTO r_json SEPARATED BY `,`.
      CONCATENATE `{` r_json `}` INTO r_json.

    WHEN cl_abap_typedescr=>kind_elem. "if it is elementary type_descr add it to json
      l_elem_descr ?= type_descr.
      r_json = dump_type( data = data type_descr = l_elem_descr ).
      "dump_type data l_elem_descr r_json.

    WHEN cl_abap_typedescr=>kind_struct."if it`s structure loop throught the components of structure

      DATA: l_structdesc TYPE REF TO cl_abap_structdescr.

      l_structdesc ?= type_descr.
      symbol_table = l_structdesc->get_symbols( ).

      LOOP AT symbol_table ASSIGNING <symbol_table>.
        ASSIGN COMPONENT <symbol_table>-name OF STRUCTURE data TO <value>.
        IF compress EQ abap_false OR <value> IS NOT INITIAL.
          lv_itemval = dump( data = <value> compress = compress pretty_name = pretty_name type_descr = <symbol_table>-type ).
          IF pretty_name EQ abap_true.
            lv_prop_name = pretty_name( <symbol_table>-name ).
          ELSE.
            lv_prop_name = <symbol_table>-name.
          ENDIF.
          CONCATENATE `"` lv_prop_name  `":` lv_itemval INTO lv_itemval.
          APPEND lv_itemval TO properties.
        ENDIF.
      ENDLOOP.

      CONCATENATE LINES OF properties INTO r_json SEPARATED BY `,`.
      CONCATENATE `{` r_json `}` INTO r_json.

    WHEN cl_abap_typedescr=>kind_table.

      DATA: l_tabledescr TYPE REF TO cl_abap_tabledescr.

      l_tabledescr ?= type_descr.
      l_typedesc = l_tabledescr->get_table_line_type( ).

      ASSIGN data TO <table>.

      " optimization for structured tables
      IF l_typedesc->kind EQ cl_abap_typedescr=>kind_struct.

        TYPES: BEGIN OF t_s_column,
                header TYPE string,
                name   TYPE string,
                type   TYPE REF TO cl_abap_datadescr,
               END OF t_s_column.

        DATA: columns TYPE STANDARD TABLE OF t_s_column.
        FIELD-SYMBOLS: <column> LIKE LINE OF columns.

        l_structdesc ?= l_typedesc.
        symbol_table = l_structdesc->get_symbols( ).
        LOOP AT symbol_table ASSIGNING <symbol_table>.
          APPEND INITIAL LINE TO columns ASSIGNING <column>.
          MOVE-CORRESPONDING <symbol_table> TO <column>.
          IF pretty_name EQ abap_true.
            <column>-header = pretty_name( <symbol_table>-name ).
          ELSE.
            <column>-header = <symbol_table>-name.
          ENDIF.
          CONCATENATE `"` <column>-header  `":` INTO <column>-header.
        ENDLOOP.
        LOOP AT <table> ASSIGNING <line>.
          CLEAR fields.
          LOOP AT columns ASSIGNING <column>.
            ASSIGN COMPONENT <column>-name OF STRUCTURE <line> TO <value>.
            IF compress EQ abap_false OR <value> IS NOT INITIAL.
              IF <column>-type->kind EQ cl_abap_typedescr=>kind_elem.
                l_elem_descr ?= <column>-type.
                lv_itemval = dump_type( data = <value> type_descr = l_elem_descr ).
                "dump_type <value> l_elem_descr lv_itemval.
              ELSE.
                lv_itemval = dump( data = <value> compress = compress pretty_name = pretty_name type_descr = <column>-type ).
              ENDIF.
              CONCATENATE <column>-header lv_itemval INTO lv_itemval.
              APPEND lv_itemval TO fields.
            ENDIF.
          ENDLOOP.
          CONCATENATE LINES OF fields INTO lv_itemval SEPARATED BY `,`.
          CONCATENATE `{` lv_itemval `}` INTO lv_itemval.
          APPEND lv_itemval TO properties.
        ENDLOOP.
      ELSE.
        LOOP AT <table> ASSIGNING <value>.
          lv_itemval = dump( data = <value> compress = compress pretty_name = pretty_name type_descr = l_typedesc ).
          APPEND lv_itemval TO properties.
        ENDLOOP.
      ENDIF.

      CONCATENATE LINES OF properties INTO r_json SEPARATED BY `,`.
      CONCATENATE `[` r_json `]` INTO r_json.

  ENDCASE.

ENDMETHOD.


METHOD DUMP_TYPE.

  CASE type_descr->type_kind.
    WHEN cl_abap_typedescr=>typekind_float
      OR cl_abap_typedescr=>typekind_int
      OR cl_abap_typedescr=>typekind_int1
      OR cl_abap_typedescr=>typekind_int2
      OR cl_abap_typedescr=>typekind_packed
      OR cl_abap_typedescr=>typekind_numeric.

      IF data IS INITIAL.
        r_json = `0`.
      ELSE.
        IF data > 0.
          r_json = data.
        ELSE.
          r_json = - data.
          CONCATENATE '-' r_json INTO r_json.
        ENDIF.
        CONDENSE r_json NO-GAPS.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_string
      OR cl_abap_typedescr=>typekind_csequence
      OR cl_abap_typedescr=>typekind_clike.

      IF data IS INITIAL.
        r_json = `""`.
      ELSE.
        r_json = escape( data ).
        CONCATENATE `"` r_json `"` INTO r_json.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_char.
      IF type_descr->output_length EQ 1 AND ( type_descr->absolute_name EQ `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL` OR
                                              type_descr->absolute_name EQ `\TYPE=BOOLEAN` OR
                                              type_descr->absolute_name EQ `\TYPE=BOOLE_D` ).
        IF data EQ abap_true.
          r_json = `true`.
        ELSE.
          r_json = `false`.
        ENDIF.
      ELSE.
        r_json = escape( data ).
        CONCATENATE `"` r_json `"` INTO r_json.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_date
      OR cl_abap_typedescr=>typekind_time
      OR cl_abap_typedescr=>typekind_num.
      CONCATENATE `"` data `"` INTO r_json.
    WHEN OTHERS.
      IF data IS INITIAL.
        r_json = `null`.
      ELSE.
        MOVE data TO r_json.
      ENDIF.

*        TYPEKIND_HEX
*        TYPEKIND_W
*        TYPEKIND_STRUCT1
*        TYPEKIND_STRUCT2
*        TYPEKIND_OREF
*        TYPEKIND_XSTRING
*        TYPEKIND_XSEQUENCE
*        TYPEKIND_DREF
*        TYPEKIND_CLASS
*        TYPEKIND_INTF
*        TYPEKIND_ANY
*        TYPEKIND_DATA
*        TYPEKIND_SIMPLE
*        TYPEKIND_TABLE
*        TYPEKIND_IREF
  ENDCASE.

ENDMETHOD.


METHOD DUMP_TYPE_EX.

  DATA: lrf_descr TYPE REF TO cl_abap_elemdescr.
  lrf_descr ?= cl_abap_typedescr=>describe_by_data( data ).
  r_json = dump_type( data = data type_descr = lrf_descr ).

ENDMETHOD.


METHOD ESCAPE.

  MOVE in TO out.

  REPLACE ALL OCCURRENCES OF `\` IN out WITH `\\`.
  REPLACE ALL OCCURRENCES OF `"` IN out WITH `\"`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN out WITH `\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN out WITH `\r`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace      IN out WITH `\b`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN out WITH `\t`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed      IN out WITH `\f`.


  " regex replacement is slower than two simple replacement,
  " but this can change, if more symbols shall be escaped
  "REPLACE ALL OCCURRENCES OF REGEX `\|"` IN out WITH `\$0`.

ENDMETHOD.


METHOD PRETTY_NAME.

  DATA: tokens TYPE TABLE OF char128.
  FIELD-SYMBOLS: <token> LIKE LINE OF tokens.

  out = in.

  TRANSLATE out TO LOWER CASE.
  TRANSLATE out USING `/_:_~_`.
  SPLIT out AT `_` INTO TABLE tokens.
  DELETE tokens WHERE table_line IS INITIAL.
  LOOP AT tokens ASSIGNING <token> FROM 2.
    TRANSLATE <token>(1) TO UPPER CASE.
  ENDLOOP.

  CONCATENATE LINES OF tokens INTO out.

ENDMETHOD.


METHOD RESTORE.

  DATA: mark        LIKE offset,
        match       LIKE offset,
        exp         TYPE REF TO cx_sy_move_cast_error,
        name_json   TYPE string,
        name_abap   TYPE string.

  FIELD-SYMBOLS: <value> TYPE ANY.

  eat_white.
  eat_char `{`.

  WHILE offset < length AND json+offset(1) NE `}`.

    eat_white.
    eat_string name_json.
    eat_white.
    eat_char `:`.
    eat_white.
    UNASSIGN <value>.

    name_abap = name_json.
    TRANSLATE name_abap TO UPPER CASE.
    ASSIGN COMPONENT name_abap OF STRUCTURE data TO <value>.

    IF <value> IS NOT ASSIGNED AND pretty_name EQ abap_true.
      name_abap = name_json.
      REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN name_abap WITH `$1_$2`. "#EC NOTEXT
      TRANSLATE name_abap TO UPPER CASE.
      ASSIGN COMPONENT name_abap OF STRUCTURE data TO <value>.
    ENDIF.

    IF <value> IS ASSIGNED.
      restore_type( EXPORTING json = json length = length pretty_name = pretty_name CHANGING data = <value> offset = offset ).
    ELSE.
      restore_type( EXPORTING json = json length = length pretty_name = pretty_name CHANGING offset = offset ).
    ENDIF.

    eat_white.

    IF json+offset(1) NE `}`.
      eat_char `,`.
    ELSE.
      EXIT.
    ENDIF.

  ENDWHILE.

  eat_char `}`.

ENDMETHOD.


METHOD RESTORE_TYPE.

  DATA: mark        LIKE offset,
        match       LIKE offset,
        sdummy      TYPE string,
        idummy      TYPE i,
        line        TYPE REF TO data,
        type_descr  TYPE REF TO cl_abap_typedescr,
        table_descr TYPE REF TO cl_abap_tabledescr,
        exp         TYPE REF TO cx_sy_move_cast_error.

  FIELD-SYMBOLS: <line>           TYPE ANY,
                 <table>          TYPE ANY TABLE,
                 <table_sorted>   TYPE SORTED TABLE,
                 <table_hashed>   TYPE HASHED TABLE,
                 <table_standard> TYPE STANDARD TABLE.

  eat_white.

  CASE json+offset(1).
    WHEN `{`. " object
      IF data IS SUPPLIED.
        restore( EXPORTING json = json pretty_name = pretty_name length = length
                 CHANGING data = data offset = offset ).
      ELSE.
        restore( EXPORTING json = json pretty_name = pretty_name length = length
                 CHANGING  offset = offset ).
      ENDIF.
    WHEN `[`. " array
      eat_char `[`.
      eat_white.
      IF json+offset(1) NE `]`.
        type_descr = cl_abap_typedescr=>describe_by_data( data ).
        IF type_descr->type_kind EQ cl_abap_typedescr=>typekind_table.
          table_descr ?= type_descr.
          ASSIGN data TO <table>.
          CREATE DATA line LIKE LINE OF <table>.
          ASSIGN line->* TO <line>.
          WHILE offset < length AND json+offset(1) NE `]`.
            CLEAR <line>.
            restore_type( EXPORTING json = json length = length pretty_name = pretty_name CHANGING data = <line> offset = offset ).
            CASE table_descr->table_kind.
              WHEN cl_abap_tabledescr=>tablekind_sorted.
                ASSIGN data TO <table_sorted>.
                INSERT <line> INTO TABLE <table_sorted>.
              WHEN cl_abap_tabledescr=>tablekind_hashed.
                ASSIGN data TO <table_sorted>.
                INSERT <line> INTO TABLE <table_hashed>.
              WHEN OTHERS.
                ASSIGN data TO <table_standard>.
                APPEND <line> TO <table_standard>.
            ENDCASE.
            eat_white.
            IF json+offset(1) NE `]`.
              eat_char `,`.
            ELSE.
              EXIT.
            ENDIF.
          ENDWHILE.
        ELSE.
          WHILE offset < length AND json+offset(1) NE `}`.
            eat_white.
            restore_type( EXPORTING json = json length = length pretty_name = pretty_name CHANGING offset = offset ).
            IF json+offset(1) NE `]`.
              eat_char `,`.
            ELSE.
              EXIT.
            ENDIF.
          ENDWHILE.
        ENDIF.
      ENDIF.
      eat_char `]`.
    WHEN `"`. " string
      IF data IS SUPPLIED.
        eat_string data.
        REPLACE ALL OCCURRENCES OF `\"` IN data WITH `"`. " unescape string
        REPLACE ALL OCCURRENCES OF `\\` IN data WITH `\`. " unescape string
      ELSE.
        eat_string sdummy.
      ENDIF.
    WHEN `-`. " number
      IF data IS SUPPLIED.
        eat_number data.
      ELSE.
        eat_number idummy.
      ENDIF.
    WHEN OTHERS.
      IF '0123456789' CS json+offset(1). " number
        IF data IS SUPPLIED.
          eat_number data.
        ELSE.
          eat_number idummy.
        ENDIF.
      ELSE. " true/false/null
        IF data IS SUPPLIED.
          eat_bool data.
        ELSE.
          eat_bool sdummy.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMETHOD.


METHOD SERIALIZE.

  DATA: lrf_descr TYPE REF TO cl_abap_typedescr.

  IF type_descr IS INITIAL.
    lrf_descr = cl_abap_typedescr=>describe_by_data( data ).
  ELSE.
    lrf_descr = type_descr.
  ENDIF.

  r_json = dump( data = data compress = compress pretty_name = pretty_name type_descr = lrf_descr ).

  IF name IS NOT INITIAL AND ( compress EQ abap_false OR r_json IS NOT INITIAL ).
    CONCATENATE `"` name `":` r_json INTO r_json.
  ENDIF.

ENDMETHOD.
ENDCLASS.
