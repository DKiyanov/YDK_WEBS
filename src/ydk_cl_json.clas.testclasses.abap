* ----------------------------------------------------------------------
CLASS abap_unit_testclass DEFINITION FOR TESTING  "#AU Duration Medium
  "#AU Risk_Level Harmless
.
  PRIVATE SECTION.

    DATA: m_ref TYPE REF TO /ui2/cl_json.                   "#EC NOTEXT

    METHODS: deserialize_form_factor FOR TESTING.
    METHODS: deserialize_target_mapping FOR TESTING.
    METHODS: deserialize_array FOR TESTING.
    METHODS: serialize_form_factor FOR TESTING.
    METHODS: serialize_table FOR TESTING.

ENDCLASS.       "abap_unit_testclass
* ----------------------------------------------------------------------
CLASS abap_unit_testclass IMPLEMENTATION.

  METHOD serialize_form_factor.
    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE boolean,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE boole_d,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF t_root,
            form_factors  TYPE t_form_factors,
        END OF t_root.

    DATA: ls_data TYPE t_root,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-form_factors-app_default    = abap_true.
    ls_data-form_factors-manual-desktop = abap_false.
    ls_data-form_factors-manual-tablet  = abap_true.
    ls_data-form_factors-manual-phone   = abap_true.

    lv_exp = '{"formFactors":{"appDefault":true,"manual":{"desktop":false,"tablet":true,"phone":true}}}'.
    lv_act = /ui2/cl_json=>serialize( data = ls_data pretty_name = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure fails' ).

    lv_exp = '{"formFactors":{"appDefault":true,"manual":{"tablet":true,"phone":true}}}'.
    lv_act = /ui2/cl_json=>serialize( data = ls_data pretty_name = abap_true compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure with compression fails' ).

    lv_exp = '{"FORM_FACTORS":{"APP_DEFAULT":true,"MANUAL":{"TABLET":true,"PHONE":true}}}'.
    lv_act = /ui2/cl_json=>serialize( data = ls_data pretty_name = abap_false compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure with NO PRETTY NAME fails' ).

  ENDMETHOD.                    "serialize_form_factor

  METHOD serialize_table.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE boolean,
            phone       TYPE boole_d,
        END OF t_form_factor,
      BEGIN OF t_line,
          index     TYPE i,
          user      LIKE sy-uname,
          client    LIKE sy-mandt,
          ff        TYPE t_form_factor,
          strings   TYPE string_table.
          INCLUDE   TYPE t_form_factor.
    TYPES: END OF t_line .
    TYPES: t_table TYPE HASHED TABLE OF t_line WITH UNIQUE KEY index.

    DATA: lt_data TYPE t_table,
          ls_data LIKE LINE OF lt_data,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-index       = 1.
    ls_data-user        = 'USER1'.
    ls_data-client      = '000'.
    ls_data-ff-desktop  = abap_false.
    ls_data-ff-tablet   = abap_true.
    ls_data-ff-phone    = abap_false.
    ls_data-desktop     = abap_true.
    ls_data-tablet      = abap_false.
    ls_data-phone       = abap_true.

    APPEND 'ABC' TO ls_data-strings.
    APPEND 'BCD' TO ls_data-strings.

    INSERT ls_data INTO TABLE lt_data.

    CLEAR: ls_data.

    ls_data-index       = 2.
    ls_data-user        = 'USER2'.
    ls_data-client      = '111'.
    ls_data-ff-desktop  = abap_true.
    ls_data-ff-tablet   = abap_true.
    ls_data-ff-phone    = abap_false.
    ls_data-desktop     = abap_false.
    ls_data-tablet      = abap_false.
    ls_data-phone       = abap_true.

    APPEND 'DEF' TO ls_data-strings.

    INSERT ls_data INTO TABLE lt_data.

    CONCATENATE `[{"index":1,"user":"USER1","client":"000","ff":{"desktop":false,"tablet":true,"phone":false},"strings":["ABC","BCD"],"desktop":true,"tablet":false,"phone":true},`
                `{"index":2,"user":"USER2","client":"111","ff":{"desktop":true,"tablet":true,"phone":false},"strings":["DEF"],"desktop":false,"tablet":false,"phone":true}]`
                INTO lv_exp.

    lv_act = /ui2/cl_json=>serialize( data = lt_data pretty_name = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of the table in JSON fails' ).

  ENDMETHOD.                    "serialize_table

  METHOD deserialize_form_factor.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE abap_bool,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE abap_bool,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF t_root,
            form_factors  TYPE t_form_factors,
        END OF t_root.

    DATA: lv_act TYPE t_root,
          lv_exp LIKE lv_act.

    lv_exp-form_factors-app_default    = abap_true.
    lv_exp-form_factors-manual-desktop = abap_false.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    json = '{ "formFactors": {"appDefault" :  true,"manual": {"desktop": false,"tablet": true,"phone": true}}}'.
    /ui2/cl_json=>deserialize( EXPORTING json = json pretty_name = abap_true CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON fails' ).

  ENDMETHOD.       "deserialize_form_factor

  METHOD deserialize_target_mapping.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE abap_bool,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE abap_bool,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF t_tm_config,
            semantic_object               TYPE string,
            semantic_action               TYPE string,
            navigation_provider           TYPE string,
            navigation_provider_role      TYPE string,
            navigation_provider_instance  TYPE string,
            target_application_alias      TYPE string,
            mapping_signature             TYPE string,
            display_info_text             TYPE string,
            form_factors                  TYPE t_form_factors,
        END OF t_tm_config,
      BEGIN OF t_config,
            tile_configuration  TYPE string,
        END OF t_config.

    DATA: lv_temp TYPE t_config,
          lv_act  TYPE t_tm_config,
          lv_exp  LIKE lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = 'SalesOrder'.
    lv_exp-semantic_action              = 'showFactsheet'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'UI3_SRVC'.
    lv_exp-navigation_provider_instance = 'UI2_FIORI_CHECKS'.
    lv_exp-target_application_alias     = 'FactsheetApp'.
    lv_exp-display_info_text            = ''.

    CONCATENATE '{"tileConfiguration":"{\"semantic_object\":\"SalesOrder\",\"semantic_action\":\"showFactsheet\",\"navigation_provider\":\"LPD\",\"navigation_provider_role\":\"UI3_SRVC\",\"navigation_provider_instance\":\"UI2_FIORI_CHECKS\",'
                '\"target_application_alias\":\"FactsheetApp\",\"unknown\":100.00,\"display_info_text\":\"\"}"}' INTO json.

    /ui2/cl_json=>deserialize( EXPORTING json = json pretty_name = abap_true CHANGING data = lv_temp ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = abap_true CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON fails' ).

    CLEAR lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = ''.
    lv_exp-semantic_action              = ''.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = ''.
    lv_exp-navigation_provider_instance = ''.
    lv_exp-target_application_alias     = ''.
    lv_exp-display_info_text            = ''.
    lv_exp-mapping_signature            = '{par1=vallkl}&[par2=Eyallk]'.
    lv_exp-form_factors-app_default    = abap_true.
    lv_exp-form_factors-manual-desktop = abap_true.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"\",\"semantic_action\":\"\",\"navigation_provider\":\"LPD\",\"display_info_text\":\"\",\"form_factors\":{\"appDefault\":true,\"manual\":'
                 '{\"desktop\":true,\"tablet\":true,\"phone\":true}},\"mapping_signature\":\"{par1=vallkl}&[par2=Eyallk]\",\"rows\":[{\"mandatory\":true,\"defaultValue\":\"\",\"isRegularExpression\":true,'
                 '\"name\":\"par1\",\"value\":\"vallkl\",\"valEnabled\":true,\"defValEnabled\":false},{\"mandatory\":false,\"isRegularExpression\":false,\"value\":\"\",\"name\":\"par2\",\"defaultValue\":'
                 '\"Eyallk\",\"valEnabled\":false,\"defValEnabled\":true}]}"}' INTO json.

    /ui2/cl_json=>deserialize( EXPORTING json = json pretty_name = abap_true CHANGING data = lv_temp ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = abap_true CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

  ENDMETHOD.       "deserialize_target_mapping

  METHOD deserialize_array.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE abap_bool,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE abap_bool,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF tp_s_sig_param,
            name                  TYPE string,
            value                 TYPE string,
            default_value         TYPE string,
            mandatory             TYPE abap_bool,
            isregularexpression   TYPE abap_bool,
            val_enabled           TYPE abap_bool,
            def_val_enabled       TYPE abap_bool,
        END OF tp_s_sig_param,
      tp_t_sig_param TYPE SORTED TABLE OF tp_s_sig_param WITH NON-UNIQUE KEY name,
      BEGIN OF t_tm_config,
            semantic_object               TYPE string,
            semantic_action               TYPE string,
            navigation_provider           TYPE string,
            navigation_provider_role      TYPE string,
            navigation_provider_instance  TYPE string,
            target_application_alias      TYPE string,
            mapping_signature             TYPE string,
            display_info_text             TYPE string,
            rows                          TYPE tp_t_sig_param,
            form_factors                  TYPE t_form_factors,
        END OF t_tm_config,
      BEGIN OF t_config,
            tile_configuration  TYPE string,
        END OF t_config.

    DATA: lv_temp TYPE t_config,
          lv_act  TYPE t_tm_config,
          ls_row  TYPE LINE OF tp_t_sig_param,
          lv_exp  LIKE lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-form_factors-app_default     = abap_true.
    lv_exp-semantic_object              = 'SalesOrder'.
    lv_exp-semantic_action              = 'showFactsheet'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'UI3_SRVC'.
    lv_exp-navigation_provider_instance = 'UI2_FIORI_CHECKS'.
    lv_exp-target_application_alias     = 'FactsheetApp'.
    lv_exp-display_info_text            = ''.
    lv_exp-mapping_signature            = '{par1=vallkl}&[par2=Eyallk]'.

    ls_row-name                  = 'par1'.
    ls_row-value                 = 'vallkl'.
    ls_row-default_value         = ''.
    ls_row-mandatory             = abap_true.
    ls_row-isregularexpression   = abap_true.
    ls_row-val_enabled           = abap_true.
    ls_row-def_val_enabled       = abap_false.
    INSERT ls_row INTO TABLE lv_exp-rows.

    ls_row-name                  = 'par2'.
    ls_row-value                 = ''.
    ls_row-default_value         = 'Eyallk'.
    ls_row-mandatory             = abap_false.
    ls_row-isregularexpression   = abap_false.
    ls_row-val_enabled           = abap_false.
    ls_row-def_val_enabled       = abap_true.
    INSERT ls_row INTO TABLE lv_exp-rows.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"SalesOrder\",\"semantic_action\":\"showFactsheet\",\"navigation_provider\":\"LPD\",\"display_info_text\":\"\",\"form_factors\":{\"appDefault\":true,\"manual\":'
                 '{\"desktop\":true,\"tablet\":true,\"phone\":true}},\"mapping_signature\":\"{par1=vallkl}&[par2=Eyallk]\",\"rows\":[{\"mandatory\":true,\"defaultValue\":\"\",\"isRegularExpression\":true,'
                 '\"name\":\"par1\",\"value\":\"vallkl\",\"valEnabled\":true,\"defValEnabled\":false},{\"mandatory\":false,\"isRegularExpression\":false,\"value\":\"\",\"name\":\"par2\",\"defaultValue\":'
                 '\"Eyallk\",\"valEnabled\":false,\"defValEnabled\":true}],'
                 '\"target_application_alias\":\"FactsheetApp\",\"navigation_provider_role\":\"UI3_SRVC\",\"navigation_provider_instance\":\"UI2_FIORI_CHECKS\" }"}' INTO json.

    /ui2/cl_json=>deserialize( EXPORTING json = json pretty_name = abap_true CHANGING data = lv_temp ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = abap_true CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

  ENDMETHOD.       "deserialize_target_mapping_array


ENDCLASS.       "abap_unit_testclass
