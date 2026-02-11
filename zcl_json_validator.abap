*&---------------------------------------------------------------------*
*& Class ZCL_JSON_VALIDATOR
*&---------------------------------------------------------------------*
*& JSON Validator with Line-Level Error Reporting and Formatting
*&---------------------------------------------------------------------*
CLASS zcl_json_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_validation_error,
        line_number TYPE i,
        column      TYPE i,
        message     TYPE string,
        severity    TYPE string,
      END OF ty_validation_error,
      ty_validation_errors TYPE STANDARD TABLE OF ty_validation_error WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_validation_result,
        is_valid TYPE abap_bool,
        errors   TYPE ty_validation_errors,
        message  TYPE string,
      END OF ty_validation_result.

    CLASS-METHODS validate
      IMPORTING
        iv_json_string    TYPE string
      RETURNING
        VALUE(rs_result)  TYPE ty_validation_result.

    CLASS-METHODS validate_and_display
      IMPORTING
        iv_json_string TYPE string.

    "--------------------------------------------------------------------
    " FORMAT_COLON_SPACING
    "   Removes all whitespace between a property name and its colon.
    "   Works on a single-line or compact JSON string.
    "   Example:  "NEWITEM"   :  →  "NEWITEM":
    "--------------------------------------------------------------------
    CLASS-METHODS format_colon_spacing
      IMPORTING
        iv_json_string    TYPE string
      RETURNING
        VALUE(rv_result)  TYPE string.

    "--------------------------------------------------------------------
    " FORMAT_COLON_SPACING_MULTILINE
    "   Same fix applied line-by-line on a multi-line JSON string.
    "   Preserves all original newlines so line numbers stay intact.
    "--------------------------------------------------------------------
    CLASS-METHODS format_colon_spacing_multiline
      IMPORTING
        iv_json_string    TYPE string
      RETURNING
        VALUE(rv_result)  TYPE string.

    "--------------------------------------------------------------------
    " FORMAT_AND_VALIDATE
    "   Convenience wrapper: formats colon spacing first, then validates.
    "   Returns the standard result structure; the cleaned JSON is
    "   returned via EV_FORMATTED_JSON.
    "--------------------------------------------------------------------
    CLASS-METHODS format_and_validate
      IMPORTING
        iv_json_string    TYPE string
      EXPORTING
        ev_formatted_json TYPE string
      RETURNING
        VALUE(rs_result)  TYPE ty_validation_result.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_token,
        type   TYPE string,
        value  TYPE string,
        line   TYPE i,
        column TYPE i,
      END OF ty_token,
      ty_tokens TYPE STANDARD TABLE OF ty_token WITH DEFAULT KEY.

    CLASS-METHODS tokenize
      IMPORTING
        iv_json          TYPE string
      RETURNING
        VALUE(rt_tokens) TYPE ty_tokens.

    CLASS-METHODS parse_value
      IMPORTING
        it_tokens        TYPE ty_tokens
      CHANGING
        cv_index         TYPE i
        ct_errors        TYPE ty_validation_errors
      RETURNING
        VALUE(rv_valid)  TYPE abap_bool.

    CLASS-METHODS parse_object
      IMPORTING
        it_tokens        TYPE ty_tokens
      CHANGING
        cv_index         TYPE i
        ct_errors        TYPE ty_validation_errors
      RETURNING
        VALUE(rv_valid)  TYPE abap_bool.

    CLASS-METHODS parse_array
      IMPORTING
        it_tokens        TYPE ty_tokens
      CHANGING
        cv_index         TYPE i
        ct_errors        TYPE ty_validation_errors
      RETURNING
        VALUE(rv_valid)  TYPE abap_bool.

    CLASS-METHODS add_error
      IMPORTING
        iv_line     TYPE i
        iv_column   TYPE i
        iv_message  TYPE string
        iv_severity TYPE string DEFAULT 'ERROR'
      CHANGING
        ct_errors   TYPE ty_validation_errors.

    CLASS-METHODS expect_token
      IMPORTING
        it_tokens       TYPE ty_tokens
        iv_index        TYPE i
        iv_expected     TYPE string
      CHANGING
        ct_errors       TYPE ty_validation_errors
      RETURNING
        VALUE(rv_valid) TYPE abap_bool.

    "--------------------------------------------------------------------
    " FIX_COLON_SPACING_IN_LINE  (private helper)
    "   State-machine that walks one text line character by character and
    "   strips whitespace between a closing key-quote and its colon.
    "   Spaces inside string values are fully preserved.
    "
    "   States used internally (single character flag):
    "     N  – NORMAL: outside any string
    "     I  – IN_KEY: inside a string that is a property key
    "     A  – AFTER_KEY: between closing key-quote and the expected ':'
    "     V  – IN_VALUE_STR: inside a value string (spaces kept as-is)
    "--------------------------------------------------------------------
    CLASS-METHODS fix_colon_spacing_in_line
      IMPORTING
        iv_line          TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.



CLASS ZCL_JSON_VALIDATOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>ADD_ERROR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINE                        TYPE        I
* | [--->] IV_COLUMN                      TYPE        I
* | [--->] IV_MESSAGE                     TYPE        STRING
* | [--->] IV_SEVERITY                    TYPE        STRING (default ='ERROR')
* | [<-->] CT_ERRORS                      TYPE        TY_VALIDATION_ERRORS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_error.
    DATA: ls_error TYPE ty_validation_error.

    ls_error-line_number = iv_line.
    ls_error-column      = iv_column.
    ls_error-message     = iv_message.
    ls_error-severity    = iv_severity.

    APPEND ls_error TO ct_errors.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>EXPECT_TOKEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TOKENS                      TYPE        TY_TOKENS
* | [--->] IV_INDEX                       TYPE        I
* | [--->] IV_EXPECTED                    TYPE        STRING
* | [<-->] CT_ERRORS                      TYPE        TY_VALIDATION_ERRORS
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD expect_token.
    DATA: ls_token TYPE ty_token.

    IF iv_index > lines( it_tokens ).
      add_error(
        EXPORTING
          iv_line    = 999
          iv_column  = 1
          iv_message = |Expected '{ iv_expected }' but reached end of JSON|
        CHANGING
          ct_errors = ct_errors ).
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    ls_token = it_tokens[ iv_index ].

    IF ls_token-type <> iv_expected.
      add_error(
        EXPORTING
          iv_line    = ls_token-line
          iv_column  = ls_token-column
          iv_message = |Expected '{ iv_expected }', found: { ls_token-value }|
        CHANGING
          ct_errors = ct_errors ).
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>PARSE_ARRAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TOKENS                      TYPE        TY_TOKENS
* | [<-->] CV_INDEX                       TYPE        I
* | [<-->] CT_ERRORS                      TYPE        TY_VALIDATION_ERRORS
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_array.
    DATA: ls_token TYPE ty_token.

    rv_valid = abap_true.
    cv_index = cv_index + 1.

    " Expect '['
    IF expect_token(
      EXPORTING it_tokens   = it_tokens
                iv_index    = cv_index
                iv_expected = '['
      CHANGING  ct_errors   = ct_errors ) = abap_false.
      rv_valid = abap_false.
    ENDIF.

    " Empty array check
    IF cv_index < lines( it_tokens ).
      ls_token = it_tokens[ cv_index + 1 ].
      IF ls_token-type = ']'.
        cv_index = cv_index + 1.
        RETURN.
      ENDIF.
    ENDIF.

    " Parse array elements
    DO.
      IF parse_value(
        EXPORTING it_tokens = it_tokens
        CHANGING  cv_index  = cv_index
                  ct_errors = ct_errors ) = abap_false.
        rv_valid = abap_false.
        " Error recovery: skip to next ',' or ']'
        DATA(lv_recovery_index) = cv_index + 1.
        WHILE lv_recovery_index <= lines( it_tokens ).
          DATA(ls_recovery_token) = it_tokens[ lv_recovery_index ].
          IF ls_recovery_token-type = ',' OR ls_recovery_token-type = ']'.
            cv_index = lv_recovery_index - 1.
            EXIT.
          ENDIF.
          lv_recovery_index = lv_recovery_index + 1.
        ENDWHILE.
        IF lv_recovery_index > lines( it_tokens ).
          EXIT.
        ENDIF.
      ENDIF.

      cv_index = cv_index + 1.
      IF cv_index > lines( it_tokens ).
        add_error(
          EXPORTING iv_line = 999 iv_column = 1
                    iv_message = 'Unexpected end of array'
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.
        EXIT.
      ENDIF.

      ls_token = it_tokens[ cv_index ].

      IF ls_token-type = ']'.
        EXIT.
      ELSEIF ls_token-type = ','.
        CONTINUE.
      ELSE.
        add_error(
          EXPORTING iv_line    = ls_token-line
                    iv_column  = ls_token-column
                    iv_message = |Expected comma or closing bracket, found: { ls_token-value }|
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.
        lv_recovery_index = cv_index + 1.
        WHILE lv_recovery_index <= lines( it_tokens ).
          ls_recovery_token = it_tokens[ lv_recovery_index ].
          IF ls_recovery_token-type = ',' OR ls_recovery_token-type = ']'.
            cv_index = lv_recovery_index.
            IF ls_recovery_token-type = ']'.
              EXIT.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          lv_recovery_index = lv_recovery_index + 1.
        ENDWHILE.
        IF lv_recovery_index > lines( it_tokens ).
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>PARSE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TOKENS                      TYPE        TY_TOKENS
* | [<-->] CV_INDEX                       TYPE        I
* | [<-->] CT_ERRORS                      TYPE        TY_VALIDATION_ERRORS
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_object.
    DATA: ls_token TYPE ty_token.

    rv_valid = abap_true.
    cv_index = cv_index + 1.

    IF expect_token(
      EXPORTING it_tokens   = it_tokens
                iv_index    = cv_index
                iv_expected = '{'
      CHANGING  ct_errors   = ct_errors ) = abap_false.
      rv_valid = abap_false.
    ENDIF.

    " Empty object check
    IF cv_index < lines( it_tokens ).
      ls_token = it_tokens[ cv_index + 1 ].
      IF ls_token-type = '}'.
        cv_index = cv_index + 1.
        RETURN.
      ENDIF.
    ENDIF.

    " Parse key-value pairs
    DO.
      " Key (must be string)
      cv_index = cv_index + 1.
      IF cv_index > lines( it_tokens ).
        add_error(
          EXPORTING iv_line = 999 iv_column = 1
                    iv_message = 'Expected property name but reached end of JSON'
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.
        EXIT.
      ENDIF.

      ls_token = it_tokens[ cv_index ].
      IF ls_token-type = '}'.
        EXIT.
      ENDIF.

      IF ls_token-type <> 'STRING'.
        add_error(
          EXPORTING iv_line    = ls_token-line
                    iv_column  = ls_token-column
                    iv_message = |Expected string key, found: { ls_token-value }|
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.

        DATA(lv_recovery_idx) = cv_index + 1.
        WHILE lv_recovery_idx <= lines( it_tokens ).
          DATA(ls_recovery_tok) = it_tokens[ lv_recovery_idx ].
          IF ls_recovery_tok-type = ':' OR ls_recovery_tok-type = ','
                                        OR ls_recovery_tok-type = '}'.
            IF ls_recovery_tok-type = '}'.
              cv_index = lv_recovery_idx.
              EXIT.
            ELSEIF ls_recovery_tok-type = ','.
              cv_index = lv_recovery_idx.
              CONTINUE.
            ELSE.
              cv_index = lv_recovery_idx - 1.
              EXIT.
            ENDIF.
          ENDIF.
          lv_recovery_idx = lv_recovery_idx + 1.
        ENDWHILE.
        IF lv_recovery_idx > lines( it_tokens ).
          EXIT.
        ENDIF.
        IF ls_recovery_tok-type = '}'.
          EXIT.
        ELSEIF ls_recovery_tok-type = ','.
          CONTINUE.
        ENDIF.
      ENDIF.

      " Colon
      cv_index = cv_index + 1.
      IF cv_index > lines( it_tokens ).
        add_error(
          EXPORTING iv_line = 999 iv_column = 1
                    iv_message = 'Expected colon but reached end of JSON'
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.
        EXIT.
      ENDIF.

      ls_token = it_tokens[ cv_index ].
      IF ls_token-type <> ':'.
        add_error(
          EXPORTING iv_line    = ls_token-line
                    iv_column  = ls_token-column
                    iv_message = |Expected colon, found: { ls_token-value }|
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.

        IF ls_token-type = '}'.
          EXIT.
        ELSEIF ls_token-type = ','.
          CONTINUE.
        ELSE.
          lv_recovery_idx = cv_index + 1.
          WHILE lv_recovery_idx <= lines( it_tokens ).
            ls_recovery_tok = it_tokens[ lv_recovery_idx ].
            IF ls_recovery_tok-type = ',' OR ls_recovery_tok-type = '}'.
              cv_index = lv_recovery_idx.
              IF ls_recovery_tok-type = '}'.
                EXIT.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            lv_recovery_idx = lv_recovery_idx + 1.
          ENDWHILE.
          IF lv_recovery_idx > lines( it_tokens ).
            EXIT.
          ENDIF.
          IF ls_recovery_tok-type = '}'.
            EXIT.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      " Value
      IF parse_value(
        EXPORTING it_tokens = it_tokens
        CHANGING  cv_index  = cv_index
                  ct_errors = ct_errors ) = abap_false.
        rv_valid = abap_false.
      ENDIF.

      " After key-value pair: expect ',' or '}'
      cv_index = cv_index + 1.
      IF cv_index > lines( it_tokens ).
        add_error(
          EXPORTING iv_line = 999 iv_column = 1
                    iv_message = 'Unexpected end of object'
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.
        EXIT.
      ENDIF.

      ls_token = it_tokens[ cv_index ].
      IF ls_token-type = '}'.
        EXIT.
      ELSEIF ls_token-type = ','.
        CONTINUE.
      ELSE.
        add_error(
          EXPORTING iv_line    = ls_token-line
                    iv_column  = ls_token-column
                    iv_message = |Expected comma or closing brace, found: { ls_token-value }|
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.

        lv_recovery_idx = cv_index + 1.
        WHILE lv_recovery_idx <= lines( it_tokens ).
          ls_recovery_tok = it_tokens[ lv_recovery_idx ].
          IF ls_recovery_tok-type = ',' OR ls_recovery_tok-type = '}'.
            cv_index = lv_recovery_idx.
            IF ls_recovery_tok-type = '}'.
              EXIT.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          lv_recovery_idx = lv_recovery_idx + 1.
        ENDWHILE.
        IF lv_recovery_idx > lines( it_tokens ).
          EXIT.
        ENDIF.
        IF ls_recovery_tok-type = '}'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>PARSE_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TOKENS                      TYPE        TY_TOKENS
* | [<-->] CV_INDEX                       TYPE        I
* | [<-->] CT_ERRORS                      TYPE        TY_VALIDATION_ERRORS
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_value.
    DATA: ls_token TYPE ty_token.

    rv_valid = abap_false.

    IF cv_index >= lines( it_tokens ).
      add_error(
        EXPORTING iv_line = 1 iv_column = 1
                  iv_message = 'Unexpected end of JSON'
        CHANGING  ct_errors = ct_errors ).
      RETURN.
    ENDIF.

    cv_index = cv_index + 1.
    ls_token = it_tokens[ cv_index ].

    CASE ls_token-type.
      WHEN '{'.
        cv_index = cv_index - 1.
        rv_valid = parse_object(
          EXPORTING it_tokens = it_tokens
          CHANGING  cv_index  = cv_index
                    ct_errors = ct_errors ).

      WHEN '['.
        cv_index = cv_index - 1.
        rv_valid = parse_array(
          EXPORTING it_tokens = it_tokens
          CHANGING  cv_index  = cv_index
                    ct_errors = ct_errors ).

      WHEN 'STRING' OR 'NUMBER' OR 'BOOLEAN' OR 'NULL'.
        rv_valid = abap_true.

      WHEN OTHERS.
        add_error(
          EXPORTING iv_line    = ls_token-line
                    iv_column  = ls_token-column
                    iv_message = |Unexpected token: { ls_token-value }|
          CHANGING  ct_errors = ct_errors ).
        rv_valid = abap_false.
    ENDCASE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>TOKENIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [<-()] RT_TOKENS                      TYPE        TY_TOKENS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD tokenize.
    DATA: lv_char      TYPE c LENGTH 1,
          lv_line      TYPE i VALUE 1,
          lv_column    TYPE i VALUE 1,
          lv_in_string TYPE abap_bool VALUE abap_false,
          lv_escape    TYPE abap_bool VALUE abap_false,
          lv_buffer    TYPE string,
          ls_token     TYPE ty_token,
          lv_len       TYPE i,
          lv_start_col TYPE i.

    lv_len = strlen( iv_json ).

    DO lv_len TIMES.
      DATA(lv_pos) = sy-index - 1.
      lv_char = iv_json+lv_pos(1).

      IF lv_in_string = abap_true.
        lv_buffer = lv_buffer && lv_char.
        IF lv_escape = abap_true.
          lv_escape = abap_false.
        ELSEIF lv_char = '\'.
          lv_escape = abap_true.
        ELSEIF lv_char = '"'.
          lv_in_string    = abap_false.
          ls_token-type   = 'STRING'.
          ls_token-value  = lv_buffer.
          ls_token-line   = lv_line.
          ls_token-column = lv_start_col.
          APPEND ls_token TO rt_tokens.
          CLEAR: lv_buffer, ls_token.
        ENDIF.

      ELSE.
        CASE lv_char.
          WHEN '"'.
            lv_in_string = abap_true.
            lv_buffer    = lv_char.
            lv_start_col = lv_column.

          WHEN '{' OR '}' OR '[' OR ']' OR ':' OR ','.
            ls_token-type   = lv_char.
            ls_token-value  = lv_char.
            ls_token-line   = lv_line.
            ls_token-column = lv_column.
            APPEND ls_token TO rt_tokens.
            CLEAR ls_token.

          WHEN ' ' OR cl_abap_char_utilities=>cr_lf
                    OR cl_abap_char_utilities=>newline
                    OR cl_abap_char_utilities=>horizontal_tab.
            " Whitespace outside strings – ignore

          WHEN OTHERS.
            IF lv_buffer IS INITIAL.
              lv_start_col = lv_column.
            ENDIF.
            lv_buffer = lv_buffer && lv_char.

            DATA(lv_next_pos) = lv_pos + 1.
            IF lv_next_pos >= lv_len.
              DATA(lv_is_delimiter) = abap_true.
            ELSE.
              DATA(lv_next_char) = iv_json+lv_next_pos(1).
              IF lv_next_char CA ' {}[]:,' OR
                 lv_next_char = cl_abap_char_utilities=>newline OR
                 lv_next_char = cl_abap_char_utilities=>cr_lf   OR
                 lv_next_char = cl_abap_char_utilities=>horizontal_tab.
                lv_is_delimiter = abap_true.
              ELSE.
                lv_is_delimiter = abap_false.
              ENDIF.
            ENDIF.

            IF lv_is_delimiter = abap_true.
              IF lv_buffer = 'true' OR lv_buffer = 'false'.
                ls_token-type = 'BOOLEAN'.
              ELSEIF lv_buffer = 'null'.
                ls_token-type = 'NULL'.
              ELSE.
                ls_token-type = 'NUMBER'.
              ENDIF.
              ls_token-value  = lv_buffer.
              ls_token-line   = lv_line.
              ls_token-column = lv_start_col.
              APPEND ls_token TO rt_tokens.
              CLEAR: lv_buffer, ls_token.
            ENDIF.
        ENDCASE.
      ENDIF.

      IF lv_char = cl_abap_char_utilities=>newline.
        lv_line   = lv_line + 1.
        lv_column = 1.
      ELSE.
        lv_column = lv_column + 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_VALIDATOR=>VALIDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON_STRING                 TYPE        STRING
* | [<-()] RS_RESULT                      TYPE        TY_VALIDATION_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate.
    DATA: lt_tokens TYPE ty_tokens,
          lv_index  TYPE i VALUE 0,
          lt_errors TYPE ty_validation_errors.

    CLEAR rs_result.
    rs_result-is_valid = abap_true.

    IF iv_json_string IS INITIAL.
      add_error(
        EXPORTING iv_line = 1 iv_column = 1
                  iv_message = 'Empty JSON string'
        CHANGING  ct_errors = lt_errors ).
      rs_result-is_valid = abap_false.
      rs_result-errors   = lt_errors.
      rs_result-message  = 'Validation failed: Empty JSON string'.
      RETURN.
    ENDIF.

    lt_tokens = tokenize( iv_json_string ).

    IF lines( lt_tokens ) = 0.
      add_error(
        EXPORTING iv_line = 1 iv_column = 1
                  iv_message = 'No valid tokens found in JSON'
        CHANGING  ct_errors = lt_errors ).
      rs_result-is_valid = abap_false.
      rs_result-errors   = lt_errors.
      rs_result-message  = 'Validation failed: No valid tokens'.
      RETURN.
    ENDIF.

    DATA(lv_valid) = parse_value(
      EXPORTING it_tokens = lt_tokens
      CHANGING  cv_index  = lv_index
                ct_errors = lt_errors ).

    IF lv_valid = abap_true AND lv_index < lines( lt_tokens ).
      DATA(ls_extra_token) = lt_tokens[ lv_index + 1 ].
      add_error(
        EXPORTING iv_line    = ls_extra_token-line
                  iv_column  = ls_extra_token-column
                  iv_message = |Unexpected token after JSON value: { ls_extra_token-value }|
        CHANGING  ct_errors = lt_errors ).
      lv_valid = abap_false.
    ENDIF.

    rs_result-is_valid = lv_valid.
    rs_result-errors   = lt_errors.
    rs_result-message  = COND #(
      WHEN lv_valid = abap_true
      THEN 'JSON is valid'
      ELSE |Validation failed with { lines( lt_errors ) } error(s)| ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_VALIDATOR=>VALIDATE_AND_DISPLAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON_STRING                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_and_display.
    DATA: ls_result TYPE ty_validation_result,
          ls_error  TYPE ty_validation_error.

    ls_result = validate( iv_json_string ).

    IF ls_result-is_valid = abap_true.
      WRITE: / '✓ JSON is valid!'.
    ELSE.
      WRITE: / '✗ JSON validation failed with', lines( ls_result-errors ), 'error(s):'.
      WRITE: /.
      LOOP AT ls_result-errors INTO ls_error.
        WRITE: / |Line { ls_error-line_number }, Column { ls_error-column }: |,
                 |[{ ls_error-severity }] { ls_error-message }|.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_JSON_VALIDATOR=>FIX_COLON_SPACING_IN_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINE                        TYPE        STRING
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fix_colon_spacing_in_line.
    "--------------------------------------------------------------------------
    " Walk the line one character at a time using a 4-state machine:
    "
    "   N (NORMAL)       – outside every string literal.
    "   I (IN_KEY)       – inside a string that is a property key.
    "                      Identified by the last structural character before
    "                      the opening quote being '{' or ',' (or nothing yet).
    "   A (AFTER_KEY)    – after the closing quote of a key; whitespace here
    "                      is buffered and discarded when ':' is found.
    "   V (IN_VALUE_STR) – inside a value string; content is passed through
    "                      unchanged, including spaces.
    "--------------------------------------------------------------------------

    DATA: lv_len          TYPE i,
          lv_pos          TYPE i,
          lv_char         TYPE c LENGTH 1,
          lv_state        TYPE c LENGTH 1,
          lv_escape       TYPE abap_bool,
          lv_space_buffer TYPE string,
          lv_output       TYPE string,
          lv_trimmed      TYPE string,
          lv_last_char    TYPE c LENGTH 1,
          lv_last_len     TYPE i.

    rv_result = iv_line.
    lv_len    = strlen( iv_line ).

    IF lv_len = 0.
      RETURN.
    ENDIF.

    lv_state  = 'N'.
    lv_escape = abap_false.
    CLEAR: lv_space_buffer, lv_output.

    DO lv_len TIMES.
      lv_pos  = sy-index - 1.
      lv_char = iv_line+lv_pos(1).

      CASE lv_state.

        " ---------------------------------------------------------------
        " NORMAL – outside any string
        " ---------------------------------------------------------------
        WHEN 'N'.
          CASE lv_char.
            WHEN '"'.
              " Decide if this opening quote starts a key or a value.
              " Key heuristic: the last non-whitespace character already
              " written to the output is '{', ',' or the output is empty.
              lv_trimmed = lv_output.
              CONDENSE lv_trimmed NO-GAPS.
              lv_last_len = strlen( lv_trimmed ).
              IF lv_last_len > 0.
                data(lv_corr_len) = lv_last_len - 1.
                lv_last_char = lv_trimmed+lv_corr_len(1).
              ELSE.
                CLEAR lv_last_char.
              ENDIF.

              IF lv_last_char = '{' OR lv_last_char = ',' OR lv_last_len = 0.
                lv_state = 'I'.    " key string
              ELSE.
                lv_state = 'V'.    " value string
              ENDIF.

              lv_output = lv_output && lv_char.

            WHEN OTHERS.
              lv_output = lv_output && lv_char.
          ENDCASE.

        " ---------------------------------------------------------------
        " IN_KEY – inside a property-key string
        " ---------------------------------------------------------------
        WHEN 'I'.
          lv_output = lv_output && lv_char.

          IF lv_escape = abap_true.
            lv_escape = abap_false.
          ELSEIF lv_char = '\'.
            lv_escape = abap_true.
          ELSEIF lv_char = '"'.
            " Closing quote of the key → start collecting whitespace
            lv_state = 'A'.
            CLEAR lv_space_buffer.
          ENDIF.

        " ---------------------------------------------------------------
        " AFTER_KEY – between closing key-quote and the expected colon
        " ---------------------------------------------------------------
        WHEN 'A'.
          IF lv_char = ':'.
            " Discard all buffered whitespace; write ':' directly
            lv_output = lv_output && ':'.
            lv_state  = 'N'.
            CLEAR lv_space_buffer.
          ELSEIF lv_char = ' ' OR lv_char = cl_abap_char_utilities=>horizontal_tab.
            " Collect but do not yet write
            lv_space_buffer = lv_space_buffer && lv_char.
          ELSE.
            " Unexpected – flush buffered spaces and write the character
            lv_output       = lv_output && lv_space_buffer && lv_char.
            CLEAR lv_space_buffer.
            lv_state        = 'N'.
          ENDIF.

        " ---------------------------------------------------------------
        " IN_VALUE_STR – inside a value string; pass everything through
        " ---------------------------------------------------------------
        WHEN 'V'.
          lv_output = lv_output && lv_char.

          IF lv_escape = abap_true.
            lv_escape = abap_false.
          ELSEIF lv_char = '\'.
            lv_escape = abap_true.
          ELSEIF lv_char = '"'.
            lv_state = 'N'.
          ENDIF.

      ENDCASE.
    ENDDO.

    rv_result = lv_output.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_VALIDATOR=>FORMAT_AND_VALIDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON_STRING                 TYPE        STRING
* | [<---] EV_FORMATTED_JSON              TYPE        STRING
* | [<-()] RS_RESULT                      TYPE        TY_VALIDATION_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_and_validate.
    " Step 1 – remove extra whitespace before colons
    ev_formatted_json = format_colon_spacing_multiline( iv_json_string ).

    " Step 2 – validate the cleaned JSON
    rs_result = validate( ev_formatted_json ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_VALIDATOR=>FORMAT_COLON_SPACING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON_STRING                 TYPE        STRING
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_colon_spacing.
    " Treat the entire string as one logical line.
    rv_result = fix_colon_spacing_in_line( iv_json_string ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JSON_VALIDATOR=>FORMAT_COLON_SPACING_MULTILINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON_STRING                 TYPE        STRING
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_colon_spacing_multiline.
    DATA: lt_lines  TYPE TABLE OF string,
          lv_line   TYPE string,
          lv_fixed  TYPE string,
          lv_result TYPE string.

    " Split on newline so each line is processed independently.
    SPLIT iv_json_string AT cl_abap_char_utilities=>newline
      INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.
      lv_fixed = fix_colon_spacing_in_line( lv_line ).

      IF sy-tabix = 1.
        lv_result = lv_fixed.
      ELSE.
        lv_result = lv_result && cl_abap_char_utilities=>newline && lv_fixed.
      ENDIF.
    ENDLOOP.

    rv_result = lv_result.
  ENDMETHOD.
ENDCLASS.