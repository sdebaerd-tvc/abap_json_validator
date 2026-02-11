# abap_json_validator
A JSON Validator class made in ABAP

## How to install
Create a new class called ZCL_JSON_VALIDATOR and copy the contents in the Text-based Sourcecode Editor.
Create a new program called Z_TEST_JSON_VALIDATOR and copy the contents in the Sourcecode Editor.
Activate both and launch the Z_TEST_JSON_VALIDATOR to perform a test.

## How to use in your ABAP logic
```abap
  DATA: lv_json_string TYPE string,
        ls_result      TYPE zcl_json_validator=>ty_validation_result,
        ls_error       TYPE zcl_json_validator=>ty_validation_error.
  " Validate the JSON
  ls_result = zcl_json_validator=>validate( lv_json_string ).

  " Display results
  IF ls_result-is_valid = abap_true.
    WRITE: / '✓ JSON IS VALID!'.
    WRITE: / ls_result-message.
  ELSE.
    WRITE: / '✗ JSON VALIDATION FAILED'.
    WRITE: / ls_result-message.
    SKIP.
    WRITE: / 'Errors found:'.
    WRITE: / '-------------------------------------------'.

    LOOP AT ls_result-errors INTO ls_error.
      WRITE: / |Line { ls_error-line_number WIDTH = 4 ALIGN = RIGHT }, |,
               |Col { ls_error-column WIDTH = 4 ALIGN = RIGHT }: |,
               |[{ ls_error-severity }] { ls_error-message }|.
    ENDLOOP.
  ENDIF.

  SKIP.
  WRITE: / '==========================================='.

  " Additional example: Validate directly and display
  SKIP 2.
  WRITE: / 'Alternative method using validate_and_display:'.
  WRITE: / '-------------------------------------------'.
  zcl_json_validator=>validate_and_display( lv_json_string ).

  SKIP 2.
  WRITE: / 'Alternative method using format_and_validate:'.
  WRITE: / '-------------------------------------------'.
  zcl_json_validator=>format_and_validate(
  EXPORTING iv_json_string    = lv_json_string
  IMPORTING ev_formatted_json = lv_formatted_json
  RECEIVING rs_result         = ls_result ).
  IF ls_result-is_valid = abap_true.
    WRITE: / '✓ JSON IS VALID!'.
    WRITE: / ls_result-message.
    WRITE: lv_formatted_json.
  ELSE.
    WRITE: / '✗ JSON VALIDATION FAILED'.
    WRITE: / ls_result-message.
    SKIP.
    WRITE: / 'Errors found:'.
    WRITE: / '-------------------------------------------'.

    LOOP AT ls_result-errors INTO ls_error.
      WRITE: / |Line { ls_error-line_number WIDTH = 4 ALIGN = RIGHT }, |,
               |Col { ls_error-column WIDTH = 4 ALIGN = RIGHT }: |,
               |[{ ls_error-severity }] { ls_error-message }|.
    ENDLOOP.
  ENDIF.
```
