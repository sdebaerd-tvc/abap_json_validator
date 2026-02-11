*&---------------------------------------------------------------------*
*& Report Z_TEST_JSON_VALIDATOR
*&---------------------------------------------------------------------*
*& Test program for JSON Validator
*&---------------------------------------------------------------------*
REPORT z_test_json_validator.

" Test cases
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_test1  RADIOBUTTON GROUP grp1 DEFAULT 'X',
              p_test2  RADIOBUTTON GROUP grp1,
              p_test3  RADIOBUTTON GROUP grp1,
              p_test4  RADIOBUTTON GROUP grp1,
              p_test5  RADIOBUTTON GROUP grp1,
              p_test6  RADIOBUTTON GROUP grp1,
              p_custom RADIOBUTTON GROUP grp1,
              p_file   RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_json TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_fpath TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b3.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath.
  PERFORM f4_file_path.

START-OF-SELECTION.

  DATA: lv_json_string TYPE string,
        ls_result      TYPE zcl_json_validator=>ty_validation_result,
        ls_error       TYPE zcl_json_validator=>ty_validation_error.

  " Select test case
  CASE abap_true.
    WHEN p_test1.
      " Valid JSON - Simple object
      lv_json_string = '{"name": "Jan", "age": 30, "city": "Brussels"}'.

    WHEN p_test2.
      " Invalid JSON - Missing closing brace
      lv_json_string =
        '{' && cl_abap_char_utilities=>newline &&
        '  "name": "Jan",' && cl_abap_char_utilities=>newline &&
        '  "age": 30,' && cl_abap_char_utilities=>newline &&
        '  "city": "Brussels"' && cl_abap_char_utilities=>newline.

    WHEN p_test3.
      " Invalid JSON - Missing comma
      lv_json_string =
        '{' && cl_abap_char_utilities=>newline &&
        '  "name": "Jan"' && cl_abap_char_utilities=>newline &&
        '  "age": 30,' && cl_abap_char_utilities=>newline &&
        '  "city": "Brussels"' && cl_abap_char_utilities=>newline &&
        '}'.

    WHEN p_test4.
      " Invalid JSON - Missing colon
      lv_json_string =
        '{' && cl_abap_char_utilities=>newline &&
        '  "name" "Jan",' && cl_abap_char_utilities=>newline &&
        '  "age": 30' && cl_abap_char_utilities=>newline &&
        '}'.

    WHEN p_test5.
      " Valid JSON - Complex nested structure
      lv_json_string =
        '{' && cl_abap_char_utilities=>newline &&
        '  "person": {' && cl_abap_char_utilities=>newline &&
        '    "name": "Ann",' && cl_abap_char_utilities=>newline &&
        '    "age": 25,' && cl_abap_char_utilities=>newline &&
        '    "hobbies": ["reading", "cycling", "cooking"],' && cl_abap_char_utilities=>newline &&
        '    "address": {' && cl_abap_char_utilities=>newline &&
        '      "street": "Hoofdstraat",' && cl_abap_char_utilities=>newline &&
        '      "number": 123' && cl_abap_char_utilities=>newline &&
        '    }' && cl_abap_char_utilities=>newline &&
        '  },' && cl_abap_char_utilities=>newline &&
        '  "active": true' && cl_abap_char_utilities=>newline &&
        '}'.

    WHEN p_test6.
      " Invalid JSON - Multiple errors
      lv_json_string =
        '{' && cl_abap_char_utilities=>newline &&
        '  "name": "Bob"' && cl_abap_char_utilities=>newline &&
        '  "age": 40,' && cl_abap_char_utilities=>newline &&
        '  "items": [1, 2, 3' && cl_abap_char_utilities=>newline &&
        '  ],' && cl_abap_char_utilities=>newline &&
        '  invalid_key: "value"' && cl_abap_char_utilities=>newline.

    WHEN p_custom.
      " Use custom JSON from parameter
      lv_json_string = p_json.

    WHEN p_file.
      " Upload JSON from file
      PERFORM upload_json_file USING p_fpath
                               CHANGING lv_json_string.
      IF lv_json_string IS INITIAL.
        WRITE: / 'Error: Could not read file or file is empty.'.
        RETURN.
      ENDIF.

  ENDCASE.

  " Display the JSON being tested
  WRITE: / 'Testing JSON:'.
  WRITE: / '==========================================='.
  WRITE: / lv_json_string.
  WRITE: / '==========================================='.
  SKIP.

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

*&---------------------------------------------------------------------*
*& Form f4_file_path
*&---------------------------------------------------------------------*
*& F4 help for file path selection
*&---------------------------------------------------------------------*
FORM f4_file_path.
  DATA: lt_files    TYPE filetable,
        lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string,
        lv_rc       TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select JSON File'
      default_extension       = 'json'
      file_filter             = 'JSON Files (*.json)|*.json|Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
      initial_directory       = 'C:\'
    CHANGING
      file_table              = lt_files
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lv_rc > 0.
    READ TABLE lt_files INTO lv_fullpath INDEX 1.
    IF sy-subrc = 0.
      p_fpath = lv_fullpath.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form upload_json_file
*&---------------------------------------------------------------------*
*& Upload JSON content from file
*&---------------------------------------------------------------------*
FORM upload_json_file USING pv_filepath TYPE rlgrap-filename
                      CHANGING cv_json_content TYPE string.

  DATA: lt_file_raw  TYPE TABLE OF string,
        lv_file_line TYPE string,
        lv_filename  TYPE string.

  CLEAR cv_json_content.

  IF pv_filepath IS INITIAL.
    MESSAGE 'Please specify a file path' TYPE 'E'.
    RETURN.
  ENDIF.

  lv_filename = pv_filepath.

  " Upload file as text
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
      has_field_separator     = ' '
    TABLES
      data_tab                = lt_file_raw
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  " Concatenate all lines into single string
  LOOP AT lt_file_raw INTO lv_file_line.
    IF sy-tabix = 1.
      cv_json_content = lv_file_line.
    ELSE.
      cv_json_content = cv_json_content && cl_abap_char_utilities=>newline && lv_file_line.
    ENDIF.
  ENDLOOP.

  IF cv_json_content IS INITIAL.
    MESSAGE 'File is empty or could not be read' TYPE 'W'.
  ELSE.
    WRITE: / |File uploaded successfully: { lines( lt_file_raw ) } lines read|.
    SKIP.
  ENDIF.

ENDFORM.