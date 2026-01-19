CLASS zcl_certi_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_certi_service IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA lt_calculated_data TYPE STANDARD TABLE OF zc_certificate.
    DATA lt_lines TYPE tline_tab.
    DATA: lv_id       TYPE thead-tdid,
          lv_language TYPE thead-tdspras,
          lv_name     TYPE thead-tdname,
          lv_object   TYPE thead-tdobject.
    MOVE-CORRESPONDING it_original_data TO lt_calculated_data.

    LOOP AT lt_calculated_data ASSIGNING FIELD-SYMBOL(<ls_calculated_data>).

      lv_id = 'GRUN'.
      lv_language = sy-langu.
      lv_name = <ls_calculated_data>-product.
      lv_object = 'MATERIAL'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = lv_id
          language                = lv_language
          name                    = lv_name
          object                  = lv_object
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        LOOP AT lt_lines INTO DATA(ls_line).
          CONCATENATE <ls_calculated_data>-producttext ls_line-tdline INTO <ls_calculated_data>-producttext SEPARATED BY space.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    MOVE-CORRESPONDING lt_calculated_data TO ct_calculated_data.

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

    LOOP AT it_requested_calc_elements INTO DATA(ls_data).

      CASE ls_data.
        WHEN 'PRODUCTTEXT'.
          APPEND 'PRODUCT' TO et_requested_orig_elements.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
