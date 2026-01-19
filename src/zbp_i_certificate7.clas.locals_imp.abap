CLASS lhc_certificate DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS setinitialvalues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR certificate~setinitialvalues.

    METHODS get_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR certificate RESULT result.

    METHODS checkProduct FOR VALIDATE ON SAVE
      IMPORTING keys FOR certificate~checkProduct.

    METHODS newversion FOR MODIFY
      IMPORTING keys FOR ACTION certificate~newVersion RESULT result.

    METHODS releaseversion FOR MODIFY
      IMPORTING keys FOR ACTION certificate~releaseversion RESULT result.

    METHODS archiveversion FOR MODIFY
      IMPORTING keys FOR ACTION certificate~archiveversion RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR certificate RESULT result.

ENDCLASS.


CLASS lhc_certificate IMPLEMENTATION.
  METHOD setinitialvalues.
    DATA lt_CertificateState      TYPE TABLE FOR CREATE zi_certificate\_CertificateState.
    DATA ls_CertificateState      LIKE LINE OF lt_CertificateState.
    DATA ls_CertificateStateValue LIKE LINE OF ls_certificatestate-%target.

    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         FIELDS ( Certificationstatus ) WITH CORRESPONDING #( keys )
         RESULT DATA(certificates).

    LOOP AT certificates INTO DATA(ls_certificate).
      " Set the new overall status
      MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
             ENTITY Certificate
             UPDATE
             FIELDS ( Version CertificationStatus )
             WITH VALUE #( FOR key IN keys
                           ( %tky                = key-%tky
                             Version             = '00001'
                             CertificationStatus = ls_Certificate-CertificationStatus ) )
             " TODO: variable is assigned but never used (ABAP cleaner)
             FAILED DATA(ls_failed)
             " TODO: variable is assigned but never used (ABAP cleaner)
             REPORTED  DATA(ls_return).

      ls_CertificateState-%key = ls_certificate-%key.
      ls_certificatestatevalue-CertUUID = ls_certificate-CertUUID.
      ls_CertificateState-CertUUID = ls_certificatestatevalue-CertUUID.

      ls_certificatestatevalue-Status = '01'.
      CLEAR ls_certificatestatevalue-StatusOld.
      ls_certificatestatevalue-Version = '00001'.
      ls_certificatestatevalue-%cid    = 'Neu'.

      ls_certificatestatevalue-%control-CertUUID      = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Status        = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-StatusOld     = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Version       = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedAt = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedBy = if_abap_behv=>mk-on.
      APPEND ls_certificatestatevalue TO ls_certificatestate-%target.

      APPEND ls_certificatestate TO lt_certificatestate.

      MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
             ENTITY certificate
             CREATE BY \_CertificateState
             FROM lt_CertificateState
                 " TODO: variable is assigned but never used (ABAP cleaner)
             REPORTED DATA(ls_return_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             MAPPED DATA(ls_mapped_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             FAILED DATA(ls_failed_ass).

    ENDLOOP.
  ENDMETHOD.

  METHOD get_features.
    READ ENTITIES OF ZI_Certificate IN LOCAL MODE
         ENTITY Certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(certificates).

    result = VALUE #( FOR Certificate IN certificates
                      ( %tky   = Certificate-%tky ) ).


    LOOP AT keys INTO DATA(key).
      LOOP AT certificates INTO DATA(ls_certificate) WHERE %key-CertUUID = key-CertUUID.
        APPEND VALUE #( %tky                = key-%tky
              %features-%action-newVersion = COND #( WHEN ls_certificate-CertificationStatus = '02'
                                                            THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled ) )
                                                             TO result.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD checkProduct.
    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(certificates).

    IF certificates IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT certificates INTO DATA(certificate).

*      SELECT SINGLE FROM i_product
*        FIELDS Product
*        WHERE Product = @certificate-Product
*        " TODO: variable is assigned but never used (ABAP cleaner)
*        INTO @DATA(ls_product).

      READ ENTITIES OF I_ProductTP_2
      ENTITY Product
      FROM CORRESPONDING #( keys )
       RESULT DATA(Products).

      IF sy-subrc <> 0.

        APPEND VALUE #( %tky = certificate-%tky ) TO failed-certificate.

        APPEND VALUE #( %tky = certificate-%tky
                        %msg = NEW zcx_certificate( severity = if_abap_behv_message=>severity-error
                                                    textid   = zcx_certificate=>Product_unknown
                                                    Product  = certificate-Product ) )
               TO reported-certificate.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD newversion.
    DATA lt_CertificateState      TYPE TABLE FOR CREATE zi_certificate\_CertificateState.
    DATA ls_CertificateState      LIKE LINE OF lt_CertificateState.
    DATA ls_CertificateStateValue LIKE LINE OF ls_certificatestate-%target.
    DATA lv_status_old            TYPE zbc_status.

    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(certificates).

    LOOP AT certificates INTO DATA(ls_Certificate).
      ls_Certificate-Version += 1.
      lv_status_old = ls_Certificate-CertificationStatus.
      ls_Certificate-CertificationStatus = '01'. " neu
    ENDLOOP.

    " Set the new overall status
    MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
           ENTITY Certificate
           UPDATE
           FIELDS ( Version CertificationStatus )
           WITH VALUE #( FOR key IN keys
                         ( %tky                = key-%tky
                           Version             = ls_Certificate-Version
                           CertificationStatus = ls_Certificate-CertificationStatus ) )
           FAILED failed
           REPORTED reported.

*    " Fill the response table
    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT certificates.

    result = VALUE #( FOR certificate IN certificates
                      ( %tky   = certificate-%tky
                        %param = certificate ) ).

    " Fill Certificate State
    LOOP AT certificates INTO ls_certificate.

      ls_CertificateState-%key = ls_certificate-%key.
      ls_certificatestatevalue-CertUUID = ls_certificate-CertUUID.
      ls_CertificateState-CertUUID = ls_certificatestatevalue-CertUUID.

      ls_certificatestatevalue-%cid      = 'Neu'.
      ls_certificatestatevalue-Status    = ls_certificate-CertificationStatus.
      ls_certificatestatevalue-StatusOld = lv_status_old.
      ls_certificatestatevalue-Version   = ls_certificate-Version.
      ls_certificatestatevalue-%control-CertUUID      = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Status        = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-StatusOld     = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Version       = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedAt = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedBy = if_abap_behv=>mk-on.
      APPEND ls_certificatestatevalue TO ls_certificatestate-%target.

      APPEND ls_certificatestate TO lt_certificatestate.

      MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
             ENTITY certificate
             CREATE BY \_CertificateState
             FROM lt_CertificateState
             REPORTED DATA(lt_reported_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             MAPPED DATA(lt_mapped_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             FAILED DATA(lt_failed_ass).

      APPEND LINES OF lt_reported_ass-certificate TO reported-certificate.
      APPEND LINES OF lt_reported_ass-certificatestate TO reported-certificatestate.

    ENDLOOP.
  ENDMETHOD.

  METHOD releaseversion.
    DATA lt_CertificateState      TYPE TABLE FOR CREATE zi_certificate\_CertificateState.
    DATA ls_CertificateState      LIKE LINE OF lt_CertificateState.
    DATA ls_CertificateStateValue LIKE LINE OF ls_certificatestate-%target.
    DATA lv_status_old            TYPE zbc_status.

    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(certificates).

    READ TABLE certificates INTO DATA(ls_Certificate) INDEX 1.
    IF sy-subrc = 0.
      lv_status_old = ls_Certificate-CertificationStatus.
      ls_Certificate-CertificationStatus = '02'. " aktiv
    ENDIF.

    " Set the new overall status
    MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
           ENTITY Certificate
           UPDATE
           FIELDS ( CertificationStatus )
           WITH VALUE #( FOR key IN keys
                         ( %tky                = key-%tky
                           CertificationStatus = ls_Certificate-CertificationStatus ) )
           FAILED failed
           REPORTED reported.

*    " Fill the response table
    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT certificates.

    result = VALUE #( FOR certificate IN certificates
                      ( %tky   = certificate-%tky
                        %param = certificate ) ).

    " Fill Certificate State
    LOOP AT certificates INTO ls_certificate.

      ls_CertificateState-%key = ls_certificate-%key.
      ls_certificatestatevalue-CertUUID = ls_certificate-CertUUID.
      ls_CertificateState-CertUUID = ls_certificatestatevalue-CertUUID.

      ls_certificatestatevalue-%cid      = 'Neu'.
      ls_certificatestatevalue-Status    = ls_certificate-CertificationStatus.
      ls_certificatestatevalue-StatusOld = lv_status_old.
      ls_certificatestatevalue-Version   = ls_certificate-Version.
      ls_certificatestatevalue-%control-CertUUID      = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Status        = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-StatusOld     = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Version       = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedAt = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedBy = if_abap_behv=>mk-on.
      APPEND ls_certificatestatevalue TO ls_certificatestate-%target.

      APPEND ls_certificatestate TO lt_certificatestate.

      MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
             ENTITY certificate
             CREATE BY \_CertificateState
             FROM lt_CertificateState
                 " TODO: variable is assigned but never used (ABAP cleaner)
             REPORTED DATA(ls_return_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             MAPPED DATA(ls_mapped_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             FAILED DATA(ls_failed_ass).

    ENDLOOP.
  ENDMETHOD.

  METHOD archiveversion.
    DATA lt_CertificateState      TYPE TABLE FOR CREATE zi_certificate\_CertificateState.
    DATA ls_CertificateState      LIKE LINE OF lt_CertificateState.
    DATA ls_CertificateStateValue LIKE LINE OF ls_certificatestate-%target.
    DATA lv_status_old            TYPE zbc_status.

    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(certificates).

    READ TABLE certificates INTO DATA(ls_Certificate) INDEX 1.
    IF sy-subrc = 0.
      lv_status_old = ls_Certificate-CertificationStatus.
      ls_Certificate-CertificationStatus = '03'. " inaktiv
    ENDIF.

    " Set the new overall status
    MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
           ENTITY Certificate
           UPDATE
           FIELDS ( CertificationStatus )
           WITH VALUE #( FOR key IN keys
                         ( %tky                = key-%tky
                           CertificationStatus = ls_Certificate-CertificationStatus ) )
           FAILED failed
           REPORTED reported.

*    " Fill the response table
    READ ENTITIES OF zi_certificate IN LOCAL MODE
         ENTITY certificate
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT certificates.

    result = VALUE #( FOR certificate IN certificates
                      ( %tky   = certificate-%tky
                        %param = certificate ) ).

    " Fill Certificate State
    LOOP AT certificates INTO ls_certificate.
      ls_CertificateState-%key = ls_certificate-%key.
      ls_certificatestatevalue-CertUUID = ls_certificate-CertUUID.
      ls_CertificateState-CertUUID = ls_certificatestatevalue-CertUUID.

      ls_certificatestatevalue-%cid      = 'Neu'.
      ls_certificatestatevalue-Status    = ls_certificate-CertificationStatus.
      ls_certificatestatevalue-StatusOld = lv_status_old.
      ls_certificatestatevalue-Version   = ls_certificate-Version.
      ls_certificatestatevalue-%control-CertUUID      = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Status        = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-StatusOld     = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-Version       = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedAt = if_abap_behv=>mk-on.
      ls_certificatestatevalue-%control-LastChangedBy = if_abap_behv=>mk-on.
      APPEND ls_certificatestatevalue TO ls_certificatestate-%target.

      APPEND ls_certificatestate TO lt_certificatestate.

      MODIFY ENTITIES OF zi_certificate IN LOCAL MODE
             ENTITY certificate
             CREATE BY \_CertificateState
             FROM lt_CertificateState
                 " TODO: variable is assigned but never used (ABAP cleaner)
             REPORTED DATA(ls_return_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             MAPPED DATA(ls_mapped_ass)
             " TODO: variable is assigned but never used (ABAP cleaner)
             FAILED DATA(ls_failed_ass).

    ENDLOOP.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.
ENDCLASS.
