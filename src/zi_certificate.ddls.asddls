@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Zertifikatsverwaltung Interface View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_Certificate
  as select from zbca_certificate as Certificate
  composition [0..*] of ZI_CertificateState as _CertificateState
  association [0..1] to I_ProductText       as _ProductText on  $projection.Product   = _ProductText.Product
                                                            and _ProductText.Language = $session.system_language
  association [0..1] to ZI_Status_VH        as _StatusText  on  $projection.CertificationStatus = _StatusText.Low
{
  key cert_uuid             as CertUUID,

      matnr                 as Product,
      version               as Version,
      cert_status           as CertificationStatus,
      _StatusText.Text      as StatusText,
      cert_ce               as CertificateCe,
      cert_gs               as CertificateGs,
      cert_tuev             as CertificateTuev,
      @Semantics.largeObject:
      { mimeType: 'MimeTypeCE',
      fileName: 'FilenameCE',
      acceptableMimeTypes: [ 'image/png', 'image/jpeg', 'application/pdf' ],
      contentDispositionPreference: #INLINE }
      attachment_ce            as AttachmentCE,
      @Semantics.mimeType: true
      mimetype_ce              as MimeTypeCE,
      filename_ce              as FilenameCE,

      @Semantics.largeObject:
      { mimeType: 'MimeTypeGS',
      fileName: 'FilenameGS',
      acceptableMimeTypes: [ 'image/png', 'image/jpeg', 'application/pdf' ],
      contentDispositionPreference: #INLINE }
      attachment_gs           as AttachmentGS,
      @Semantics.mimeType: true
      mimetype_gs             as MimeTypeGS,
      filename_gs             as FilenameGS,

      @Semantics.largeObject:
      { mimeType: 'MimeTypeTuev',
      fileName: 'FilenameTuev',
      acceptableMimeTypes: [ 'image/png', 'image/jpeg', 'application/pdf' ],
      contentDispositionPreference: #INLINE }
      attachment_tuev           as AttachmentTuev,
      @Semantics.mimeType: true
      mimetype_tuev             as MimeTypeTuev,
      filename_tuev             as FilenameTuev,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,

      case
        when ( cert_status = '01' or cert_status = '04' ) // neu oder teilaktiv
          then 2         //ICON_YELLOW_LIGHT
        when ( cert_status = '03'  or cert_status is initial ) // inaktiv oder leer
          then 1         //ICON_RED_LIGHT
        else 3           //ICON_GREEN_LIGHT
      end                   as Criticality,

      _CertificateState,
      _ProductText,
      _StatusText
}
