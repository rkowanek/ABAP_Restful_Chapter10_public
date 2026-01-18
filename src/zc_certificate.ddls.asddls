@EndUserText.label: 'Zertifikatsverwaltung Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZC_Certificate
  provider contract transactional_query
  as projection on ZI_Certificate as Certificate
{
  key     CertUUID,

          @ObjectModel.text.element: ['ProductName']
          @Consumption.valueHelpDefinition: [{entity: {name: 'I_ProductText', element: 'Product'} }]
          @Search.defaultSearchElement: true
          @Consumption.semanticObject: 'Product'
          Product,
          Version,
          @Search.defaultSearchElement: true
          _ProductText[Language = $session.system_language].ProductName as ProductName,
          CertificationStatus,
          @Search.defaultSearchElement: true
          @Consumption.valueHelpDefinition: [{entity: {name: 'ZI_STATUS_VH', element: 'Text'} }]
          StatusText,
          CertificateCe,
          CertificateGs,
          CertificateTuev,
          AttachmentCE,
          MimeTypeCE,
          FilenameCE,
          AttachmentGS,
          MimeTypeGS,
          FilenameGS,
          AttachmentTuev,
          MimeTypeTuev,
          FilenameTuev,
          LocalLastChangedAt,
          Criticality,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_CERTI_SERVICE'
  virtual ProductText : abap.char( 150 ),

          /* Associations */
          _CertificateState : redirected to composition child ZC_CertificateState,

          _ProductText
}
