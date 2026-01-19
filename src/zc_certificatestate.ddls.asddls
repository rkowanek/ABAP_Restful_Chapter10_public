@EndUserText.label: 'Zertifikatsverwaltung Status Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_CertificateState
as projection on ZI_CertificateState as CertificateState {
    key StateUUID,
    CertUUID,

    Product,
    Version,
    Status,
    StatusOld,
    LastChangedBy,
    @Semantics.dateTime: true
    LastChangedAt,
    LocalLastChangedAt
    ,
    
    /* Associations */
    _Certificate : redirected to parent ZC_Certificate
} 
