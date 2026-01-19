@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Wertehilfe für Fixwerte an Domainen'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DOMAIN_VALUE_VH   
as select from    dd07l as FixedValue
    left outer join dd07t as ValueText on  FixedValue.domname    = ValueText.domname
                                       and FixedValue.domvalue_l = ValueText.domvalue_l
                                       and FixedValue.as4local   = ValueText.as4local
{

      @UI.hidden: true
  key FixedValue.domname    as DomainName,
      FixedValue.domvalue_l as Low,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.5
      @Semantics.text: true // identifies the text field
      ValueText.ddtext      as Text

}
where
      FixedValue.as4local  = 'A' // Active
//  and ValueText.ddlanguage = $session.system_language
