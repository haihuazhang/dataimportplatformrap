@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define view entity ZZC_ZT_DTIMP_STRUC
//  provider contract transactional_query
  as projection on ZZR_ZT_DTIMP_STRUC
{
  key UUID,
  UUIDConf,
  RootNode,
  SheetName,
  SheetNameUp,
  StartLine,
  StartColumn,
  HasFieldnameLine,
  HasDescLine,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  LocalLastChangedAt,
  _Fields : redirected to composition child ZZC_ZT_DTIMP_FIELD,
  _Conf : redirected to parent ZZC_ZT_DTIMP_CONF
  
}
