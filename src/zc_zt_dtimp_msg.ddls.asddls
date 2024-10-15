@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define  view entity ZC_ZT_DTIMP_MSG
//  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_ZT_DTIMP_MSG
{
  key UUID,
  UUIDRecord,
  Line,
  Type,
  Id,
  MsgNumber,
  Message,
  MessageV1,
  MessageV2,
  MessageV3,
  MessageV4,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  LocalLastChangedAt,
  _Record : redirected to parent ZZC_ZT_DTIMP_FILES
  
}
