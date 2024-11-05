@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define view entity ZZC_ZT_DTIMP_DATA
  //  provider contract TRANSACTIONAL_QUERY
  as projection on ZZR_ZT_DTIMP_DATA
{
  key UUID,
      UUIDRecord,
      DataJSON,
      Line,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,
      _Record : redirected to parent ZZC_ZT_DTIMP_FILES

}
