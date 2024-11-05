@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define view entity ZZC_ZT_DTIMP_FIELD
  //  provider contract TRANSACTIONAL_QUERY
  as projection on ZZR_ZT_DTIMP_FIELD
{
  key UUID,
      UUIDStruc,
      UUIDConf,
      FieldName,
      FieldDescription,
//      KeyField,
      IsKeyField,
      Sequence,
      IsForeignField,
      ForeignField,
      FieldLength,
      FieldType1,
      FieldDecimal,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,
      _Structure : redirected to parent ZZC_ZT_DTIMP_STRUC,
      _Conf      : redirected to ZZC_ZT_DTIMP_CONF

}
