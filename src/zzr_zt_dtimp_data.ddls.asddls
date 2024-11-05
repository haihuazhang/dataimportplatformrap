@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
define  view entity ZZR_ZT_DTIMP_DATA
  as select from zzt_dtimp_data as ImportData
  association to parent ZZR_ZT_DTIMP_FILES as _Record on $projection.UUIDRecord = _Record.UUID
{
  key uuid as UUID,
  uuid_record as UUIDRecord,
  data_json as DataJSON,
  line as Line,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.lastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed_at as LastChangedAt,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed_at as LocalLastChangedAt,
  _Record
  
}
