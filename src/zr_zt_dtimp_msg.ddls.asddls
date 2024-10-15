@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
define view entity ZR_ZT_DTIMP_MSG
  as select from zzt_dtimp_msg as Message
  association to parent ZZR_ZT_DTIMP_FILES as _Record on $projection.UUIDRecord = _Record.UUID
{
  key uuid as UUID,
  uuid_record as UUIDRecord,
  line as Line,
  type as Type,
  id as Id,
  msg_number as MsgNumber,
  message as Message,
  message_v1 as MessageV1,
  message_v2 as MessageV2,
  message_v3 as MessageV3,
  message_v4 as MessageV4,
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
