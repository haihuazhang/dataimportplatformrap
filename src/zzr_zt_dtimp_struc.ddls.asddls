@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
define view entity ZZR_ZT_DTIMP_STRUC
  as select from zzt_dtimp_struc as Structure
  composition of many ZZR_ZT_DTIMP_FIELD  as _Fields
  association to parent ZZI_ZT_DTIMP_CONF as _Conf on $projection.UUIDConf = _Conf.UUID
{
  key uuid                  as UUID,
      uuid_conf             as UUIDConf,
      root_node             as RootNode,
      sheet_name            as SheetName,
      sheet_name_up         as SheetNameUp,
      start_line            as StartLine,
      start_column          as StartColumn,
      has_fieldname_line    as HasFieldnameLine,
      has_desc_line         as HasDescLine,
      @Semantics.user.createdBy: true
      created_by            as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at            as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by       as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      _Fields,
      _Conf

}
