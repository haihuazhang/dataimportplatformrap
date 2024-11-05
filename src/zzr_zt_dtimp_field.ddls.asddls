@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
define view entity ZZR_ZT_DTIMP_FIELD
  as select from zzt_dtimp_field as Field
  association to parent ZZR_ZT_DTIMP_STRUC as _Structure on $projection.UUIDStruc = _Structure.UUID
  association to one ZZI_ZT_DTIMP_CONF     as _Conf      on $projection.UUIDConf = _Conf.UUID
{
  key uuid                  as UUID,
      uuid_struc            as UUIDStruc,
      uuid_conf             as UUIDConf,
      field_name            as FieldName,
      field_desc            as FieldDescription,
      //  key_field as KeyField,
      is_key_field          as IsKeyField,
      sequence              as Sequence,
      is_foreign_field      as IsForeignField,
      foreign_field         as ForeignField,
      field_length          as FieldLength,
      @Consumption.valueHelpDefinition: [{  entity: {   name: 'ZZR_DTIMP_DATATYPE_VH' ,
                                                              element: 'value_low'  }     }]
      field_type            as FieldType1,
      field_decimal         as FieldDecimal,
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
      _Structure,
      _Conf

}
