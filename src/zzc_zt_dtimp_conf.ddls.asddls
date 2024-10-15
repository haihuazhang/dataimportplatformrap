@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZZI_ZT_DTIMP_CONF'
@AbapCatalog.extensibility: {
  extensible: true,
  dataSources: ['Conf'],
  elementSuffix: 'ZAG',
  quota: {
    maximumFields: 500,
    maximumBytes: 5000
  }, allowNewCompositions: true  
}
define root view entity ZZC_ZT_DTIMP_CONF 
  provider contract transactional_query
  as projection on ZZI_ZT_DTIMP_CONF as Conf
{
  @ObjectModel.text.element: ['Objectname']
  key UUID,
  Object,
  Objectname,
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZZR_DTIMP_FUNC', element: 'FunctionModuleName' }}]
  Fmname,
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZZR_DTIMP_CLASS', element: 'id' }, useForValidation: true}]
  ClassName,
  Mimetype,
  Sheetname,
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZZR_DTIMP_STRUC', element: 'StructureName' }, useForValidation: true}]
  Structname,
  MimeTypeForTemplate,
  Template,
  FileName,
  StartLine,
  StartColumn,
  LocalLastChangedAt
  
}
