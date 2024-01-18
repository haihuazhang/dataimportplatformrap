@EndUserText.label: 'CDS of Batch Data Import Structure'
@ObjectModel.query.implementedBy:'ABAP:ZZCL_DTIMP_DDICS'
define custom entity ZZR_DTIMP_STRUC
{
      @EndUserText.label : 'Structure Name'
  key StructureName : abap.char(64);
      @EndUserText.label : 'Description'
      StructureDesc : abap.char( 120 );

}
