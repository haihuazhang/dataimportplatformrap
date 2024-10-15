@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZZR_ZT_DTIMP_FILES'
define root view entity ZZC_ZT_DTIMP_FILES
  provider contract transactional_query
  as projection on ZZR_ZT_DTIMP_FILES
{          @ObjectModel.text.element: ['Name']
  key      UUID,
           Name,
           @ObjectModel.text.element: ['ConfObjectName']
           UuidConf,
           _configuration.Objectname as ConfObjectName,
           MimeType,
           Attachment,
           FileName,
           LocalLastChangedAt,
           JobCount,
           JobName,
           LogHandle,
           Status,
           StatusText,
           StatusCriticality,

           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  JobStatus            : abap.char( 1 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  JobStatusText        : abap.char( 20 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  JobStatusCriticality : abap.int1,

           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  LogStatus            : abap.char( 1 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  LogStatusText        : abap.char( 20 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  LogStatusCriticality : abap.int1,
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  ApplicationLogUrl    : abap.string( 256 ),

           _configuration : redirected to ZZC_ZT_DTIMP_CONF,
           _Messages      : redirected to composition child ZC_ZT_DTIMP_MSG

}
