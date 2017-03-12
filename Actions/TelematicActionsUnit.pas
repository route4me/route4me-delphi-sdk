unit TelematicActionsUnit;

interface

uses
  SysUtils, BaseActionUnit, VendorUnit, EnumsUnit, NullableBasicTypesUnit;

type
  TTelematicActions = class(TBaseAction)
  public
    /// <summary>
    /// GET a telematics vendor
    /// </summary>
    function Get(VendorId: integer; out ErrorString: String): TVendor; overload;

    /// <summary>
    /// GET all telematics vendors
    /// </summary>
    function Get(out ErrorString: String): TVendorList; overload;

    /// <summary>
    /// Search vendors
    /// </summary>
    function Search(Size: NullableVendorSizeType; IsIntegrated: NullableBoolean;
      Feature, Country, Search: NullableString; Page: NullableInteger;
      PerPage: NullableInteger; out ErrorString: String): TVendorList;
  end;

implementation

{ TTelematicActions }

uses
  SettingsUnit, GenericParametersUnit, CommonTypesUnit, GetVendorsResponseUnit;

function TTelematicActions.Get(VendorId: integer; out ErrorString: String): TVendor;
var
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('vendor_id', IntToStr(VendorId));

    Result := FConnection.Get(TSettings.EndPoints.TelematicsGateway, Parameters,
      TVendor, ErrorString) as TVendor;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTelematicActions.Get(out ErrorString: String): TVendorList;
var
  Parameters: TGenericParameters;
  Response: TGetVendorsResponse;
begin
  Result := TVendorList.Create;

  Parameters := TGenericParameters.Create;
  try
    Response := FConnection.Get(TSettings.EndPoints.TelematicsGateway, Parameters,
      TGetVendorsResponse, ErrorString) as TGetVendorsResponse;
    try
      if (Response <> nil) then
          Result.AddRange(Response.Vendors);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTelematicActions.Search(Size: NullableVendorSizeType;
  IsIntegrated: NullableBoolean; Feature, Country, Search: NullableString;
  Page: NullableInteger; PerPage: NullableInteger; out ErrorString: String): TVendorList;
var
  Parameters: TGenericParameters;
  Response: TGetVendorsResponse;
begin
  Result := TVendorList.Create;

  Parameters := TGenericParameters.Create;
  try
    if (IsIntegrated.IsNotNull) then
      if (IsIntegrated.Value) then
        Parameters.AddParameter('is_integrated', '1')
      else
        Parameters.AddParameter('is_integrated', '0');
    if (Feature.IsNotNull) then
      Parameters.AddParameter('feature', Feature);
    if (Size.IsNotNull) then
      Parameters.AddParameter('size', TVendorSizeTypeDescription[Size.Value]);
    if (Country.IsNotNull) then
      Parameters.AddParameter('country', Country);
    if (Search.IsNotNull) then
      Parameters.AddParameter('s', Search);
    if (Page.IsNotNull) then
      Parameters.AddParameter('page', IntToStr(Page));
    if (PerPage.IsNotNull) then
      Parameters.AddParameter('per_page', IntToStr(PerPage));

    Response := FConnection.Get(TSettings.EndPoints.TelematicsGateway, Parameters,
      TGetVendorsResponse, ErrorString) as TGetVendorsResponse;
    try
      if (Response <> nil) then
          Result.AddRange(Response.Vendors);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
