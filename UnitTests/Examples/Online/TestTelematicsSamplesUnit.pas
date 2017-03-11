unit TestTelematicsSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestTelematicsSamples = class(TTestOnlineExamples)
  private
  published
    procedure GetAllVendors;
    procedure GetVendor;
    procedure SearchVendors;
  end;

implementation

uses
  VehicleUnit, VendorUnit, EnumsUnit, NullableBasicTypesUnit;

procedure TTestTelematicsSamples.GetVendor;
var
  ErrorString: String;
  VendorId: integer;
  Vendor: TVendor;
begin
  VendorId := 153;
  Vendor := FRoute4MeManager.Telematics.Get(VendorId, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckNotNull(Vendor);
  finally
    FreeAndNil(Vendor);
  end;

  VendorId := -123;
  Vendor := FRoute4MeManager.Telematics.Get(VendorId, ErrorString);
  try
    CheckNotEquals(EmptyStr, ErrorString);
    CheckNull(Vendor);
  finally
    FreeAndNil(Vendor);
  end;
end;

procedure TTestTelematicsSamples.GetAllVendors;
var
  ErrorString: String;
  Vendors: TVendorList;
begin
  Vendors := FRoute4MeManager.Telematics.Get(ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckNotNull(Vendors);
    CheckTrue(Vendors.Count > 0);
  finally
    FreeAndNil(Vendors);
  end;
end;

procedure TTestTelematicsSamples.SearchVendors;
var
  ErrorString: String;
  Vendors: TVendorList;

  Size: NullableVendorSizeType;
  IsIntegrated: NullableBoolean;
  Feature, Country, Search: NullableString;
  Page: NullableInteger;
  PerPage: NullableInteger;
begin
  Size := TVendorSizeType.vsGlobal;
  IsIntegrated := True;
  Feature := 'Satellite';
  Country := 'GB';
  Search := NullableString.Null;
  Page := 1;
  PerPage := 15;

  Vendors := FRoute4MeManager.Telematics.Search(Size, IsIntegrated,
    Feature, Country, Search, Page, PerPage, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckNotNull(Vendors);
    CheckTrue(Vendors.Count > 0);
  finally
    FreeAndNil(Vendors);
  end;

  Size := TVendorSizeType.vsRegional;
  IsIntegrated := True;
  Feature := 'Satellite';
  Country := 'AU';
  Search := NullableString.Null;
  Page := 1;
  PerPage := 15;

  Vendors := FRoute4MeManager.Telematics.Search(Size, IsIntegrated,
    Feature, Country, Search, Page, PerPage, ErrorString);
  try
    CheckNotEquals(EmptyStr, ErrorString);
    CheckNotNull(Vendors);
    CheckTrue(Vendors.Count = 0);
  finally
    FreeAndNil(Vendors);
  end;

  Size := NullableVendorSizeType.Null;
  IsIntegrated := NullableBoolean.Null;
  Feature := 'Satellite';
  Country := NullableString.Null;
  Search := NullableString.Null;
  Page := NullableInteger.Null;
  PerPage := NullableInteger.Null;

  Vendors := FRoute4MeManager.Telematics.Search(Size, IsIntegrated,
    Feature, Country, Search, Page, PerPage, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckNotNull(Vendors);
    CheckTrue(Vendors.Count > 0);
  finally
    FreeAndNil(Vendors);
  end;
end;

initialization
  RegisterTest('Examples\Online\Telematics\', TTestTelematicsSamples.Suite);
end.
