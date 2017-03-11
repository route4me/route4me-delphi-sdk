unit SearchVendorsUnit;

interface

uses SysUtils, Classes, BaseExampleUnit, EnumsUnit;

type
  TSearchVendors = class(TBaseExample)
  public
    procedure Execute(Size: TVendorSizeType; IsIntegrated: Boolean;
      Feature, Country, Search: String; Page, PerPage: Integer);
  end;

implementation

uses NullableBasicTypesUnit, VendorUnit;

procedure TSearchVendors.Execute(Size: TVendorSizeType; IsIntegrated: Boolean;
  Feature, Country, Search: String; Page, PerPage: Integer);
var
  ErrorString: String;
  Vendors: TVendorList;
begin
  Vendors := Route4MeManager.Telematics.Search(Size, IsIntegrated,
    Feature, Country, Search, Page, PerPage, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn('SearchVendors successfully');
      WriteLn('');
    end
    else
      WriteLn(Format('SearchVendors error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Vendors);
  end;
end;

end.
