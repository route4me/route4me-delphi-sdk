unit GetAllVendorsUnit;

interface

uses SysUtils, Classes, BaseExampleUnit, EnumsUnit;

type
  TGetAllVendors = class(TBaseExample)
  public
    procedure Execute();
  end;

implementation

uses NullableBasicTypesUnit, VendorUnit;

procedure TGetAllVendors.Execute();
var
  ErrorString: String;
  Vendors: TVendorList;
begin
  Vendors := Route4MeManager.Telematics.Get(ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn('GetAllVendors successfully');
      WriteLn('');
    end
    else
      WriteLn(Format('GetAllVendors error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Vendors);
  end;
end;

end.
