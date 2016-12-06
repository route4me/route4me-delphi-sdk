unit LocationSearchUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TLocationSearch = class(TBaseExample)
  public
    procedure Execute(Query: String; Fields: TArray<String>);
  end;

implementation

uses AddressBookContactUnit, CommonTypesUnit;

procedure TLocationSearch.Execute(Query: String; Fields: TArray<String>);
var
  ErrorString: String;
  Total: integer;
  FindedResults: T2DimensionalStringArray;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 0;

  FindedResults := Route4MeManager.AddressBookContact.Find(
    Query, Fields, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Length(FindedResults) > 0) then
    begin
      WriteLn(Format('LocationSearch executed successfully, ' +
        '%d contacts returned, total = %d', [Length(FindedResults), Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('LocationSearch error: "%s"', [ErrorString]));
  finally
    Finalize(FindedResults);
  end;
end;

end.
