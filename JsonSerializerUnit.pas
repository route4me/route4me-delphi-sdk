unit JsonSerializerUnit;

interface

uses
  System.JSon, Rest.Json;

type
  TJsonSerializer = class(TJson)
  public
    class function ObjectToJson(AObject: TObject): String;
  end;

implementation

{ TJsonSerializer }

class function TJsonSerializer.ObjectToJson(AObject: TObject): String;
begin
  Result := '';
end;

end.
