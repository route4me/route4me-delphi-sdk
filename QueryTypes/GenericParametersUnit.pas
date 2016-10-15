unit GenericParametersUnit;

interface

type

  TGenericParameters = class
  public
    function ToJsonString: String;
  end;

implementation

{ TGenericParameters }

uses MarshalUnMarshalUnit;

function TGenericParameters.ToJsonString: String;
begin
  Result := TMarshalUnMarshal.ToJson(Self);
end;

end.
