unit StatusResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TStatusResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;
  public
    property Status: boolean read FStatus write FStatus;
  end;

implementation

end.
