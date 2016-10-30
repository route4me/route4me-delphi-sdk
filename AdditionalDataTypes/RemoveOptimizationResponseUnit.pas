unit RemoveOptimizationResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TRemoveOptimizationResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('removed')]
    FRemoved: Boolean;
  public
    property Status: boolean read FStatus write FStatus;
    property Removed: Boolean read FRemoved write FRemoved;
  end;

implementation

end.
