unit AddNewConfigValueResponseUnit;

interface

uses
  REST.Json.Types,
  NullableBasicTypesUnit, GenericParametersUnit, JSONNullableAttributeUnit;

type
  TAddNewConfigValueResponse = class(TGenericParameters)
  private
    [JSONName('result')]
    [Nullable]
    FResult: NullableString;

    [JSONName('affected')]
    [Nullable]
    FAffected: NullableInteger;
  public
    property Result: NullableString read FResult;
    property Affected: NullableInteger read FAffected;
  end;

implementation

end.
