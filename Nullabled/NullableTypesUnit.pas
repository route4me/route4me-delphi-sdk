unit NullableTypesUnit;

interface

type
  TNullableBoolean = class
  private
    [JSONNameAttribute('')]
    FValue: boolean;
  public
    constructor Create(Value: boolean);

    property Value: boolean read FValue write FValue;
  end;

implementation

{ TNullableBoolean }

constructor TNullableBoolean.Create(Value: boolean);
begin
  FValue := Value;
end;

end.
