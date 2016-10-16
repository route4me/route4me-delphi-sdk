unit NullableBasicTypesUnit;

interface
uses
    SysUtils, Math, Types;

type
    NullableObject = record
    strict private
        FValue: TObject;
        FIsNull: boolean;

        function GetValue: TObject;
    public
        constructor Create(PValue: TObject);

        class operator Implicit(A: NullableObject): TObject;
        class operator Implicit(PValue: TObject): NullableObject;
        class operator Equal(A, B: NullableObject): boolean;
        class operator NotEqual(A, B: NullableObject): boolean;

        class function Null: NullableObject; static;

        procedure Free;

        property Value: TObject read GetValue;
        property IsNull: boolean read FIsNull;
        function IsNotNull: boolean;
    end;

    NullableInt64 = record
    strict private
        FValue: int64;
        FIsNull: boolean;

        function GetValue: int64;
    public
        constructor Create(PValue: int64);

        class operator Implicit(A: NullableInt64): int64;
        class operator Implicit(PValue: int64): NullableInt64;
        class operator Equal(A, B: NullableInt64): boolean;
        class operator NotEqual(A, B: NullableInt64): boolean;

        class function Null: NullableInt64; static;

        property Value: int64 read GetValue;
        property IsNull: boolean read FIsNull;
        function IsNotNull: boolean;
    end;

    NullableInteger = record
    strict private
        FValue: integer;
        FIsNull: boolean;

        function GetValue: integer;
    private
        function LessThen(Another: NullableInteger): boolean;
        function GreaterThen(Another: NullableInteger): boolean;
        function LessThenOrEqual(Another: NullableInteger): boolean;
        function GreaterThenOrEqual(Another: NullableInteger): boolean;
    public
        constructor Create(PValue: integer);

        class operator Implicit(A: NullableInteger): integer;
        class operator Implicit(PValue: integer): NullableInteger;
        class operator Equal(A, B: NullableInteger): boolean;
        class operator NotEqual(A, B: NullableInteger): boolean;
        class operator LessThan(A, B: NullableInteger): boolean;
        class operator GreaterThan(A, B: NullableInteger): boolean;
        class operator LessThanOrEqual(A, B: NullableInteger): boolean;
        class operator GreaterThanOrEqual(A, B: NullableInteger): boolean;

        class function Null: NullableInteger; static;

        function ToString(): String;

        property Value: integer read GetValue;
        property IsNull: boolean read FIsNull;
        function IsNotNull: boolean;
    end;

    NullableDouble = record
    strict private
    const
        NullStr = 'Null';
    var
        FValue: double;
        FIsNull: boolean;

        function GetValue: double;

        function LessThen(Another: NullableDouble): boolean;
        function GreaterThen(Another: NullableDouble): boolean;
        function LessThenOrEqual(Another: NullableDouble): boolean;
        function GreaterThenOrEqual(Another: NullableDouble): boolean;
    public
        constructor Create(PValue: double); 

        class function NullIfNaN(Value: double): NullableDouble; static;

        function Equals(OtherValue: NullableDouble; Epsilon: double): boolean;  

        class operator Implicit(A: NullableDouble): double;
        class operator Implicit(PValue: double): NullableDouble;
        class operator Equal(A, B: NullableDouble): boolean;
        class operator NotEqual(A, B: NullableDouble): boolean;
        class operator LessThan(A, B: NullableDouble): boolean;
        class operator GreaterThan(A, B: NullableDouble): boolean;
        class operator LessThanOrEqual(A, B: NullableDouble): boolean;
        class operator GreaterThanOrEqual(A, B: NullableDouble): boolean;

        class function Null: NullableDouble; static;

        function ToString(): String; overload;
        class function FromString(PValue: String): NullableDouble; static;

        property Value: double read GetValue;
        property IsNull: boolean read FIsNull;
        function IsNotNull: boolean;

        function IfNull(DefaultValue: double): double;
    end;
    NullableDoubleArray = array of NullableDouble;

    NullableString = record
    private
        FValue: String;
        FIsNull: boolean;

        function GetValue: String;
    public
        constructor Create(PValue: String);

        class operator Implicit(A: NullableString): String;
        class operator Implicit(PValue: String): NullableString;
        class operator Equal(A, B: NullableString): boolean;
        class operator NotEqual(A, B: NullableString): boolean;

        class function Null: NullableString; static;

        function ToString(): String;

        property Value: String read GetValue;
        property IsNull: boolean read FIsNull;
        function IsNotNull: boolean;

        function IfNull(DefaultValue: string): string;
    end;

    NullableBoolean = record
    strict private
        FValue: boolean;
        FIsNull: boolean;

        function GetValue: boolean;
    public
        constructor Create(PValue: boolean);

        function ToString(): String;

        class operator Implicit(A: NullableBoolean): boolean;
        class operator Implicit(PValue: boolean): NullableBoolean;
        class operator Equal(A, B: NullableBoolean): boolean;
        class operator NotEqual(A, B: NullableBoolean): boolean;

        class function Null: NullableBoolean; static;

        property Value: boolean read GetValue;
        property IsNull: boolean read FIsNull;
        function IsNotNull: boolean;
    end;

implementation

{ NullableInt64 }

constructor NullableInt64.Create(PValue: int64);
begin
    FValue := PValue;
    FIsNull := False;
end;

class operator NullableInt64.Equal(A, B: NullableInt64): boolean;
begin
    if (A.IsNull <> B.IsNull) then
    begin
        Result := False;
    end
    else
    if (A.IsNull = B.IsNull) and (A.IsNull) then
    begin
        Result := True;
    end
    else
    if (A.IsNull = B.IsNull) and (not A.IsNull) then
    begin
        Result := (A.Value = B.Value);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableInt64.GetValue: int64;
begin
    if (FIsNull) then
        raise Exception.Create('Невозможно получить значение - оно равно Null')
    else
        Result := FValue;
end;

class operator NullableInt64.Implicit(A: NullableInt64): int64;
begin
    Result := A.Value;
end;

class operator NullableInt64.Implicit(PValue: int64): NullableInt64;
begin
    Result := NullableInt64.Create(PValue);
end;

class operator NullableInt64.NotEqual(A, B: NullableInt64): boolean;
begin
    Result := not (A = B);
end;

class function NullableInt64.Null: NullableInt64;
begin
    Result.FIsNull := True;
end;

function NullableInt64.IsNotNull: boolean;
begin
    Result := not IsNull;
end;

{ NullableInteger }

constructor NullableInteger.Create(PValue: integer);
begin
    FValue := PValue;
    FIsNull := False;
end;

class operator NullableInteger.Equal(A, B: NullableInteger): boolean;
begin
    if (A.IsNull <> B.IsNull) then
    begin
        Result := False;
    end
    else
    if (A.IsNull = B.IsNull) and (A.IsNull) then
    begin
        Result := True;
    end
    else
    if (A.IsNull = B.IsNull) and (not A.IsNull) then
    begin
        Result := (A.Value = B.Value);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableInteger.GetValue: integer;
begin
    if (FIsNull) then
        raise Exception.Create('Невозможно получить значение - оно равно Null')
    else
        Result := FValue;
end;

class operator NullableInteger.GreaterThan(A, B: NullableInteger): boolean;
begin
    Result := A.GreaterThen(B);
end;

class operator NullableInteger.GreaterThanOrEqual(A,
  B: NullableInteger): boolean;
begin
    Result := A.GreaterThenOrEqual(B);
end;

function NullableInteger.GreaterThen(Another: NullableInteger): boolean;
begin
    if ((Self.IsNull) and (not Another.IsNull)) or
        ((not Self.IsNull) and (Another.IsNull)) then
        raise Exception.Create('Нельзя сравнивать число с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
        Result := (Self.Value > Another.Value)
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableInteger.GreaterThenOrEqual(Another: NullableInteger): boolean;
begin
    if ((Self.IsNull) and (not Another.IsNull)) or
        ((not Self.IsNull) and (Another.IsNull)) then
        raise Exception.Create('Нельзя сравнивать число с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
        Result := (Self.Value >= Another.Value)
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

class operator NullableInteger.Implicit(A: NullableInteger): integer;
begin
    Result := A.Value;
end;

class operator NullableInteger.Implicit(PValue: integer): NullableInteger;
begin
    Result := NullableInteger.Create(PValue);
end;

class operator NullableInteger.LessThan(A, B: NullableInteger): boolean;
begin
    Result := A.LessThen(B);
end;

class operator NullableInteger.LessThanOrEqual(A, B: NullableInteger): boolean;
begin
    Result := A.LessThenOrEqual(B);
end;

function NullableInteger.LessThen(Another: NullableInteger): boolean;
begin
    if (Self.IsNull) and (not Another.IsNull) then
        raise Exception.Create('Нельзя сравнивать число ' + Another.ToString() + ' с Null');
    if (not Self.IsNull) and (Another.IsNull) then
        raise Exception.Create('Нельзя сравнивать число ' + Self.ToString() + ' с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
        Result := (Self.Value < Another.Value)
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableInteger.LessThenOrEqual(Another: NullableInteger): boolean;
begin
    if (Self.IsNull) and (not Another.IsNull) then
        raise Exception.Create('Нельзя сравнивать число ' + Another.ToString() + ' с Null');
    if (not Self.IsNull) and (Another.IsNull) then
        raise Exception.Create('Нельзя сравнивать число ' + Self.ToString() + ' с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
        Result := (Self.Value <= Another.Value)
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

class operator NullableInteger.NotEqual(A, B: NullableInteger): boolean;
begin
    Result := not (A = B);
end;

class function NullableInteger.Null: NullableInteger;
begin
    Result.FIsNull := True;
end;

function NullableInteger.ToString: String;
begin
    if FIsNull then
        Result := 'Null'
    else
        Result := IntToStr(FValue);
end;

function NullableInteger.IsNotNull: boolean;
begin
    Result := not IsNull;
end;

{ NullableDouble }

constructor NullableDouble.Create(PValue: double);
begin
    FValue := PValue;
    FIsNull := False;
end;

class operator NullableDouble.Equal(A, B: NullableDouble): boolean;
var
    Epsilon: double;
begin
    // пока точность "зашита"
    Epsilon := 0.0001;

    if (A.IsNull <> B.IsNull) then
    begin
        Result := False;
    end
    else
    if (A.IsNull = B.IsNull) and (A.IsNull) then
    begin
        Result := True;
    end
    else
    if (A.IsNull = B.IsNull) and (not A.IsNull) then
    begin
        Result := SameValue(A.Value, B.Value, Epsilon);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableDouble.Equals(OtherValue: NullableDouble; Epsilon: double): boolean;
begin
    if Self.IsNull and OtherValue.IsNull then
        Result := True
    else
    if ((not Self.IsNull) and (OtherValue.IsNull)) or
       ((Self.IsNull) and (not OtherValue.IsNull)) then
       Result := False
    else
        Result := SameValue(Self.Value, OtherValue.Value, Epsilon);
end;

class function NullableDouble.FromString(PValue: String): NullableDouble;
begin
    if (PValue = EmptyStr) or (PValue = NullStr)
        then Result := NullableDouble.Null
        else Result := StrToFloat(PValue);    
end;

function NullableDouble.GetValue: double;
begin
    if (FIsNull) then
        raise Exception.Create('Невозможно получить значение - оно равно Null')
    else
        Result := FValue;
end;

class operator NullableDouble.GreaterThan(A, B: NullableDouble): boolean;
begin
    Result := A.GreaterThen(B);
end;

class operator NullableDouble.GreaterThanOrEqual(A, B: NullableDouble): boolean;
begin
    Result := A.GreaterThenOrEqual(B);
end;

function NullableDouble.GreaterThen(Another: NullableDouble): boolean;
var
    cv: TValueRelationship;
begin
    if ((Self.IsNull) and (not Another.IsNull)) or
        ((not Self.IsNull) and (Another.IsNull)) then
        raise Exception.Create('Нельзя сравнивать число с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
    begin
        cv := CompareValue(Self.Value, Another.Value);
        Result := (cv = GreaterThanValue);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableDouble.GreaterThenOrEqual(Another: NullableDouble): boolean;
var
    cv: TValueRelationship;
begin
    if ((Self.IsNull) and (not Another.IsNull)) or
        ((not Self.IsNull) and (Another.IsNull)) then
        raise Exception.Create('Нельзя сравнивать число с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
    begin
        cv := CompareValue(Self.Value, Another.Value);
        Result := (cv = GreaterThanValue) or (cv = EqualsValue);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

class operator NullableDouble.Implicit(A: NullableDouble): double;
begin
    Result := A.Value;
end;

function NullableDouble.IfNull(DefaultValue: double): double;
begin
    if Self.IsNull then
        Result := DefaultValue
    else
        Result := Self.Value;    
end;

class operator NullableDouble.Implicit(PValue: double): NullableDouble;
begin
    Result := NullableDouble.Create(PValue);
end;

class operator NullableDouble.LessThan(A, B: NullableDouble): boolean;
begin
    Result := A.LessThen(B);
end;

class operator NullableDouble.LessThanOrEqual(A, B: NullableDouble): boolean;
begin
    Result := A.LessThenOrEqual(B);
end;

function NullableDouble.LessThen(Another: NullableDouble): boolean;
var
    cv: TValueRelationship;
begin
    if ((Self.IsNull) and (not Another.IsNull)) or
        ((not Self.IsNull) and (Another.IsNull)) then
        raise Exception.Create('Нельзя сравнивать число с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
    begin
        cv := CompareValue(Self.Value, Another.Value);
        Result := (cv = LessThanValue);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableDouble.LessThenOrEqual(Another: NullableDouble): boolean;
var
    cv: TValueRelationship;
begin
    if ((Self.IsNull) and (not Another.IsNull)) or
        ((not Self.IsNull) and (Another.IsNull)) then
        raise Exception.Create('Нельзя сравнивать число с Null');

    if (Self.IsNull) and (Another.IsNull) then
        Result := False
    else
    if (not Self.IsNull) and (not Another.IsNull) then
    begin
        cv := CompareValue(Self.Value, Another.Value);
        Result := (cv = LessThanValue) or (cv = EqualsValue);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

class operator NullableDouble.NotEqual(A, B: NullableDouble): boolean;
begin
    Result := not (A = B);
end;

class function NullableDouble.Null: NullableDouble;
begin
    Result.FIsNull := True;
end;

class function NullableDouble.NullIfNaN(Value: double): NullableDouble;
begin
    if Math.IsNAN(Value) then
        Result := NullableDouble.Null
    else
        Result := NullableDouble.Create(Value);
end;

function NullableDouble.ToString: String;
begin
    if FIsNull then
        Result := NullStr
    else
        Result := FloatToStr(FValue);
end;

function NullableDouble.IsNotNull: boolean;
begin
    Result := not IsNull;
end;

{ NullableString }

constructor NullableString.Create(PValue: String);
begin
    FValue := PValue;
    FIsNull := False;
end;

class operator NullableString.Equal(A, B: NullableString): boolean;
begin
    if (A.IsNull <> B.IsNull) then
    begin
        Result := False;
    end
    else
    if (A.IsNull = B.IsNull) and (A.IsNull) then
    begin
        Result := True;
    end
    else
    if (A.IsNull = B.IsNull) and (not A.IsNull) then
    begin
        Result := (A.Value = B.Value);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableString.GetValue: String;
begin
    if (FIsNull) then
        raise Exception.Create('Невозможно получить значение - оно равно Null')
    else
        Result := FValue;
end;

function NullableString.IsNotNull: boolean;
begin
    Result := not IsNull;
end;

class operator NullableString.Implicit(A: NullableString): String;
begin
    Result := A.Value;
end;

function NullableString.IfNull(DefaultValue: string): string;
begin
    if Self.IsNull then
        Result := DefaultValue
    else
        Result := Self.Value;
end;

class operator NullableString.Implicit(PValue: String): NullableString;
begin
    Result := NullableString.Create(PValue);
end;

class operator NullableString.NotEqual(A, B: NullableString): boolean;
begin
    Result := not (A = B);
end;

class function NullableString.Null: NullableString;
begin
    Result.FIsNull := True;
end;

function NullableString.ToString: String;
begin
    if (FIsNull) then
        Result := 'Null'
    else
        Result := FValue;
end;

{ NullableBoolean }

constructor NullableBoolean.Create(PValue: boolean);
begin
    FValue := PValue;
    FIsNull := False;
end;

class operator NullableBoolean.Equal(A, B: NullableBoolean): boolean;
begin
    if (A.IsNull <> B.IsNull) then
    begin
        Result := False;
    end
    else
    if (A.IsNull = B.IsNull) and (A.IsNull) then
    begin
        Result := True;
    end
    else
    if (A.IsNull = B.IsNull) and (not A.IsNull) then
    begin
        Result := (A.Value = B.Value);
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

function NullableBoolean.GetValue: boolean;
begin
    if (FIsNull) then
        raise Exception.Create('Невозможно получить значение - оно равно Null')
    else
        Result := FValue;
end;

class operator NullableBoolean.Implicit(A: NullableBoolean): boolean;
begin
    Result := A.Value;
end;

class operator NullableBoolean.Implicit(PValue: boolean): NullableBoolean;
begin
    Result := NullableBoolean.Create(PValue);
end;

class operator NullableBoolean.NotEqual(A, B: NullableBoolean): boolean;
begin
    Result := not (A = B);
end;

class function NullableBoolean.Null: NullableBoolean;
begin
    Result.FIsNull := True;
end;

function NullableBoolean.ToString: String;
begin
    if (FIsNull) then
        Result := 'Null'
    else
    if (FValue) then
        Result := 'True'
    else
    if (not FValue) then
        Result := 'False'
    else
        raise Exception.Create('Невозможно получить строковое представление NullableBoolean');
end;

function NullableBoolean.IsNotNull: boolean;
begin
    Result := not IsNull;
end;

{ NullableObject }

constructor NullableObject.Create(PValue: TObject);
begin
    FValue := PValue;
    FIsNull := False;
end;

class operator NullableObject.Equal(A, B: NullableObject): boolean;
begin
    if (A.IsNull <> B.IsNull) then
    begin
        Result := False;
    end
    else
    if (A.IsNull = B.IsNull) and (A.IsNull) then
    begin
        Result := True;
    end
    else
    if (A.IsNull = B.IsNull) and (not A.IsNull) then
    begin
        Result := (A.Value.Equals(B.Value));
    end
    else
        raise Exception.Create('Непредвиденный вариант сравнения');
end;

procedure NullableObject.Free;
begin
  FreeAndNil(FValue);
end;

function NullableObject.GetValue: TObject;
begin
    if (FIsNull) then
        raise Exception.Create('Невозможно получить значение - оно равно Null')
    else
        Result := FValue;
end;

class operator NullableObject.Implicit(A: NullableObject): TObject;
begin
    Result := A.Value;
end;

class operator NullableObject.Implicit(PValue: TObject): NullableObject;
begin
    Result := NullableObject.Create(PValue);
end;

function NullableObject.IsNotNull: boolean;
begin
    Result := not IsNull;
end;

class operator NullableObject.NotEqual(A, B: NullableObject): boolean;
begin
    Result := not (A = B);
end;

class function NullableObject.Null: NullableObject;
begin
    Result.FIsNull := True;
    Result.FValue := nil;
end;

end.
