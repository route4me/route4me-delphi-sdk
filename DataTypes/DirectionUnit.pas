unit DirectionUnit;

interface

uses
  REST.Json.Types, SysUtils, System.Generics.Collections,
  Generics.Defaults, JSONNullableAttributeUnit,
  DirectionLocationUnit, DirectionStepUnit, NullableBasicTypesUnit;

type
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Direction.dtd
  /// </remarks>
  TDirection = class
  private
    [JSONName('location')]
    [NullableObject(TDirectionLocation)]
    FLocation: NullableObject;

    [JSONName('steps')]
    [NullableObject(TDirectionStepListClass)]
    FSteps: NullableObject;

    function GetLocation: TDirectionLocation;
    procedure SetLocation(const Value: TDirectionLocation);
    function GetSteps: TDirectionStepList;
    procedure SetSteps(const Value: TDirectionStepList);
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    property Location: TDirectionLocation read GetLocation write SetLocation;

    property Steps: TDirectionStepList read GetSteps write SetSteps;
  end;

  TDirectionArray = TArray<TDirection>;
  TDirectionList = TList<TDirection>;
  TDirectionListClass = class(TDirectionList);

function SortDirections(Directions: TDirectionArray): TDirectionArray;

implementation

function SortDirections(Directions: TDirectionArray): TDirectionArray;
begin
  SetLength(Result, Length(Directions));
  TArray.Copy<TDirection>(Directions, Result, Length(Directions));
  TArray.Sort<TDirection>(Result, TComparer<TDirection>.Construct(
    function (const Direction1, Direction2: TDirection): Integer
    begin
      Result := Direction1.Location.CompareTo(Direction2.Location);
    end));
end;

{ TDirection }

constructor TDirection.Create;
begin
  FLocation := NullableObject.Null;
  FSteps := NullableObject.Null;
end;

destructor TDirection.Destroy;
begin
  FLocation.Free;
  FSteps.Free;

  inherited;
end;

function TDirection.Equals(Obj: TObject): Boolean;
var
  Other: TDirection;
  i: integer;
  SortedSteps1, SortedSteps2: TDirectionStepArray;
begin
  Result := False;

  if not (Obj is TDirection) then
    Exit;

  Other := TDirection(Obj);

  if (FSteps.IsNull and Other.FSteps.IsNotNull) or
    (FSteps.IsNotNull and Other.FSteps.IsNull) or
    (FLocation.IsNull and Other.FLocation.IsNotNull) or
    (FLocation.IsNotNull and Other.FLocation.IsNull) then
    Exit;

  if (Location <> nil) and (Location <> Other.Location) then
    Exit;

  if (Steps <> nil) then
  begin
    if (Steps.Count <> Other.Steps.Count) then
      Exit;

    SortedSteps1 := DirectionStepUnit.SortDirectionSteps(Steps.ToArray);
    SortedSteps2 := DirectionStepUnit.SortDirectionSteps(Other.Steps.ToArray);
    for i := 0 to Length(SortedSteps1) - 1 do
      if (not SortedSteps1[i].Equals(SortedSteps2[i])) then
        Exit;
  end;

  Result := True;
end;

function TDirection.GetLocation: TDirectionLocation;
begin
  if FLocation.IsNull then
    Result := nil
  else
    Result := FLocation.Value as TDirectionLocation;
end;

function TDirection.GetSteps: TDirectionStepList;
begin
  if FSteps.IsNull then
    Result := nil
  else
    Result := FSteps.Value as TDirectionStepList;
end;

procedure TDirection.SetLocation(const Value: TDirectionLocation);
begin
  FLocation := Value;
end;

procedure TDirection.SetSteps(const Value: TDirectionStepList);
begin
  FSteps := Value;
end;

end.
