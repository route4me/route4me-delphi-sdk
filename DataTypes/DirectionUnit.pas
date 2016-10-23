unit DirectionUnit;

interface

uses
  REST.Json.Types, SysUtils, System.Generics.Collections,
  Generics.Defaults,
  DirectionLocationUnit, DirectionStepUnit;

type

  TDirection = class
  private
    [JSONName('location')]
    FLocation: TDirectionLocation;

    [JSONName('steps')]
    FSteps: TArray<TDirectionStep>;
  public
    constructor Create;

    property Location: TDirectionLocation read FLocation write FLocation;
    property Steps: TArray<TDirectionStep> read FSteps write FSteps;
  end;

  TDirectionArray = TArray<TDirection>;

function SortDirections(Directions: TDirectionArray): TDirectionArray;

implementation

function SortDirections(Directions: TDirectionArray): TDirectionArray;
begin
  SetLength(Result, Length(Directions));
  TArray.Copy<TDirection>(Directions, Result, Length(Directions));
  TArray.Sort<TDirection>(Result, TComparer<TDirection>.Construct(
    function (const Direction1, Direction2: TDirection): Integer
    begin
      Result := Direction1.Location.Compare(Direction2.Location);
    end));
end;

{ TDirection }

constructor TDirection.Create;
begin
  FLocation := nil;
  SetLength(FSteps, 0);
end;

end.
