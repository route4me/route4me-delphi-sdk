unit DirectionUnit;

interface

uses
  REST.Json.Types, DirectionLocationUnit, DirectionStepUnit;

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

implementation

{ TDirection }

constructor TDirection.Create;
begin
  FLocation := nil;
  SetLength(FSteps, 0);
end;

end.
