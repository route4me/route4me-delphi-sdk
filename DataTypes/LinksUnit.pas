unit LinksUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  /// <summary>
  ///  Links
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Links.dtd
  /// </remarks>
  TLinks = class
  private
    [JSONName('route')]
    [Nullable]
    FRoute: NullableString;

    [JSONName('view')]
    [Nullable]
    FView: NullableString;

    [JSONName('optimization_problem_id')]
    [Nullable]
    FOptimizationProblemId: NullableString;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    property Route: NullableString read FRoute write FRoute;
    property View: NullableString read FView write FView;
    property OptimizationProblemId: NullableString read FOptimizationProblemId write FOptimizationProblemId;
  end;

implementation

{ TLinks }

constructor TLinks.Create;
begin
  FRoute := NullableString.Null;
  FView := NullableString.Null;
  FOptimizationProblemId := NullableString.Null;
end;

function TLinks.Equals(Obj: TObject): Boolean;
var
  Other: TLinks;
begin
  Result := False;

  if not (Obj is TLinks) then
    Exit;

  Other := TLinks(Obj);

  Result :=
    (FRoute = Other.FRoute) and
    (FView = Other.FView) and
    (FOptimizationProblemId = Other.FOptimizationProblemId);
end;

end.
