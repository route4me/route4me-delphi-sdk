unit TrackingHistoryResponseUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  GenericParametersUnit, NullableBasicTypesUnit, JSONNullableAttributeUnit,
  CommonTypesUnit, TrackingHistoryUnit;

type
  TTrackingHistorySummary = class(TGenericParameters)
  private
    [JSONName('total_distance')]
    [Nullable]
    FTotalDistance: NullableDouble;

    [JSONName('matched_time')]
    [Nullable]
    FMatchedTime: NullableInteger;

    [JSONName('matched_distance')]
    [Nullable]
    FMatchedDistance: NullableDouble;

    [JSONName('total_time')]
    [Nullable]
    FTotalTime: NullableInteger;

    [JSONName('trace_time')]
    [Nullable]
    FTraceTime: NullableInteger;

    [JSONName('trace_distance')]
    [Nullable]
    FTraceDistance: NullableDouble;
  public
    constructor Create; override;

    property TotalDistance: NullableDouble read FTotalDistance write FTotalDistance;
    property MatchedTime: NullableInteger read FMatchedTime write FMatchedTime;
    property MatchedDistance: NullableDouble read FMatchedDistance write FMatchedDistance;
    property TotalTime: NullableInteger read FTotalTime write FTotalTime;
    property TraceTime: NullableInteger read FTraceTime write FTraceTime;
    property TraceDistance: NullableDouble read FTraceDistance write FTraceDistance;
  end;

  TMmd = class(TGenericParameters)
  private
    [JSONName('status')]
    [Nullable]
    FStatus: NullableInteger;

    [JSONName('summary')]
    [NullableObject(TTrackingHistorySummary)]
    FSummary: NullableObject;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Status: NullableInteger read FStatus write FStatus;
    property Summary: NullableObject read FSummary write FSummary;
  end;

  TTrackingHistoryResponse = class(TGenericParameters)
  private
    [JSONName('data')]
    [NullableArray(TTrackingHistory)]
    FTrackingHistories: TTrackingHistoryArray;

    [JSONName('mmd')]
    [NullableObject(TMmd)]
    FMmd: NullableObject;

    function GetMmd: TMmd;
  public
    constructor Create; override;
    destructor Destroy; override;

    property TrackingHistories: TArray<TTrackingHistory> read FTrackingHistories;
    property Mmd: TMmd read GetMmd;
  end;

implementation

constructor TTrackingHistoryResponse.Create;
begin
  inherited;

  SetLength(FTrackingHistories, 0);
  FMmd := NullableObject.Null;
end;

destructor TTrackingHistoryResponse.Destroy;
begin
  FMmd.Free;

  inherited;
end;

function TTrackingHistoryResponse.GetMmd: TMmd;
begin
  if (FMmd.IsNotNull) then
    Result := FMmd.Value as TMmd
  else
    Result := nil;
end;

{ TTrackingHistorySummary }

constructor TTrackingHistorySummary.Create;
begin
  inherited;

  FTotalDistance := NullableDouble.Null;
  FMatchedTime := NullableInteger.Null;
  FMatchedDistance := NullableDouble.Null;
  FTotalTime := NullableInteger.Null;
  FTraceTime := NullableInteger.Null;
  FTraceDistance := NullableDouble.Null;
end;

{ TMmd }

constructor TMmd.Create;
begin
  inherited;

  Status := NullableInteger.Null;
  FSummary := NullableObject.Null;
end;

destructor TMmd.Destroy;
begin
  FSummary.Free;

  inherited;
end;

end.
