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
    FTotalDistance: NullableDouble;

    [JSONName('matched_time')]
    FMatchedTime: NullableInteger;

    [JSONName('matched_distance')]
    FMatchedDistance: NullableDouble;

    [JSONName('total_time')]
    FTotalTime: NullableInteger;

    [JSONName('trace_time')]
    FTraceTime: NullableInteger;

    [JSONName('trace_distance')]
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
  public
    constructor Create; override;
    destructor Destroy; override;

    property TrackingHistories: TArray<TTrackingHistory> read FTrackingHistories;
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
