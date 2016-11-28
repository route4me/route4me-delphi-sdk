unit TestOnlineExamplesUnit;

interface

uses
  TestFramework, SysUtils, ConnectionUnit, Route4MeManagerUnit, IConnectionUnit;

type
  TTestOnlineExamples = class(TTestCase)
  private
    const c_ApiKey = '11111111111111111111111111111111';
  protected
    FRoute4MeManager: TRoute4MeManager;
    FConnection: IConnection;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

{ TTestOnlineExamples }

procedure TTestOnlineExamples.SetUp;
begin
  inherited;

  FConnection := TConnection.Create(c_ApiKey);
  FRoute4MeManager := TRoute4MeManager.Create(FConnection);
end;

procedure TTestOnlineExamples.TearDown;
begin
  FreeAndNil(FRoute4MeManager);

  inherited;
end;

end.
