unit BaseRoute4MeTestUnit;

interface

uses
  TestFramework, IRoute4MeManagerUnit;

type
  TBaseRoute4MeTest = class(TTestCase)
  private
    //your api key
    const c_ApiKey = '11111111111111111111111111111111';
  protected
    FRoute4MeManager: IRoute4MeManager;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

{ TBaseRoute4MeTest }

uses Route4MeManagerUnit;

procedure TBaseRoute4MeTest.SetUp;
begin
  inherited;

  // Create the manager with the api key
  FRoute4MeManager := TRoute4MeManager.Create(c_ApiKey);
end;

procedure TBaseRoute4MeTest.TearDown;
begin
  FRoute4MeManager := nil;

  inherited;
end;

end.
