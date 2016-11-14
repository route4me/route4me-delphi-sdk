unit BaseExampleUnit;

interface

uses
  OutputUnit, IRoute4MeManagerUnit, IConnectionUnit;

type
  TBaseExample = class abstract
  private
    FOutput: IOutput;
  protected
    Route4MeManager: IRoute4MeManager;

    procedure WriteLn(Message: String);
  public
    destructor Destroy; override;

    procedure Init(Output: IOutput; Connection: IConnection);
  end;

  TBaseExampleClass = class of TBaseExample;

implementation

{ TBaseExample }

uses Route4MeManagerUnit;

destructor TBaseExample.Destroy;
begin
  FOutput := nil;
  Route4MeManager := nil;

  inherited;
end;

procedure TBaseExample.Init(Output: IOutput; Connection: IConnection);
begin
  FOutput := Output;

  // Create the manager with the api key
  Route4MeManager := TRoute4MeManager.Create(Connection);
end;

procedure TBaseExample.WriteLn(Message: String);
begin
  FOutput.Writeln(Message);
end;

end.
