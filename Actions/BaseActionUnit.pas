unit BaseActionUnit;

interface

uses
  IConnectionUnit;

type
  TBaseAction = class abstract
  protected
    FConnection: IConnection;
  public
    constructor Create(Connection: IConnection);
    destructor Destroy; override;
  end;

implementation

{ TBaseAction }

constructor TBaseAction.Create(Connection: IConnection);
begin
  Inherited Create;

  FConnection := Connection;
end;

destructor TBaseAction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

end.
