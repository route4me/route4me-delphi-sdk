unit OutputUnit;

interface

type
  IOutput = interface
    ['{79A064C7-E83F-4A6B-A948-37A3E4E02EA4}']
    procedure Writeln(Message: String);
  end;

  TOutputConsole = class(TInterfacedObject, IOutput)
  public
    procedure Writeln(Message: String);
  end;

  TOutputDummy = class(TInterfacedObject, IOutput)
  public
    procedure Writeln(Message: String);
  end;

implementation

{ TOutputDummy }

procedure TOutputDummy.Writeln(Message: String);
begin

end;

{ TOutputConsole }

procedure TOutputConsole.Writeln(Message: String);
begin
  System.Writeln(Message);
end;

end.
