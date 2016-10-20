unit DataObjectUnit;

interface

uses
  System.Generics.Collections,
  JSONNullableAttributeUnit,
  EnumsUnit, AddressUnit;

type
  TDataObject = class
  private
    FOptimizationProblemId: String;
    FState: TOptimizationState;
    FUserErrors: TList<String>;
    FIsSentToBackground: boolean;
    FAddresses: TArray<TAddress>;
  public
//    [DataMember(Name = 'optimization_problem_id')]
    property OptimizationProblemId: String read FOptimizationProblemId write FOptimizationProblemId;

//    [DataMember(Name = 'state')]
    property State: TOptimizationState read FState write FState;

//    [DataMember(Name = 'user_errors')]
    property UserErrors: TList<String> read FUserErrors write FUserErrors;

//    [DataMember(Name = 'sent_to_background')]
    property IsSentToBackground: boolean read FIsSentToBackground write FIsSentToBackground;

//    [DataMember(Name = 'addresses')]
    property Addresses: TArray<TAddress> read FAddresses write FAddresses;

(*    [DataMember(Name = 'parameters')]
    property RouteParameters Parameters { get; set; }

    [DataMember(Name = 'routes')]
    property DataObjectRoute[] Routes { get; set; }

    [DataMember(Name = 'links')]
    property Links Links { get; set; }

    [DataMember(Name = 'tracking_history')]
    property TrackingHistory[] TrackingHistory { get; set; }

    [DataMember(Name = 'directions')]
    property Direction[] Directions { get; set; }

    [DataMember(Name = 'path')]
    property DirectionPathPoint[] Path { get; set; }   *)
  end;

implementation

end.
