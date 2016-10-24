unit MainExamplesUnit;

interface

uses
  Classes, SysUtils;

type
  TExamples = class
  public
    class procedure Run;
  end;

implementation

uses
  System.Generics.Collections,
  DataObjectUnit, Route4MeExamplesUnit, NullableBasicTypesUnit, AddressUnit,
  AddressBookContactUnit;

class procedure TExamples.Run;
var
  Examples: TRoute4MeExamples;
  DataObject, DataObject1, DataObject2: TDataObject;
  RouteSingleDriverRoute10Stops: TDataObjectRoute;
  RouteId_SingleDriverRoute10Stops: NullableString;
  RouteId_SingleDriverRoundTrip: NullableString;
  RouteIdToMoveTo: NullableString;
  DestinationIds: TArray<integer>;
  RouteDestinationIdToMove, AfterDestinationIdToMoveAfter: NullableInteger;
  OptimizationProblemId: NullableString;
  RouteId_MultipleDepotMultipleDriver: NullableString;
  RouteId_MultipleDepotMultipleDriverTimeWindow: NullableString;
  RouteId_SingleDepotMultipleDriverNoTimeWindow: NullableString;
  RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow: NullableString;
  RouteId_SingleDriverMultipleTimeWindows: NullableString;
  DestinationToRemove: TAddress;
  GetRouteDirections, GetRoutePathPoints: boolean;
  RouteId_DuplicateRoute: NullableString;
  RouteIdsToDelete: TList<String>;
  Contact1, Contact2: TAddressBookContact;
  AddressIdsToRemove: TList<String>;
  TerritoryId: NullableString;
//  Order1, Order2: TOrder;
  OrderIdsToRemove: TList<String>;
begin
  try
    Examples := TRoute4MeExamples.Create();
//    Examples := TRoute4MeExamples.CreateDebug();

    DataObject := Examples.SingleDriverRoute10Stops();
    DataObject1 := DataObject;

    if (DataObject <> nil) and (DataObject.Routes <> nil) and (Length(DataObject.Routes) > 0) then
    begin
      RouteSingleDriverRoute10Stops := DataObject.Routes[0];
      RouteId_SingleDriverRoute10Stops := RouteSingleDriverRoute10Stops.RouteId;
      Examples.ResequenceRouteDestinations(RouteSingleDriverRoute10Stops);
    end
    else
    begin
      RouteSingleDriverRoute10Stops := nil;
      RouteId_SingleDriverRoute10Stops := NullableString.Null;
      WriteLn('ResequenceRouteDestinations not called. RouteSingleDriverRoute10Stops = null.');
    end;

    DestinationIds := Examples.AddRouteDestinations(RouteId_SingleDriverRoute10Stops);
    if (Length(DestinationIds) > 0) then
      Examples.RemoveRouteDestination(RouteId_SingleDriverRoute10Stops, DestinationIds[0]);

    DataObject := Examples.SingleDriverRoundTrip();
    DataObject2 := DataObject;

    if (DataObject <> nil) and (DataObject.Routes <> nil) and (Length(DataObject.Routes) > 0) then
    begin
      RouteId_SingleDriverRoundTrip := DataObject.Routes[0].RouteId;
      Examples.ResequenceRouteDestinations(RouteSingleDriverRoute10Stops);
    end
    else
    begin
      RouteId_SingleDriverRoundTrip := NullableString.Null;
      WriteLn('ResequenceRouteDestinations not called. RouteSingleDriverRoute10Stops = null.');
    end;

    routeIdToMoveTo := routeId_SingleDriverRoundTrip;

    if (DataObject1 <> nil) and (Length(DataObject1.Routes) > 0) and
      (Length(DataObject1.Routes[0].Addresses) > 1) and
      (DataObject1.Routes[0].Addresses[1].RouteDestinationId.IsNotNull) then
      routeDestinationIdToMove := dataObject1.Routes[0].Addresses[1].RouteDestinationId.Value
    else
      routeDestinationIdToMove := NullableInteger.Null;

    if (DataObject2 <> nil) and (Length(DataObject2.Routes) > 0) and
      (Length(DataObject2.Routes[0].Addresses) > 1) and
      (DataObject1.Routes[0].Addresses[0].RouteDestinationId.IsNotNull) then
      AfterDestinationIdToMoveAfter := dataObject2.Routes[0].Addresses[0].RouteDestinationId.Value
    else
      AfterDestinationIdToMoveAfter := NullableInteger.Null;

    if routeIdToMoveTo.IsNotNull and routeDestinationIdToMove.IsNotNull and
      AfterDestinationIdToMoveAfter.IsNotNull then
      Examples.MoveDestinationToRoute(RouteIdToMoveTo, RouteDestinationIdToMove, AfterDestinationIdToMoveAfter)
    else
      WriteLn(Format(
        'MoveDestinationToRoute not called. routeDestinationId = %d, afterDestinationId = %d.',
        [routeDestinationIdToMove.Value, AfterDestinationIdToMoveAfter.Value]));
    ReadLn; exit;

    OptimizationProblemId := Examples.SingleDriverRoundTripGeneric();

    DataObject := Examples.MultipleDepotMultipleDriver();
    if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
      RouteId_MultipleDepotMultipleDriver := DataObject.Routes[0].RouteId
    else
      RouteId_MultipleDepotMultipleDriver := NullableString.Null;

    DataObject := Examples.MultipleDepotMultipleDriverTimeWindow();
    if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
      RouteId_MultipleDepotMultipleDriverTimeWindow := dataObject.Routes[0].RouteId
    else
      RouteId_MultipleDepotMultipleDriverTimeWindow := NullableString.Null;

    DataObject := Examples.SingleDepotMultipleDriverNoTimeWindow();
    if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
      RouteId_SingleDepotMultipleDriverNoTimeWindow := DataObject.Routes[0].RouteId
    else
      RouteId_SingleDepotMultipleDriverNoTimeWindow := NullableString.Null;

    DataObject := Examples.MultipleDepotMultipleDriverWith24StopsTimeWindow();
    if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
      RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow := DataObject.Routes[0].RouteId
    else
      RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow := NullableString.Null;

    DataObject := Examples.SingleDriverMultipleTimeWindows();
    if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
      RouteId_SingleDriverMultipleTimeWindows := DataObject.Routes[0].RouteId
    else
      RouteId_SingleDriverMultipleTimeWindows := NullableString.Null;

    if (optimizationProblemId.IsNotNull) then
      Examples.GetOptimization(optimizationProblemId)
    else
      WriteLn('GetOptimization not called. OptimizationProblemID is null.');

    examples.GetOptimizations();

    if (optimizationProblemId.IsNotNull) then
      DataObject := Examples.AddDestinationToOptimization(OptimizationProblemId, True)
    else
    begin
        WriteLn('AddDestinationToOptimization not called. optimizationProblemID is null.');
        DataObject := nil;
    end;

    if (optimizationProblemId.IsNotNull) then
    begin
        if (DataObject <> nil) and (Length(DataObject.Addresses) > 0) then
        begin
          DestinationToRemove := DataObject.Addresses[High(DataObject.Addresses)];
          Examples.RemoveDestinationFromOptimization(OptimizationProblemId,
            DestinationToRemove.RouteDestinationId, False);
        end
        else
          WriteLn('RemoveDestinationFromOptimization not called. DestinationToRemove is null.');
    end
    else
      WriteLn('RemoveDestinationFromOptimization not called. OptimizationProblemID is null.');

    if (OptimizationProblemId.IsNotNull) then
      Examples.ReOptimization(OptimizationProblemId)
    else
      WriteLn('ReOptimization not called. OptimizationProblemID is null.');

    if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
    begin
      Examples.UpdateRoute(RouteId_SingleDriverRoute10Stops);
      Examples.ReoptimizeRoute(RouteId_SingleDriverRoute10Stops);
      GetRouteDirections := True;
      GetRoutePathPoints := True;
      Examples.GetRoute(RouteId_SingleDriverRoute10Stops, GetRouteDirections, GetRoutePathPoints);
    end
    else
        WriteLn('UpdateRoute, ReoptimizeRoute, GetRoute not called. routeId_SingleDriverRoute10Stops is null.');

    examples.GetRoutes();
    examples.GetUsers();

    if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
      Examples.LogCustomActivity('Test User Activity ' + DateTimeToStr(Now), routeId_SingleDriverRoute10Stops)
    else
      WriteLn('LogCustomActivity not called. routeId_SingleDriverRoute10Stops is null.');

    if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
      Examples.GetActivities(RouteId_SingleDriverRoute10Stops)
    else
      WriteLn('GetActivities not called. routeId_SingleDriverRoute10Stops is null.');

    if (routeIdToMoveTo.IsNotNull) and (RouteDestinationIdToMove <> 0) then
    begin
        examples.GetAddress(routeIdToMoveTo, routeDestinationIdToMove);
        examples.AddAddressNote(routeIdToMoveTo, routeDestinationIdToMove);
        examples.GetAddressNotes(routeIdToMoveTo, routeDestinationIdToMove);
    end
    else
      WriteLn('AddAddressNote, GetAddress, GetAddressNotes not called. routeIdToMoveTo == null || routeDestinationIdToMove == 0.');

    RouteId_DuplicateRoute := NullableString.Null;
    if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
      RouteId_DuplicateRoute := Examples.DuplicateRoute(RouteId_SingleDriverRoute10Stops)
    else
      WriteLn('DuplicateRoute not called. routeId_SingleDriverRoute10Stops is null.');

    //disabled by default, not necessary for optimization tests
    //not all accounts are capable of storing gps data
(*      if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
    begin
      Examples.SetGPSPosition(routeId_SingleDriverRoute10Stops);
      Examples.TrackDeviceLastLocationHistory(routeId_SingleDriverRoute10Stops);
    end
    else
      WriteLn('SetGPSPosition, TrackDeviceLastLocationHistory not called. routeId_SingleDriverRoute10Stops is null.');
    }*)

    RouteIdsToDelete := TList<String>.Create();
    if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
      RouteIdsToDelete.Add(routeId_SingleDriverRoute10Stops);
    if (RouteId_SingleDriverRoundTrip.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_SingleDriverRoundTrip);
    if (RouteId_DuplicateRoute.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_DuplicateRoute);
    if (RouteId_MultipleDepotMultipleDriver.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_MultipleDepotMultipleDriver);
    if (RouteId_MultipleDepotMultipleDriverTimeWindow.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_MultipleDepotMultipleDriverTimeWindow);
    if (RouteId_SingleDepotMultipleDriverNoTimeWindow.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_SingleDepotMultipleDriverNoTimeWindow);
    if (RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow);
    if (RouteId_SingleDriverMultipleTimeWindows.IsNotNull) then
      RouteIdsToDelete.Add(RouteId_SingleDriverMultipleTimeWindows);

    if (RouteIdsToDelete.Count > 0) then
      Examples.DeleteRoutes(RouteIdsToDelete.ToArray())
    else
      WriteLn('RouteIdsToDelete.Count = 0. DeleteRoutes not called.');
    RouteIdsToDelete.Free;

    // Remove optimization
    if (OptimizationProblemId.IsNotNull) then
      examples.RemoveOptimization(optimizationProblemId)
    else
      WriteLn('RemoveOptimization not called. optimizationProblemID is null.');

    Randomize;

    // Address Book
    Contact1 := Examples.AddAddressBookContact();
    Contact2 := Examples.AddAddressBookContact();
    Examples.GetAddressBookContacts();
    if (Contact1 <> nil) then
    begin
      Contact1.LastName := 'Updated ' + IntToStr(Random(100));
      Examples.UpdateAddressBookContact(Contact1);
    end
    else
      WriteLn('contact1 = null. UpdateAddressBookContact not called.');

    AddressIdsToRemove := TList<String>.Create();
    if (Contact1 <> nil) then
      AddressIdsToRemove.Add(Contact1.Id);
    if (Contact2 <> nil) then
      AddressIdsToRemove.Add(Contact2.Id);
    Examples.RemoveAddressBookContacts(AddressIdsToRemove.ToArray());
    AddressIdsToRemove.Free;

    // Avoidance Zones
    TerritoryId := Examples.AddAvoidanceZone();
    examples.GetAvoidanceZones();
    if (territoryId.IsNotNull) then
      Examples.GetAvoidanceZone(territoryId)
    else
      WriteLn('GetAvoidanceZone not called. territoryId is null.');

    if (territoryId.IsNotNull) then
      Examples.UpdateAvoidanceZone(territoryId)
    else
      WriteLn('UpdateAvoidanceZone not called. territoryId is null.');

    if (territoryId.IsNotNull) then
      examples.DeleteAvoidanceZone(territoryId)
    else
      WriteLn('DeleteAvoidanceZone not called. territoryId is null.');


(* TODO: Orders
    // Orders
    Order1 := Examples.AddOrder();
    Order2 := examples.AddOrder();
    Examples.GetOrders();
    if (Order1 <> nil) then
    begin
      Order1.LastName := 'Updated ' + IntToStr(Random(100));
      examples.UpdateOrder(Order1);
    end
    else
      WriteLn('Order1 == null. UpdateOrder not called.');

    OrderIdsToRemove := TList<String>.Create();
    if (Order1 <> nil) then
      OrderIdsToRemove.Add(Order1.OrderId);
    if (Order2 <> nil) then
      OrderIdsToRemove.Add(Order2.OrderId);
    Examples.RemoveOrders(orderIdsToRemove.ToArray());
    OrderIdsToRemove.Free();
*)

    examples.GenericExample();
    examples.GenericExampleShortcut();

    WriteLn('');
    WriteLn('Press any key');

    ReadLn;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end;

end.
