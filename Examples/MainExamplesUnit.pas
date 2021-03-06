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
  AddressBookContactUnit, OutputUnit, ConnectionUnit, OrderUnit,
  CommonTypesUnit, AddOrderToRouteParameterProviderUnit, AddOrderToRouteRequestUnit,
  EnumsUnit, UserParameterProviderUnit, UserParametersUnit;

const
  //your api key
  c_ApiKey = '11111111111111111111111111111111';

class procedure TExamples.Run;
var
  Examples: TRoute4MeExamples;
  i: integer;
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
  RouteIdsToDelete: TListString;
  RouteIdsToMerge: TListString;
  Contact1, Contact2: TAddressBookContact;
  AddressIdsToRemove: TList<integer>;
  TerritoryId: NullableString;
  Order1, Order2: TOrder;
  OrderIdsToRemove: TList<integer>;
  Connection: TConnection;
  Routes: TDataObjectRouteList;
  OrderedAddresses: TOrderedAddressArray;
  ParametersProvider: IAddOrderToRouteParameterProvider;
  UserParameters: TUserParameters;
  AddNewUserParameterProvider: IUserParameterProvider;
  Parameters: TAddOrderToRouteRequest;
  RouteId: String;
  RouteDestinationId: integer;
  AddressId: NullableInteger;
  MemberId: integer;
  EMail: String;
  Limit, Offset: integer;
  SessionGuid: NullableString;
begin
  try
    Connection := TConnection.Create(c_ApiKey);
    Examples := TRoute4MeExamples.Create(TOutputConsole.Create, Connection);
    try
      try
        Randomize;

        AddNewUserParameterProvider := TUserParameterProvider.Create;
        EMail := 'marketing@kcswest.com';
        UserParameters := AddNewUserParameterProvider.GetParameters(EMail);
        try
          MemberId := Examples.AddNewUser(UserParameters);
          Examples.GetUserDetails(MemberId);

          UserParameters.MemberId := MemberId;
          UserParameters.FirstName := 'John';
          Examples.UpdateUser(UserParameters);

          SessionGuid := Examples.Authentication(UserParameters.Email, UserParameters.Password);
        finally
          FreeAndNil(UserParameters);
        end;

        if SessionGuid.IsNotNull then
          Examples.ValidateSession(SessionGuid, MemberId);
        Examples.GetUsers();
        Examples.RemoveUser(MemberId);

        Examples.BatchForwardGeocodeAddress('Los20%Angeles20%International20%Airport,20%CA');
        DataObject1 := Examples.SingleDriverRoute10Stops();

        if (DataObject1 <> nil) and (DataObject1.Routes <> nil) and (Length(DataObject1.Routes) > 0) then
        begin
          RouteSingleDriverRoute10Stops := DataObject1.Routes[0];
          RouteId_SingleDriverRoute10Stops := RouteSingleDriverRoute10Stops.RouteId;
          Examples.ResequenceRouteDestinations(RouteSingleDriverRoute10Stops);
          Examples.ResequenceAllRouteDestinations(RouteId_SingleDriverRoute10Stops);
        end
        else
        begin
          RouteSingleDriverRoute10Stops := nil;
          RouteId_SingleDriverRoute10Stops := NullableString.Null;
          WriteLn('ResequenceRouteDestinations, ResequenceAllRouteDestinations not called. ' +
            'RouteSingleDriverRoute10Stops = null.');
        end;

        if (RouteSingleDriverRoute10Stops <> nil) then
          Examples.ShareRoute(RouteSingleDriverRoute10Stops.RouteId, 'oooooo@gmail.com');

        DestinationIds := Examples.AddRouteDestinationsOptimally(RouteId_SingleDriverRoute10Stops);
        if (Length(DestinationIds) > 0) then
          Examples.RemoveRouteDestination(RouteId_SingleDriverRoute10Stops, DestinationIds[0]);

        DestinationIds := Examples.AddRouteDestinations(RouteId_SingleDriverRoute10Stops);
        if (Length(DestinationIds) > 0) then
          Examples.RemoveRouteDestination(RouteId_SingleDriverRoute10Stops, DestinationIds[0]);

        DataObject2 := Examples.SingleDriverRoundTrip();

        if (DataObject2 <> nil) and (DataObject2.Routes <> nil) and (Length(DataObject2.Routes) > 0) then
          RouteId_SingleDriverRoundTrip := DataObject2.Routes[0].RouteId
        else
          RouteId_SingleDriverRoundTrip := NullableString.Null;

        RouteIdToMoveTo := routeId_SingleDriverRoundTrip;

        if (DataObject1 <> nil) and (Length(DataObject1.Routes) > 0) and
          (Length(DataObject1.Routes[0].Addresses) > 1) and
          (DataObject1.Routes[0].Addresses[1].RouteDestinationId.IsNotNull) then
          RouteDestinationIdToMove := DataObject1.Routes[0].Addresses[1].RouteDestinationId.Value
        else
          RouteDestinationIdToMove := NullableInteger.Null;

        if (DataObject2 <> nil) and (Length(DataObject2.Routes) > 0) and
          (Length(DataObject2.Routes[0].Addresses) > 1) and
          (DataObject2.Routes[0].Addresses[0].RouteDestinationId.IsNotNull) then
          AfterDestinationIdToMoveAfter := DataObject2.Routes[0].Addresses[0].RouteDestinationId.Value
        else
          AfterDestinationIdToMoveAfter := NullableInteger.Null;

        if RouteIdToMoveTo.IsNotNull and RouteDestinationIdToMove.IsNotNull and
          AfterDestinationIdToMoveAfter.IsNotNull then
          Examples.MoveDestinationToRoute(RouteIdToMoveTo, RouteDestinationIdToMove, AfterDestinationIdToMoveAfter)
        else
          WriteLn(Format(
            'MoveDestinationToRoute not called. routeDestinationId = %d, afterDestinationId = %d.',
            [RouteDestinationIdToMove.Value, AfterDestinationIdToMoveAfter.Value]));

        OptimizationProblemId := Examples.SingleDriverRoundTripGeneric();

        DataObject := Examples.MultipleDepotMultipleDriver();
        try
          if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
            RouteId_MultipleDepotMultipleDriver := DataObject.Routes[0].RouteId
          else
            RouteId_MultipleDepotMultipleDriver := NullableString.Null;
        finally
          FreeAndNil(DataObject);
        end;

        DataObject := Examples.MultipleDepotMultipleDriverTimeWindow();
        try
          if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
            RouteId_MultipleDepotMultipleDriverTimeWindow := DataObject.Routes[0].RouteId
          else
            RouteId_MultipleDepotMultipleDriverTimeWindow := NullableString.Null;
        finally
          FreeAndNil(DataObject);
        end;

        DataObject := Examples.SingleDepotMultipleDriverNoTimeWindow();
        try
          if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
            RouteId_SingleDepotMultipleDriverNoTimeWindow := DataObject.Routes[0].RouteId
          else
            RouteId_SingleDepotMultipleDriverNoTimeWindow := NullableString.Null;
        finally
          FreeAndNil(DataObject);
        end;

        DataObject := Examples.MultipleDepotMultipleDriverWith24StopsTimeWindow();
        try
          if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
            RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow := DataObject.Routes[0].RouteId
          else
            RouteId_MultipleDepotMultipleDriverWith24StopsTimeWindow := NullableString.Null;
        finally
          FreeAndNil(DataObject);
        end;

        DataObject := Examples.SingleDriverMultipleTimeWindows();
        try
          if (DataObject <> nil) and (Length(DataObject.Routes) > 0) then
            RouteId_SingleDriverMultipleTimeWindows := DataObject.Routes[0].RouteId
          else
            RouteId_SingleDriverMultipleTimeWindows := NullableString.Null;
        finally
          FreeAndNil(DataObject);
        end;

        if (OptimizationProblemId.IsNotNull) then
          Examples.GetOptimization(OptimizationProblemId)
        else
          WriteLn('GetOptimization not called. OptimizationProblemID is null.');

        Examples.GetOptimizations();

        if (OptimizationProblemId.IsNotNull) then
          DataObject := Examples.AddDestinationToOptimization(OptimizationProblemId, True)
        else
        begin
            WriteLn('AddDestinationToOptimization not called. optimizationProblemID is null.');
            DataObject := nil;
        end;

        try
          if (OptimizationProblemId.IsNotNull) then
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
        finally
          FreeAndNil(DataObject);
        end;

        if (OptimizationProblemId.IsNotNull) then
          Examples.ReOptimization(OptimizationProblemId)
        else
          WriteLn('ReOptimization not called. OptimizationProblemID is null.');

        if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
        begin
          Examples.UpdateRoute(RouteId_SingleDriverRoute10Stops);

          Examples.UpdateRoutesCustomFields(RouteId_SingleDriverRoute10Stops,
              RouteDestinationIdToMove);

          Examples.ReoptimizeRoute(RouteId_SingleDriverRoute10Stops);
          GetRouteDirections := True;
          GetRoutePathPoints := True;
          Examples.GetRoute(RouteId_SingleDriverRoute10Stops, GetRouteDirections, GetRoutePathPoints);
        end
        else
            WriteLn('UpdateRoute, UpdateRoutesCustomFields, ReoptimizeRoute, GetRoute not called. ' +
              'routeId_SingleDriverRoute10Stops is null.');

        Limit := 10;
        Offset := 5;
        Routes := Examples.GetRoutes(Limit, Offset);
        try
          RouteIdsToMerge := TListString.Create;
          try
            if (Routes.Count > 0) then
              RouteIdsToMerge.Add(Routes[0].RouteId);
            if (Routes.Count > 1) then
              RouteIdsToMerge.Add(Routes[1].RouteId);

            if (RouteIdsToMerge.Count > 0) then
              Examples.MergeRoutes(RouteIdsToMerge)
            else
              WriteLn('RouteIdsToMerge.Count = 0. MergeRoutes not called.');
          finally
            FreeAndNil(RouteIdsToMerge);
          end;
        finally
          FreeAndNil(Routes);
        end;

        Examples.GetAreaAddedActivities;
        Examples.GetAreaUpdatedActivities;
        Examples.GetAreaRemovedActivities;
        Examples.GetDestinationDeletedActivities;
        Examples.GetDestinationOutOfSequenceActivities;
        Examples.GetDriverArrivedEarlyActivities;
        Examples.GetDriverArrivedLateActivities;
        Examples.GetDriverArrivedOnTimeActivities;
        Examples.GetGeofenceEnteredActivities;
        Examples.GetGeofenceLeftActivities;
        Examples.GetAllDestinationInsertedActivities;
        Examples.GetAllDestinationMarkedAsDepartedActivities;
        Examples.GetAllDestinationMarkedAsVisitedActivities;
        Examples.GetMemberCreatedActivities;
        Examples.GetMemberDeletedActivities;
        Examples.GetMemberModifiedActivities;
        Examples.GetDestinationMovedActivities;
        Examples.GetAllNoteInsertedActivities;
        Examples.GetRouteDeletedActivities;
        Examples.GetRouteOptimizedActivities;
        Examples.GetRouteOwnerChangedActivities;
        Examples.GetDestinationUpdatedActivities;

        Limit := 5;
        Offset := 0;
        Examples.GetAllActivities(Limit, Offset);

        if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
        begin
          Examples.LogSpecificMessage('Test User Activity ' + DateTimeToStr(Now), RouteId_SingleDriverRoute10Stops);
          Examples.GetTeamActivities(RouteId_SingleDriverRoute10Stops, Limit, Offset);
          Examples.GetDestinationInsertedActivities(RouteId_SingleDriverRoute10Stops);
          Examples.GetDestinationMarkedAsDepartedActivities(RouteId_SingleDriverRoute10Stops);
          Examples.GetNoteInsertedActivities(RouteId_SingleDriverRoute10Stops);
        end
        else
          WriteLn('LogCustomActivity not called. routeId_SingleDriverRoute10Stops is null.');

        Randomize;

        // Address Book
        Contact1 := Examples.CreateLocation('Test FirstName 1', 'Test Address 1');
        Contact2 := Examples.CreateLocation('Test FirstName 2', 'Test Address 2');
        try
          Examples.GetLocations();
          Examples.GetLocationsByIds([Contact1.Id, Contact2.Id]);
          Examples.GetLocation('FirstName');
          Examples.LocationSearch('FirstName', ['first_name', 'address_1']);
          Examples.DisplayRouted();
          if (Contact1 <> nil) then
          begin
            AddressId := Contact1.Id;
            Contact1.LastName := 'Updated ' + IntToStr(Random(100));
            Examples.UpdateLocation(Contact1);
          end
          else
            WriteLn('contact1 = null. UpdateAddressBookContact not called.');

          AddressIdsToRemove := TList<integer>.Create();
          try
            if (Contact1 <> nil) then
              AddressIdsToRemove.Add(Contact1.Id);
            if (Contact2 <> nil) then
              AddressIdsToRemove.Add(Contact2.Id);
            Examples.RemoveLocations(AddressIdsToRemove.ToArray());
          finally
            FreeAndNil(AddressIdsToRemove);
          end;
        finally
          FreeAndNil(Contact2);
          FreeAndNil(Contact1);
        end;


        // Addresses
        if (RouteIdToMoveTo.IsNotNull) and (RouteDestinationIdToMove <> 0) then
        begin
          RouteId := RouteIdToMoveTo;
          RouteDestinationId := RouteDestinationIdToMove;

          Examples.GetAddress(RouteId, RouteDestinationId);
          Examples.MarkAddressAsDetectedAsVisited(RouteId, RouteDestinationId, True);
          Examples.MarkAddressAsDetectedAsDeparted(RouteId, RouteDestinationId, True);
          if AddressId.IsNotNull then
          begin
            MemberId := 1;
            // todo: � ����� ������������ 0, � �� status=true/false. ����� MemberId ����� ������?
            Examples.MarkAddressAsVisited(RouteId, AddressId, MemberId, True);
            // todo: status ������������ False, ����� MemberId ����� ������?
            Examples.MarkAddressAsDeparted(RouteId, AddressId, MemberId, True);
          end;
          Examples.AddAddressNote(RouteId, RouteDestinationId);
          Examples.GetAddressNotes(RouteId, RouteDestinationId);
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
        //if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
        //begin
        //  ExamplesOld.SetGPSPosition(RouteId_SingleDriverRoute10Stops);
        //  ExamplesOld.TrackDeviceLastLocationHistory(RouteId_SingleDriverRoute10Stops);
        //end
        //else
        //  WriteLn('SetGPSPosition, TrackDeviceLastLocationHistory not called. routeId_SingleDriverRoute10Stops is null.');

        RouteIdsToDelete := TListString.Create();
        try
          if (RouteId_SingleDriverRoute10Stops.IsNotNull) then
            RouteIdsToDelete.Add(RouteId_SingleDriverRoute10Stops);
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
        finally
          FreeAndNil(RouteIdsToDelete);
        end;

        // Remove optimization
        if (OptimizationProblemId.IsNotNull) then
          Examples.RemoveOptimization(OptimizationProblemId)
        else
          WriteLn('RemoveOptimization not called. OptimizationProblemID is null.');


        // Avoidance Zones
        TerritoryId := Examples.AddCircleAvoidanceZone();
        TerritoryId := Examples.AddRectangularAvoidanceZone();
        TerritoryId := Examples.AddPolygonAvoidanceZone();

        Examples.GetAvoidanceZones();
        if (TerritoryId.IsNotNull) then
          Examples.GetAvoidanceZone(TerritoryId)
        else
          WriteLn('GetAvoidanceZone not called. territoryId is null.');

        if (TerritoryId.IsNotNull) then
          Examples.UpdateAvoidanceZone(TerritoryId)
        else
          WriteLn('UpdateAvoidanceZone not called. territoryId is null.');

        if (TerritoryId.IsNotNull) then
          Examples.DeleteAvoidanceZone(TerritoryId)
        else
          WriteLn('DeleteAvoidanceZone not called. territoryId is null.');

        // Territories
        TerritoryId := Examples.AddCircleTerritory();
        TerritoryId := Examples.AddRectangularTerritory();
        TerritoryId := Examples.AddPolygonTerritory();

        Examples.GetTerritories();
        if (TerritoryId.IsNotNull) then
          Examples.GetTerritory(TerritoryId)
        else
          WriteLn('GetTerritory not called. territoryId is null.');

        if (TerritoryId.IsNotNull) then
          Examples.UpdateTerritory(TerritoryId)
        else
          WriteLn('GetTerritory not called. territoryId is null.');

        if (TerritoryId.IsNotNull) then
          Examples.RemoveTerritory(TerritoryId)
        else
          WriteLn('RemoveTerritory not called. territoryId is null.');

        // Orders
        Order1 := Examples.AddOrder();
        Order2 := Examples.AddOrder();
        try
          // Get an order by order_id
          Examples.GetOrder(Order1.Id);

          // Get all the orders created under the specific Route4Me account.
          Examples.GetOrders();

          // Retrieve orders inserted on a specified date
          Examples.GetOrders(Now);

          // Retrieve orders scheduled for a specified date
          Examples.GetOrdersScheduledFor(Now);

          // Search for all order records which contain specified text in any field
          Examples.GetOrdersWithSpecifiedText('John');

          if (Order1 <> nil) then
          begin
            Order1.LastName := 'Updated ' + IntToStr(Random(100));
            Examples.UpdateOrder(Order1);
          end
          else
            WriteLn('Order1 == null. UpdateOrder not called.');

          // AddOrderToRoute sample
          DataObject := Examples.MultipleDepotMultipleDriver();
          try
            if (DataObject <> nil) then
            begin
              ParametersProvider := TAddOrderToRouteParameterProvider.Create;
              OrderedAddresses := ParametersProvider.GetAddresses;
              try
                OrderedAddresses[0].OrderId := Order1.Id;
                OrderedAddresses[1].OrderId := Order2.Id;

                // AddOrderToOptimization sample
                OptimizationProblemId := DataObject.OptimizationProblemId;
                Examples.AddOrderToOptimization(OptimizationProblemId,
                  DataObject.Parameters, OrderedAddresses);

                // AddOrderToRoute sample
                if (Length(DataObject.Routes) > 0) then
                begin
                  RouteId_MultipleDepotMultipleDriver := DataObject.Routes[0].RouteId;

                  Examples.AddOrderToRoute(RouteId_MultipleDepotMultipleDriver,
                    DataObject.Parameters, OrderedAddresses);
                end;
              finally
                for i := Length(OrderedAddresses) - 1 downto 0 do
                  FreeAndNil(OrderedAddresses[i]);
                ParametersProvider := nil;
              end;

              Examples.DeleteRoutes([RouteId_MultipleDepotMultipleDriver]);
            end;
          finally
            FreeAndNil(DataObject);
          end;

          OrderIdsToRemove := TList<integer>.Create();
          try
            if (Order1 <> nil) then
              OrderIdsToRemove.Add(Order1.Id);
            if (Order2 <> nil) then
              OrderIdsToRemove.Add(Order2.Id);
            Examples.RemoveOrders(OrderIdsToRemove.ToArray());
          finally
            FreeAndNil(OrderIdsToRemove);
          end;
        finally
          FreeAndNil(Order2);
          FreeAndNil(Order1);
        end;

        Examples.GenericExample(Connection);
        Examples.GenericExampleShortcut(Connection);
      except
        on E: Exception do
          Writeln(E.ClassName, ': ', E.Message);
      end;
    finally
      FreeAndNil(Examples);
    end;
  finally
    WriteLn('');
    WriteLn('Press any key');

    ReadLn;
  end;
end;

end.
