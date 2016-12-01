/*
 * Ankur Goswami (agoswam3@ucsc.edu)
 * Morgan Grant (mlgrant@ucsc.edu)
 */

/* Degrees, minutes to radians */

dtor(degmin( D, M ), R) :-
   Pi is pi,
   R is (M / 60 + D) * Pi / 180.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

haversine_degmin( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   dtor(Lat1, Lat1_rad), dtor(Lat2, Lat2_rad),
   dtor(Lon1, Lon1_rad), dtor(Lon2, Lon2_rad),
   haversine_radians(Lat1_rad, Lon1_rad, Lat2_rad, Lon2_rad, Distance).

fly( From, To ) :-
  (From = To ->
      display('no zero-flight queries allowed'), nl,
      fail; true),
  (\+ airport( From, _, _, _ ) ->
      format('airport not found: ~a~n', [From]),
      fail; true),
  (\+ airport( To, _, _, _ ) ->
      format('airport not found: ~a~n', [To]),
      fail; true),
   fly_recur( From, To, time(0, 0), Flight_list ),
   nl,
   print_flight_list( Flight_list ).

fly_recur( From, To, Time, Flight_list ) :-
   gt_time( time(24, 0), Time ),
   flight( From, To, Depart_time),
   gt_time( Depart_time, Time ),
   arrive_time( To, From, Flight_length ),
   add_time( Depart_time, Flight_length, Landing_time ),
   Flight_list = [ flight_data( From, To, Depart_time, Landing_time ) ].

fly_recur( From, To, Time, Flight_list ) :-
   gt_time( time(24, 0), Time),
   flight( From, Stop, Depart_time),
   gt_time( Depart_time, Time ),
   arrive_time( Stop, From, Flight_length ),
   add_time( Depart_time, Flight_length, Landing_time ),
   add_time( Landing_time, time(0, 30), Next_depart_time ),
   fly_recur( Stop, To, Next_depart_time, Recur_list ),
   Fd = flight_data( From, Stop, Depart_time, Landing_time ),
   append([Fd], Recur_list, Flight_list ).

print_flight_list( [] ).

print_flight_list( [First|Rest] ) :-
   flight_data( From, To, Depart_time, Arrive_time ) = First,
   disp_flight( From, To, Depart_time, Arrive_time ),
   print_flight_list( Rest ).

disp_flight( From, To, time(D_hr, D_min), time(A_hr, A_min) ) :-
   airport( From, Fname, _, _),
   airport( To, Tname, _, _),
   to_upper( From, Fcode ),
   to_upper( To, Tcode ),
   format( 'depart ~a ~a ~d:~d~n', [Fcode, Fname, D_hr, D_min]),
   format( 'arrive ~a ~a ~d:~d~n', [Tcode, Tname, A_hr, A_min]).

arrive_time( To, From, Time ) :-
   airport(To, _, To_lat, To_lon),
   airport(From, _, From_lat, From_lon),
   haversine_degmin( From_lat, From_lon, To_lat, To_lon, Distance ),
   Hrs is truncate( Distance / 500 ),
   Mins is truncate( Distance * 60 / 500 ) mod 60,
   Time = time( Hrs, Mins ).

gt_time( time( H1, M1 ), time( H2, M2 ) ) :-
   H1 >= H2,
  (H1 =:= H2 ->
      M1 > M2 ; true).

add_time( time( H1, M1 ), time( H2, M2 ), Result ) :-
   HI is H1 + H2,
   MI is M1 + M2,
  (MI >= 60 ->
      HR is HI + 1,
      MR is MI mod 60
      ;
      HR is HI,
      MR is MI),
   Result = time(HR, MR).

to_upper(atl, 'ATL').
to_upper(bos, 'BOS').
to_upper(chi, 'CHI').
to_upper(den, 'DEN').
to_upper(dfw, 'DFW').
to_upper(lax, 'LAX').
to_upper(mia, 'MIA').
to_upper(nyc, 'NYC').
to_upper(sea, 'SEA').
to_upper(sfo, 'SFO').
to_upper(sjc, 'SJC').

