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
   flight( From, To, Time),
   disp_flight( From, To, Time).

fly( From, To) :-
   flight( From, Stop, Time),
   fly( Stop, To ),
   disp_flight( From, Stop, Time ).

disp_flight( From, To, Time ) :-
   airport( From, Fname, _, _),
   to_upper( From, Fcode ),
   time(Hr, Min) is Time,
   format( 'depart ~a ~a ~d:~d~n', [Fcode, Fname, Hr, Min]).

arrive_time( To, From, Time ) :-
   airport(To, _, To_lat, To_lon),
   airport(From, _, From_lat, From_lon),
   haversine_degmin( From_lat, From_lon, To_lat, To_lon, Distance ),
   Total_mins is Distance * 60 / 500,
   display(Total_mins), nl,
   Hrs is truncate(Total_mins / 60.0),
   Mins is truncate(Total_mins mod 60.0),
   format( '~d ~d~n', [Hrs, Mins] ).

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

