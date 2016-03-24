%% Copyright (c) 2016, Gregor Meyenberg  <gregor@meyenberg.de>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mnesia_utile).

-include_lib("stdlib/include/qlc.hrl")

-export([find/2]).
-export([find_by_id/2]).
-export([all/1]).
-export([remove/2]).



%%%===================================================================
%%% API functions
%%%===================================================================


%% find specific records from table
-spec find(atom(),fun()) -> record() |  not_found.
find(Table,Filter)->
		case do(qlc:q([X || X <- mnesia:table(Table), 
								Filter(X)]))  of
		[] -> not_found;
		Results -> Results
	end.

-spec find_by_id(atom(),non_neg_integer()) -> record() |  not_found.
find_by_id(Table,Id)->
	find(Table,id_filter(Id)).

%%get all records from database
-spec all(atom()) -> tuple() |  no_rows.
all(Table)->
	case do(qlc:q([X || X <- mnesia:table(Table)]))  of
		[] -> no_rows;
		Results -> Results
	end.

-spec store(atom(), record()) -> tuple().
store(Table,Record)->
	Fw = fun() ->
			mnesia:write(Record)
		end,
	[Table,Id|_] = tuple_to_list(Record),
	mnesia:transaction(Fw).

-spec remove(atom(),tuple()) -> any().
remove(Table,Record) ->
	[Table,Id|_] = tuple_to_list(Record),
	case find(Table,filterById(Id))of
		not_found -> not_found;
		[Result] -> {atomic, Val} = mnesia:transaction(
					fun () -> mnesia:delete_object(Result) end
					),
					Val
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

id_filter(Id)->
	fun(R) ->
		[_,Id2|_]=tuple_to_list(R),
		Id==Id2
	end.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	case mnesia:transaction(F) of
		{atomic, Val} -> Val;
		{aborted,{no_exists,_}} -> 
								create_tables(),
								do(Q)
	end. 




