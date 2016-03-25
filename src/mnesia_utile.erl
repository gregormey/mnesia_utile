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

-include_lib("stdlib/include/qlc.hrl").

-export([find/2]).
-export([find_by_id/2]).
-export([all/1]).
-export([store/1]).
-export([remove/2]).



%%%===================================================================
%%% API functions
%%%===================================================================


%% finds specific records from table using a filter function.
-spec find(atom(),fun()) -> list() |  not_found.
find(Table,Filter)->
		case do(qlc:q([X || X <- mnesia:table(Table), 
								Filter(X)]))  of
		[] -> not_found;
		Results -> Results
	end.

%% finds a record by it id from a table
-spec find_by_id(atom(),any()) -> tuple() |  not_found.
find_by_id(Table,Id)->
	case find(Table,id_filter(Id)) of
		not_found -> not_found;
		[Result] -> Result
	end.

%%get all records from a table
-spec all(atom()) -> tuple() |  no_rows.
all(Table)->
	case do(qlc:q([X || X <- mnesia:table(Table)]))  of
		[] -> no_rows;
		Results -> Results
	end.

%%create or update a record. First property of
%%the record will be used as identifier
-spec store(tuple()) -> ok | tuple().
store(Record)->
	Fw = fun() ->
			mnesia:write(Record)
		end,
	case mnesia:transaction(Fw) of
		{atomic, ok} -> ok;
		Val -> Val
	end.

%% removes a record from a table identified by  the Id field
-spec remove(atom(),any()) -> not_found | ok.
remove(Table,Id) ->
	case find_by_id(Table,Id)of
		not_found -> not_found;
		Result -> {atomic, Val} = mnesia:transaction(
					fun () -> mnesia:delete_object(Result) end
					),
					Val
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% filter function to search by Id
id_filter(Id)->
	fun(R) ->
		[_,Id2|_]=tuple_to_list(R),
		Id==Id2
	end.

%% runs mnesia querys with qlc
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val}=mnesia:transaction(F),
	Val.
	
