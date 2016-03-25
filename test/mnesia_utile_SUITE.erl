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

-module(mnesia_utile_SUITE).
-compile(export_all).

-import(ct_helper, [doc/1]).
 
-record(create_read_update_delete,{key= undefined :: string(), value= undefined :: non_neg_integer()}).
-record(create_find_filter,{key= undefined :: string(), value= undefined :: non_neg_integer()}).
  

all() -> [create_read_update_delete,create_find_filter].

init_per_suite(Config) ->
    Config.

create_read_update_delete(_)->
	doc("A record can be stored, read, uddated and deleted in a mnesia database."),
	{atomic,ok}=mnesia:create_table(create_read_update_delete, [{attributes, record_info(fields, create_read_update_delete)},{disc_copies,[node()]}]),
	
	%define sone value variations
	A1=#create_read_update_delete{key="A",value=0},
	A2=#create_read_update_delete{key="A",value=1},

	ok=mnesia_utile:store(A1),
	A1=mnesia_utile:find_by_id(create_read_update_delete, "A"),
	ok= mnesia_utile:store(A2),
	A2=mnesia_utile:find_by_id(create_read_update_delete, "A"),
	ok=mnesia_utile:remove(create_read_update_delete, "A"), 
	not_found=mnesia_utile:find_by_id(create_read_update_delete, "A").

create_find_filter(_)-> 
	doc("Several records can be stored and filtered by different criterias."),
	{atomic,ok}=mnesia:create_table(create_find_filter, [{attributes, record_info(fields, create_find_filter)},{disc_copies,[node()]}]),
	no_rows=mnesia_utile:all(create_find_filter), 

	%%define some values
	A=#create_find_filter{key="A",value=0},
	B=#create_find_filter{key="B",value=1},
	C=#create_find_filter{key="C",value=2},
	D=#create_find_filter{key="D",value=3},

	%%create some rows
	ok=mnesia_utile:store(A),
	ok=mnesia_utile:store(B),
	ok=mnesia_utile:store(C),
	ok=mnesia_utile:store(D),

	%% define some filters
	FindA=fun(R) ->
		"A"==R#create_find_filter.key
	end,

	FindGr1=fun(R) ->
		R#create_find_filter.value>1
	end,

	FindGr3=fun(R) ->
		R#create_find_filter.value>3
	end,

	%%find all
	4=length(mnesia_utile:all(create_find_filter)),

	%%try some filters
	[A]=mnesia_utile:find(create_find_filter,FindA),
	[C,D]=mnesia_utile:find(create_find_filter,FindGr1),
	not_found=mnesia_utile:find(create_find_filter,FindGr3).	





 
