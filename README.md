# mnesia_utile
[![Build Status](https://travis-ci.org/gregormey/mnesia_utile.svg?branch=master)](http://travis-ci.org/regormey/mnesia_utile)

This is a collection of of some mnesia utile functions. It makes standard database operations like create and update and basic querys with mnesia more useable.

# Requires 

`Erlang >= 17.5`

# Installation
Clone it and just run `make` 

or

if you want to use it as a dependency in your project add the following to your `Makefile` for the use with [erlang.mk](http://erlang.mk) 
```
DEPS = mnesia_utile`
dep_mnesia_utile = git https://github.com/gregormey/mnesia_utile master
```


# Usage
```
1> mnesia:create_schema([node()]).
ok
2> mnesia:start().
ok
3> rd(address,{street="",city="",zip=""}).
address
4> rd(person,{name="",phone=[],address=#address{}}).
person
5> mnesia:create_table(person, [{attributes, record_info(fields, person)},{disc_copies,[node()]}]).
{atomic,ok}
6> mnesia_utile:store(#person{name="John",phone=["001-541-754-3010"],address=#address{street="5322 Otter Ln", city="Middleberge",zip="FL 32068"}}).
ok
7> mnesia_utile:store(#person{name="Mary",phone=["001-541-543-1030"],address=#address{street="1807 Glenwood St. NE", city="Middleberge",zip="FL 32068"}}).
ok
8> mnesia_utile:store(#person{name="Edward",phone=["001-271-827-8672","001-271-816-9356"],address=#address{street="200 E MAIN ST", city="PHOENIX",zip="AZ 85123"}}).
ok
9> mnesia_utile:all(person).
[#person{name = "Edward",
         phone = ["001-271-827-8672","001-271-816-9356"],
         address = #address{street = "200 E MAIN ST",
                            city = "PHOENIX",zip = "AZ 85123"}},
 #person{name = "Mary",
         phone = ["001-541-543-1030"],
         address = #address{street = "1807 Glenwood St. NE",
                            city = "Middleberge",zip = "FL 32068"}},
 #person{name = "John",
         phone = ["001-541-754-3010"],
         address = #address{street = "5322 Otter Ln",
                            city = "Middleberge",zip = "FL 32068"}}]
10> mnesia_utile:find_by_id(person,"John").
#person{name = "John",
        phone = ["001-541-754-3010"],
        address = #address{street = "5322 Otter Ln",
                           city = "Middleberge",zip = "FL 32068"}}
11> mnesia_utile:find(person,fun (R) -> "Middleberge"==R#person.address#address.city end).
[#person{name = "Mary",
         phone = ["001-541-543-1030"],
         address = #address{street = "1807 Glenwood St. NE",
                            city = "Middleberge",zip = "FL 32068"}},
 #person{name = "John",
         phone = ["001-541-754-3010"],
         address = #address{street = "5322 Otter Ln",
                            city = "Middleberge",zip = "FL 32068"}}]
12> mnesia_utile:remove(person,"John").
ok
```
