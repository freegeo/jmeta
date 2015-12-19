# jmeta #

Yet another data validation library (based on the duck typing approach) for **Erlang**.

[![Build Status](https://travis-ci.org/freegeo/jmeta.svg)](https://travis-ci.org/freegeo/jmeta)

If you have any questions feel free to contact the author: said.dk@gmail.com (Kostya ^__^)

#### Build & Test ####

```bash
$ rebar compile
$ rebar eunit
```

## Bring me some examples! Now! ##

#### Startup & Tricks out of the box ####

```erl-sh
1> jmeta:start().
ok
2> jmeta:is({tuple, {a, b, c}}).
true
3> jmeta:is({integer, "5"}).
{error, {not_a, {std, integer, []}}}
4> ListOfRandomTypes = [hi, 26, <<"John">>, there, 42, [a, b, c], {1, 2, 3}, guys, ""].
[hi, 26, <<"John">>, there, 42, [a, b, c], {1, 2, 3}, guys, ""]
5> jmeta:pick({atom, ListOfRandomTypes}).
[hi, there, guys]
6> jmeta:list_of({atom, ListOfRandomTypes}).
[[{error, {not_a, {std, atom, []}}}, {pos, 2}],
 [{error, {not_a, {std, atom, []}}}, {pos, 3}],
 [{error, {not_a, {std, atom, []}}}, {pos, 5}],
 [{error, {not_a, {std, atom, []}}}, {pos, 6}],
 [{error, {not_a, {std, atom, []}}}, {pos, 7}],
 [{error, {not_a, {std, atom, []}}}, {pos, 9}]]
```

### The Type System ###

So, how do I add my own primitive types? Simple enough!
The idea is if anything satisfies constrains successfully then this is what you are looking for.

Let's define a new type!

A note: all the following examples assume that **jmeta** has been bootstrapped successfully
(see the **Startup** section).

```erl-sh
1> jmeta:add({type, gender, [{guards, [fun(Gender) -> lists:member(Gender, [male, female, na]) end]}]}).
29
2> jmeta:is({gender, male}).
true
3> jmeta:is({gender, human}).
{error, {not_a, {std, gender, []}}}
```

So, to define a new primitive type you have to specify a type name and to associate one or more unary or binary guards
with that type. Keep in mind, we were using the standard namespace called "std". That's pretty possible but I do not
recommend you to do so. Instead of using the std namespace you should define your own one. There is no special
syntax on it, **jmeta** creates a new namespace once it appears.

OK, let's try again.

```erl-sh
1> jmeta:add({type, {'thedicegame.myproject.local', d6_result}, [{guards, [fun(V) -> V >= 1 andalso V =< 6 end]}]}).
1
2> jmeta:is({d6_result, 3}).
** exception error: {'jmeta.exception', {{std, d6_result}, is_not_defined}}
3> jmeta:is({{'thedicegame.myproject.local', d6_result}, 3}).
true
```

Such huge namespaces are pretty inconvenient in the use, so we can hide them out by using a simple macro.

```erlang
-define(DICE(X), {'thedicegame.myproject.local', X}).
```

#### Parametrized Types ####

You can define a type with parameters. You can also specify default values.
Using parameters you can tweak a type directly during the validation stage.

A note: I'm gonna use std in the following examples in order to simplify them,
but that's definitely a wrong way to do so. In real projects you always use namespaces.

```erl-sh
1> jmeta:add({type, dice,
      [{guards, [fun(V, [{d, D}]) -> V >= 1 andalso V =< D end]},
       {params, [{d, 6}]}
      ]}).
29
2> jmeta:pick({dice, lists:seq(-20, 20)}).
[1, 2, 3, 4, 5, 6]
3> jmeta:pick({{dice, [{d, 2}]}, lists:seq(-20, 20)}).
[1, 2]
4> jmeta:is({{dice, [{d, 10}]}, 15}).
{error, {not_a, {std, dice, [{d, 10}]}}}
```

Feel free to omit the params section (the defaults) of a type if it bothers you somehow.
You can simply put defaults directly into a guard whenever you need it. See the **regex** type of
the **jmeta_library**.

#### Mixins & Modificators ####

The mixins mechanism is the way to combine types together. It's the simplest way to achieve variant types but
also it's very useful in some other situations.

```erl-sh
1> jmeta:add({type, range,
      [{guards, [fun(V, Params) when is_integer(V) ->
        {Min, Max} = jframe:find([min, max], Params),
        V >= Min andalso V =< Max
      end]}]}).
29
2> jmeta:pick({{range, [{min, 1}, {max, 4}]}, lists:seq(-20, 20)}).
[1, 2, 3, 4]
3> jmeta:add({type, range1_7_and_4_9,
      [{mixins, [
        {range, [{min, 1}, {max, 7}]},
        {range, [{min, 4}, {max, 9}]}
      ]}]}).
30
4> jmeta:pick({range1_7_and_4_9, lists:seq(-20, 20)}).
[4, 5, 6, 7]
```

In the example above the type **range1_7_and_4_9** is a conjunction of its nested types (range 1-7 & range 4-9).
By defaul **jmeta** composes all types defined in the mixins section using the **all** modificator. That's very
strict mode means that all the corresponding constrains should be passed successfully before a given value is approved.
Sometime the all modificator turns a type into a sealed one. As for example try to mix integer and string types.
But there is also not so strict version of the all modificator - the **any** modificator. Using this modificator
you create true variant types and achieve some other interesting effects. The any modificator assumes that
this is enough if a given value satisfies any type of listed types of the mixins section.

```erl-sh
5> jmeta:add({type, range1_7_or_4_9,
     [{mixins, [
       {range, [{min, 1}, {max, 7}]},
       {range, [{min, 4}, {max, 9}]}
     ]}, {mode, {mixins, any}}]}).
31
6> jmeta:pick({range1_7_or_4_9, lists:seq(-20, 20)}).
[1, 2, 3, 4, 5, 6, 7, 8, 9]
7> jmeta:add({type, int_or_str, [{mixins, [integer, string]}, {mode, {mixins, any}}]}).
32
8> jmeta:pick({int_or_str, [1, 2.44, 5, <<"Hello">>, 8, [1, 2, 3], {a, b, c}, <<"John">>]}).
[1, 5, <<"Hello">>, 8, <<"John">>]
```

You can achieve the same effect by specifying a multi guards function and combining them together using
the **any** modificator. Let's see how it works!

```erl-sh
1> jmeta:add({type, int_or_str2, [
    {guards, [fun erlang:is_integer/1, fun erlang:is_bitstring/1]},
    {mode, {guards, any}}]}).
29
2> jmeta:pick({int_or_str2, [1, 2.44, 5, <<"Hello">>, 8, [1, 2, 3], {a, b, c}, <<"John">>]}).
[1, 5, <<"Hello">>, 8, <<"John">>]
```

If you need all the modificators set at once you combine them into a list.

```erlang
{mode, [{guards, any}, {mixins, any}]}
```

### Validating Complex Types ###

OK, so how do I validate a complex type?
There is a special syntax on it!

First things first.

#### Complex Types: Frames ####

Frame is a complex type introduced in **jmeta**. Technically it's just a key-value list.
Let's inspect some interesting tricks you can do on frames using **jframe** module.

```erl-sh
1> F1 = jframe:new([{name, <<"John">>}, {age, 46}]).
[{name, <<"John">>}, {age, 46}]
2> F2 = jframe:store([{age, 47}, {gender, m}], F1).
[{name, <<"John">>}, {age, 47}, {gender, m}]
3> F3 = jframe:update([{age, fun(Age) -> Age - 5 end},
                       {name, fun(Name) -> <<Name/bitstring, " Doe">> end}], F2).
[{name, <<"John Doe">>}, {age, 42}, {gender, m}]
4> jframe:find(name, F3).
<<"John Doe">>
5> jframe:find([name, gender, {schooled, na}, {age, 99}, married], F3).
{<<"John Doe">>, m, na, 42, undefined}
```

These are not just the possible tricks of course. Please inspect **jframe** and the corresponding tests.

#### Declaring Frames ####

Let's try to define some frames.

```erlang
-module(real_project_setup).

%% API
-export([setup/0]).

setup() ->
  Identifier =
    {type, identifier,
      [{mixins, [null, integer]},
        {mode, [{mixins, any}]}
      ]},
  Entity =
    {frame, entity,
      [{fields, [{id, {is, identifier}}]}
    ]},
  Person = {frame, person,
    [{fields,
      [
        {first_name, {is, string128}},
        {last_name, {is, string128}},
        {middle_name, [{is, string128}, {optional, true}]}
      ]}
    ]},
  Student =
    {frame, student,
      [{extend, [entity, person]},
        {fields,
          [
            {grade, [{is, integer}, {guards, [fun(Grade) -> Grade >= 1 andalso Grade =< 12 end]}]},
            {age, [{is, integer}, {guards, [fun(Age) -> Age > 5 andalso Age < 100 end]}]},
            {courses, {list_of, atom}}
          ]}
      ]},
  lists:foreach(fun jmeta:add/1, [Identifier, Entity, Person, Student]),
  ok.
```

For those fields which are single value fields you use the **is** keyword.
For enumerations you use the **list_of** keyword.
 
*A node: the **list_of** keyword supports nesting, means you can define list of list of N,
and we can go deeper of course.*

By default all the declared fields are mandatory. It's possible to override this behavior by using
the **optional** modificator. It's also possible to define some **custom field guards**, but there is no modificators
on that feature. You either apply all of the guards at once or you either use none of them.  

*An important note: please inspect the **jmeta_tests.erl** module for a better understanding.*

OK, let's see how it works!

```erl-sh
1> jmeta:start().
ok
2> real_project_setup:setup().
ok
3> jmeta:is({student, [{id, empty}, {first_name, <<"Kostya">>}, {grade, 13}, {axe, 27},
            {courses, [<<"math">>, english, 42]}]}).
{error, [{not_a, {std, student, []}},
         {violated, [{id, {not_a, {std, identifier, []}}},
                     {last_name, missed},
                     {grade, {{is, {std, integer, []}}, but_breaking_a_guard}},
                     {age, missed},
                     {courses, [[{error, {not_a, {std, atom, []}}}, {pos, 1}],
                                [{error, {not_a, {std, atom, []}}}, {pos, 3}]]}]},
         {extra_keys, [axe]}]}
4> jmeta:is({student, [{id, 1}, {first_name, <<"John">>}, {last_name, <<"Doe">>}, {grade, 5}, {age, 13},
            {courses, [math, english, physics]}]}).
true
```

Pretty cool, huh? :)

Well, this is probably it. Inspect the code, inspect the tests, play with the features and feel free to contact me
whenever you need it!

Have fun!