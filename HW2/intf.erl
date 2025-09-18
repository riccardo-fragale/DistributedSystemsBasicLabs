-module(intf).


-export([new/0]).
-export([add/4]).
-export([remove/2]).
-export([lookup/2]).
-export([ref/2]).
-export([name/2]).
-export([list/1]).
-export([broadcast/2]).

%% IN THE COMMENTS A PREVIOUS IMPLEMENTATION WITH MAPS
new()->
    [].  %Return empty

add(Name, Ref, Pid, Intf)->
    %Additional check to eliminate a possible duplicate
    [{Name,Ref,Pid} | lists:keydelete(Name,1,Intf)].

remove(Name,Intf)->
    %maps:remove(Name,Intf).
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf ) ->
    %case maps:get(Name,Intf,notfound) of 
       % notfound -> notfound;
       %{_Ref, Pid} -> {ok, Pid}
    %end.
    case lists:keyfind(Name, 1, Intf) of
        {_Name,_Ref,Pid} -> {ok,Pid};  %_Name to check again
        false -> notfound
    end.


ref(Name, Intf) ->
    %case maps:get(Name,Intf, notfound) of
        %notfound -> notfound;
        %{Ref, _Pid} -> {ok, Ref}
    %end.
    case lists:keyfind(Name, 1, Intf) of
        {_Name,Ref,_Pid} -> {ok,Ref};
        false -> notfound
    end.

name(Ref, Intf)->
    %case [Name || {Name, {R, _Pid}} <- maps:to_list(Intf), R =:= Ref] of 
        %[Name | _] -> {ok, Name};
        %[] -> notfound
    %end.
    case lists:keyfind(Ref, 2, Intf) of
        {Name,_Ref,_Pid} -> {ok,Name};
        false -> notfound
    end.

list(Intf)->
    %maps:keys(Intf).
    [N || {N,_Ref,_Pid} <- Intf].

broadcast(Message, Intf) ->
    %[Pid ! Message || {_Name, {_Ref, Pid}} <- maps:to_list(Intf)].
    lists:foreach(
        fun({_Name,_Ref,Pid}) when is_pid(Pid) ->
            Pid ! Message;
        ({_Name,_Ref,{RegName,Node}}) ->
            {RegName,Node} ! Message;
        (_) ->
            ok
        end,
        Intf
    ),
    ok.






