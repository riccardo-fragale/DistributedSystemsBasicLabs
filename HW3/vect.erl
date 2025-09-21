-module(vect).

-export([zero/0]).
-export([inc/2]).
-export([merge/2]).
-export([leq/2]).
-export([rec/3]).
-export([clock/1]).
-export([update/3]).
-export([safe/2]).

zero() -> 
    [].

inc(Name, Time) ->
                case lists:keyfind(Name, 1, Time) of
                    {Name,T} -> lists:keyreplace(Name, 1, Time, {Name, T+1});
                    false -> [{Name,1} |Time]
                end.

merge([], Time) ->
            Time;
merge([{Name, Ti}|Rest], Time) ->
                        case lists:keyfind(Name, 1, Time) of
                            {Name, Tj} -> [{Name, erlang:max(Ti,Tj)} |merge(Rest, lists:keydelete(Name, 1, Time))];
                            false -> [{Name, Ti} |merge(Rest, Time)]
                        end.


leq([], _) ->
            true;
leq([{Name, Ti}|Rest],Time) ->
                        case lists:keyfind(Name, 1, Time) of
                            {Name, Tj} when Ti =< Tj -> leq(Rest,Time);
                            _ -> false
                        end.


clock(_) ->
    [].

%I needed to introduce this otherwise it was giving me errors
rec(Name,Local,Remote)->
    inc(Name, merge(Local,Remote)).


update(Node, Time, Clock) -> %Time contains the arriving clock of the message, clock is the vector clock
                    NodeTime = 
                        case lists:keyfind(Node, 1, Time) of
                            {Node,T} -> T;
                            false -> 0  %if node not in the clock, treat it as 0
                    end,
                    %Add element in our clock
                    case lists:keyfind(Node,1,Clock) of
                        {Node, OldTime} -> 
                            NewTime = erlang:max(NodeTime,OldTime),
                            lists:keyreplace(Node,1,Clock,{Node,NewTime});
                        false ->
                            [{Node,NodeTime} | Clock]
                end.

safe(Time, Clock) ->
    lists:all(
        fun({Name, Ti}) ->
            case lists:keyfind(Name, 1, Clock) of
                {Name, Tj} when Ti =< Tj -> true;
                _ -> false
            end
        end,
        Time
    ).

                        



