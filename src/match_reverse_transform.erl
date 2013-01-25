-module(match_reverse_transform).
-export([parse_transform/2]).

-record(state, {
          options,
	  match_reverse = []
        }).

parse_transform(Forms, Options) ->
    %io:format("Forms: ~p~n", [Forms]),
    {Forms1, State} = parse_trans:transform(fun do_transform/4, #state{options=Options}, Forms, Options),
    Result = parse_trans:revert(Forms1),
    %io:format("NewForms: ~p~n", [Result]),
    Result.

do_transform(attribute, {attribute, _, match_reverse, FunctionName} = Form, _Context, #state{match_reverse=MRx} = State) ->
    {Form, false, State#state{match_reverse=[FunctionName|MRx]}};

do_transform(function, {function, Line, Name, 1, Clauses} = Form, Context, #state{match_reverse=MRx} = State) ->
    Xlate = lists:member(Name, MRx),
    io:format("function ~p, xlate: ~p ~p~n", [Name, Xlate, case Xlate of true -> Clauses; _ -> [] end]),

    NewClauses = lists:flatten([visit_clause(Clause) || Clause <- Clauses]),
    
    {{function, Line, Name, 1, NewClauses}, true, State};

do_transform(_Type, Form, _Context, State) ->
    {Form, true, State}.


visit_clause({clause, _, [{MatchType, Line, _MatchValue}] = Match, [] = _Guards, [{_RetType, _, _RetValue}] = Body} = Clause) when
      MatchType /= var ->
    [Clause, {clause, Line, Body, [], Match}];
visit_clause(Clause) ->
    [Clause].


