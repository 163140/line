-module(line).
-include_lib("eunit/include/eunit.hrl").
-export([new/2, len/1]).
-author("ea1a87").


-type point():: {point, float(), float()}	. %% Точка в двухмерном пространстве
-type line() ::	{line	, point(), point()}	.	%% Прямая на плоскости проходящая заданная двумя точками

%% @doc Возвращает новую прямую заданную координатами точками А и В, точкой А и углом между прямой и абциссой,
-spec new( point(), point() | Angle::number() ) -> line().
new(A,B)		->
	{point, X, Y} = A,														%% Да, это точка
	if
		is_number(B)-> 															%% Второй аргумент угол?
			{point, X+10,	Y + math:tan(B) * 10}	=	C,
			{line	,	A,		C	};
		true				->
			{point, X1, Y1} = B								,				%% Второй аргумент - вторая точка
			true						=	is_number(X1+Y1),
			{line	,	A	,	B	}
end.
new_test_()	-> [
	?_assert(at_line(	{point,  0, 0},	{point, 10, 10}	)	=:=	{	line,	{point,  0, 0},	{point, 10, 10} }),
	?_assert(at_line(	{point, 10,10},	1.86						)	=:=	{ line,	{point, 10,10},	{point, 20, 10+10*math:tan(1.86)} }),
].

%% @doc Длина отрезка АВ заданного точками А и B.
-spec len( line() ) -> float().
len(Line) ->
	{line	,	A	,	B	}	=	Line,
	{point, X1, Y1}	=	A		,
	{point,	X2,	Y2}	=	B		,
	Dx	=	X2 - X1,
	Dy	=	Y2 - Y1,
	math:sqrt(math:pow(Dx, 2) + math:pow(Dy, 2)).
len_test()->?_assert( {line, {point,10,96}, {point,-22,80} } =:= math:sqrt(1280) ).
