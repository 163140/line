-module(line).
-include_lib("eunit/include/eunit.hrl").
-export([new/2, len/1, find/3, line_eq/1]).
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
	?_assert(new(	{point,  0, 0},	{point, 10, 10}	)	=:=	{	line,	{point,  0, 0},	{point, 10, 10} }),
	?_assert(new(	{point, 10,10},	1.86						)	=:=	{ line,	{point, 10,10},	{point, 20, 10+10*math:tan(1.86)} }),
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
len_test()->?_assert( len({line, {point,10,96}, {point,-22,80} }) =:= math:sqrt(1280) ).

%% @doc Рассчитывает неизвестную координату по известной на заданной прямой
-spec find('x?' | 'y?', line(), number() )	->	number().
find('x?', Line, Y)	->
	{line_eq,	K,	B}	=	line_eq(Line),
	(Y - B) / K;
find('y?', Line, X)	->
	{line_eq,	K,	B}	=	line_eq(Line),
	K * X + B.
find_test_() -> [
	?_assert( find( 'x?', {line, {point,0,0}, {point,10,10}}, 7 )  =:= 7 ),
	?_assert( find( 'y?', {line, {point,0,0}, {point,10,10}}, 3 )  =:= 3 )
].

%% @doc Возвращает коэффициенты уравнения прямой заданной двумя точками.
-spec line_eq(Line) -> {'line_eq', number(), number()}.
line_eq(Line) ->
	{line	, A1, A2}	=	Line,
	{point,	X1,	Y1}	=	A1,
	{point,	X2,	Y2}	=	A2,
	Dx	=	X2 - X1			,
	Dy	=	Y2 - Y1			,
	K		= Dy/Dx				,
	B		=	Y1 - (K*X1)	,
	{line_eq,	K, B}.
line_eq_test() -> ?_assert( line_eq({ line, {point,10,20}, {point,20,50} }) =:= {line_eq, 3, -10} ). 




