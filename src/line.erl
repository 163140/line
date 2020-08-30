-module(line).
-include_lib("eunit/include/eunit.hrl").
-export([new/2, len/1, find/3, line_eq/1, intersect/2]).
-author("ea1a87").


-type point():: {point, float(), float()}	. %% Точка в двухмерном пространстве
-type line() ::	{line	, point(), point()}	.	%% Прямая на плоскости проходящая заданная двумя точками

%% @doc Возвращает новую прямую заданную координатами точками А и В, точкой А и углом между прямой и абциссой,
%%			или уравнением прямой
-spec new( point() | number(), point() | number() ) -> line().
new(A,B)		->
	{point, X, Y} = A,														%% Да, это точка
	if
		is_number(A) and is_number(B)	-> 						%% Прямая задана уравнением?
			K	=	A,
			C1= {point, 0	,	B			},
			C2=	{point,	10,	K*X+B	},
			{line, C1, C2};
		is_number(B)									->						%% Если это не два коэффициента, то второе число это угол
			C	=	{point, X+10,	Y + math:tan(B) * 10},
			{line, A, C};
		true				->
			{point, X1, Y1} = A						,				%% Оба аргумента - точки
			{point, X2, Y2} = B						,				%% Оба аргумента - точки
			true = is_number(X1+Y1+X2+Y2)	,
			{line	,	A	,	B	}
end.
new_test_()	-> [
	?_assert(new(	1							, 30							) =:= {	line,	{point,  0,30},	{point, 40, 10} }),
	?_assert(new(	{point,  0, 0},	{point, 10, 10}	)	=:=	{	line,	{point,  0, 0},	{point, 10, 10} }),
	?_assert(new(	{point, 10,10},	1.86						)	=:=	{ line,	{point, 10,10},	{point, 20, 10+10*math:tan(1.86)} })
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
-spec line_eq( line() ) -> {'line_eq', number(), number()}.
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

%% @doc Возвращает точку перечесения двух прямых заданных точками или указывает что прямые параллельны
-spec intersect( line(), line() )	-> point() | 'parallel'.
intersect(Line1, Line2) ->
	{line_eq,	K1,	B1}	=	Line1,
	{line_eq,	K2,	B2}	=	Line2,
	if
		K1 =/= K2	-> 
			X	=	(B2 - B1) / (K1 - K2),
			Y	=	find('y?', Line1, X),
			{point,	X, Y};
		true			->
			parallel
end.
intersect_test_() -> [
	?_assert( intersect({line, {point,0,0}, {point,10,10}}, {line, {point,10,0}, {point, 0,10}}) =:= {point,5,5}	),
	?_assert( intersect({line, {point,0,0}, {point,10,10}},	{line, {point,10,0}, {point,20,10}}) =:=	'parallel'	)
].
