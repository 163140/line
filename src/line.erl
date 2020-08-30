-module(line).
-include_lib("eunit/include/eunit.hrl").
-export([new/2, len/1, find/3, line_eq/1, intersect/2]).
-author("ea1a87").


-type point():: {point, number(), number()}	. %% Точка в двухмерном пространстве
-type line() ::	{line	, point()	, point()	}	.	%% Прямая на плоскости проходящая заданная двумя точками

%% @doc Возвращает новую прямую заданную координатами точками А и В, точкой А и углом между прямой и абциссой,
%%			или уравнением прямой
-spec new( point() | number(), point() | number() ) -> line().
new(A,B)		->
	if
		is_number(A) and is_number(B)	-> 						%% Прямая задана уравнением?
			K	=	A,
			X	=	10,
			C1= {point, 0	,	B			},
			C2=	{point,	X,	K*X+B	},
			{line, C1, C2};
		is_number(B)									->						%% Если это не два коэффициента, то второе число это угол
			{point, X, Y} = A,
			Dx=	10,
			C	=	{point, X+Dx,	Y + math:tan(B) * Dx},
			{line, A, C};
		true				->
			{point, X1, Y1} = A						,				%% Оба аргумента - точки
			{point, X2, Y2} = B						,				%% Оба аргумента - точки
			true = is_number(X1+Y1+X2+Y2)	,
			{line	,	A	,	B	}
end.
new_test_()	-> [
	?_assert(new(	1							, 30							)	== { line,	{point,	0	, 30},	{point, 10, 40									}	}	),
	?_assert(new(	{point, 10,10},	1.86						)	== { line,	{point, 10, 10},	{point, 20, 10+10*math:tan(1.86)} }	),
	?_assert(new(	{point,  0, 0},	{point, 10, 10}	)	== { line,	{point, 0	, 0	},	{point, 10, 10									} }	)
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
len_test()->?_assert( len({line, {point, 10, 96}, {point, -22, 80} }) =:= math:sqrt(1280) ).

%% @doc Рассчитывает неизвестную координату по известной на заданной прямой
-spec find('x?' | 'y?', line(), number() )	-> float().
find('x?', Line, Y)	->
	{line_eq,	K,	B}	=	line_eq(Line),
	(Y - B) / K;
find('y?', Line, X)	->
	{line_eq,	K,	B}	=	line_eq(Line),
	K * X + B.
find_test_() -> [
	?_assert( find( 'x?', {line, {point,0,0}, {point,10,10}}, 7 )  =:= 7.0 ),
	?_assert( find( 'y?', {line, {point,0,0}, {point,10,10}}, 3 )  =:= 3.0 )
].

%% @doc Возвращает коэффициенты уравнения прямой заданной двумя точками.
-spec line_eq( line() ) -> {'line_eq', float(), float()}.
line_eq(Line) ->
	{line	, A1, A2}	=	Line,
	{point,	X1,	Y1}	=	A1,
	{point,	X2,	Y2}	=	A2,
	Dx	=	X2 - X1			,
	Dy	=	Y2 - Y1			,
	K		= Dy/Dx				,
	B		=	Y1 - (K*X1)	,
	{line_eq,	K, B}.
line_eq_test() -> ?_assert( line_eq({ line, {point,10,20}, {point,20,50} }) =:= {line_eq, 3.0, -10.0} ). 

%% @doc Возвращает точку перечесения двух прямых заданных точками или указывает что прямые параллельны
-spec intersect( line(), line() )	-> point() | 'parallel'.
intersect(Line1, Line2) ->
	LineA = line_eq(Line1)		,
	LineB = line_eq(Line2)		,
	{line_eq,	K1,	B1}	=	LineA	,
	{line_eq,	K2,	B2}	=	LineB	,
	if
		K1 =/= K2	-> 
			X	=	(B2 - B1) / (K1 - K2),
			Y	=	find('y?', Line1, X),
			{point,	X, Y};
		true			->
			parallel
end.
intersect_test_() -> [
	?_assert( intersect({line, {point,0,0}, {point,10,10}}, {line, {point, 0,10}, {point,10, 0}}) =:=	{point, 5.0, 5.0}	),
	?_assert( intersect({line, {point,0,0}, {point,10,10}},	{line, {point,10, 0}, {point,20,10}}) =:=	'parallel'				)
].
