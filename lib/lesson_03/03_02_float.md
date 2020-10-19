### Float

Число с плавающей точкой. Как и в большинстве других языков, реализованно по стандарту IEEE 754. Соответственно, занимает 8 байт памяти.

И подвержено потере точности при вычислениях, точно так же, как и в других языках.
5> 0.1 + 0.2.
0.30000000000000004

Таков стандарт IEEE 754 :) 

реализовать функцию is_equal/3, которая сравнивает два float значения на равенство с допустимой погрешностью. Погрешность передается 3-м аргументом.

is_equal(F1, F2, Precision) ->
    abs(F1 - F2) < Precision.


is_equal_test() ->
    ?assertEqual(true, is_equal(3.5, 3.5, 0.01)),
    ?assertEqual(true, is_equal(3.51, 3.51, 0.01)),
    ?assertEqual(false, is_equal(3.51, 3.53, 0.01)),
    ?assertEqual(true, is_equal(3.51, 3.53, 0.1)),
    ?assertEqual(false, is_equal(3.501, 3.503, 0.001)),
    ?assertEqual(true, is_equal(3.501, 3.503, 0.01)),
    ?assertEqual(true, is_equal(-7.77, -7.75, 0.1)),
    ?assertEqual(true, is_equal(-10.95, -11.0, 0.2)),
    ?assertEqual(true, is_equal(-10.95, -11.0, 0.06)),
    ?assertEqual(false, is_equal(-10.95, -11.0, 0.02)),
    ok.

реализовать функцию distance/2, которая вычисляет расстояние между двумя точками