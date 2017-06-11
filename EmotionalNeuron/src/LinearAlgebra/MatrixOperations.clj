; date last modified 10.06.2017
; developed by Ivanov Dmitry (errfrom)
; matrix operations

; совпадает ли число элементов в каждой строке матрицы
(defn matrix? [m]
  (if (== (count (set (map count m))) 1) true false))

; умножение матриц
(defn matrix-mult [m1 m2]
  (if (or (not (matrix? m1)) (not (matrix? m2)))
    (throw (Exception. "Число элементов в каждой строке матрицы
                        должно быть одинаковым."))
    (if (not= (count (first m1)) (count m2))
      (throw (Exception. "Число столбцов m1 должно быть равно числу строк m2."))
      (transpose (_matrix-mult m1 (transpose m2))))))

(defn _matrix-mult [m1 tm2]
  (if (nil? (seq tm2)) nil
    (into [(matrix-map apply +
           (matrix-map vector-elems-mult (first tm2) m1))]
           (_matrix-mult m1 (rest tm2)))))

; транспонирование матрицы
; [[a11 a12] [a21 a22]] -> [[a11 a21] [a12 a22]]
(defn transpose [m]
  (def nrows (count m))
  (vec (map vec (partition nrows (_transpose m)))))

(defn _transpose [m]
  (if (nil? (seq (first m))) nil
    (into (vec (map first m)) (_transpose (vec (map vec (map rest m)))))))

; параллельное перемножение элементов двух векторов
; (vector-elems-mult [a1 a2] [b1 b2]) -> [a1*b1 a2*b2]
(defn vector-elems-mult [a b]
  (if (nil? (seq a)) nil
    (if (not= (count a) (count b))
      (throw (Exception. "Векторы должны быть одинаковы по длине."))
      (into [(* (first a) (first b))]
        (vector-elems-mult (rest a) (rest b))))))

(defn matrix-map [func v m]
  (if (nil? (seq m)) nil
    (into [(func v (first m))] (matrix-map func v (rest m)))))
