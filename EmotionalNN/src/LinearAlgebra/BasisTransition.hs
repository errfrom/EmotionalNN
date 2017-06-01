-- Made by Ivanov Dmitry
-- 01.06.2017

import qualified Data.Vector as V
import qualified Data.Matrix as M
import           Data.Maybe  (fromJust)

-- Реализация модели перехода от одного базиса к другому
-- Нахождение разложения вектора по новому базису, зная его
-- разложение по старому.
-- Учитывая, что векторное пространство определено над полем Double
-- (опуская некоторые ограничения, можно считать полем вещественных чисел)

type Matrix a = M.Matrix a
type Vector a = V.Vector a
type Basis  a = [V.Vector a] -- Некоторая система независимых векторов
data VectorType = New | Old

-- Нахождения вектора, зная матрицу перехода и его старые/новые координаты.
-- Полиморфизм "старые/новые" определяется структурой VectorType.
-- Таким образом, если указано New, то будут найдены старые координаты,
-- если Old - новые соответственно.
-- vectorType указывает "тип" входного вектора, а не выходного.
getVectorByTransitionMatrix :: (Fractional a, Eq a, Num a)
  => Matrix a -> Vector a -> VectorType -> Maybe (Vector a)
getVectorByTransitionMatrix tm v vectorType
  |not $ isSquareMatrix tm = Nothing  -- Матрица перехода всегда квадратная
  |otherwise               =
    -- X = A * X', где X - старый вектор, X' - новый вектор
    -- Если vectorType = New => result = tm * v
    -- Если vectorType = Old => result = tm^-1 * v
    let vector = M.colVector v
    in Just $ M.getCol 1 $
      case vectorType of
        New -> tm * vector
        Old -> (*) ((fromJust . inverseMatrix) tm) $ vector

-- Получение обратной матрицы.
inverseMatrix :: (Eq a, Fractional a, Num a) => Matrix a -> Maybe (Matrix a)
inverseMatrix m
  |not $ isSquareMatrix m = Nothing -- NOTE: Можно сделать класс Square
  |det == 0               = Nothing
  |otherwise              = Just $ (/det) <$> (fromJust transposedCm)
  where det          = M.detLaplace m
        transposedCm = M.transpose <$> getComplementsMatrix m

-- Получение матрицы алгебраических дополнений
-- Такая квадратная матрица, которая получается из
-- первоначальной при замене каждого элемента на
-- его алгебраическое дополнение.
getComplementsMatrix :: (Num a) => Matrix a -> Maybe (Matrix a)
getComplementsMatrix m
 |not $ isSquareMatrix m = Nothing
 |otherwise              =
    let r = M.nrows m -- Количество строк матрицы
        c = M.ncols m -- Количество столбцов соответственно
    in Just $ M.fromList r c [getComplement i j m | i <- [1..r], j <- [1..c]]

-- Получение алгебраического дополнения элемента,
-- стоящего на пересечении i строки и j столбца.
-- Алгебраическое дополнение элемента квадратной матрицы,
-- стоящего на пересечении i-ой строки и j-ого столбца:
-- Aij = (-1)^(i+j) * Mij, где Mij - определитель матрицы n-1
-- порядка, которая получается из первоначальной матрицы при вычеркивании
-- i-ой строки и j-ого столбца.(Минор)
getComplement :: (Num a) => Int -> Int -> Matrix a -> a
getComplement i j m
  |(==) 0 $ mod (i + j) 2 = minor
  |otherwise              = -minor
  where minor = (M.detLaplace . M.minorMatrix i j) m

-- Определяет, является ли данная матрица квадратной.
isSquareMatrix :: (Num a) => Matrix a -> Bool
isSquareMatrix m
 |M.nrows m == M.ncols m = True
 |otherwise              = False
