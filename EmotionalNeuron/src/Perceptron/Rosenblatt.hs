-- date last modified: 23.04.2017
-- created by Ivanov Dmitry (errfrom)
-- implementation of Rosenblatt's Perceptron

{-# LANGUAGE RecordWildCards #-}

module Perceptron.Rosenblatt (setPerceptron, teachPerceptron) where

import qualified Data.Maybe
import Data.List
import DataTypes

setPerceptron :: Float -> Float -> Rosenblatt
-- Инициализация перцептрона
setPerceptron learningRate bias
 |learningRate <= 0 || learningRate > 1 = setPerceptron defaultLearningRate bias
 |otherwise                             = Rosenblatt learningRate bias 0 0 --FIXME:
  where defaultLearningRate             = 0.015

{-

RUS:
  Обучение персептрона.

  Итерационная подстройка веса,
  последовательно уменьшающей ошибку в выходных векторах.

  Принцип работы нейрона:
    На вход подается n значений x и n значений y, где
    x - некоторый входной сигнал, соответствующий определенному
    логическому(Bool) значению y.

  Инициализируется:
    Веса инициализируются в нулевой список.
    Порог bias = 1.
    Скорость обучения(LearningRate) стандартно равный 0.015,
    который показывает, с какой скоростью персептрон обучается.

  Для каждого элемента x:
    Вычисляется линейная комбинация g = X1 * w1 + X2 * w2 ... + Xn * wn.
    Если g >= 0, то возвращается True; если же нет, False.
    Пусть у' - возращенный ответ.
    Пусть u - значение обновления, равное (Learning Rate * (y - y')).
    Значение weight увеличивется на (u * xk).
    Значение порога(bias) увеличивается на u.

  В результате, если u = 0 - ошибки не возникло.

ENG:
  Perceptron training.

  An algorithm that would automatically
  learn the optimal weight coefficients that are then multiplied with the input features
  in order to make the decision of whether a neuron fires or not.

  Rosenblatt's initial perceptron rule:
    Initialize the weights to 0.
    Initialize the bias to 1.
    Initialize the Learning Rate to value between > 0.0 and 1.0.

  For each element x:
    A linear combination is calculated as g = X1 * w1 + X2 * w2 ... + Xn * wn.
    If g >= 0, then True; If not - False.
    Let y' be the response.
    Let u - update value, equals to (Learning Rate * (y - y')).
    Weight value increases by (u * xk).
    Bias value increases by u.

  As a result, if u = 0, no error occurred.

-}

teachPerceptron :: (Eq a, Num a) => Rosenblatt -> [Float] -> [Float] -> a -> Rosenblatt
teachPerceptron p@(Rosenblatt{..}) prioriValues posterioriValues numIter
-- posterioriValues имеют числовое значение <- 1||-1, а не логическое,
-- т.к. это облегчает логику
 |(length prioriValues) /= (length posterioriValues) = p
 -- обязательное условие, где каждому значению x
 -- соответствует единственное значение y

 -- FIXME:
 -- |null weights =
 --    let weights' = replicate (length prioriValues) 0
 --   in teachPerceptron p{weights=weights'} prioriValues posterioriValues numIter

 |otherwise =
    if numIter == 0 then p
    else
      let p' = teach p prioriValues posterioriValues
      in teachPerceptron p' prioriValues posterioriValues (numIter - 1)

predict :: Rosenblatt -> Float -> Float
predict perceptron prioriValue
-- Активационная функция.
 |getNetInput perceptron prioriValue >= 0 = 1
 |otherwise                               = -1

getNetInput :: Rosenblatt -> Float -> Float
-- Линейная комбинация.
getNetInput Rosenblatt{..} prioriValue = bias + prioriValue * weight

teach :: Rosenblatt -> [Float] -> [Float] -> Rosenblatt
-- Одна итерация. Реализация алгоритма.
teach p [] [] = p
teach p@(Rosenblatt{..}) prioriValues@(x:xs) (y:ys) =
  let   update   = learningRate * (y - (predict p x))
        bias'    = bias + update
        weight'  = weight + (x * update)
        errors'  = errors + (if update == 0 then 0 else 1)
        p'       = p{bias   = bias'
                    ,weight = weight'
                    ,errors = errors'}
  in teach p' xs ys
