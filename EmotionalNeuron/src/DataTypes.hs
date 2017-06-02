-- date last modified: 23.04.2017
-- created by Ivanov Dmitry (errfrom)
-- different data types

module DataTypes where

data Rosenblatt =
  Rosenblatt {learningRate :: Float
             ,bias         :: Float
             ,weight       :: Float --FIXME:
             ,errors       :: Integer}
  deriving (Show)








































