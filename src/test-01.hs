import Control.Monad ((>=>))
import Data.Maybe (maybeToList)

import JCSolver.Field
import JCSolver.Solve
import JCSolver.Line

solveHelper :: [[Int]] -> [[Int]] -> [Field]
solveHelper hns vns =
  maybeToList (flCreate hns vns) >>= slAllSolutions flForkByCells fieldTransform''
  where
    lineTransform = slSmartLoop $ lnSimpleTransform >=> lnTransformByExtremeOwners
    lineTransform' = slForkAndSyncAll lnForkByOwners lineTransform
    fieldTransform = slSmartLoop $ slSmartLoop (flTransformByLines lineTransform) >=> flTransformByLines lineTransform'
    fieldTransform' = slForkAndSmartSync flForkByCells fieldTransform
    fieldTransform'' = slSmartLoop $ fieldTransform >=> fieldTransform'

bear :: ([[Int]], [[Int]])
bear =
  ( [[2,2],[1,1,1,1],[1,6,1],[2,2],[1,1,1,1],[1,1,1,1],[1,2,1],[1,4,1],[1,1],[4]]
  , [[3],[1,5],[1,1,1],[2,2,1,1],[1,2,1],[1,2,1],[2,2,1,1],[1,1,1],[1,5],[3]]
  )

(hns, vns) = bear
t1 = solveHelper hns vns
