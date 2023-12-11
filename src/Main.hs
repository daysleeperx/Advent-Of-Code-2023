module Main (main) where

import qualified Day00.Test as Test
import qualified Day01.Trebuchet as Trebuchet
import qualified Day02.CubeConundrum as CubeConundrum
import qualified Day03.GearRatios as GearRatios
import qualified Day04.Scratchcards as Scratchcards
import qualified Day05.IfYouGiveASeedAFertilizer as IfYouGiveASeedAFertilizer
import qualified Day06.WaitForIt as WaitForIt
import qualified Day07.CamelCards as CamelCards
import qualified Day08.HauntedWasteland as HauntedWasteland
import qualified Day09.MirageMaintenance as MirageMaintenance
import System.Environment (getArgs)

solvers :: [FilePath -> IO ()]
solvers =
    [ Test.solve
    , Trebuchet.solve
    , CubeConundrum.solve
    , GearRatios.solve
    , Scratchcards.solve
    , IfYouGiveASeedAFertilizer.solve
    , WaitForIt.solve
    , CamelCards.solve
    , HauntedWasteland.solve
    , MirageMaintenance.solve
    ]

main :: IO ()
main = do
    [day, filePath] <- getArgs
    let solver = solvers !! read day
    solver filePath
