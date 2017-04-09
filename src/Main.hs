{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Lens
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T

data Count = Count
  { _common          :: Int
  , _goldenCommon    :: Int
  , _rare            :: Int
  , _goldenRare      :: Int
  , _epic            :: Int
  , _goldenEpic      :: Int
  , _legendary       :: Int
  , _goldenLegendary :: Int
  }

makeLenses ''Count

emptyCount :: Count
emptyCount = Count 0 0 0 0 0 0 0 0

showT :: Show a => a -> Text
showT = T.pack . show

instance Show Count where
  show Count{..} = T.unpack $ T.intercalate "\n"
    [ " Common:           " <> showT _common
    , " Golden Common:    " <> showT _goldenCommon
    , " Rare:             " <> showT _rare
    , " Golden Rare:      " <> showT _goldenRare
    , " Epic:             " <> showT _epic
    , " Golden Epic:      " <> showT _goldenEpic
    , " Legendary:        " <> showT _legendary
    , " Golden Legendary: " <> showT _goldenLegendary
    , "\n Total: " <> showT ( _common + _goldenCommon + _rare + _goldenRare + _epic + _goldenEpic + _legendary + _goldenLegendary)
    ]

commands :: [(Text,  Count -> Count)]
commands =
  [ ("reset", const emptyCount)
  , ("c" , inc common)
  , ("gc", inc goldenCommon)
  , ("r" , inc rare)
  , ("gr", inc goldenRare)
  , ("e" , inc epic)
  , ("ge", inc goldenEpic)
  , ("l" , inc legendary)
  , ("gl", inc goldenLegendary)
  ]
  where inc lens' c = c & lens' %~ (+1)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

loop :: MVar Count -> IO ()
loop mVar = do
  count <- takeMVar mVar
  putStrLn ("Current Count:\n" <> show count)
  command <- fmap T.pack getLine
  clearScreen
  case lookup command commands of
    Just func -> do
      putMVar mVar (func count)
      loop mVar
    Nothing -> do
      putMVar mVar count
      putStrLn $ T.unpack ("Unknown command: " <> command)
      loop mVar

main :: IO ()
main = do
  clearScreen
  count <- newMVar emptyCount
  loop count
