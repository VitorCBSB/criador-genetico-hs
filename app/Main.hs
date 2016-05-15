module Main where

import Lib
import System.Environment
import Options.Applicative

main :: IO ()
main = execParser opts >>= actualMain
    where
        opts = info (helper <*> genParams)
            (  fullDesc
            <> progDesc "Generate a logic element grid for genetic circuits in Verilog.")

validate validation value =
    if validation value then
        return value
    else
        readerError "Validation failed. Re-run with -h for info."

genParams :: Parser GeneticParams
genParams = GeneticParams
    <$> option (auto >>= validate (>= 0))
        (  long "numIn"
        <> short 'i'
        <> help "Input quantity for the genetic circuit. Must be at least 0.")
    <*> option (auto >>= validate (>= 1))
        (  long "numOut"
        <> short 'o'
        <> help "Output quantity for the genetic circuit. Must be at least 1.")
    <*> option (auto >>= validate (>= 2))
        (  long "leNumIn"
        <> short 'l'
        <> help "Amount of inputs for the basic logic element. Must be at least 2.")
    <*> option (auto >>= validate (>= 1))
        (  long "numRows"
        <> short 'r'
        <> help "Number of rows for the genetic logic element grid. Must be at least 1.")
    <*> option (auto >>= validate (>= 1))
        (  long "numCols"
        <> short 'c'
        <> help "Number of columns for the genetic logic element grid. Must be at least 1.")
    <*>
        pure [True, True, True, True, True, True, True, True]

actualMain :: GeneticParams -> IO ()
actualMain geneticParams =
    do
        arquivoLogicE <- readFile "logic_e_modelo"
        arquivoGenetico <- readFile "genetico_modelo"
        arquivoFenotipo <- readFile "fenotipo_modelo"

        writeFile "logic_e.v" (criaArquivoLogicE geneticParams arquivoLogicE)
        writeFile "genetico.v" (criaArquivoGenetico geneticParams arquivoGenetico)
        writeFile "fenotipo.v" (criaArquivoFenotipo geneticParams arquivoFenotipo)
