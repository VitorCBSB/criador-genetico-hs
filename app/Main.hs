module Main where

import Lib
import System.Environment

main :: IO ()
main = getArgs >>= actualMain

actualMain :: [String] -> IO ()
actualMain [numInArg, numOutArg, leNumInArg, numRowsArg, numColsArg] = 
    let numIn = read numInArg
        numOut = read numOutArg
	leNumIn = read leNumInArg
	numRows = read numRowsArg
	numCols = read numColsArg
	genParams = GeneticParams numIn numOut leNumIn numRows numCols [True, True, True, True, True, True, True, True]
    in
      do
        arquivoLogicE <- readFile "logic_e_modelo"
        arquivoGenetico <- readFile "genetico_modelo"
        arquivoFenotipo <- readFile "fenotipo_modelo"

	writeFile "logic_e.v" (criaArquivoLogicE genParams arquivoLogicE)
	writeFile "genetico.v" (criaArquivoGenetico genParams arquivoGenetico)
	writeFile "fenotipo.v" (criaArquivoFenotipo genParams arquivoFenotipo)

actualMain _ = do
    nome <- getProgName
    putStrLn $ "uso: " ++ nome ++ " <num_in> <num_out> <le_num_in> <num_rows> <num_cols>"

