module Lib
    ( GeneticParams(..),
      criaArquivoLogicE,
      criaArquivoGenetico,
      criaArquivoFenotipo
    ) where

import Data.List.Extra

data GeneticParams = GeneticParams {
    numIn :: Integer,
    numOut :: Integer,
    leNumIn :: Integer,
    numRows :: Integer,
    numCols :: Integer,
    funcoes :: [Bool]
    } deriving (Show)

numFuncs :: GeneticParams -> Integer
numFuncs genParams = toInteger $ length $ funcoes genParams

bitsFunc :: GeneticParams -> Integer
bitsFunc genParams = ceiling $ logBase 2 (fromIntegral $ numFuncs genParams)

numLes :: GeneticParams -> Integer
numLes genParams = numRows genParams * numCols genParams

numPinos :: GeneticParams -> Integer
numPinos genParams = numIn genParams + numLes genParams

bitsPinos :: GeneticParams -> Integer
bitsPinos genParams = ceiling $ logBase 2 (fromIntegral $ numPinos genParams)

bitsLeInputs :: GeneticParams -> Integer
bitsLeInputs genParams = leNumIn genParams * bitsPinos genParams

bitsLe :: GeneticParams -> Integer
bitsLe genParams = bitsLeInputs genParams + bitsFunc genParams

bitsLes :: GeneticParams -> Integer
bitsLes genParams = bitsLe genParams * numLes genParams

bitsOut :: GeneticParams -> Integer
bitsOut genParams = bitsPinos genParams * numOut genParams

bitsTotal :: GeneticParams -> Integer
bitsTotal genParams = bitsLes genParams + bitsOut genParams

criaArquivoLogicE :: GeneticParams -> String -> String
criaArquivoLogicE genParams = replace "#funcs" (geraFuncoes genParams)
                            . replace "#num_funcs_1" (show $ numFuncs genParams - 1)
                            . replace "#total_pinos" (show $ numPinos genParams - 1)
                            . replace "#bits_inputs" (show $ bitsLeInputs genParams - 1)
                            . replace "#bits_func" (show $ bitsFunc genParams - 1)

geraFuncoes :: GeneticParams -> String
geraFuncoes genParams =
   let todasFuncoes = ["and", "or", "xor", "not", "nand", "xnor", "nor", "buf"]
       funcoesUsadas = map snd $ filter fst (zip (funcoes genParams) todasFuncoes)
       funcModelo = "\t#func func#index(all_funcs[#index], #inputs);"
       replaceTudoLogicE (idx, func) = (replace "#inputs" (replaceInputs func)
                                      . replace "#index" (show idx)
                                      . replace "#func" func) funcModelo
       replaceInputs func =
           let inputModelo = "all_inputs[conf_ins[#cur_max:#cur_min]]"
               currentMax idx = (idx * bitsPinos genParams) - 1
               currentMin idx = currentMax idx - (bitsPinos genParams - 1)
               replaceInput idx = (replace "#cur_min" (show $ currentMin idx)
                                 . replace "#cur_max" (show $ currentMax idx)) inputModelo
        in
           if func == "buf" || func == "not" then
               replaceInput (leNumIn genParams)
           else
               intercalate ", " $ map replaceInput [leNumIn genParams, leNumIn genParams - 1..1]
     in
       intercalate "\n" $ zipWith (curry replaceTudoLogicE) [0..] funcoesUsadas

criaArquivoGenetico :: GeneticParams -> String -> String
criaArquivoGenetico genParams = replace "#all_inputs_for_out" geraOutputAssigns
                              . replace "#les" (geraLes genParams)
                              . replace "#num_pinos" (show $ numPinos genParams - 1)
                              . replace "#num_les_1" (show $ numLes genParams - 1)
                              . replace "#num_out" (show $ numOut genParams - 1)
                              . replace "#num_in" (show $ numIn genParams - 1)
                              . replace "#bits_pinos" (show $ bitsPinos genParams - 1)
                              . replace "#tam_le" (show $ bitsLe genParams - 1)
                              . replace "#r" (show $ numRows genParams - 1)
                              . replace "#c" (show $ numCols genParams - 1)
  where
    geraOutputAssigns =
      let outputModelo = "all_inputs[conf_outs[#idx]]"
      in
        intercalate ", " $ map (\idx -> replace "#idx" (show idx) outputModelo) [numOut genParams - 1, numOut genParams - 2..0]

geraLes :: GeneticParams -> String
geraLes genParams =
    let leModelo = intercalate "\n" ["logic_e le#r#c("
                                   , "\t.conf_func(conf_les[#r][#c][#bits_top:#bits_next]),"
                                   , "\t.conf_ins(conf_les[#bits_rest:0]),"
                                   , "\t.all_inputs(all_inputs),"
                                   , "\t.out(le_out[#n])"
                                   , ");\n"]
        replaceTudoLE idx =
            let col = idx `div` numRows genParams
                row = idx `mod` numRows genParams
                bitsTop = bitsFunc genParams + bitsLeInputs genParams
            in
              replace "#n" (show idx)
                . replace "#bits_rest" (show $ (bitsTop - bitsFunc genParams) - 1)
                . replace "#bits_next" (show $ bitsTop - bitsFunc genParams)
                . replace "#bits_top" (show $ bitsTop - 1)
                . replace "#r" (show row)
                . replace "#c" (show col)
    in
      intercalate "\n" $ map (`replaceTudoLE` leModelo) [0..numLes genParams - 1]

criaArquivoFenotipo :: GeneticParams -> String -> String
criaArquivoFenotipo genParams = replace "#bits_pinos_1" (show $ bitsPinos genParams - 1)
                              . replace "#num_outputs_1" (show $ numOut genParams - 1)
                              . replace "#bits_les_1" (show $ bitsLe genParams - 1)
                              . replace "#r" (show $ numRows genParams - 1)
                              . replace "#c" (show $ numCols genParams - 1)
                              . replace "#crom_translate_to_descrs" (geraAssociacoesCromossomo genParams)
                              . replace "#num_inputs_1" (show $ numIn genParams - 1)
                              . replace "#bits_total" (show $ bitsTotal genParams - 1)

geraAssociacoesCromossomo :: GeneticParams -> String
geraAssociacoesCromossomo genParams =
    let currentLEBot idx = idx * bitsLe genParams
        currentLETop idx = ((idx + 1) * bitsLe genParams) - 1
        currentOutBot idx = bitsLes genParams + (idx * bitsPinos genParams)
        currentOutTop idx = bitsLes genParams + (((idx + 1) * bitsPinos genParams) - 1)
        col idx = idx `div` numRows genParams
        row idx = idx `mod` numRows genParams
        replaceTudoLE idx = replace "#cur_le_bot" (show $ currentLEBot idx)
                          . replace "#cur_le_top" (show $ currentLETop idx)
                          . replace "#cur_c" (show $ col idx)
                          . replace "#cur_r" (show $ row idx)
        replaceTudoOut idx = replace "#cur_out_bot" (show $ currentOutBot idx)
                           . replace "#cur_out_top" (show $ currentOutTop idx)
                           . replace "#cur_idx_out" (show idx)
        associacoesLes =
            let leModelo = "\tassign descricao_les[#cur_r][#cur_c] = cromossomo[#cur_le_top:#cur_le_bot];"
            in
                intercalate "\n" $ map (`replaceTudoLE` leModelo) [0..numLes genParams - 1]
        associacoesOuts =
            let outModelo = "\tassign descricao_outs[#cur_idx_out] = cromossomo[#cur_out_top:#cur_out_bot];"
            in
                intercalate "\n" $ map (`replaceTudoOut` outModelo) [0..numOut genParams - 1]
    in
      associacoesLes ++ "\n\n" ++ associacoesOuts
