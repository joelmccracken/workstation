{- text based table generation
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Table where

type Table = [[String]]

-- | A table with a header that is set off with lines under each
-- header item.
tableWithHeader :: [String] -> [[String]] -> Table
tableWithHeader header rows = header : map linesep header : rows
  where
	linesep = map (const '-')

-- | Formats a table to lines, automatically padding columns to the same size.
formatTable :: Table -> [String]
formatTable table = map (\r -> unwords (map pad (zip r colsizes))) table
  where
	pad (cell, size) = cell ++ take (size - length cell) padding
	padding = repeat ' '
	colsizes = reverse $ (0:) $ drop 1 $ reverse $
		sumcols (map (map length) table)
	sumcols [] = repeat 0
	sumcols [r] = r
	sumcols (r1:r2:rs) = sumcols $ zipWith max r1 r2 : rs
