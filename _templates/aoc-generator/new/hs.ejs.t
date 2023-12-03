---
to: "<%= typeof day !== 'undefined' && day !== null ? `src/Day${String(day).padStart(2, '0')}/${name}.hs` : null %>"
sh: cd <%= cwd %>/<%= `src/Day${String(day).padStart(2, '0')}` %> && touch input.txt && touch test.txt
---
-- Day <%=day%>: <%=name%>

module <%= `Day${String(day).padStart(2,0)}.${name}` %> (solve) where

import Text.Megaparsec
import Text.Megaparsec.Char

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    putStr contents