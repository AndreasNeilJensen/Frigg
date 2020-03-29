-- Program by Andreas Neil Jensen, All rights reserved.
import Data.List
import Control.Arrow

-- Jeg laver min egen datatype kaldt "HuffmanTree".
data HuffmanTree = Leaf Char Int | Branch HuffmanTree HuffmanTree Int deriving Show

-- Sådan her forestiller jeg mig at min codemap skal se ud.
type Codemap = [(Char,[Char])]

-- En funktion der returner vægten enten af en leaf node eller en branch node.
weight :: HuffmanTree -> Int
weight (Leaf _ w) = w
weight (Branch _ _ w) = w

-- Denne funktion merger to Huffman trees sammen til en Branch node.
merge :: HuffmanTree -> HuffmanTree -> HuffmanTree
merge tree1 tree2 = Branch tree1 tree2 ((weight tree1) + (weight tree2))

-- Denne funktion omdanner en tekststreng til en array af pairs indeholdene de forskellige chars der udgør tekststrengen samt antallet af forekomster.
-- Funktionen opnår dette ved at gruppere og sortere alle tekststrengens chars således at man kan bruge forekomsten af en bestemt char samt længden af 
-- denne forekomst til at bestemme hvor mange af den pågældene char der er brugt i tekststrengen. 
-- Map bliver derefter brugt til at skabe en liste der er resultatet af den anonyme funktion kaldt på chararrayen.
frequencies :: String -> [(Char, Int)]
frequencies inputString = 
    map (\x -> (head x, length x)) (group (sort inputString))    

-- Funktionen omdanner en char, int pair array, til et huffman træ.
-- en (char,int) array bliver sorted efter weight og omdannet til en array af huffman træer der derefter bliver brugt i funktionen 'build'
-- i build merger vi de forskellige leafs lavet i det forrige trin efter laveste weight rekursivt indtil der kun er ét huffman træ tilbage.
buildTree :: [(Char, Int)] -> HuffmanTree
buildTree = build . map (uncurry Leaf) . sortBy (\(_,w1) (_,w2) -> compare w1 w2)
    where   build [] = error "Dude... why would you attempt to build a tree from a empty string??? :D" 
            build (remainingTrees:[]) = remainingTrees
            build (firstTree:secondTree:remainingTrees) = build (insertBy (\a1 a2 -> compare (weight a1) (weight a2)) (merge firstTree secondTree) remainingTrees)

-- buildCodemap tager et huffmantræ og traverser den alt imens den mapper venstre og højre traversals 
-- med henholdsvist '0' og '1' bits for at symbolisere bit repræsentationen af de enkelte leafs igennem huffman coding.
buildCodemap :: HuffmanTree -> Codemap
buildCodemap (Leaf char _) = [(char,[])]
buildCodemap (Branch leftTree rightTree _) = map (second ('0' :)) (buildCodemap leftTree) ++ map (second ('1' :)) (buildCodemap rightTree)

-- konvertere strings til en koderepræsentation ved hjælp af et genereret codemap.
-- convertStringToCode tager en string og et codemap som parametre som den bruger 
-- til at kalde sig selv rekursivt over alle de forskellige chars i den pågældene string.
convertStringToCode :: Eq t => [t] -> [(t, [a])] -> [a]
convertStringToCode [] _ = [] -- done! :D
convertStringToCode (x:xs) codemap = getValueFromKey x codemap ++ convertStringToCode xs codemap 

-- sammenligner rekursivt den givende char op imod alle instanserne af char string pairs i den givne codemap.. Finder den et match returnere 
-- funktionen den tilsvarende code som det så er meningen at convertStringToCode bruger i sin rekursion.
getValueFromKey :: Eq t => t -> [(t, p)] -> p
getValueFromKey _ [] = error "Bro.. you have to use a compatible codemap (and/or string) when encoding... Obviously :D" -- samme grund som før..
getValueFromKey match ((c, string) : remainingKeyValuePairs) = (if (c == match) then string else getValueFromKey match remainingKeyValuePairs)

-- funktionen er bare simpel syntaktisk sukker (som Kurt ville have sagt) for at gøre encoding af strings mere lækkert at skrive.
encode :: [Char] -> [Char]
encode inputString = convertStringToCode inputString $ buildCodemap $ buildTree $ frequencies inputString

-- funktionen konvertere rekursivt en kodestring til dens originale betydning ved hjælp af et Huffmantræ og kodestrengen.
-- den traverser huffmantræet alt efter hvordan kodestrengen ser ud, indtil den når en leaf node hvor den så enten kalder sig selv
-- rekursivt eller er færdig med at operere.
decode :: HuffmanTree -> [Char] -> [Char]
decode huffmanTree = convert huffmanTree
    where   convert (Leaf c _) []           = [c] 
            convert (Leaf c _) bs           = c : convert huffmanTree bs 
            convert (Branch _ _ _) []       = error "You need to use a compatible tree (and/or codestring) when decoding.. duh!" 
            convert (Branch l r _) (b:bs)   = convert (if b == '0' then l else r) bs 
