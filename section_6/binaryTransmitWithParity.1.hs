import Data.Char

type Bit = Int
type BitP = Int

--1011 = 13
--1248
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w, b) <- zip weights bits]
    where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[BitP]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [BitP] -> [BitP]
checkParity bits | isCorrectParity = bits
                 | otherwise = error "Parity bit ERROR"
                 where isCorrectParity = getParityBit bits == createParityBit (dropParity bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . dropParity . checkParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

createParityBit :: [Bit] -> Bit
createParityBit bits | odd (sum bits) = 1
                     | otherwise = 0

addParityBit :: [Bit] -> [BitP]
addParityBit bits = (createParityBit bits) : bits

getParityBit :: [BitP] -> Bit
getParityBit bits = head bits

dropParity :: [BitP] -> [Bit]
dropParity bits = tail bits
