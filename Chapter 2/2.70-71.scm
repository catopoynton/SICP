(load "huffman.scm")
(display (length (encode sha-na tree)))

;84 bits are required to encode this part of the song. As there are a elements in our alphabet, each element can be represented by a unique 3bit code (as 2^3 = 8)
;As there are 36 elements in the song, we would requre 36*3 = 108 bits to represent it using an fixed length encoding.  