# BSUIRHaskell2015
Function programming BSUIR 2015

## lab1

### Build via Cabal
`cabal build`

### Command-line arguments
*     --csvcolsplitter=DELIM       Column delimiter (default=,)
*     --csvignorefirstcol=VAL      Ignore first column (True\False values is required) (default=False)
*     --csvignorelastcol=VAL       Ignore last column (True\False values is required) (default=True)
*     --csvignoreheader=VAL        Ignore header (True\False values is required) (default=False)
*     --inputfile=infile           Input filename
*     --outputfile=outfile         Output filename
*     --clastercount=count         Claster count
*     --precision=NUM              Precision
*     --distancetype=DISTANCETYPE  Distance type (Euclidean, Hamming) (default=Euclidian)
*     --expcoeff=NUM               m - exp coefficient
*     --israndmatrix               First action in FCM (True - generate matrix, False - generate centers)

#### Example
`lab1 --inputfile=butterfly.txt --outputfile=1.txt --clastercount=2`
`lab1 --inputfile=glass.txt --outputfile=2.txt --clastercount=7 --israndmatrix=False`
`lab1 --inputfile=irises.txt --clastercount=3 --israndmatrix=False --distancetype=Hamming`