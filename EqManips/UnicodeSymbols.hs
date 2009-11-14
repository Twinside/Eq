module EqManips.UnicodeSymbols where

------------------------------------
-- Miscellaneou mathematical symbols
------------------------------------
forAll :: Int
forAll    = 0x2200 {- ∀ -}

exist :: Int
exist     = 0x2203 {- ∃ -}

notExist :: Int
notExist  = 0x2204 {- ∄ -}

empty :: Int
empty     = 0x2205 {- ∅ -}

increment :: Int
increment = 0x2206 {- ∆ -}

nabla :: Int
nabla     = 0x2207 {- ∇ -}

-----------------------------------
-- Set membership
-----------------------------------
elementof :: Int
elementof      = 0x2208 {- ∈ -}

notelementof :: Int
notelementof   = 0x2209 {- ∉ -}

smallelementof :: Int
smallelementof = 0x220A {- ∊ -}

contains :: Int
contains       = 0x220b {- ∋ -}

smallcontains :: Int
smallcontains  = 0x220D {- ∍ -}


-----------------------------------
-- N-ary operators
----------------------------------
product :: Int
product   = 0x220F {- ∏ -}

coproduct :: Int
coproduct = 0x2210 {- ∐ -}

sum :: Int
sum       = 0x2211 {- ∑ -}


-----------------------------------
-- Simple operators
-----------------------------------
minus :: Int
minus          = 0x2212 {- − -}

multiplicationSign :: Int
multiplicationSign = 0x00D7 {- × -}

minusorplus :: Int
minusorplus    = 0x2213 {- ∓ -}

dotplus :: Int
dotplus        = 0x2214 {- ∔ -}

divsplash :: Int
divsplash      = 0x2215 {- ∕ -}

setminus :: Int
setminus       = 0x2216 {- ∖ -}

asterisk :: Int
asterisk       = 0x2217 {- ∗ -}

ring :: Int
ring           = 0x2218 {- ∘ -}

bullet :: Int
bullet         = 0x2219 {- ∙ -}

squareroot :: Int
squareroot     = 0x221A {- √ -}

cuberoot :: Int
cuberoot       = 0x221B {- ∛ -}

fouthroot :: Int
fouthroot      = 0x221C {- ∜ -}

proportionalto :: Int
proportionalto = 0x221D {- ∝ -}



-----------------------------------
-- Miscellaneous math symbols
-----------------------------------
infinity :: Int
infinity       = 0x221E {- ∞ -}

rightangle :: Int
rightangle     = 0x221F {- ∟ -}

angle :: Int
angle          = 0x2220 {- ∠ -}

measuredangle :: Int
measuredangle  = 0x2221 {- ∡ -}

sphericalangle :: Int
sphericalangle = 0x2222 {- ∢ -}


-----------------------------------
-- Operators 2 the return
-----------------------------------
divides :: Int
divides      = 0x2223 {- ∣ -}

doesntdivide :: Int
doesntdivide = 0x2224 {- ∤ -}

parrallelto :: Int
parrallelto  = 0x2225 {- ∥ -}

unparallelto :: Int
unparallelto = 0x2226 {- ∦ -}

--------------------------------------------------
----            Weird letters
--------------------------------------------------
doubleStruckItalicSmalld :: Int 
doubleStruckItalicSmalld = 0x2146

-----------------------------------
-- Logical and sets operators
-----------------------------------
logicalNot :: Int
logicalNot   = 0x00AC {- ¬ -}

logicalAnd :: Int
logicalAnd   = 0x2227 {- ∧ -}

logicalOr :: Int
logicalOr    = 0x2228 {- ∨ -}

intersection :: Int
intersection = 0x2229 {- ∩ -}

union :: Int
union        = 0x222A {- ∪ -}



-----------------------------------
-- Integrals
-----------------------------------
integral :: Int
integral                     = 0x222B {- ∫ -}

integralDouble :: Int
integralDouble               = 0x222C {- ∬ -}

integralTriple :: Int
integralTriple               = 0x222D {- ∭ -}

contourIntegral :: Int
contourIntegral              = 0x222E {- ∮ -}

surfaceIntegral :: Int
surfaceIntegral              = 0x222F {- ∯ -}

volumeIntegral :: Int
volumeIntegral               = 0x2230 {- ∰ -}

clockwiseIntegral :: Int
clockwiseIntegral            = 0x2231 {- ∱ -}

clockwiseCountourIntegral :: Int
clockwiseCountourIntegral    = 0x2232 {- ∲ -}

anticlockWiseContourIntegral :: Int
anticlockWiseContourIntegral = 0x2233 {- ∳ -}


-- Misc math symbols
therefor :: Int
therefor = 0x2234 {- ∴ -}

because :: Int
because  = 0x2235 {- ∵ -}


-- Relatioons
ratio :: Int
ratio      = 0x2236 {- ∶ -}


proportion :: Int
proportion = 0x2237 {- ∷ -}


-- operator
dotMinus :: Int
dotMinus = 0x2238 {- ∸ -}


-- Relation
excess :: Int
excess = 0x2239 {- ∹ -}


-- Operator
geometricProportion :: Int
geometricProportion = 0x223A {- ∺ -}


-----------------------------------
-- Relations
-----------------------------------
homothetic :: Int
homothetic    = 0x223B {- ∻ -}

tilde :: Int
tilde         = 0x223C {- ∼ -}

reversedTilde :: Int
reversedTilde = 0x223D {- ∽ -}

invertedLazys :: Int
invertedLazys = 0x223E {- ∾ -}


-- Misc math symbol
sineWave :: Int
sineWave = 0x223F {- ∿ -}


-- Operator
wreathProduct :: Int
wreathProduct             = 0x2240 {- ≀ -}

notTilde :: Int
notTilde                  = 0x2241 {- ≁ -}

minusTilde :: Int
minusTilde                = 0x2242 {- ≂ -}

asymEqualTo :: Int
asymEqualTo               = 0x2243 {- ≃ -}

notAsymEqualTo :: Int
notAsymEqualTo            = 0x2244 {- ≄ -}

aproxEqualTo :: Int
aproxEqualTo              = 0x2245 {- ≅ -}

aproxButNotEqualTo :: Int
aproxButNotEqualTo        = 0x2246 {- ≆ -}

neitherAproxNorEqual :: Int
neitherAproxNorEqual      = 0x2247 {- ≇ -}

almostEqual :: Int
almostEqual               = 0x2248 {- ≈ -}

notAlmostEqual :: Int
notAlmostEqual            = 0x2249 {- ≉ -}

almostEqualorEqual :: Int
almostEqualorEqual        = 0x224A {- ≊ -}

tripleTilde :: Int
tripleTilde               = 0x224B {- ≋ -}

allEqualTo :: Int
allEqualTo                = 0x224C {- ≌ -}

equavalent :: Int
equavalent                = 0x224D {- ≍ -}

geomEquiv :: Int
geomEquiv                 = 0x224E {- ≎ -}

diffBetween :: Int
diffBetween               = 0x224F {- ≏ -}

approachLimit :: Int
approachLimit             = 0x2250 {- ≐ -}

geomEqual :: Int
geomEqual                 = 0x2251 {- ≑ -}

aproxEqual :: Int
aproxEqual                = 0x2252 {- ≒ -}

imageOf :: Int
imageOf                   = 0x2253 {- ≓ -}

colonEquals :: Int
colonEquals               = 0x2254 {- ≔ -}

equalsColon :: Int
equalsColon               = 0x2255 {- ≕ -}

ringInEqual :: Int
ringInEqual               = 0x2256 {- ≖ -}

ringEqualTo :: Int
ringEqualTo               = 0x2257 {- ≗ -}

correspondsTo :: Int
correspondsTo             = 0x2258 {- ≘ -}

estimates :: Int
estimates                 = 0x2259 {- ≙ -}

equiangularTo :: Int
equiangularTo             = 0x225A {- ≚ -}

starEquals :: Int
starEquals                = 0x225B {- ≛ -}

deltaEqual :: Int
deltaEqual                = 0x225C {- ≜ -}

equalByDef :: Int
equalByDef                = 0x225D {- ≝ -}

measuredBy :: Int
measuredBy                = 0x225E {- ≞ -}

questionedEqualTo :: Int
questionedEqualTo         = 0x225F {- ≟ -}

notEqualTo :: Int
notEqualTo                = 0x2260 {- ≠ -}

identicalTo :: Int
identicalTo               = 0x2261 {- ≡ -}

notIdenticalTo :: Int
notIdenticalTo            = 0x2262 {- ≢ -}

strictlyEquivalentTo :: Int
strictlyEquivalentTo      = 0x2263 {- ≣ -}

lessThanOrEqualTo :: Int
lessThanOrEqualTo         = 0x2264 {- ≤ -}

greaterThanOrEqualTo :: Int
greaterThanOrEqualTo      = 0x2265 {- ≥ -}

lessThanOverEqualTo :: Int
lessThanOverEqualTo       = 0x2266 {- ≦ -}

greaterThanOverEqualTo :: Int
greaterThanOverEqualTo    = 0x2267 {- ≧ -}

lessThanButNotEqual :: Int
lessThanButNotEqual       = 0x2268 {- ≨ -}

greaterThanButnotEqualTo :: Int
greaterThanButnotEqualTo  = 0x2269 {- ≩ -}

muchLessThan :: Int
muchLessThan              = 0x226A {- ≪ -}

muchGreaterThan :: Int
muchGreaterThan           = 0x226B {- ≫ -}

between :: Int
between                   = 0x226C {- ≬ -}

notEquivalentTo :: Int
notEquivalentTo           = 0x226D {- ≭ -}

notLessThan :: Int
notLessThan               = 0x226E {- ≮ -}

notGreaterThan :: Int
notGreaterThan            = 0x226F {- ≯ -}

neitherLessThanNorEqualTo :: Int
neitherLessThanNorEqualTo = 0x2270 {- ≰ -}

subset :: Int
subset                    = 0x2282 {- ⊂ -}

superset :: Int
superset                  = 0x2283 {- ⊃ -}

notASubset :: Int
notASubset                = 0x2284 {- ⊄ -}

notASuperset :: Int
notASuperset              = 0x2285 {- ⊅ -}

subsetOrEqualTo :: Int
subsetOrEqualTo           = 0x2286 {- ⊆ -}

superSetOrEqual :: Int
superSetOrEqual           = 0x2287 {- ⊇ -}

neitherSubsetNorEqual :: Int
neitherSubsetNorEqual     = 0x2288 {- ⊈ -}

neitherSupersetNorEqual :: Int
neitherSupersetNorEqual   = 0x2289 {- ⊉ -}

subsetWithNotEqual :: Int
subsetWithNotEqual        = 0x228A {- ⊊ -}

supersetofWithNotEqual :: Int
supersetofWithNotEqual    = 0x228B {- ⊋ -}


-- operators
multiset :: Int
multiset      = 0x228C {- ⊌ -}

multisetMult :: Int
multisetMult  = 0x228D {- ⊍ -}

multisetUnion :: Int
multisetUnion = 0x228E {- ⊎ -}


-- greek letters
alpha :: Int
alpha = 0x03B1 {- α -}

beta :: Int
beta = 0x03B2 {- β -}

chi :: Int
chi = 0x03C7 {- χ -}

gamma :: Int
gamma = 0x3B3 {- γ -}

delta :: Int
delta = 0x03B4 {- δ -}

epslion :: Int
epslion = 0x03B6 {- ε -}

theta :: Int
theta = 0x3B8 {- θ -}

pi :: Int
pi = 0x03C0 {- π -}

rho :: Int
rho = 0x03C1 {- ρ -}

phi :: Int
phi = 0x03C6 {- φ -}

tau :: Int
tau = 0x03C4 {- τ -}

omega :: Int
omega = 0x03C9 {- ω -}

lambda :: Int
lambda = 0x03BB {- λ -}

sigma :: Int
sigma = 0x03C3 {- σ -}

mu :: Int
mu = 0x03BC {- μ -}

psi :: Int
psi = 0x03C8 {- ψ -}

xor :: Int
xor = 0x22BB {- ⊻ -}


-- Relation
{-
 = 0x228F {- ⊏ -}
 = 0x2290 {- ⊐ -}
 = 0x2291 {- ⊑ -}
 = 0x2292 {- ⊒ -}
 = 0x2293 {- ⊓ -}
 = 0x2294 {- ⊔ -}
 = 0x2295 {- ⊕ -}
 = 0x2296 {- ⊖ -}
 = 0x2297 {- ⊗ -}
 = 0x2298 {- ⊘ -}
 = 0x2299 {- ⊙ -}
 = 0x229A {- ⊚ -}
 = 0x229B {- ⊛ -}
 = 0x229C {- ⊜ -}
 = 0x229D {- ⊝ -}
 = 0x229E {- ⊞ -}
 = 0x229F {- ⊟ -}
 = 0x22A0 {- ⊠ -}
 = 0x22A1 {- ⊡ -}
 = 0x22A2 {- ⊢ -}
 = 0x22A3 {- ⊣ -}
 = 0x22A4 {- ⊤ -}
 = 0x22A5 {- ⊥ -}
 = 0x22A6 {- ⊦ -}
 = 0x22A7 {- ⊧ -}
 = 0x22A8 {- ⊨ -}
 = 0x22A9 {- ⊩ -}
 = 0x22AA {- ⊪ -}
 = 0x22AB {- ⊫ -}
 = 0x22AC {- ⊬ -}
 = 0x22AD {- ⊭ -}
 = 0x22AE {- ⊮ -}
 = 0x22AF {- ⊯ -}
 = 0x22B0 {- ⊰ -}
 = 0x22B1 {- ⊱ -}
 = 0x22B2 {- ⊲ -}
 = 0x22B3 {- ⊳ -}
 = 0x22B4 {- ⊴ -}
 = 0x22B5 {- ⊵ -}
 = 0x22B6 {- ⊶ -}
 = 0x22B7 {- ⊷ -}
 = 0x22B8 {- ⊸ -}
 = 0x22B9 {- ⊹ -}
 = 0x22BA {- ⊺ -}
 = 0x22BC {- ⊼ -}
 = 0x22BD {- ⊽ -}
 = 0x22BE {- ⊾ -}
 = 0x22BF {- ⊿ -}
 = 0x22C0 {- ⋀ -}
 = 0x22C1 {- ⋁ -}
 = 0x22C2 {- ⋂ -}
 = 0x22C3 {- ⋃ -}
 = 0x22C4 {- ⋄ -}
 = 0x22C5 {- ⋅ -}
 = 0x22C6 {- ⋆ -}
 = 0x22C7 {- ⋇ -}
 = 0x22C8 {- ⋈ -}
 = 0x22C9 {- ⋉ -}
 = 0x22CA {- ⋊ -}
 = 0x22CB {- ⋋ -}
 = 0x22CC {- ⋌ -}
 = 0x22CD {- ⋍ -}
 = 0x22CE {- ⋎ -}
 = 0x22CF {- ⋏ -}
 = 0x22D0 {- ⋐ -}
 = 0x22D1 {- ⋑ -}
 = 0x22D2 {- ⋒ -}
 = 0x22D3 {- ⋓ -}
 = 0x22D4 {- ⋔ -}
 = 0x22D5 {- ⋕ -}
 = 0x22D6 {- ⋖ -}
 = 0x22D7 {- ⋗ -}
 = 0x22D8 {- ⋘ -}
 = 0x22D9 {- ⋙ -}
 = 0x22DA {- ⋚ -}
 = 0x22DB {- ⋛ -}
 = 0x22DC {- ⋜ -}
 = 0x22DD {- ⋝ -}
 = 0x22DE {- ⋞ -}
 = 0x22DF {- ⋟ -}
 = 0x22E0 {- ⋠ -}
 = 0x22E1 {- ⋡ -}
 = 0x22E2 {- ⋢ -}
 = 0x22E3 {- ⋣ -}
 = 0x22E4 {- ⋤ -}
 = 0x22E5 {- ⋥ -}
 = 0x22E6 {- ⋦ -}
 = 0x22E7 {- ⋧ -}
 = 0x22E8 {- ⋨ -}
 = 0x22E9 {- ⋩ -}
 = 0x22EA {- ⋪ -}
 = 0x22EB {- ⋫ -}
 = 0x22EC {- ⋬ -}
 = 0x22ED {- ⋭ -}
 = 0x22EE {- ⋮ -}
 = 0x22EF {- ⋯ -}
 = 0x22F0 {- ⋰ -}
 = 0x22F1 {- ⋱ -}
 = 0x22F2 {- ⋲ -}
 = 0x22F3 {- ⋳ -}
 = 0x22F4 {- ⋴ -}
 = 0x22F5 {- ⋵ -}
 = 0x22F6 {- ⋶ -}
 = 0x22F7 {- ⋷ -}
 = 0x22F8 {- ⋸ -}
 = 0x22F9 {- ⋹ -}
 = 0x22FA {- ⋺ -}
 = 0x22FB {- ⋻ -}
 = 0x22FC {- ⋼ -}
 = 0x22FD {- ⋽ -}
 = 0x22FE {- ⋾ -}
 = 0x22FF {- ⋿ -}
-}
{-
Dump for others chars, to lazy to prepare them    
 = 0x2271 {- ≱ -}
 = 0x2272 {- ≲ -}
 = 0x2273 {- ≳ -}
 = 0x2274 {- ≴ -}
 = 0x2275 {- ≵ -}
 = 0x2276 {- ≶ -}
 = 0x2277 {- ≷ -}
 = 0x2278 {- ≸ -}
 = 0x2279 {- ≹ -}
 = 0x227A {- ≺ -}
 = 0x227B {- ≻ -}
 = 0x227C {- ≼ -}
 = 0x227D {- ≽ -}
 = 0x227E {- ≾ -}
 = 0x227F {- ≿ -}
 = 0x2280 {- ⊀ -}
 = 0x2281 {- ⊁ -}

 --}

