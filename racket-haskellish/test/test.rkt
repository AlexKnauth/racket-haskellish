#lang racket-haskellish

require ⫶racket-haskellish/test/chk⫶

;; Stuff from Learn You a Haskell for Great Good! by Miran Lipovaca

;; So, What's Haskell?
def xs = [1,2,3,4,5,6,7,8]
def doubleMe_ish xs = map (*2) xs
chk doubleMe_ish (doubleMe_ish (doubleMe_ish xs)) = [8,16..64]
def a = 5 + 4
;; Chapter 1. Starting Out
chk 2 + 15 = 17
chk 49 * 100 = 4900
chk 1892 - 1472 = 420
chk 5 / 2 = 2 + (1/2)
chk (50 * 100) - 4999 = 1 ; TODO precedence or no precendence?
chk 50 * (100 - 4999) = -244950
chk 5 * (-3) = -15
chk True && False = False
chk True && True = True
chk False || True = True
chk not False = True
chk not (True && True) = False
chk 5 == 5 = True
chk 1 == 0 = False
chk 5 /= 5 = False
chk 5 /= 4 = True
chk "hello" == "hello" = True
chk True == 5 = False ; TODO this produces false, but Haskell would throw a type error here
chk 5 + 4.0 = 9.0
;; Calling Functions
chk succ 8 = 9
chk min 9 10 = 9
chk min 3.4 3.2 = 3.2
chk max 100 101 = 101
chk (succ 9 + max 5 4) + 1 = 16 ; TODO
chk ((succ 9) + (max 5 4)) + 1 = 16 ; TODO
chk succ 9 * 10 = 100 ; equivalent to ((succ 9) * 10)
chk succ (9 * 10) = 91
chk div 92 10 = 9
chk 92 `div` 10 = 9
def bar x = x + 1
chk bar (bar 3) = 5
chk bar(bar(3)) = 5
;; Baby's First Functions
def doubleMe x = x + x
chk doubleMe 9 = 18
chk doubleMe 8.3 = 16.6
def doubleUs x y = (x * 2) + (y * 2) ; TODO precedence or no precedence?
chk doubleUs 4 9 = 26
chk doubleUs 2.3 34.2 = 73.0
chk doubleUs 28 88 + doubleMe 123 = 478
def doubleUs' x y = doubleMe x + doubleMe y
chk doubleUs' 4 9 = 26
chk doubleUs' 2.3 34.2 = 73.0
chk doubleUs' 28 88 + doubleMe 123 = 478
def doubleSmallNumber x = if x > 100
                            then x
                            else x*2
chk doubleSmallNumber 21 = 42
chk doubleSmallNumber 100 = 200
chk doubleSmallNumber 101 = 101
def doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
chk doubleSmallNumber' 21 = 43
chk doubleSmallNumber' 100 = 201
chk doubleSmallNumber' 101 = 102
def conanO'Brien = "It's a-me, Conan O'Brien!"
chk conanO'Brien = "It's a-me, Conan O'Brien!"
chk conanO'Brien == "It's a-me, Conan O'Brien!" = True
;; An Intro to Lists
def lostNumbers = [4,8,15,16,23,42]
chk lostNumbers = [4,8,15,16,23,42]
chk [2+2,3*3,4^4] = [4,9,256]
;; Concatenation
chk [1,2,3,4] ++ [9,10,11,12] = [1,2,3,4,9,10,11,12]
chk ("hello" ++ " ") ++ "world" = "hello world"
chk ['w','o'] ++ ['o','t'] = "woot"
chk "hello" == ['h','e','l','l','o'] = True
chk 'A':" SMALL CAT" = "A SMALL CAT"
chk 5:[1,2,3,4,5] = [5,1,2,3,4,5]
chk [1,2,3,4] ++ [5] = [1,2,3,4,5]
chk [1,2,3] == (1:(2:(3:[]))) = True ; TODO get rid of some of these parens
chk [] /= [[]] = True
;; Accessing List Elements
chk "Steve Buscemi" !! 6 = 'B'
chk [9.4,33.2,96.2,11.2,23.25] !! 1 = 33.2
;; Lists Inside Lists
def b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
chk b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
chk b ++ [[1,1,1,1]] = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
chk [6,6,6]:b = [[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
chk b !! 2 = [1,2,2,3,4]
;; Comparing Lists
;; TODO implement generic comparison functions
;[3,4,2] < [3,4,3] ; True
;ghci> [3,2,1] > [2,1,0]  
;True  
;ghci> [3,2,1] > [2,10,100]  
;True  
;ghci> [3,4,2] > [3,4]  
;True  
;ghci> [3,4,2] > [2,4]  
;True  
;ghci> [3,4,2] == [3,4,2]  
;True
;Also, a nonempty list as always considered to be greater than an empty one.
;; More List Operations
chk head [5,4,3,2,1] = 5
chk tail [5,4,3,2,1] = [4,3,2,1]
chk last [5,4,3,2,1] = 1
chk init [5,4,3,2,1] = [5,4,3,2]
chk length [5,4,3,2,1] = 5
chk null [1,2,3] = False
chk null [] = True
chk reverse [5,4,3,2,1] = [1,2,3,4,5]
chk take 3 [5,4,3,2,1] = [5,4,3]
chk take 1 [3,9,3] = [3]
chk take 5 [1,2] = [1,2]
chk take 0 [6,6,6] = []
chk drop 3 [8,4,2,1,5,6] = [1,5,6]
chk drop 0 [1,2,3,4] = [1,2,3,4]
chk drop 100 [1,2,3,4] = []
chk minimum [8,4,2,1,5,6] = 1
chk maximum [1,9,2,3,4] = 9
chk sum [5,2,1,6,3,2,5,7] = 31
chk product [6,2,1,2] = 24
chk product [1,2,5,6,7,9,2,0] = 0
chk 4 `elem` [3,4,5,6] = True
chk 10 `elem` [3,4,5,6] = False
;; Texas Ranges
chk [1..20] = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
;['a'..'z']
;['K'..'Z']
chk [2,4..20] = [2,4,6,8,10,12,14,16,18,20]
chk [3,6..20] = [3,6,9,12,15,18]
[0.1, 0.3 .. 1] ; probably not what you wanted
chk [13,26..(24*13)] = [13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
chk take 24 [13,26..] = [13,26..(24*13)]
chk take 10 (cycle [1,2,3]) = [1,2,3,1,2,3,1,2,3,1]
chk take 12 (cycle "LOL ") = "LOL LOL LOL "
chk take 10 (repeat 5) = [5,5,5,5,5,5,5,5,5,5]
chk replicate 3 10 = [10,10,10]
;; I'm a list compression
[x*2 | x <- [1..10]]
[x*2 | x <- [1..10], (x*2) >= 12] ; should be [12,14,16,18,20]
[ x | x <- [50..100], (x `mod` 7) == 3] ; should be [52,59,66,73,80,87,94]
def boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
boomBangs [7..13] ; should be ["BOOM!","BOOM!","BANG!","BANG!"]
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19] ; should be [10,11,12,14,16,17,18,20]
[ x*y | x <- [2,5,10], y <- [8,10,11]] ; should be [16,20,22,40,50,55,80,100,110]
[ x*y | x <- [2,5,10], y <- [8,10,11], (x*y) > 50] ; should be [55,80,100,110]
def nouns = ["hobo","frog","pope"]  
def adjectives = ["lazy","grouchy","scheming"]  
;[adjective ++ (" " ++ noun) | adjective <- adjectives, noun <- nouns]
; should be:
;["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",  
;"grouchy pope","scheming hobo","scheming frog","scheming pope"]
def length' xs = sum [1 | _ <- xs]
;def removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
;removeNonUppercase "Hahaha! Ahahaha!" ; should be "HA"  
;removeNonUppercase "IdontLIKEFROGS" ; should be "ILIKEFROGS"
def xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
[ [ x | x <- xs, even x ] | xs <- xxs] ; should be [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
;; Tuples
(1, 3)
(3, 'a', "hello")
(50, 50.4, "hello", 'b')
;; Using Tuples
[[1,2],[8,11],[4,5]]
[[1,2],[8,11,5],[4,5]] ; probably not what you wanted
[(1,2),(8,11),(4,5)]
("Christopher", "Walken", 55)
;TODO implement generic <
;(3,2,1) < (3,2,2)
;; Using Pairs
fst (8, 11)
fst ("Wow", False)
snd (8, 11)
snd ("Wow", False)
zip [1,2,3,4,4] [5,5,5,5,5]
zip [1..5] ["one","two","three","four","five"]
zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
zip [1..] ["apple","orange","cherry","mango"]
;; Finding the Right Triangle
def triples = [(a,b,c) | c <- [1..10], a <- [1..10], b <- [1..10]]
def triples' = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a]]
def rightTriangles = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a],
                                ((a^2) + (b^2)) == (c^2)]
def rightTriangles' = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a],
                                 ((a^2) + (b^2)) == (c^2),
                                 ((a+b)+c) == 24]
rightTriangles'

