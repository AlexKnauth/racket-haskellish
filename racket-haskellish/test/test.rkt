#lang racket-haskellish

;; Stuff from Learn You a Haskell for Great Good! by Miran Lipovaca

;; So, What's Haskell?
def xs = [1,2,3,4,5,6,7,8]
def doubleMe_ish xs = map (*2) xs
doubleMe_ish (doubleMe_ish (doubleMe_ish xs))
def a = 5 + 4
;; Chapter 1. Starting Out
2 + 15
49 * 100
1892 - 1472
5 / 2
(50 * 100) - 4999 ; TODO precedence or no precendence?
50 * (100 - 4999)
5 * (-3)
True && False
True && True
False || True
not False
not (True && True)
5 == 5
1 == 0
5 /= 5
5 /= 4
"hello" == "hello"
True == 5 ; TODO this produces false, but Haskell would throw a type error here
5 + 4.0
;; Calling Functions
succ 8
min 9 10
min 3.4 3.2
max 100 101
(succ 9 + max 5 4) + 1 ; TODO
((succ 9) + (max 5 4)) + 1 ; TODO
succ 9 * 10 ; equivalent to ((succ 9) * 10)
succ (9 * 10)
div 92 10
92 `div` 10
def bar x = x + 1
bar (bar 3)
bar(bar(3))
;; Baby's First Functions
def doubleMe x = x + x
doubleMe 9
doubleMe 8.3
def doubleUs x y = (x * 2) + (y * 2) ; TODO precedence or no precedence?
doubleUs 4 9
doubleUs 2.3 34.2
doubleUs 28 88 + doubleMe 123
def doubleUs' x y = doubleMe x + doubleMe y
doubleUs' 4 9
doubleUs' 2.3 34.2
doubleUs' 28 88 + doubleMe 123
def doubleSmallNumber x = if x > 100
                            then x
                            else x*2
doubleSmallNumber 21
doubleSmallNumber 100
doubleSmallNumber 101
def doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
doubleSmallNumber' 21
doubleSmallNumber' 100
doubleSmallNumber' 101
def conanO'Brien = "It's a-me, Conan O'Brien!"
conanO'Brien
conanO'Brien == "It's a-me, Conan O'Brien!"
;; An Intro to Lists
def lostNumbers = [4,8,15,16,23,42]
lostNumbers
[2+2,3*3,4^4]
;; Concatenation
[1,2,3,4] ++ [9,10,11,12]
("hello" ++ " ") ++ "world"
['w','o'] ++ ['o','t']
"hello" == ['h','e','l','l','o']
'A':" SMALL CAT"
5:[1,2,3,4,5]
[1,2,3,4] ++ [5]
[1,2,3] == (1:(2:(3:[]))) ; TODO get rid of some of these parens
[] /= [[]]
;; Accessing List Elements
"Steve Buscemi" !! 6
[9.4,33.2,96.2,11.2,23.25] !! 1
;; Lists Inside Lists
def b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
b
b ++ [[1,1,1,1]]
[6,6,6]:b
b !! 2
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
head [5,4,3,2,1]
tail [5,4,3,2,1]
last [5,4,3,2,1]
init [5,4,3,2,1]
length [5,4,3,2,1]
null [1,2,3]
null []
reverse [5,4,3,2,1]
take 3 [5,4,3,2,1]
take 1 [3,9,3]
take 5 [1,2]
take 0 [6,6,6]
drop 3 [8,4,2,1,5,6]
drop 0 [1,2,3,4]
drop 100 [1,2,3,4]
minimum [8,4,2,1,5,6]
maximum [1,9,2,3,4]
sum [5,2,1,6,3,2,5,7]
product [6,2,1,2]
product [1,2,5,6,7,9,2,0]
4 `elem` [3,4,5,6]
10 `elem` [3,4,5,6]
;; Texas Ranges
[1..20]
;['a'..'z']
;['K'..'Z']
[2,4..20]
[3,6..20]
[0.1, 0.3 .. 1] ; probably not what you wanted
[13,26..(24*13)]
take 24 [13,26..]
take 10 (cycle [1,2,3])
take 12 (cycle "LOL ")
take 10 (repeat 5)
replicate 3 10
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

