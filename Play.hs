module Play where 

import Data.Array.IO
import Data.Array.MArray
import Data.List ((\\))
import Control.Monad 
import Data.Bits
import Data.Word

import System.Random
import Color 
import Command

--全体の速度に関わるもの(1分を越える場合)

--終盤の開始
last_start = 54
--中盤の読む深さ
mid_te = 5

-- 番兵付きの10x10配列
type Board = IOUArray (Int,Int) Int 

initBoard :: IO Board 
initBoard = 
    do board <- newArray ( (0,0), (9,9) ) none
       mapM_ (\i ->
                  do writeArray board (i,0) sentinel 
                     writeArray board (i,9) sentinel
                     writeArray board (0,i) sentinel 
                     writeArray board (9,i) sentinel) [0..9]
       writeArray board (4,4) white 
       writeArray board (4,5) black 
       writeArray board (5,4) black 
       writeArray board (5,5) white 
       return board 

isValidMove :: Board -> Color -> (Int,Int) -> IO Bool 
isValidMove board color (i,j) =
    do e <- readArray board (i,j) 
       if e == none then 
           isEffective board color (i,j)
       else
           return False 
-- 8方向
dirs = [ (i,j) | i <- [1,0,-1], j <- [1,0,-1] ] \\ [(0,0)]

-- 石を置いたときに、ひっくり返せるかどうか
isEffective :: Board -> Color -> (Int,Int) -> IO Bool
isEffective board color (i,j) = 
    do ms <- flippableIndices board color (i,j)
       return $ not $ null ms

-- 石を置いたときにひっくり返えるところ
flippableIndices :: Board -> Color -> (Int,Int) -> IO [(Int,Int)]
flippableIndices board color (i,j) = 
    do bs <- mapM (\(di,dj) -> flippableIndicesLine board color (di,dj) (i+di,j+dj)) dirs
       return $ concat bs 

flippableIndicesLine board color (di,dj) (i,j) =
    checkLine (di,dj) (i,j) []
    where                    
      ocolor = oppositeColor color
      checkLine (di,dj) (i,j) r =
          do c <- readArray board (i,j) 
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else 
                 return []
      checkLine' (di,dj) (i,j) r =
          do c <- readArray board (i,j) 
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else if c == color then 
                      return r 
                  else
                      return []

{- 
   boardをそのまま返すのは一般には危険。
   だが、今のdoMoveの使用方法ならば問題ない。
-}
doMove :: Board -> Mv -> Color -> IO Board 
doMove board GiveUp  color = return board
doMove board Pass    color = return board
doMove board (M i j) color =       
    do ms <- flippableIndices board color (i,j)
       mapM_ (\(ii,jj) -> writeArray board (ii,jj) color) ms 
       writeArray board (i,j) color 
       return board 
  
-- 合法手
validMoves :: Board -> Color -> IO [ (Int,Int) ]
validMoves board color =
     filterM (isValidMove board color) 
             [ (i,j) | i <- [1..8], j <- [1..8]]


-- 石の数
count :: Board -> Color -> IO Int 
count board color =
    do is <- filterM (\i -> 
                          do e <- readArray board i 
                             return $ e == color) 
                     [ (i,j) | i <- [1..8], j <- [1..8]]
       return $ length is


-- 盤面の出力
putBoard :: Board -> IO ()
putBoard board = 
    do putStrLn " |A B C D E F G H " 
       putStrLn "-+----------------"
       mapM_ putBoardLine [1..8]
       putStrLn "  (X: Black,  O: White)"
    where
      putC c | c == none  = putStr " " 
             | c == white = putStr "O"
             | c == black = putStr "X"
      putBoardLine j =
          do putStr $ show j ++ "|"
             mapM_ (\i -> do e <- readArray board (i,j) 
                             putC e >> putStr " ") [1..8]
             putStrLn ""



--ここから作成----------------------------------

type MyBoard = (Word64,Word64)

play :: Board -> Color -> IO Mv 
play board color =
	do	{ms <- validMoves board color;
			cnt1 <- count board white;
			cnt2 <- count board black;
			case ms of 
				{[] -> return Pass;
				_  ->
					if cnt1 + cnt2 <= 4 then
						do {k <- getStdRandom $ randomR (0, length ms-1);
							let (i,j) = ms !! k in
								return $ M i j}
					else if cnt1 + cnt2 >= last_start then
						play_last board color
					else if cnt1 + cnt2 >= 15 then
						play_mid ms board color
					else
						play_fst board color}}

--序盤
play_fst :: Board -> Color -> IO Mv
play_fst board color = let 
	ocolor = oppositeColor color
	ilis = [(i,j) | i <- [1..8], j <- [1..8]]
	scoring myboard = (-1) * (length (validMoves' myboard ocolor)) + (foldr (\(i,j) sum -> let {e = readmyboard myboard (i,j);
																							sc = if e /= color then 0 else score_atom (i,j)} in
																								sc + sum) 0 ilis)
	in do {myboard <- board2myboard board;
			let ((ni,nj),sc'') = foldr (\(i,j) ((ii,jj),sc) -> let sc' = scoring (myMove myboard (M i j) color) in
																			if sc' <= sc then ((i,j),sc') else ((ii,jj),sc)) ((0,0),-1) (validMoves' myboard color) in
				return $ M ni nj}

--中盤
play_mid :: [(Int,Int)] -> Board -> Color -> IO Mv
play_mid ms board color = let
	ocolor = oppositeColor color
	ilis = [(i,j) | i <- [1..8], j <- [1..8]]
	mid_te' = if length ms <= 7 then mid_te else
							if length ms <= 15 then mid_te - 1 else mid_te - 2
	score myboard color' num cut =
		let ms' = validMoves' myboard color' in
					if num == 0 then scoring myboard else
							if color' == color then
								if ms' == [] then (-3) + score myboard (oppositeColor color') (num-1) (-512) else
									fst $ foldr (\(i,j) (sc,flag) -> let sc' = score (myMove myboard (M i j) color) ocolor (num-1) sc in
																			if flag == 1 then (sc,1) else
																				if sc' >= cut then (sc',1) else
																					if sc' >= sc then (sc',0) else (sc,0)) (-512,0) ms'
							else
								if ms' == [] then 1 + score myboard (oppositeColor color') (num-1) 128 else
									fst $ foldr (\(i,j) (sc,flag) -> let sc' = score (myMove myboard (M i j) ocolor) color (num-1) sc in
																			if flag == 1 then (sc,1) else
																				if sc' <= cut then (sc',1) else 
																					if sc' <= sc then (sc',0) else (sc,0)) (128,0) ms'
	scoring myboard = foldr (\(i,j) sum -> let {e = readmyboard myboard (i,j);
																							sc = if e /= color then 0 else score_atom (i,j)} in
																								sc + sum) 0 ilis
	in do {myboard <- board2myboard board;putStr("depth:");print(mid_te');
			let ((ni,nj),sc'') = foldr (\(i,j) ((ii,jj),sc) -> let sc' = score (myMove myboard (M i j) color) ocolor mid_te' (-512) in
																			if sc' >= sc then ((i,j),sc') else ((ii,jj),sc)) ((0,0),-512) (validMoves' myboard color) in
				return $ M ni nj}


--終盤
play_last :: Board -> Color -> IO Mv
play_last board color = let
	ocolor = oppositeColor color
	score myboard color' cut =
		let {ms' = validMoves' myboard color';
				 ms'' = validMoves' myboard (oppositeColor color');
				 cnt = mycount myboard color} in
					if ms' == [] && ms'' == [] then cnt else
						if color' == color then
							if ms' == [] then score myboard (oppositeColor color') 0 else
								fst $ foldr (\(i,j) (sc,flag) -> let sc' = (score (myMove myboard (M i j) color) ocolor sc) in
																		if flag == 1 then (sc,1) else
																			if sc' >= cut then (sc',1) else
																				if sc' >= sc then (sc',0) else (sc,0)) (0,0) ms'
						else
							if ms' == [] then score myboard (oppositeColor color') 64 else
								fst $ foldr (\(i,j) (sc,flag) -> let sc' = (score (myMove myboard (M i j) ocolor) color sc) in
																		if flag == 1 then (sc,1) else
																			if sc' <= cut then (sc',1) else
																				if sc' <= sc then (sc',0) else (sc,0)) (64,0) ms'
	in do {myboard <- board2myboard board;
			let ((ni,nj),sc'') = foldr (\(i,j) ((ii,jj),sc) -> let sc' = score (myMove myboard (M i j) color) ocolor 0 in
																			if sc' >= sc then ((i,j),sc') else ((ii,jj),sc)) ((0,0),0) (validMoves' myboard color) in
				return $ M ni nj}
					
	



--MyBoardの操作

board2myboard :: Board -> IO MyBoard
board2myboard board = let
	initmyboard = (0::Word64,0::Word64) in
	do {preboard <- getElems board;
	let preboard2 = filter (\i -> i /= sentinel) preboard in
			return $ foldr (\i (wht,blk) -> let	ith = preboard2 !! i in
																if ith == white then (setBit wht i,blk) else 
																	if ith == black then (wht,setBit blk i) else (wht,blk)) initmyboard [0..63]}

readmyboard :: MyBoard -> (Int,Int) -> Int
readmyboard (wht,blk) (i,j) =
	if (i == 0) || (i == 9) || (j == 0) || (j == 9) then sentinel else
		if testBit wht (i*8 + j-9) then white else
			if testBit blk (i*8 + j-9) then black else none

writemyboard :: MyBoard -> (Int,Int) -> Int -> MyBoard
writemyboard (wht,blk) (i,j) e = 
	if e == black then (wht,setBit blk (i*8 + j-9)) else (setBit wht (i*8 + j-9),blk)

isValidMove' :: MyBoard -> Color -> (Int,Int) -> Bool
isValidMove' myboard color (i,j) = let
		e = readmyboard myboard (i,j) in
       if e == none then
					isEffective' myboard color (i,j)
       else
				False


-- 石を置いたときに、ひっくり返せるかどうか
isEffective' :: MyBoard -> Color -> (Int,Int) -> Bool
isEffective' myboard color (i,j) = 
		let	ms = flippableIndices' myboard color (i,j) in
			not (null ms)

-- 石を置いたときにひっくり返えるところ
flippableIndices' :: MyBoard -> Color -> (Int,Int) -> [(Int,Int)]
flippableIndices' myboard color (i,j) = 
		let bs = map (\(di,dj) -> flippableIndicesLine' myboard color (di,dj) (i+di,j+dj)) dirs
			in	concat bs

flippableIndicesLine' :: MyBoard -> Color -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
flippableIndicesLine' myboard color (di,dj) (i,j) =
	checkLine (di,dj) (i,j) []
	where                
		ocolor = oppositeColor color
		checkLine (di,dj) (i,j) r =
			let	c = readmyboard myboard (i,j) in
				if c == ocolor then 
					checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
				else 
					[]
		checkLine' (di,dj) (i,j) r =
			let c = readmyboard myboard (i,j) in
				if c == ocolor then 
					checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
				else if c == color then 
					r 
				else
					[]
  
-- 合法手
validMoves' :: MyBoard -> Color -> [(Int,Int)]
validMoves' myboard color =
		filter (isValidMove' myboard color) 
			[ (i,j) | i <- [1..8], j <- [1..8]]


--MyBoardに1手打つ
myMove :: MyBoard -> Mv -> Color -> MyBoard 
myMove myboard GiveUp  color = myboard
myMove myboard Pass    color = myboard
myMove myboard (M i j) color =
	let	{ms = flippableIndices' myboard color (i,j);
			myboard'' = foldr (\(ii,jj) myboard' -> writemyboard myboard' (ii,jj) color) myboard ms}
		in	writemyboard myboard'' (i,j) color

--石の数
mycount :: MyBoard -> Color -> Int
mycount myboard color =
	let is = filter (\i -> 
												let e = readmyboard myboard i in
														e == color) 
										[ (i,j) | i <- [1..8], j <- [1..8]] in
			length is


score_atom :: (Int,Int) -> Int
score_atom (i,j) =
	[ 30,-12,  0, -1, -1,  0,-12, 30,
	 -12,-15, -3, -3, -3, -3,-15,-12,
	   0, -3,  0, -1, -1,  0, -3,  0,
		-1, -3, -1, -1, -1, -1, -3, -1,
	  -1, -3, -1, -1, -1, -1, -3, -1,
		 0, -3,  0, -1, -1,  0, -3,  0,
	 -12,-15, -3, -3, -3, -3,-15,-12,
	  30,-12,  0, -1, -1,  0,-12, 30] !! (i*8 + j-9)



    
         
