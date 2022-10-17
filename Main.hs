module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 

import System.Environment

maze_path = "fill in"

-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A

-- Question 1

get_maze :: String -> IO [String]
get_maze string = do
    content <- readFile string
    let x = lines content
    return x

-- Question 2

print_maze :: [String] -> IO ()
print_maze [] = return()
print_maze (x:xs) = do
    putStrLn x
    print_maze xs

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
is_wall m (x,y) = if '#' == get m x y then True else False

-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
place_player m (x,y) = set m x y '@'


---- Part B

-- Question 5

move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) z= if z == 'a' then (x-1,y) else (if z == 'd' then (x+1,y) else (if z == 'w' then (x,y-1) else (if z == 's' then (x,y+1) else (if z == 'q' then (x,y) else (x,y)))))

-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move m (x,y) z = if is_wall m (move (x,y) z) == False then True else False 

-- Question 7

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop m (x,y) = do
    print_maze (place_player m (x,y))
    (a:as) <- getLine
    let direct = a
    if can_move m (x,y) direct then game_loop m (move (x,y) direct) else game_loop m(x,y)



---- Part C

-- Question 8

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path = error "Not implemented"

-- Question 9

main :: IO ()
main = error "Not implemented"
