module Top where

main :: IO ()
main = do
  let code :: [BC] = [PUSH 7, DUP, PUSH 1, SWAP, SUB, MUL]
  print (expect 42 (runBC code))

  -- ( 3^2 + (5 - 4) )^2
  let exp :: Exp = Square (Add (Square (Num 3)) (Sub (Num 5) (Num 4)))
  print (expect 100 (eval exp))

  -- let exp2 = Sub (Num 5) (Num 4)
  -- let compiled2 :: [BC] = compile exp2
  -- print compiled2
  -- print (expect 1 (runBC compiled2))

  -- runBC . compile === eval
  let compiled :: [BC] = compile exp
  print compiled
  print (expect 100 (runBC compiled))


compile :: Exp -> [BC]
compile e =
  case e of
    Num x -> [PUSH x]
    Add x y -> (compile x) ++ ((compile y) ++ [ADD])
    Sub x y -> (compile y) ++ ((compile x) ++ [SUB])
    Square x -> (compile x) ++ ((compile x) ++ [MUL])


data Exp = Num Int | Add Exp Exp | Sub Exp Exp | Square Exp

eval :: Exp -> Int
eval e =
  case e of
    Num x -> x
    Add x y -> (eval x) + (eval y)
    Sub x y -> (eval x) - (eval y)
    Square x -> (eval x) * (eval x)



type Code = [BC]
data BC = ADD | SUB | MUL | DUP | SWAP | PUSH Int deriving Show

runBC :: [BC] -> Int
runBC bcs = do
  let is = step bcs []
  if length is /= 1
  then error "final stack does not have 1 element"
  else head is

step :: [BC] -> [Int] -> [Int]
step bcs is =
  case bcs of
    [] -> is
    bc:bcs -> do
      let (new_is) = do_op bc is
      step bcs new_is

do_op :: BC -> [Int] -> [Int]
do_op bc is =
  case bc of
    ADD -> add is
    SUB -> sub is
    MUL -> mul is
    DUP -> dup is
    SWAP -> swap is
    PUSH x -> push is x

add :: [Int] -> [Int]
add is =
  case is of
    a:b:is -> (a+b):is
    _ -> error "not enough elements on stack to add"

sub :: [Int] -> [Int]
sub is =
  case is of
    a:b:is -> (a-b):is
    _ -> error "not enough elements on stack to sub"

mul :: [Int] -> [Int]
mul is =
  case is of
    a:b:is -> (a*b):is
    _ -> error "not enough elements on stack to mul"

dup :: [Int] -> [Int]
dup is =
  case is of
    a:is -> a:a:is
    _ -> error "not enough elements on stack to dup"

swap :: [Int] -> [Int]
swap is =
  case is of
    a:b:is -> b:a:is
    _ -> error "not enough elements on stack to swap"

push :: [Int] -> Int -> [Int]
push is x = x:is


expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)