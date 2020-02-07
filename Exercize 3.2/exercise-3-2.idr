import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length xs = sum (map (\x => 1) xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x::xs) = (my_reverse xs)++[x]

my_map : (a -> b) -> List a -> List b
my_map _ [] = []
my_map f (x::xs) = (f x)::(my_map f xs)

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map _ [] = []
my_vect_map f (x::xs) = (f x)::(my_vect_map f xs)

createEmpties : Vect n (Vect 0 el)
createEmpties {n} = replicate n []

transposeHelper : (x : Vect n el) ->
                  (xsTrans : Vect n (Vect len el)) ->
                  Vect n (Vect (S len) el)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n el) -> Vect n (Vect m el)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

mkRow : Num a => (x : Vect m a) ->
                 (ysTrans : Vect p (Vect m a)) ->
                 Vect p a
mkRow x [] = []
mkRow x (y :: xs) = sum (zipWith (*) x y) :: mkRow x xs

multHelper : Num a => (xs : Vect n (Vect m a)) ->
                      (ysTrans : Vect p (Vect m a)) ->
                      Vect n (Vect p a)
multHelper [] ysTrans = []
multHelper (x :: xs) ysTrans = mkRow x ysTrans :: multHelper xs ysTrans


multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] ys = []
multMatrix xs ys = let ysTrans = transposeMat ys in
                          multHelper xs ysTrans
