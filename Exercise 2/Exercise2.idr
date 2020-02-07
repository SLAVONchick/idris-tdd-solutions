module Exercise2

export
palindrome : Nat -> (s: String) -> Bool
palindrome n s = if length s < n
                    then False
                    else (reverse . toLower) s == toLower s

export
counts : String -> (Nat, Nat)
counts s = ((length . words) s, length s)

export
top_ten : Ord a => List a -> List a
top_ten = List.take 10 . reverse . sort

export
over_length : Nat -> List String -> Nat
over_length n = length . filter ((> n) . length)
