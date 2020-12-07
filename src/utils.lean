import data.vector
import data.list
-- can't prove decidablity to use normal filter 

def filter {α : Type } (p : α → bool) : list α → list α
| []     := []
| (a::l) := if p a then a :: filter l else filter l

def from_list {α} {n : ℕ} : list α → option (vector α n) 
| l     := if h: l.length = n then some ⟨l, h⟩ else none

def join_str : list string → string
| []    := ""
| (x::xs) := x ++ " " ++ (join_str xs)

def split_l {α: Type} [decidable_eq α]: α → list α → list (list α )
| delim [] := []
| delim (x::xs) := let rest := split_l delim xs in
    if x=delim then []::rest else 
    match rest with 
    | []      := [[x]]
    | (y::ys) := (x::y)::ys
    end

def split_lines : string → list string
| s  := s.split (='\n')

def split_para : string → list string
| s := join_str <$> (split_l "" (split_lines s))

def suml : list ℕ → ℕ := list.foldr (+) 0