import system.io 
import tactic
open io
open list
open prod
open ordering

def combinations : ℕ → list ℕ → list (list ℕ)
| 0 l        := [[]]
| n []       := []
| n (x::xs)  := map (λ l, x::l) (combinations (n-1) xs) ++ (combinations n xs)

def suml : list ℕ → ℕ := list.foldr (+) 0
def productl : list ℕ → ℕ := list.foldr (*) 1

def find_k_add :ℕ → ℕ → list ℕ → list (list ℕ)
| k n l := list.filter ((=n) ∘ suml) (combinations k l)

def parse (str : string) : list ℕ := filter (≠0) (map string.to_nat (string.split (='\n') str)) 

def main : io unit := do
    contents ← fs.read_file "inputs/day1.txt",
    let l : list ℕ := parse contents.to_string,
    put_str_ln $ to_string $ map productl (find_k_add 2 2020 l),
    put_str_ln $ to_string $ map productl (find_k_add 3 2020 l)