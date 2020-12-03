import system.io 
import tactic
open io
open nat
open list
open functor
open decidable

structure entry :=
    (letter : char)
    (min_num : ℕ)
    (max_num : ℕ)
    (password : string)

def entry_to_string : entry → string 
| e := "letter: " ++ e.letter.to_string 
         ++ " min: " ++ (to_string e.min_num) 
         ++ " max: " ++ (to_string e.max_num) 
         ++ " password: " ++ (to_string e.password) 

instance : has_to_string entry := ⟨entry_to_string⟩ 

def line_to_entry (str : string) : option entry :=
let sp := string.split (=' ') str
in do
    fst ←  (nth sp 0),
    snd ← (nth sp 1),
    password ← (nth sp 2),
    let min_max := string.split (='-') fst,
    min ← (nth min_max 0),
    max ← (nth min_max 1),
    letter ← (nth snd.data 0),
    return (entry.mk letter (min.to_nat) 
           (max.to_nat) password) 


def parse (str : string) : (option (list entry)) := 
monad.sequence (functor.map line_to_entry (filter (≠"") (string.split (='\n') str)))

def valid_1 : entry → bool 
| e  := let num := length $ filter (=e.letter) e.password.data
in num ≤ e.max_num ∧ num ≥ e.min_num

def valid_2 : entry → bool 
| e  := let p := e.password.data match (p.nth (e.min_num - 1), p.nth (e.max_num - 1)) with
| (some c1, some c2) := xor (c1=e.letter) (c2=e.letter)
| (_, _) := false
end

-- can't prove decidablity to use normal filter 
def filter {α : Type } (p : α → bool) : list α → list α
| []     := []
| (a::l) := if p a then a :: filter l else filter l


def num_valid (pred : entry → bool) : list entry → ℕ 
| l := length $ filter pred l

def main : io unit := do
    contents ← fs.read_file "inputs/day2.txt",
    let parsed := parse contents.to_string,
    put_str_ln $ to_string $ functor.map (num_valid valid_1) parsed,
    put_str_ln $ to_string $ functor.map (num_valid valid_2) parsed