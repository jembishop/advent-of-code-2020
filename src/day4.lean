import system.io 
import utils
import data.list.sort
open io 
open nat

def passport := list (string × string)

instance : has_to_string passport := ⟨λ x, x.to_string⟩ 

def parse_field : string → option (string × string)
| s := do
    let spl := s.split (=':'),
    att ← spl.nth 0,
    val ← spl.nth 1,
    return (att, val)

def parse_passport : string → option passport 
| s         :=  monad.sequence $ functor.map parse_field (filter (≠"") (s.split (λ x, x=' ' ∨ x='\n')))


def parse : string → option (list passport)
| s     := monad.sequence $ parse_passport <$> (split_para s)

def valid : passport → bool
| p := p.length = 8 ∨ 
(p.length = 7 ∧ (list.length (list.filter (λ x: (string × string), x.1 = "cid") p ))= 0)


def is_number : string → bool 
| s     := s.data.all (λ x, char.is_digit x)

def num_v :ℕ × ℕ → string → bool 
| (x,y) s     := let p := string.to_nat s in p ≤ y ∧ p ≥ x ∧ is_number s

def byr_v := num_v (1920, 2002)
def iyr_v := num_v (2010, 2020)
def eyr_v := num_v (2020, 2030)

def hgt_v : string → bool 
| s     := let p := list.span char.is_digit s.data in 
    match (p.1.as_string, p.2.as_string) with 
    |(n, "cm")   := num_v (150, 193) n
    |(n, "in")   := num_v (59, 76) n
    | _          := false
    end

def hcl_v : string → bool
| s     := match s.data with
    | ('#'::xs) := xs.all (λ x: char, (char.is_digit x) ∨ "abcdef".data.mem x)
    | _     := false
    end

def ecl_v : string → bool
|s     := ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].mem s

def pid_v : string → bool
|s     := is_number s ∧ s.length = 9

def sort_fields : passport → passport
| p := list.insertion_sort (λ x y, x.1.data ≤ y.1.data) p

def valid_fields : passport → bool
| p     := let zipped := list.zip [byr_v, ecl_v, eyr_v, hcl_v, hgt_v, iyr_v, pid_v] 
           (filter (λ x: (string×string), x.1 ≠ "cid") (sort_fields p))
           in 
           zipped.all (λ t: ((string → bool) × (string×string)), t.1 t.2.2) 

def num_valid_fields : list passport → ℕ 
:= list.length ∘ filter valid_fields 

def main : io unit := do
    contents ← fs.read_file "inputs/day4.txt",
    let parsed := parse contents.to_string,
    put_str_ln $ to_string $ (list.length ∘ (filter valid)) <$> parsed,
    put_str_ln $ to_string $ (num_valid_fields ∘ (filter valid)) <$> parsed