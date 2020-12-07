import system.io
import utils
import data.list.sort
open io

meta def group_by : list char → list (list char)
| [] := []
| (x::xs) := let (ys, zs) := list.span (=x) xs in (x::ys) :: group_by zs

def qgroup := list (list char)
def parse : string → list (list (list char))
| s        := list.map (list.map (λx: string, x.data))
 $ list.map ((filter (≠""))∘ (string.split (=' '))) (split_para s)

def part : list char → char × (list char)
| []    := ('#', [])
| (x::xs)    := (x, xs)

def concat_l {α} : list (list α ) → list α := list.foldr (++) []

meta def qs : qgroup → list (list char)
| q := (group_by (list.insertion_sort (≤) (concat_l q)))

meta def n_unique : qgroup → ℕ
| a := (qs a).length 

meta def n_all : qgroup → ℕ
| a := (filter (λ x, list.length x = a.length) (qs a)).length 

meta def main : io unit := do
    contents ← fs.read_file "inputs/day6.txt",
    let parsed := parse contents.to_string,
    put_str_ln $ to_string $ suml $ list.map n_unique parsed,
    put_str_ln $ to_string $ suml $ list.map n_all parsed
