⍝ Fibonacci sequence (APL)
⍝ Simple readable placeholder (APL glyphs may vary by font).
fib ← {
  n ← ⍵
  a ← 0
  b ← 1
  r ← ⍬
  :For i :In ⍳n
      r ,← a
      next ← a + b
      a ← b
      b ← next
  :EndFor
  r
}

⍝ END OF FIBONACCI SAMPLE
⍝ END OF FIBONACCI SAMPLE
⍝ END OF FIBONACCI SAMPLE
