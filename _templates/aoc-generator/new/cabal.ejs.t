---
inject: true
to: aoc2023.cabal
after: other-modules
---
                     , <%= `Day${String(day).padStart(2, '0')}.${name}` %>