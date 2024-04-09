#!/usr/bin/env ruby

1.next # → 2.
1.1.next # → Error, because should the result be 1.2? 1.1.1? 1.1.000000009?
         #   Impossible to say, thus, you can't do 1.1.next.
