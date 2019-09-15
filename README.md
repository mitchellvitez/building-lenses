# Building Lenses

Code to accompany my [Building Lenses](//vitez.me/building-lenses) blog post. This repo contains all the exercise answers from that post, so if you don't want to spoil them turn back now!

## Philosophy

Use as few imports beyond the prelude as possible. I did import `coerce` and `Coercible` from `Data.Coerce` to implement `Setter`.

This is done to ensure that you should be able to answer every exercise in the post with only the code that exists here (plus knowledge of `Data.Coerce` and of course `Prelude`).
