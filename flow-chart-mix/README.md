## Task 1. FlowChart mix

Project organization:

* [`fc-int.rkt`](./fc-int.rkt): implementation of FlowChart' interpreter written on Racket
* [`fc-fc-int.rkt`](./fc-fc-int.rkt): implementation of FlowChart' interpreter written on FlowChart
* [`tm-int.rkt`](./tm-int.rkt): implmentation of Turing Machine' interpreter on FlowChart
* [`fc-mix.rkt`](./fc-mix.rkt): implementation of differnet mixes:
  * `fc-mix` - simple mix for I projection
  * `fc-mix-outer` - simple mix for II projection (outer)
  * `fc-mix-trick` - mix with the Trick for II projection (inner)
  * `fc-mix-inner-inner` - mix for III projection (the most inner)

* [`runner.rkt`](./runner.rkt) - file for running and testing different projections. See following sections in this file:
  * `I FUTAMURA PROJECTION` - test all mix implementations for correctness
  * `II FUTAMURA PROJECTION` - get compiler after specialization on interpreter, compile program and run it on input data
  * `III FUTAMURA PROJECTION` - get compiler from compiler generator, compile program and run it on input data
