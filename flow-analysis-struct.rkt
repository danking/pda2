#lang racket

(provide (all-defined-out))

(struct FlowAnalysis
  (flow flow-ctx flow-across ctx-gte? sigma-gte?
   initial-work-set initial-flow-state initial-ctx-state))
