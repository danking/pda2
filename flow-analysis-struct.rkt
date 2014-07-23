#lang racket

(provide (all-defined-out))

(struct FlowAnalysis
  (flow flow-ctx flow-across initial-work-set initial-flow-state initial-ctx-state))
