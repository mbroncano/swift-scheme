//
//  Eval.swift
//  Scheme
//
//  Created by Manuel Broncano on 6/30/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

//! MARK: Evaluation
extension Form {
    // non-recursive evaluation
    // supports TCO/TCE
    // detached stack style (heap frame)
    // internal special forms
    // explicit continuation
    // define-syntax support
    public func eval(global: Form) throws -> Form {
        let emptyRes = Form.Undefined
        var result = emptyRes

        // remaining, frame, evaluated <- continuation
        //
        // (define a (+ 2 3)), (()), #<undef>
        // (a (+ 2 3)), (()), (#<symbol: define>)
        // ((+ 2 3)), (()), (#<symbol: define> #<symbol: a>)
        // (+ 2 3), (() ()), ()
        // [push the sub expression]
        // (2 3), (() ()), (#<function: +>)
        // (3), (() ()), (#<function: +> 2)
        // (), (() ()), (#<function: +> 2 3)
        // [compute, push 5]
        // (), (()), (#<symbol: define> #<symbol: a> 5)
        // (), (((a . 5))), (#<symbol: define> #<symbol: a> 5)
        // [finished]
        // res -> #<undef>
        // global -> (((a . 5)))

        for expr in self {
            var stack = [(expr.car, global, emptyRes)]
            
            while let (form, frame, eval) = stack.popLast() {
                let iter = form.makeIterator()
                var eval = eval

                log("------------------------------------ [\(frame)]")

                // determine atomic, func, closure or special form
                if case .Undefined = eval {
                    let rest = try nextParam(iter)

                    // a list of the form (func args ...)
                    if case .Pointer = rest {
                        // evaluate the procedure first, the the argument (pass by value)
                        evalParam(frame: frame, rest: rest, eval: eval, stack: &stack)

                    } else {
                        // determine whether self evaluating expression, variable, special form or macro
                        var atom: Form //= .Undefined
                        if case let .Symbol(string) = rest, specialForm[string] == nil {
                            // symbol that's not a special form
                            atom = try resolve(name: string, frame: frame)
                            // hack: add back the symbol if macro
                            if case .Transformer = atom {
                                try updateResult(rest, stack: &stack, result: &result)
                            }
                        } else {
                            // everything else, exit
                            atom = rest
                        }
                        log("[\(stack.count)] atom: \(atom)")
                        try updateResult(atom, stack: &stack, result: &result)
                    }

                } else if case let .Symbol(string) = eval.car {
                    // special forms handling
                    log("[\(stack.count)] special form: \(string)")

                    if let syntax_func = specialForm[string] {
                        try syntax_func(&stack, &eval, frame, iter, global, &result)

                    } else {

                        // macro case
                        // take the transformer
                        guard case let .Transformer(keywords, patterns) = eval.cdr.car
                            else { throw Exception.General("Missing transformer") }

                        // hygienic macro support - we won't evaluate the arguments
                        let body = try expand(keywords: eval.car + keywords, patterns: patterns, form: eval.car + form)
                        stack.append((body.car, frame, emptyRes))
                    }

                } else if let rest = iter.next() {
                    // evaluate remaining params
                    log("[\(stack.count)] escape!!")
                    evalParam(frame: frame, rest: rest, eval: eval, stack: &stack)

                } else {
                    // apply the function if we have no more params to evaluate
                    log("[\(stack.count)] apply: \(eval.car) to: \(eval.cdr.car)")

                    switch eval.car {
                    case let .Function(builtin):
                        // builtin functions
                        let res = try builtin(eval.cdr)
                        log("[\(stack.count)] result: \(res)")
                        try updateResult(res, stack: &stack, result: &result)

                    case let .Closure(formal, body, lframe):
                        let eframe = try bind(formals: formal, values: eval.cdr) + lframe
                        // hack: lambda body is an implicit begin, save some code repetition here
                        let res = Form.Symbol("begin") + .Null
                        stack.append((body, eframe, res))

                    case let .Continuation(saved_stack):
                        // restore full stack and continue execution
                        stack = saved_stack
                        try updateResult(eval.cdr.car, stack: &stack, result: &result)

                    default:
                        throw Form.Exception.General("Not a function: \(eval.car)")
                    }
                }
            }
            guard stack.count == 0 else { throw Form.Exception.General("stack error!") }
            log("> \(result) [#\(Pair.count) alloc]")
        }
        return result
    }
}
