//
//  Syntax.swift
//  Scheme
//
//  Created by Manuel Broncano on 8/21/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

func evalParam(frame: Form, rest: Form, eval: Form, stack: inout [StackRecord]) {
    // evaluate the procedure first, the the argument (pass by value)
    stack.append((rest.cdr, frame, eval))
    stack.append((rest.car, extend(frame), emptyRes))
}

func nextParam(_ iter: AnyIterator<Form>) throws -> Form {
    guard let next = iter.next() else {
        //        log("\(toeval.car)")
        throw Form.Exception.General("Parameter expected")
    }
    return next
}

func updateFrame(_ frame: Form, stack: inout [StackRecord], global: Form) throws {
    if var prev = stack.popLast() {
        prev.1 = frame
        stack.append(prev)
    } else {
        // no stack, top level frame
        try global.asPair().car = frame.car
    }
}

func updateResult(_ res: Form, stack: inout [StackRecord], result: inout Form) throws {
    // add our partial result to the parent
    if var prev = stack.popLast() {
        prev.2 = prev.2 << res
        stack.append(prev)
    } else {
        // no stack, top level result
        result = res
    }
}

let emptyRes = Form.Undefined
func log(_ string: String) {
//    print(string)
}

typealias SyntaxFunc = (inout [StackRecord], inout Form, Form, AnyIterator<Form>, Form, inout Form) throws -> Void

let specialForm: [String: SyntaxFunc] = [
    "quote": syntax_quote,
    "lambda": syntax_lambda,
    "set!": syntax_set,
    "define": syntax_define,
    "begin": syntax_begin,
    "cond": syntax_cond,
    "if": syntax_if,
    "let": syntax_let,
    "letrec": syntax_letrec,
    "call/cc": syntax_callcc,
    "define-syntax": syntax_definesyntax,
    "syntax-rules": syntax_syntaxrules
]

fileprivate let syntax_begin: SyntaxFunc = { stack, eval, frame, iter, global, result in
    if let rest = iter.next() {
        if case .Null = rest.cdr {
            log("[\(stack.count)] tail call: \(rest.car)")
            stack.append((rest.car, frame, emptyRes))
        } else {
            log("[\(stack.count)] escape!!")
            stack.append((rest.cdr, frame, eval))
            stack.append((rest.car, frame, emptyRes))
        }
    }
}

fileprivate let syntax_cond: SyntaxFunc = { stack, eval, frame, iter, global, result in
    if case .Null = eval.cdr {
        // evaluate the predicate if it's not done yet
        let rest = try nextParam(iter)
        // check if it's an else
        if case let .Symbol(else_token) = rest.car, else_token == "else" {
            log("else")
        }

        // save the clause body in eval first
        eval = eval << rest.car
        stack.append((rest.cdr, frame, eval))
        stack.append((rest.car.car, frame, emptyRes))
    } else {
        // we have it already (cond body pred)
        let pred = try eval.cdr.cdr.car.asBoolean()
        if pred {
            log("[\(stack.count)] tail!!")
            let body = Form.Symbol("begin") + eval.cdr.car.cdr
            stack.append((body, frame, emptyRes))
        } else {
            // reset the clause, continue
            try eval.asPair().cdr = .Null
            // check for more predicates
            if let rest = iter.next() {
                // save the clause body in eval first
                eval = eval << rest.car
                stack.append((rest.cdr, frame, eval))

                // check for else, replace for #t
                var newpred = rest.car.car
                if case let .Symbol(symbol) = newpred, symbol == "else" {
                    newpred = .Boolean(true)
                }
                stack.append((newpred, frame, emptyRes))
            }
        }
    }
}

fileprivate let syntax_set: SyntaxFunc = { stack, eval, frame, iter, global, result in
    // do we have already the var name?
    if case .Null = eval.cdr {
        try eval = eval << nextParam(iter).car
    }

    // check the var name is a symbol
    if case .Symbol = eval.cdr.car {

        // do we have already the second param?
        if case .Null = eval.cdr.cdr {
            let rest = try nextParam(iter)
            stack.append((rest.cdr, frame, eval))
            stack.append((rest.car, frame, emptyRes))

        } else {
            // we have, proceed to define
            try set(symbol: eval.cdr.car, value: eval.cdr.cdr.car, frame: frame)
        }
    } else {
        throw Form.Exception.General("Invalid parameter: \(eval.cdr.car)")
    }
}

fileprivate let syntax_define: SyntaxFunc = { stack, eval, frame, iter, global, result in
    // do we have already the varname?
    if case .Null = eval.cdr {
        try eval = eval << nextParam(iter).car
    }

    // first form: (define (name formal ...) body ...)
    if case .Pointer = eval.cdr.car {
        let name = eval.cdr.car.car
        let vars = eval.cdr.car.cdr
        let body = try nextParam(iter)
        try define(symbol: name, value: .Closure(vars, body, frame), frame: frame)
        try updateFrame(frame, stack: &stack, global: global)

        // second form: (define var <expr>)
    } else if case .Symbol = eval.cdr.car {

        // do we have already the second param?
        if case .Null = eval.cdr.cdr {
            if let rest = iter.next() {
                // when (define var expr) evaluate
                stack.append((rest.cdr, frame, eval))
                stack.append((rest.car, frame, emptyRes))

            } else {
                // when (define var) we set it to undefined
                eval = eval << .Undefined
            }
        } else {
            // we have, proceed to define
            try define(symbol: eval.cdr.car, value: eval.cdr.cdr.car, frame: frame)
            try updateFrame(frame, stack: &stack, global: global)
        }
    } else {
        throw Form.Exception.General("Invalid parameter: \(eval.cdr.car)")
    }
}

fileprivate let syntax_if: SyntaxFunc = { stack, eval, frame, iter, global, result in
    if case .Null = eval.cdr {
        // evaluate the predicate if it's not done yet
        let rest = try nextParam(iter)
        stack.append((rest.cdr, frame, eval))
        stack.append((rest.car, frame, emptyRes))

    } else {
        // we have it already
        let pred = eval.cdr.car.isFalse()
        var rest = try nextParam(iter)

        // if false and no second pred, result is undefined
        if pred {
            if let second = iter.next() { rest = second } else { rest = .Undefined }
        }

        if case .Undefined = rest {} else {
            log("[\(stack.count)] tail call: \(rest.car)")
            stack.append((rest.car, frame, emptyRes))
        }
    }
}

fileprivate let syntax_let: SyntaxFunc = { stack, eval, frame, iter, global, result in
    // (let ((var vexpr) .. (var .. vexpr)) expr .. expr)
    // ((lambda (var .. var) expr .. expr) vexpr .. expr)

    // (let tag ((name val) ...) body1 body2 ...)
    // ---> ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag) val ...)
    // (begin (define tag (lambda (name ...) body1 body2 ...)) (tag val ...))

    let rest = try nextParam(iter)
    if case .Pointer = rest.car {
        let vars = try rest.car.splice()
        let lambda = (Form.Symbol("lambda") + (vars.car + rest.cdr)) + vars.cdr
        stack.append((lambda, extend(frame), emptyRes))
    } else if case .Symbol = rest.car {
        let tag = rest.car
        let bindings = try nextParam(iter)
        let vars = try bindings.car.splice()
        let body = bindings.cdr
        let lambda = Form.Symbol("lambda") + (vars.car + body)
        let define = Form.Symbol("define") + (tag + (lambda + .Null))
        let begin = Form.Symbol("begin") + (define + .Null) + (tag + vars.cdr)
        stack.append((begin, extend(frame), emptyRes))
    } else {
        throw Form.Exception.General("invalid let form")
    }
}

fileprivate let syntax_quote: SyntaxFunc = { stack, eval, frame, iter, global, result in
    let rest = try nextParam(iter).car
    log("[\(stack.count)] quote: \(rest)")
    try updateResult(rest, stack: &stack, result: &result)
}

fileprivate let syntax_lambda: SyntaxFunc = { stack, eval, frame, iter, global, result in
    let formal = try nextParam(iter).car
    let body = try nextParam(iter)
    try updateResult(.Closure(formal, body, frame), stack: &stack, result: &result)
}

fileprivate let syntax_letrec: SyntaxFunc = { stack, eval, frame, iter, global, result in
    // (letrec ((v1 e1) ... (vn en)) body)
    // (begin (define v1 e1) ... (define vn en) body)
    let rest = try nextParam(iter)
    let ints = rest.car.reversed().reduce(.Null) {
        (.Symbol("define") + $1.car) + $0
    }
    let body = Form.Symbol("begin") + (ints.car + rest.cdr)
    stack.append((body, extend(frame), emptyRes))
}

fileprivate let syntax_callcc: SyntaxFunc = { stack, eval, frame, iter, global, result in
    let rest = try nextParam(iter)
    let lambda = rest.car
    let body = lambda + (Form.Continuation(stack) + .Null)
    stack.append((body, extend(frame), emptyRes))
}

fileprivate let syntax_definesyntax: SyntaxFunc = { stack, eval, frame, iter, global, result in
    // do we have already the varname?
    if case .Null = eval.cdr {
        try eval = eval << nextParam(iter).car
    }

    // second form: (define-syntax <symbol> <transformer>)
    if case .Symbol = eval.cdr.car {

        // do we have already the second param?
        if case .Null = eval.cdr.cdr {
            if let rest = iter.next() {
                // when (define var expr) evaluate
                stack.append((rest.cdr, frame, eval))
                stack.append((rest.car, frame, emptyRes))

            } else {
                // when (define var) we set it to undefined
                eval = eval << .Undefined
            }
        } else if case .Transformer = eval.cdr.cdr.car {
            // we have, proceed to define
            try define(symbol: eval.cdr.car, value: eval.cdr.cdr.car, frame: frame)
            try updateFrame(frame, stack: &stack, global: global)
        } else {
            throw Form.Exception.General("Expecting transformer: \(eval.cdr.cdr.car)")
        }
    } else {
        throw Form.Exception.General("Expecting symbol: \(eval.cdr.car)")
    }
}

fileprivate let syntax_syntaxrules: SyntaxFunc = { stack, eval, frame, iter, global, result in
    //                    (syntax-rules (<keywords>)
    //                        ((<pattern>) <template>)
    //                            ...
    //                            ((<pattern>) <template>)))
    let keywords = try nextParam(iter).car // check if it's a list of symbols
    let patterns = try nextParam(iter)     // check if it's a list of pattern
    try updateResult(.Transformer(keywords, patterns), stack: &stack, result: &result)
}
