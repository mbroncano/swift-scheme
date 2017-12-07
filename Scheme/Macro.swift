//
//  Macro.swift
//  Scheme
//
//  Created by Manuel Broncano on 8/21/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

// (+ 1 2 (op a b c)), [op: *, a: 3, b: 4, c: #<undef>] -> (+ 1 2 (* 3 4))
// (+ . a), [a: (4 5 6)] -> (+ 4 5 6)
func replace(expression: Form, vars: [String: Form]) throws -> Form {
    let result = expression.rmap { acc, cur in
        // support (a . b) constructs
        var acc = acc
        if case let .Symbol(varcdr) = cur.cdr, let valcdr = vars[varcdr] {
            acc = valcdr.car
        }

        // ignore ellipsis for now
        if case let .Symbol(variable) = cur.car, variable == "..." {
            return acc
        }

        // if it's not a variable, it must be a symbol
        guard case let .Symbol(variable) = cur.car, let value = vars[variable] else {
            return cur.car + acc
        }

        // undefined variables are skipped too
        if case .Undefined = value.car {
            return acc
        }

        // process ellipsis and return a list
        if case let .Symbol(next_variable) = cur.cdr.car, next_variable == "..." {
            return value
        }

        // replace variable value
        return value.car + acc
    }

    return result
}

// (_)
// (_ e1 ...)
// (_ (else . body))
// (_ (bool-expression . body) . rest)
func match(pattern: Form, expression: Form, keys: [String], vars: inout [String: Form]) throws -> Bool {
    let expr = expression.makeIterator()
    var pattern = pattern

    // check if the last element is the symbol '...' <ellipsis>
    // if so, repeat the pattern as many time as required e.g.
    // ((name value) ...) <-> ((x 1)(y 2)(z 3)) => ((name value)(name value)(name value))
    if case let .Symbol(string) = pattern.last.car, string == "..." {
        // reverse the list, remove the ellipsis
        let rev = pattern.reversed().dropFirst()

        // compute the number of times we need to repeat the first element
        let num = expression.length - rev.count
        if num > 0 {
            let new = Array(repeating: rev.first!, count: num) + Array(rev)

            // reverse the pattern back
            pattern = new.reduce(.Null) { acc, cur in cur.car + acc }
        }
    }

    // check all items in the pattern
    for item in pattern {
        // skip the last item for dotted lists
        guard case .Pointer = item else { break }

        // if there are no forms left we use .Undefined, (#<undef> + x) -> x
        let form_pair = expr.next()
        let form_token = form_pair?.car ?? .Undefined

        switch item.car {
        case .Pointer:
            // we have a list, iterate - if it doesn't match, fail
            guard try match(pattern: item.car, expression: form_token, keys: keys, vars: &vars) else { return false }

        case let .Symbol(token_string):
            // we have a symbol, check for the keys
            if keys.contains(token_string) {
                guard case let .Symbol(form_string) = form_token, token_string == form_string
                    else { return false }

            } else if token_string != "_" {
                // it can only be a variable, we store it (if it's not a wildcard)
                vars[token_string, default: .Undefined] += form_token
            }

        default:
            // the token is not a symbol or a list, just check for equality (in the equal? sense)
            guard form_token == item.car else { return false }
        }

        // we check now if the pattern is an improper list
        // dotted lists consume the rest params like '...' does
        if case let .Symbol(variable) = item.cdr {
            let rest_token = form_pair?.cdr ?? .Undefined
            vars[variable, default: .Undefined] += rest_token

            // consume and exit the loop
            while expr.next() != nil {}
            break
        }
    }

    // pattern is over but we still have expression items, it's not a match
    guard expr.next() == nil else { return false }

    // the pattern matches
    return true
}

// expand a macro
func expand(keywords: Form, patterns: Form, form: Form) throws -> Form {
    // array of keywords
    let keys: [String] = try keywords.reduce([]) { $0 + [try $1.car.asSymbol()] }

    // iterate over each possible pattern
    for item in patterns {
        // if one matches, try to expand it
        let pattern = item.car

        // variables dictionary
        var vars = [String: Form]()

        if try match(pattern: pattern.car, expression: form, keys: keys, vars: &vars) {
            return try replace(expression: pattern.cdr, vars: vars)
        }
    }

    throw Form.Exception.General("During expansion, no rules matches: \(form)")
}
