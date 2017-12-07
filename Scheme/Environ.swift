//
//  Environment.swift
//  Scheme
//
//  Created by Manuel Broncano on 8/21/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

typealias StackRecord = (Form, Form, Form)

// returns ((()))
func environment() -> Form {
    return extend()
}

// b?, ([(a . 1) (b . 2)] [(c . 3) (d . 4)]) -> (b . 2)
fileprivate func find(name: String, frame: Form) -> Pair? {
    for entry in frame {
        for item in entry.car {
            guard case let .Pointer(pair) = item.car else { continue }
            guard case let .Symbol(string) = pair.car else { continue }
            guard string == name else { continue }
            return pair
        }
    }

    return nil
}

// ([(a . 1) (b . 2)] [(c . 3) (d . 4)]) -> ([] [(a . 1) ...])
func extend(_ frame: Form = .Null) -> Form {
    return Form.Pointer(Pair()) + frame
}

// b?, ([(a . 1) (b . 2)] [(c . 3) (d . 4)]) -> (b . 2)
// cons?, ([(a . 1) (b . 2)] [(c . 3) (d . 4)]) -> #<symbol: cons>
// foo?,  ([(a . 1) (b . 2)] [(c . 3) (d . 4)]) -> exception(!)
func resolve(name: String, frame: Form) throws -> Form {
    if let pair = find(name: name, frame: frame) {
        return pair.cdr
    }

    if let res = Form.builtin[name] {
        return res
    }

    throw Form.Exception.General("Symbol not found: \(name)")
}

// b, 9, ([(a . 1) (b . 2)] [(c . 3) (d . 4)])
//   --> ([(a . 1) (b . 9)] [(c . 3) (d . 4)])
func set(symbol: Form, value: Form, frame: Form) throws {
    let name = try symbol.asSymbol()
    guard let pair = find(name: name, frame: frame)
        else { throw Form.Exception.General("Symbol not found: \(name)") }

    pair.cdr = value
}

// x, 0, ([(a . 1) (b . 2)] [(c . 3) (d . 4)])
//   --> ([(a . 1) (b . 9) (x . 0)] [(c . 3) (d . 4)])
func define(symbol: Form, value: Form, frame: Form) throws  {
    let name = try symbol.asSymbol()
    if let pair = find(name: name, frame: frame) {
        pair.cdr = value
        return
    }

    let pair = try frame.car.last.asPair()
    pair.cdr = ((symbol + value) + .Null)
}

// ((a b c)), ((1 2 3)) -> ((a 1) (b 2) (c 3))
// (a), ((1 2 3)) -> ((a (1 2 3)))
// ((a . b)), ((1 2 3)) -> ((a 1) (b (2 3)))
func bind(formals: Form, values: Form) throws -> Form {
    return try zip(formals.makeIterator(), values.makeIterator()).reduce(.Null) { acc, pair in
        let (symbol, value) = pair
        switch symbol {
        case .Symbol:
            return (symbol + value) + acc
        case .Pointer:
            return (symbol.car + value.car) + acc
        default:
            throw Form.Exception.General("Must be a symbol or list: \(symbol)")
        }
    }
}

