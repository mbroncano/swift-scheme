//
//  Builtin.swift
//  Scheme
//
//  Created by Manuel Broncano on 8/30/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

//! MARK: Basic arithmetic
fileprivate func aritOp1(expr: Form, op: (Decimal, Decimal) -> Decimal, def: Decimal) throws -> Form {
    return .Number(try expr.lazy.map({ try $0.car.asNumber() }).reduce(def, op))
}

fileprivate func aritOp2(expr: Form, op: (Decimal, Decimal) -> Decimal, def: Decimal) throws -> Form {
    let first = try expr.car.asNumber()
    guard case .Pointer = expr.cdr else { return .Number(op(def, first)) }
    return try aritOp1(expr: expr.cdr, op: op, def: first)
}

//! MARK: Predicates
fileprivate func predOp2(expr: Form, op: (Decimal, Decimal) -> Bool) throws -> Form {
    let nums = try expr.lazy.map({ try $0.car.asNumber() })
    let list = zip(nums.dropLast(), nums.dropFirst())
    let result = list.first(where: { !op($0.0, $0.1) })
    return .Boolean(result == nil)
}

//! MARK: Built in procedures

extension Form {

    static let builtin: [String:Form] = [
        "+": .Function({ try aritOp1(expr: $0, op: +, def: 0) }),
        "*": .Function({ try aritOp1(expr: $0, op: *, def: 1) }),
        "-": .Function({ try aritOp2(expr: $0, op: -, def: 0) }),
        "/": .Function({ try aritOp2(expr: $0, op: /, def: 1) }),
        "=": .Function({ try predOp2(expr: $0, op: ==) }),
        "<": .Function({ try predOp2(expr: $0, op: <) }),
        ">": .Function({ try predOp2(expr: $0, op: >) }),

        "eq?": .Function({ .Boolean($0.car === $0.cdr.car) }),
        "eqv?": .Function({ .Boolean($0.car === $0.cdr.car) }),
        "equal?": .Function({ .Boolean($0.car == $0.cdr.car) }),
        "pair?": .Function({ .Boolean($0.car.isPair()) }),
        "null?": .Function({ .Boolean($0.car.isNull()) }),

        "car": .Function({ try $0.car.asPair().car }),
        "cdr": .Function({ try $0.car.asPair().cdr }),
        "cons": .Function({ $0.car + $0.cdr.car }),
        "list": .Function({ $0.copy() }),
        "length": .Function({ .Number(Decimal($0.car.length)) }),
        "reverse": .Function({ $0.car.reduce(.Null) { acc, cur in cur.car + acc } }),
        ]
}

