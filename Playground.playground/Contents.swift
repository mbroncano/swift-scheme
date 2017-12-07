//: Playground - noun: a place where people can play

import Cocoa
@testable import Framework

let form = try Form.Parse(string: "(+ 2 3 (* 4 5) . 5)")
//let form = try Form.Parse(string: "+")
do {
//    try form.makeListIterator().reduce(.Null, { $1 }).asList().cdr = .List(Form.Pair(car: .Symbol("a")))
//    form[0] = Form.Symbol("-")
//    print("\(form)")
//    print("\(form[3][2])")
//    let a = try form.lazy.map { try $0.asNumber() }
//    print(a)
    let a = try Form.Parse(string: "(+ 2 3)")
    let b = try Form.Parse(string: "(((x . 1)(y . 2))((z . 3)(w . 4)))")
    let c = try Form.Parse(string: "(2 3)")
    let n = Form([a, b, c])
    print(a << c)
    var copy = n.reversed().reduce(.Null) { return $1.car + $0 }
    print(copy + (n + .Null))
//    for i in try form.lazy.map({ try $0.asNumber() }) {
//        print(i)
//    }
} catch {
    print(error)
}
