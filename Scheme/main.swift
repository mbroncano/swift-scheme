//
//  main.swift
//  Scheme
//
//  Created by Manuel Broncano on 6/25/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

// ((()))
// ([(a 1) (b 2)] [(c 3) (d 4)])
var global = environment()

/*
let test: [(String, String?)] = [
    ("(define (fib n) (fib-iter 1 0 n))", "#<undefined>"),
    ("(define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (+ count -1))))", "#<undefined>"),
    ("(fib 10)", "6765"),
    ("(define-syntax or (syntax-rules () ((or) #f) ((or test) test) ((or test1 test2 ... ) (let ((x test1)) (if x x (or test2 ... ))))))", "#<undefined>"),
        ("(or)", "#f"),
    ("(or (= 2 2) (> 2 1))", "#t"),
    ("(or (= 2 3) (= 2 1) (= 2 3))", "#f"),
    ("""
        (define-syntax my-let
        (syntax-rules ()
        ((my-let ((name val) ...) body1 body2 ...)
        ((lambda (name ...) body1 body2 ...)
        val ...))))
    """, "#<undefined>"),
    ("(my-let ((x 2) (y 3)) (my-let ((x 7) (z (+ x y))) (* z x)))", "35"),
]

test.forEach {
    let (input, _) = $0
    do {
        let form = try Form.Parse(string: input)
        let eval = try form.eval(global: global)
        print("\(eval)")
    } catch {
        print(error)
    }
}
*/

while true {
    do {
        print("> ", terminator: "")
        var form = Form.Undefined
        var expression = ""
        while true {
            if let input = readLine() {
                expression += input
                do {
                    form = try Form.Parse(string: expression)
                } catch Form.Exception.invalidToken(let token) {
                    print("invalid token: \(token)")
                    break
                } catch Form.Exception.General(let error) {
                    print("\(error)")
                    break
                } catch {
                    expression += "\n"
                    continue
                }

                let eval = try form.eval(global: global)
                if case .Undefined = eval { break }
                print(eval.description)
            }
            break
        }
    } catch {
        print(error)
    }
}

