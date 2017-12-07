//
//  Test.swift
//  Test
//
//  Created by Manuel Broncano on 6/25/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import XCTest

class Test: XCTestCase {
    var global = environment()

    override func setUp() {
        super.setUp()
    }
    
    override func tearDown() {
        super.tearDown()
    }

    func testIter() {
        let test = [
            ("0", "0"),
            ("(+ (* 3 4) (* 5 6))", "42"),
            ("(+ 0 (+ 1 2 (* 3 4) 5 6) 7 8)", "41"),
            ("(list 1 2 3)", "(1 2 3)"),
            ("(length (cdr '(0 1 2)))", "2"),
            ("'a", "a"),
            ("(null? '())", "#t"),
            ("(null? 'a)", "#f"),
            ("(pair? '(1 2 3))", "#t"),
            ("(pair? 1)", "#f"),
            ("(pair? 1)", "#f"),
            ("(eqv? 'a 'a)", "#t"),
            ("(eqv? 'a 'b)", "#f"),
            ("(eqv? 2 2)", "#t"),
            ("(eqv? (cons 1 2) (cons 1 2))", "#f"),
            ("(eqv? (lambda () 1) (lambda () 2))", "#f"),
            ("(eqv? #f 'nil)", "#f"),
            ("(eqv? \"asdf\" \"asdf\")", "#f"),
            ("(equal? \"asdf\" \"asdf\")", "#t"),
            ("(let ((x '(1 2 3))(y '(4 5 6))) (cons x y))", "((1 2 3) 4 5 6)"),
            ("(let ((p (lambda (x) x))) (eqv? p p))", "#t"),
            ("(let ((x 2) (y 3)) (* x y))", "6"),
            ("(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))", "35"),
            ("(cons 1 '())", "(1)"),
            ("(cons '() 1)", "(() . 1)"),
            ("(if (> 2 3) 'true 'false)", "false"),
            ("(begin (define a (+ 2 3)) a)", "5"),
            ("(begin (define a 666) a)", "666"),
            ("(cons 1 2)", "(1 . 2)"),
            ("((lambda x x) 1 2 3)", "(1 2 3)"), // list
            ("((lambda (x . y) x) 1 2 3)", "1"), // car
            ("((lambda (x . y) y) 1 2 3)", "(2 3)"), // cdr
            ("(* 3 (call/cc (lambda (k) (+ 1 2))))", "9"),
            ("(* 3 (call/cc (lambda (k) (+ 1 (k 2)))))", "6"),
            ("(let ((x 5)) (define foo (lambda (y) (bar x y))) (define bar (lambda (a b) (+ (* a b) a))) (foo (+ x 3)))", "45"),
            ]

        for (input, output) in test {
            do {
                let result = try Form.Parse(string: input).eval(global: global).description
                XCTAssertEqual(result, output, input)
            } catch {
                XCTFail("\(error) - \(input)")
            }
        }

    }

    func testFib() {
        let fib_test = """
            (define (fib n)
                (define (fib-iter a b count)
                    (if (= count 0)
                        b
                        (fib-iter (+ a b) a (+ count -1))))
                (fib-iter 1 0 n))
            (fib 100)
        """
        XCTAssertEqual(NSDecimalNumber(decimal: try Form.Parse(string: fib_test).eval(global: global).asNumber()).doubleValue, 354224848179261915075, accuracy: 1e-5)
    }

    func evalNumber(_ input: String) throws -> Double {
        let eval = try Form.Parse(string: input).eval(global: global)
        return try NSDecimalNumber(decimal: eval.asNumber()).doubleValue
    }

    func evalSymbol(_ input: String) throws -> String {
        let eval = try Form.Parse(string: input).eval(global: global)
        return try eval.asSymbol()
    }

    func evalBoolean(_ input: String) throws -> Bool {
        let eval = try Form.Parse(string: input).eval(global: global)
        return try eval.asBoolean()
    }

    func testSet() {
        let lambda_test = """
        (define (make-account balance)
            (define (withdraw amount)
                (if (< amount balance)
                    (begin (set! balance (- balance amount))
                           balance)
                    'Insufficient_funds))
            (define (deposit amount)
                (set! balance (+ balance amount))
                balance)
            (define (dispatch m)
                (cond ((eq? m 'withdraw) withdraw)
                      ((eq? m 'deposit) deposit)
                      (else 'Unknown_request
                m)))
            dispatch)
        """
        XCTAssertNoThrow(try Form.Parse(string: lambda_test).eval(global: global))
        XCTAssertNoThrow(try Form.Parse(string: "(define acc (make-account 50))").eval(global: global))
        XCTAssertNoThrow(try Form.Parse(string: "(define acc2 (make-account 66))").eval(global: global))
        XCTAssertEqual(try evalNumber("((acc 'deposit) 40)"), 90)
        XCTAssertEqual(try evalNumber("((acc2 'withdraw) 65)"), 1)
        XCTAssertEqual(try evalNumber("((acc 'withdraw) 60)"), 30)
        XCTAssertEqual(try evalSymbol("((acc 'withdraw) 50)"), "Insufficient_funds")
        XCTAssertEqual(try evalNumber("((acc2 'deposit) 40)"), 41)
    }

    func testSqrt() {
        let sqrt_test = """
            (define (my-sqrt x)
                (define (abs x)
                    (if (< x 0)
                        (- x)
                        x))
                (define (square x)
                    (* x x))
                (define (average x y)
                    (/ (+ x y) 2))
                (define (good-enough? guess)
                    (< (abs (+ (square guess) (* x -1))) 0.00001))
                (define (improve guess)
                    (average guess (/ x guess)))
                (define (sqrt-iter guess)
                    (if (good-enough? guess)
                        guess
                        (sqrt-iter (improve guess))))
            (sqrt-iter 1.0))
            (my-sqrt 2)
        """
        XCTAssertEqual(NSDecimalNumber(decimal: try Form.Parse(string: sqrt_test).eval(global: global).asNumber()).doubleValue, sqrt(2), accuracy: 1e-5)
    }

    func testCallCC() {
        let code = """
            (define-syntax my-cond
                (syntax-rules (else)
                    ((_) (if #f 'not-reached))
                    ((_ (else . body))
                        (begin . body))
                    ((_ (bool-expression . body) . rest)
                        (if bool-expression
                            (begin . body)
                            (my-cond . rest)))))

            (define (find-leaf obj tree)
                (call/cc
                    (lambda (cc)
                        (letrec ((iter
                            (lambda (tree)
                                (my-cond
                                    ((null? tree) #f)
                                    ((pair? tree)
                                        (iter (car tree))
                                        (iter (cdr tree)))
                                    (else
                                        (if (eqv? obj tree)
                                            (cc obj)))))))
                                            (iter tree)))))
        """
        XCTAssertNoThrow(try Form.Parse(string: code).eval(global: global))
        XCTAssertEqual(try evalNumber("(find-leaf 7 '(1 (2 3) 4 (5 (6 7))))"), 7)
        XCTAssertEqual(try evalSymbol("(find-leaf 'x '(a (2 b) 4 (5 (x 7))))"), "x")
        XCTAssertFalse(try evalBoolean("(find-leaf 9 '(1 (2 3) 4 (5 (6 7))))"))

    }

    func testParse() {
        let test: [(String, String?)] = [
            ("(+ 1 2)", nil),
            ("(+ 1 2 (3 4) . a)", nil),
            ("(#t #f 123)", nil),
            ("()", nil),
            ("(a . b)", nil),
            ("'(a . b)", "(quote (a . b))"),
            ]

        test.forEach {
            let (input, output) = $0
            do {
                let form = try Form.Parse(string: input).car
                XCTAssertEqual(form.description, output ?? input)
            } catch {
                XCTFail(error.localizedDescription)
            }
        }
    }

    func testMacro() {

        let let_test = """
            (define-syntax my-let
              (syntax-rules ()
                ((my-let ((name val) ...) body1 body2 ...)
                 ((lambda (name ...) body1 body2 ...)
                  val ...))
                ((my-let tag ((name val) ...) body1 body2 ...)
                 ((letrec ((tag (lambda (name ...)
                                  body1 body2 ...)))
                    tag)
                  val ...))))

            (define-syntax or
                (syntax-rules ()
                    ((or) #f)
                    ((or test) test)
                    ((or test1 test2 ...) (my-let ((x test1)) (if x x (or test2 ...))))))
        """
        XCTAssertNoThrow(try Form.Parse(string: let_test).eval(global: global))

        let test = [
            ("(my-let ((x '(1 2 3))(y '(4 5 6))) (cons x y))", "((1 2 3) 4 5 6)"),
            ("(my-let ((p (lambda (x) x))) (eqv? p p))", "#t"),
            ("(my-let ((x 2) (y 3)) (* x y))", "6"),
            ("(my-let ((x 2) (y 3)) (my-let ((x 7) (z (+ x y))) (* z x)))", "35"),
            ("(or)", "#f"),
            ("(or (< 2 3))", "#t"),
            ("(or (> 2 3) 'less)", "less"),
            ("(or (> 2 3) (> 4 5) 'more)", "more"),
            ("(or (> 2 3) (> 4 5) (= 6 7) 'dunno 'other)", "dunno")
        ]
        for (input, output) in test {
            do {
                let result = try Form.Parse(string: input).eval(global: global).description
                XCTAssertEqual(result, output, input)
            } catch {
                XCTFail("\(error) - \(input)")
            }
        }
    }

    func testMacro2() {
        let cond_test = """
            (define-syntax my-cond
                (syntax-rules (else)
                    ((_) (if #f 'not-reached))
                    ((_ (else . body))
                        (begin . body))
                    ((_ (bool-expression . body) . rest)
                        (if bool-expression
                            (begin . body)
                            (my-cond . rest)))))
        """
        XCTAssertNoThrow(try Form.Parse(string: cond_test).eval(global: global))
        XCTAssertEqual(try Form.Parse(string: "(my-cond)").eval(global: global).description, "#<undefined>")
        XCTAssertEqual(try Form.Parse(string: "(my-cond ((> 3 4) 'more) ((< 3 4) 'less))").eval(global: global).description, "less")
    }

    func testQueens() {
        let queens_test = """
            (define-syntax my-or
                (syntax-rules ()
                    ((my-or) #f)
                    ((my-or test) test)
                    ((my-or test1 test2 ...) (let ((x test1)) (if x x (my-or test2 ...))))))

            (define (my-abs x)
                (if (< x 0) (- x) x))

            (define (cadr x)
                (car (cdr x)))

            (define (attack? q1 q2)
                (my-or (= (car q1) (car q2))
                    (= (cadr q1) (cadr q2))
                    (= (my-abs (- (car q1) (car q2)))
                       (my-abs (- (cadr q1) (cadr q2))))))

            (define (safe? q qs)
                (cond ((null? qs) #t)
                      ((attack? q (car qs)) #f)
                      (else (safe? q (cdr qs)))))

            (define (queens n)
                (let queen ((n n) (x 1) (y 1) (qs '()) (qss '()))
                    (cond ((< n x) (cons (reverse qs) qss))
                          ((< n y) qss)
                          ((safe? (list x y) qs)
                            (queen n x (+ y 1) qs
                                (queen n (+ x 1) 1
                                    (cons (list x y) qs) qss)))
                          (else (queen n x (+ y 1) qs qss)))))
        """
        XCTAssertNoThrow(try Form.Parse(string: queens_test).eval(global: global))
        XCTAssertEqual(try Form.Parse(string: "(queens 1)").eval(global: global).description, "(((1 1)))")
        XCTAssertEqual(try Form.Parse(string: "(queens 2)").eval(global: global).description, "()")
        XCTAssertEqual(try Form.Parse(string: "(queens 3)").eval(global: global).description, "()")
        XCTAssertEqual(try Form.Parse(string: "(queens 4)").eval(global: global).description, "(((1 3) (2 1) (3 4) (4 2)) ((1 2) (2 4) (3 1) (4 3)))")
    }
}
