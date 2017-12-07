//
//  Form.swift
//  Scheme
//
//  Created by Manuel Broncano on 6/25/17.
//  Copyright © 2017 Manuel Broncano. All rights reserved.
//

import Foundation

public class Pair: CustomStringConvertible, Equatable {
    static var count = 0
    var debug: Int

    deinit { Pair.count -= 1 }

    var car: Form
    var cdr: Form

    init(car: Form = .Null, cdr: Form = .Null) {
        self.car = car
        self.cdr = cdr

        debug = Pair.count
        Pair.count += 1
    }

    public var description: String {
        switch (car, cdr) {
        case (.Null, .Null):
            return "()"
        case (_, .Null):
            return "\(car)"
        case (_, let .Pointer(list)):
            return "\(car) \(list)"
        default:
            return "\(car) . \(cdr)"
        }
    }

    //! MARK: Equatable

    // note this won't correctly deal with cyclic graphs
    public static func ==(lhs: Pair, rhs: Pair) -> Bool {
        return (lhs.car == rhs.car) && (lhs.cdr == rhs.cdr)
    }
}

public indirect enum Form: CustomStringConvertible,
                           ExpressibleByStringLiteral,
                           Sequence,
                           Equatable {

    //! basic form transform
    public typealias BuiltinFunction = (Form) throws -> Form

    case Undefined
    case Null
    case Symbol(String)
    case StringType(NSString)
    case CharacterType(Character)
    case Number(Decimal)
    case Boolean(Bool)
    case Function(BuiltinFunction)
    case Closure(Form, Form, Form) // formal, body, frame
    case Continuation([(Form, Form, Form)]) // ([form, frame, eval)]
    case Transformer(Form, Form) // keyword, pattern list
    case Pointer(Pair)

    // remove this below to debug
    func log(_ string: String) {
//        print(string)
    }

    //! MARK: CustomStringConvertible

    public var description: String {
        switch self {
        case .Undefined:
            return "#<undefined>"
        case .Null:
            return "()"
        case .Function:
            return "<#function>"
        case let .Transformer(key, pattern):
            return "<#transformer: \(key) \(pattern )>"
        case let .Closure(formal, body, _):
            return "<#closure \(formal), \(body)>"//, \(frame)>"
        case let .Pointer(list):
            return "(\(list))"
        case let .Boolean(bool):
            return bool ? "#t" : "#f"
        case let .Symbol(string):
            return "\(string)"
        case let .Number(number):
            return "\(number)"
        case .Continuation:
            return "#<continuation>"
        case let .StringType(string):
            return "\"\(string)\""
        case let .CharacterType(character):
            return character == "\n" ? "#\\newline" :
                   character == " " ? "#\\space" :
                   "#\\\(character)"
        }
    }

    //! MARK: Basic accessors

    var car: Form {
        guard case let .Pointer(list) = self else { return .Null }
        return list.car
    }

    var cdr: Form {
        guard case let .Pointer(list) = self else { return .Null }
        return list.cdr
    }

    //! MARK: Operators

    // a + b -> (a . b)
    static func +(lhs: Form, rhs: Form) -> Form {
        return .Pointer(Pair(car: lhs, cdr: rhs))
    }

    // (a b c) << d -> (a b c d)
    // <#undef> << d -> (d)
    // '() << d -> (d)
    static func +=(lhs: inout Form, rhs: Form) {
        lhs = lhs << rhs
    }

    // (a b c) << d -> (a b c d)
    // <#undef> << d -> (d)
    // '() << d -> (d)
    static func <<(lhs: Form, rhs: Form) -> Form {
        let new = Form.Pointer(Pair(car: rhs))
        switch lhs {
        case .Undefined, .Null:
            return new
        default:
            return lhs.reversed().reduce(new) { $1.car + $0 }
        }
    }

    // deep copy
    public func copy() -> Form {
        return self.reversed().reduce(.Null) { acc, cur in
            // improper list: copy the last pair, ignore the tail
            guard case .Pointer = cur.cdr
                else { return cur.car + cur.cdr }

            // trivial case: current car appended to the tail
            guard case .Pointer = cur.car
                else { return cur.car + acc }

            // recursive case: copy the list and append the tail
            return cur.car.copy() + acc
        }
    }

    //! MARK: Parsing

    enum Exception: Error {
        case Parse(String)
        case General(String)
        case invalidToken(String)
    }

    // we are slightly more tolerant than r5rs regarding identifiers, and miss some other constructs
    // https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_9.html#SEC72
    static func Tokenize(string: String) throws -> [String] {
        let special = "!$%&*/:<=>?^_~+\\-@a-zA-Z"
        let number = "[+-]?\\d+\\.?\\d*"
        let identifier = "[\(special)][\(special)\\d]*"
        let boolean = "#[tf]"
        let character = "#\\\\space|#\\\\newline|#\\\\.{1}"
        let literal = "\"(?:[^\\\\\"]|\\.)*\""
        let ellipsis = "\\.{3}"
        let token = "\(identifier)|\(ellipsis)|\(boolean)|\(number)|\(character)|\(literal)|[()'.]"
        // let delimiter = "(?=[();\"\\s])"
        let pattern = "\(token)|(;.*$)|([^\\s]+)"

        let regex = try NSRegularExpression(pattern: pattern, options: [])
        let range = NSMakeRange(0, string.count)
        let result = try regex.matches(in: string, options: [], range: range).map { (match) -> String in
            // invalid token
            guard match.range(at: 2).location == NSNotFound else {
                let token = (string as NSString).substring(with: match.range(at: 2))
                throw Exception.invalidToken(token) }

            // skip comments
            guard match.range(at: 1).location == NSNotFound else { return "" }

            let range = match.range(at: 0)
            return (string as NSString).substring(with: range)
            }.filter { $0.count > 0 } // remove empty strings

        return result
    }

    // returns a list of program statements
    static public func Parse(string: String) throws -> Form {
        let tokens = try Form.Tokenize(string: string).reversed()
        let stack = try tokens.reduce([Form.Null]) { stack, atom in
            var stack = stack
            let form: Form
            if atom == ")" {
                form = .Null

            } else if atom == "(" {
                guard let list = stack.popLast(), let last = stack.popLast()
                    else { throw Exception.Parse("Non-matching parenthesis found") }

                form = list + last

            } else {
                guard let last = stack.popLast()
                    else { throw Exception.Parse("Non-matching parenthesis found") }

                if atom == "'" {
                    form = (.Symbol("quote") + (last.car + .Null)) + last.cdr
                } else if atom == "." {
                    form = last.car
                } else if let _ = Float(atom), let number = Decimal(string: atom) {
                    form = .Number(number) + last
                } else if atom == "#t" {
                    form = .Boolean(true) + last
                } else if atom == "#f" {
                    form = .Boolean(false) + last
                } else if atom.count > 2, atom.dropLast(atom.count - 2) == "#\\" {
                    let char = atom.dropFirst(2)
                    if char.count == 1 {
                        form = Form.CharacterType(char.first!) + last
                    } else if char == "space" {
                        form = Form.CharacterType(Character.init(" ")) + last
                    } else if char == "newline" {
                        form = Form.CharacterType(Character.init("\n")) + last
                    } else {
                        throw Exception.invalidToken(atom)
                    }
                } else if atom.first == "\"" {
                    form = .StringType(atom as NSString) + last
                } else {
                    form = .Symbol(atom) + last
                }
            }
            return stack + [form]
        }

        guard let result = stack.last, stack.count == 1
            else { throw Exception.Parse("Non-matching parenthesis found") }

        return result
    }

    //! MARK: ExpressibleByStringLiteral

    public typealias StringLiteralType = String

    public init(stringLiteral value: String) {
        do { self = try Form.Parse(string: value) } catch { self = .Undefined }
    }

    //! MARK: Accessors

    public func asSymbol() throws -> String {
        guard case let .Symbol(string) = self
            else { throw Exception.General("Must be a symbol: \(self)") }

        return string
    }

    public func asNumber() throws -> Decimal {
        guard case let .Number(number) = self
            else { throw Exception.General("Must be a number: \(self)") }

        return number
    }

    public func asBoolean() throws -> Bool {
        guard case let .Boolean(bool) = self
            else { throw Exception.General("Must be a boolean: \(self)") }

        return bool
    }

    public func asPair() throws -> Pair {
        guard case let .Pointer(list) = self
            else { throw Exception.General("Must be a pair: \(self)") }

        return list
    }

    //! MARK: Predicates

    // everything but #f is true
    public func isFalse() -> Bool {
        guard case let .Boolean(bool) = self else { return false }
        return !bool
    }

    public func isNull() -> Bool {
        guard case .Null = self else { return false }
        return true
    }

    public func isPair() -> Bool {
        guard case .Pointer = self else { return false }
        return true
    }

    public func isUndefined() -> Bool {
        guard case .Undefined = self else { return false }
        return true
    }

    //! MARK: Sequence

    // (a b c) => (a b c), (b c), (c), ()
    // (a b . c) => (a b . c), (b . c), (c), ()
    public func makeIterator() -> AnyIterator<Form> {
        var form: Form = self
        return AnyIterator {
            if case .Null = form { return nil }
            defer { form = form.cdr }
            return form
        }
    }

    // (a b c) => (c)
    // (a b . c) => (c)
    public var last: Form {
        return self.reduce(.Null) { $1 }
    }

    // () => 0
    // (a b c) => 3
    // (a b . c) => 3
    // (a (b . c)) => 2
    public var length: Int {
        return self.reduce(0, { acc, _ in acc + 1 })
    }

    //! MARK: Equatable

    // implements the 'equal?' function
    public static func ==(lhs: Form, rhs: Form) -> Bool {
        let res: Bool
        switch (lhs, rhs) {
        case (.Null, .Null):
            res = true
        case (let .Closure(_, lhs, _), let .Closure(_, rhs, _)):
            res = lhs == rhs
        case (let .Number(lhs), let .Number(rhs)):
            res = lhs == rhs
        case (let .Symbol(lhs), let .Symbol(rhs)):
            res = lhs == rhs
        case (let .Boolean(lhs), let .Boolean(rhs)):
            res = lhs == rhs
        case (let .Pointer(lhs), let .Pointer(rhs)):
            res = lhs == rhs
        case (let .StringType(lhs), let .StringType(rhs)):
            res = lhs == rhs
        default:
            res = false
        }
        return res
    }

    // implements the 'eqv?' function
    public static func ===(lhs: Form, rhs: Form) -> Bool {
        let res: Bool
        switch (lhs, rhs) {
        case (let .Pointer(lhs), let .Pointer(rhs)):
            res = lhs === rhs
        case (let .StringType(lhs), let .StringType(rhs)):
            res = lhs === rhs
        default:
            res = lhs == rhs
        }
        return res
    }

    //! MARK: Syntax helpers

    //! creates a new list by applying a function to each item in the list
    func map(_ op: BuiltinFunction) throws -> Form {
        return try self.reversed().reduce(.Null, { acc, cur in try op(cur) + acc} )
    }

    // recursive map
    func rmap(_ op:(Form, Form) -> Form) -> Form {
        return self.reversed().reduce(.Null) { acc, cur in
            if case .Pointer = cur.car {
                let res = cur.car.rmap(op)
                return res + acc
            }

            return op(acc, cur)
        }
    }

    // ((x 1)(y 2)(z 3)) -> ((x y z) . (1 2 3))
    func splice() throws -> Form {
        return try self.map { $0.car.car } + self.map { $0.car.cdr.car }
    }
}

//! MARK: Collection (unused)

/*
extension Form: Collection {
    public typealias Index = Int
    public typealias SubSequence = Slice<Form>

    public func index(after i: Int) -> Int {
        return i + 1
    }

    public var startIndex: Int {
        return 0
    }

    public var endIndex: Int {
        return self.reduce(0, { acc, _ in acc + 1})
    }

    public subscript(position: Int) -> Form {
        get { return self.enumerated().first(where: { $0.0 == position})?.1 ?? .Undefined }
    }
}
*/

