//: Playground - noun: a place where people can play

import Darwin

func plusTwo(n: Int) -> Int {
    return n + 2
}

Optional.Some(1).map(plusTwo)

Optional.Some(1).map { $0 + 2 }

Optional.None.map { $0 + 2 }

//: ## Functor
func fmap<T, U>(a: T?, f: T -> U) -> U? {
    switch a {
    case .Some(let x): return f(x)
    case .None: return .None
    }
}

infix operator <^> { associativity left }

func <^><T, U>(f: T -> U, a: T?) -> U? {
    return a.map(f)
}

plusTwo <^> Optional.Some(7)

infix operator ~> { associativity left }

func ~><T, U>(a: T?, f: T-> U) -> U? {
    return a.map(f)
}

Optional.Some(7) ~> plusTwo ~> plusTwo

Optional.Some(-1000000.0) ~> log10 ~> log10

//: ## Applicative
extension Optional {
    func apply<U>(f: (Wrapped -> U)?) -> U? {
        switch f {
        case .Some(let someF): return self.map(someF)
        case .None: return .None
        }
    }
}

infix operator <*> { associativity left }

func <*><T, U>(f: (T -> U)?, a: T?) -> U? {
    return a.apply(f)
}

Optional.Some({ $0 + 1 }) <*> Optional.Some(2)

//: Monads

// bind
infix operator >>- { associativity left }

func >>-<T, U>(a: T?, f: T -> U?) -> U? {
    return a.flatMap(f)
}

func half(a: Int) -> Int? {
    return a % 2 == 0 ? a / 2 : .None
}

Optional(3) >>- half

Optional(4) >>- half

Optional.None >>- half

Optional(512) >>- half >>- half >>- half

Optional(511) >>- half >>- half >>- half