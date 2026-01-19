import XCTest
@testable import PygmentsSwift

final class RegexLexerFeatureTests: XCTestCase {
    func testUsingActionDelegatesLexing() {
        final class DigitLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\d+", action: .token(.number.child("Integer")))),
                        .rule(Rule("\\s+", action: .token(.whitespace))),
                    ]
                ]
            }
        }

        final class WrapperLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("nums:\\s*(\\d+(?:\\s+\\d+)*)", action: .using(DigitLexer.self, stack: ["root"]))),
                    ]
                ]
            }
        }

        let lexer = WrapperLexer()
        let tokens = lexer.getTokens("nums: 12 34")

        // Expect DigitLexer tokens to be emitted.
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "12" }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "34" }))
    }

    func testDefaultTransitionPopsStateWithoutConsuming() {
        final class DefaultPopLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\{", action: .token(.punctuation), newState: .ops([.push("inner")]))),
                        .rule(Rule("[a-zA-Z_]+", action: .token(.name))),
                    ],
                    "inner": [
                        .default(.ops([.pop]))
                    ]
                ]
            }
        }

        let lexer = DefaultPopLexer()
        let tokens = lexer.getTokens("{abc")

        // After consuming '{', inner state default-pops, then 'abc' is lexed by root.
        XCTAssertTrue(tokens.contains(where: { $0.type == .punctuation && $0.value == "{" }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "abc" }))
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }
}
