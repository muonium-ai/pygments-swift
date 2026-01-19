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

    func testUsingThisSingleStateIsRootPrefixed() {
        final class UsingThisStackLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        // Delegate whole input to `inner`.
                        // Corner case: in Pygments, `state="inner"` implies stack (root, inner).
                        .rule(Rule("\\{\\{x\\}\\}y", action: .usingThis(stack: ["inner"]))),
                        .rule(Rule("y", action: .token(.keyword))),
                        .rule(Rule("\\n", action: .token(.whitespace))),
                    ],
                    "inner": [
                        .rule(Rule("\\{\\{", action: .token(.punctuation))),
                        .rule(Rule("x", action: .token(.name))),
                        // Pop back to root before consuming trailing 'y'.
                        .rule(Rule("\\}\\}", action: .token(.punctuation), newState: .ops([.pop]))),
                        .rule(Rule("y", action: .token(.name))),
                        .rule(Rule("\\n", action: .token(.whitespace))),
                    ]
                ]
            }
        }

        let lexer = UsingThisStackLexer()
        let tokens = lexer.getTokens("{{x}}y")

        // If `usingThis(stack: ["inner"])` does NOT implicitly prefix root,
        // the pop in inner would be a no-op (stack count == 1) and 'y' would be lexed as Name.
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "y" }))
        XCTAssertFalse(tokens.contains(where: { $0.type == .name && $0.value == "y" }))
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }

    func testDefaultTransitionPopsStateWithoutConsuming() {
        final class DefaultPopLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\n", action: .token(.whitespace))),
                        .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
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

    func testCombinedPushMergesStates() {
        final class CombinedLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\n", action: .token(.whitespace))),
                        .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                        .rule(Rule("\\{", action: .token(.punctuation), newState: .ops([.pushCombined(["inner", "close"])])))
                    ],
                    "inner": [
                        .rule(Rule("a", action: .token(.name))),
                        .rule(Rule("b", action: .token(.keyword)))
                    ],
                    "close": [
                        .rule(Rule("\\}", action: .token(.punctuation), newState: .ops([.pop])))
                    ]
                ]
            }
        }

        let lexer = CombinedLexer()
        let tokens = lexer.getTokens("{ab}")

        XCTAssertEqual(tokens.map { $0.value }.joined(), "{ab}\n")
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "a" }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) && $0.value == "b" }))
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }

    func testInheritMarkerSplicesSuperclassRules() {
        class BaseLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\n", action: .token(.whitespace))),
                        .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                        .rule(Rule("a", action: .token(.name)))
                    ]
                ]
            }
        }

        final class ChildLexer: BaseLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\n", action: .token(.whitespace))),
                        .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                        .rule(Rule("b", action: .token(.keyword))),
                        .inherit,
                        .rule(Rule("c", action: .token(.string)))
                    ]
                ]
            }
        }

        let lexer = ChildLexer()
        let tokens = lexer.getTokens("bac")

        // Ensure we got b (child), a (inherited), c (child).
        let nonWs = tokens.filter { !$0.type.isSubtype(of: .whitespace) && !$0.type.isSubtype(of: .text) }
        XCTAssertEqual(nonWs.map { $0.value }.joined(), "bac")
        XCTAssertEqual(nonWs.map { $0.type.description }, [TokenType.keyword.description, TokenType.name.description, TokenType.string.description])
    }

    func testNoMatchNewlineResetsStateToRoot() {
        final class NewlineStateLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule("\\{", action: .token(.punctuation), newState: .ops([.push("inner")]))),
                        .rule(Rule("a", action: .token(.name))),
                    ],
                    "inner": [
                        // Intentionally no rule for "\\n".
                        .rule(Rule("a", action: .token(.keyword))),
                    ]
                ]
            }
        }

        let lexer = NewlineStateLexer()
        let tokens = lexer.getTokens("{\na")

        // Mirror Pygments RegexLexer behavior: unmatched newline yields Whitespace and resets to root.
        // Therefore the 'a' after the newline should be tokenized by root as Name (not by inner as Keyword).
        XCTAssertTrue(tokens.contains(where: { $0.type == .punctuation && $0.value == "{" }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .whitespace && $0.value == "\n" }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .name && $0.value == "a" }))
        XCTAssertFalse(tokens.contains(where: { $0.type == .keyword && $0.value == "a" }))
    }
}
