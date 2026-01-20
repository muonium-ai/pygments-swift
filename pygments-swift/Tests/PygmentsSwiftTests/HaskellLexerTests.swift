import XCTest
@testable import PygmentsSwift

final class HaskellLexerTests: XCTestCase {
    func testHaskellBasics() {
        let lexer = HaskellLexer()
        let input = """
        -- comment
        module Main where

        fib :: Int -> Int
        fib n | n < 2 = n
        fib n = fib (n - 1) + fib (n - 2)
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "->" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
