import XCTest
@testable import PygmentsSwift

final class ElmLexerTests: XCTestCase {
    func testElmBasics() {
        let lexer = ElmLexer()
        let input = """
        -- comment
        module Main exposing (fib)

        fib n =
            if n < 2 then
                n
            else
                fib (n - 1) + fib (n - 2)
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "then" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
