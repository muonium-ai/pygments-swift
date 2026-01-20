import XCTest
@testable import PygmentsSwift

final class GdscriptLexerTests: XCTestCase {
    func testGdscriptBasics() {
        let lexer = GdscriptLexer()
        let input = """
        # comment
        extends Node

        func fib(n: int) -> int:
            if n < 2:
                return n
            return fib(n - 1) + fib(n - 2)
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "func" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "return" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
