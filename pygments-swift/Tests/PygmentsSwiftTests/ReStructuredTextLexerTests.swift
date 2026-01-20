import XCTest
@testable import PygmentsSwift

final class ReStructuredTextLexerTests: XCTestCase {
    func testReStructuredTextBasics() {
        let lexer = ReStructuredTextLexer()
        let input = """
        Fibonacci
        =========

        .. note:: Inline literal ``fib(n)`` and *emphasis*.
        .. This is a comment.

        `Link`_
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
