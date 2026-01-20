import XCTest
@testable import PygmentsSwift

final class AwkLexerTests: XCTestCase {
    func testAwkBasics() {
        let lexer = AwkLexer()
        let input = """
        # comment
        function fib(n) { if (n < 2) return n; return fib(n-1)+fib(n-2) }
        BEGIN { print fib(10) }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "BEGIN" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
