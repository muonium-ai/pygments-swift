import XCTest
@testable import PygmentsSwift

final class ScssLexerTests: XCTestCase {
    func testScssBasics() {
        let lexer = ScssLexer()
        let input = """
        // comment
        $base: 10;
        .fib { color: #ff00aa; padding: $base * 1px; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "$base" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value.contains("10") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
