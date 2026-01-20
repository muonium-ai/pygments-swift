import XCTest
@testable import PygmentsSwift

final class CssLexerTests: XCTestCase {
    func testCssBasics() {
        let lexer = CssLexer()
        let input = """
        /* comment */
        .value { color: #0ea5e9; margin: 8px; }
        @media (prefers-color-scheme: dark) { body { color: #fff; } }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Attribute") && $0.value == "color" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased().contains("media") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
