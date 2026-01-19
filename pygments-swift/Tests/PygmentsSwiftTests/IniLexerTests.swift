import XCTest
@testable import PygmentsSwift

final class IniLexerTests: XCTestCase {
    func testIniLexing() {
        let lexer = IniLexer()
        let input = """
        [core]
        key = value
        ; comment
        """

        let tokens = lexer.getTokens(input)

        // Basic sanity checks (we don't assert the full stream yet).
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }))

        // Ensure we emit at least one newline whitespace token.
        XCTAssertTrue(tokens.contains(where: { $0.type == .whitespace && $0.value == "\n" }))
    }
}
