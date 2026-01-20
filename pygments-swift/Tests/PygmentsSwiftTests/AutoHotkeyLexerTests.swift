import XCTest
@testable import PygmentsSwift

final class AutoHotkeyLexerTests: XCTestCase {
    func testAutoHotkeyBasics() {
        let lexer = AutoHotkeyLexer()
        let input = #"""
        ; comment
        ^j::
        MsgBox, "Hello"
        return
        /* multi */
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
