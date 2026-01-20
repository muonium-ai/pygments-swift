import XCTest
@testable import PygmentsSwift

final class AbnfLexerTests: XCTestCase {
    func testAbnfBasics() {
        let lexer = AbnfLexer()
        let input = #"""
        ; comment
        rule = ALPHA / DIGIT
        hex = %x41-5A
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
