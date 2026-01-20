import XCTest
@testable import PygmentsSwift

final class CddlLexerTests: XCTestCase {
    func testCddlBasics() {
        let lexer = CddlLexer()
        let input = #"""
        ; comment
        person = {
          name: tstr,
          age: uint,
        }
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
