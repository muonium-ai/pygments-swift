import XCTest
@testable import PygmentsSwift

final class RegoLexerTests: XCTestCase {
    func testRegoBasics() {
        let lexer = RegoLexer()
        let input = #"""
        package example
        default allow = false

        allow {
          input.user == "bob"
        }
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "package" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
