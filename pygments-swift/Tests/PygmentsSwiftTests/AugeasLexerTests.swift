import XCTest
@testable import PygmentsSwift

final class AugeasLexerTests: XCTestCase {
    func testAugeasBasics() {
        let lexer = AugeasLexer()
        let input = #"""
        (* comment *)
        module Test =
        let x = /a.*/
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
