import XCTest
@testable import PygmentsSwift

final class AlloyLexerTests: XCTestCase {
    func testAlloyBasics() {
        let lexer = AlloyLexer()
        let input = #"""
        module demo
        sig Person {}
        fact { some Person }
        run { } for 3
        // comment
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
