import XCTest
@testable import PygmentsSwift

final class TypstLexerTests: XCTestCase {
    func testTypstBasics() {
        let lexer = TypstLexer()
        let input = #"""
        = Title
        // comment
        *strong* _emph_
        #let x = 1
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .generic) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
