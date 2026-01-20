import XCTest
@testable import PygmentsSwift

final class ChapelLexerTests: XCTestCase {
    func testChapelBasics() {
        let lexer = ChapelLexer()
        let input = #"""
        proc main() {
          var x = 1;
          // comment
          return;
        }
        "hi";
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
