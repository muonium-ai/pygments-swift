import XCTest
@testable import PygmentsSwift

final class BibTeXLexerTests: XCTestCase {
    func testBibTeXBasics() {
        let lexer = BibTeXLexer()
        let input = #"""
        % comment
        @article{knuth1984,
          title = "Literate Programming",
          year = 1984,
        }
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "article" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
