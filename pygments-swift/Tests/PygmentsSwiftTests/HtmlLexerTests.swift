import XCTest
@testable import PygmentsSwift

final class HtmlLexerTests: XCTestCase {
    func testHtmlBasics() {
        let lexer = HtmlLexer()
        let input = """
        <!doctype html>
        <html lang=\"en\">
          <body>
            <h1 class=\"t\">Hello</h1>
            <!-- comment -->
          </body>
        </html>
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Tag") && $0.value == "html" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Attribute") && $0.value == "lang" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
