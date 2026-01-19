import XCTest
@testable import PygmentsSwift

final class JsonLdLexerTests: XCTestCase {
    func testJsonLdLexingKeywords() {
        let lexer = JsonLdLexer()
        let input = """
        {
          "@context": {"name": "http://schema.org/name"},
          "@id": "http://example.com",
          "name": "Alice"
        }
        """

        let tokens = lexer.getTokens(input)

        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "\"@context\"" }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "\"@id\"" }))

        // Non-keyword object keys remain Name.Tag.
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Tag") && $0.value == "\"name\"" }))

        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }
}
