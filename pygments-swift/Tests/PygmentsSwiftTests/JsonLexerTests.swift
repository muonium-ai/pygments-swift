import XCTest
@testable import PygmentsSwift

final class JsonLexerTests: XCTestCase {
    func testJsonLexingBasics() {
        let lexer = JsonLexer()
        let input = """
        {
          // comment
          "a": 1,
          "b": 2.5e+2,
          "c": true,
          "d": null,
          "e": "str\\n\\t\\u1234"
        }
        """

        let tokens = lexer.getTokens(input)

        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Tag") && $0.value == "\"a\"" }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .number.child("Integer") && $0.value.contains("1") }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .number.child("Float") }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Constant") && $0.value == "true" }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Constant") && $0.value == "null" }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .comment.child("Single") }))

        // Ensure we don't emit Error for valid JSON.
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }
}
