import XCTest
@testable import PygmentsSwift

final class JavaScriptLexerTests: XCTestCase {
    func testJavaScriptLexingBasics() {
        let lexer = JavaScriptLexer()
        let input = """
        // comment
        const x = 42;
        function f(a) { return `hi ${a}`; }
        """

        let tokens = lexer.getTokens(input)

        let summary = tokens.prefix(80).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)

        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
