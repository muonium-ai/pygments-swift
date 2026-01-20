import XCTest
@testable import PygmentsSwift

final class PropertiesLexerTests: XCTestCase {
    func testPropertiesBasics() {
        let lexer = PropertiesLexer()
        let input = """
        # comment
        app.name=Fibonacci
        fib.limit:10
        unicode=F\\u0069b
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "app.name" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && ($0.value == "=" || $0.value == ":") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.lowercased().contains("\\u0069") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
