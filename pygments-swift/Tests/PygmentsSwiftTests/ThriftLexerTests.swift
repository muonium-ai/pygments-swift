import XCTest
@testable import PygmentsSwift

final class ThriftLexerTests: XCTestCase {
    func testThriftBasics() {
        let lexer = ThriftLexer()
        let input = """
        // comment
        namespace swift fib
        struct Item { 1: required i32 n, 2: optional string label }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "struct" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
