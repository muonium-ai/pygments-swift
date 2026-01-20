import XCTest
@testable import PygmentsSwift

final class SqlLexerTests: XCTestCase {
    func testSqlBasics() {
        let lexer = SqlLexer()
        let input = """
        -- comment
        SELECT n, expected FROM fib_case WHERE n >= 10 ORDER BY n;
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.uppercased() == "SELECT" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == ">=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
