import XCTest
@testable import PygmentsSwift

final class GraphQLLexerTests: XCTestCase {
    func testGraphQLBasics() {
        let lexer = GraphQLLexer()
        let input = """
        # comment
        query Fib($n: Int!) { fib(n: $n) }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "query" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "$n" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
