import XCTest
@testable import PygmentsSwift

final class PromqlLexerTests: XCTestCase {
    func testPromqlBasics() {
        let lexer = PromqlLexer()
        let input = #"""
        # comment
        sum by (job) (rate(http_requests_total{method="GET"}[5m]))
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "by" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
