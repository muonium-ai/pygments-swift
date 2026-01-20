import XCTest
@testable import PygmentsSwift

final class TurtleLexerTests: XCTestCase {
    func testTurtleBasics() {
        let lexer = TurtleLexer()
        let input = """
        # comment
        @prefix ex: <http://example.com/> .
        ex:item ex:n 10 ; ex:label "fib" .
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("fib") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
