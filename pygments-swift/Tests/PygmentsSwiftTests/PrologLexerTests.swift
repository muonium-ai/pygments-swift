import XCTest
@testable import PygmentsSwift

final class PrologLexerTests: XCTestCase {
    func testPrologBasics() {
        let lexer = PrologLexer()
        let input = """
        % comment
        fib(0, 0).
        fib(N, F) :- N > 1, F is N.
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == ":-" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
