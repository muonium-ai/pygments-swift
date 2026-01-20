import XCTest
@testable import PygmentsSwift

final class NushellLexerTests: XCTestCase {
    func testNushellBasics() {
        let lexer = NushellLexer()
        let input = """
        # comment
        def fib [n:int] {
            if $n < 2 { $n } else { (fib ($n - 1)) + (fib ($n - 2)) }
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "def" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.contains("$n") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
